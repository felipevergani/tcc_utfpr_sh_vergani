# =============================================================================
# 06_gam.R  —  GAM de profundidade variavel (de Sousa Mendes et al., 2025)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Fiel a de Sousa Mendes (2025): modela o ESTOQUE CUMULATIVO diretamente, usando
# a profundidade inferior (profund_inf) como COVARIAVEL FLEXIVEL — sem harmonizar
# as camadas para profundidades fixas via splines. A predicao do estoque 0-L e
# feita avaliando o modelo em profund_inf = L.
#
# Estrutura: nivel de CAMADA (camada_profvar.rds), uma linha por camada.
#   alvo    = soc_stock_gm2_cum_qmap (estoque cumulativo observado na camada)
#   depth   = profund_inf (covariavel flexivel, o cerne do "profundidade variavel")
#   spatial = te(coord_x_utm, coord_y_utm, profund_inf) -> curva de acumulo varia
#             no espaco (perfil-especifica); + s() nas covariaveis + fatores.
# 1 GAM por dobra (mgcv::bam, discrete) -> prediz nos DOIS limites (30 e 100).
# Avaliacao nos NAO-censurados (evento=1), mesma regua do 03/04/05.
# Entrada:  data/processed/camada_profvar.rds, perfil_sobrevivencia.rds, meta_preditores.rds
# Saida:    outputs/tabelas/06_gam.csv, 06_gam_pred.csv ; figuras/06_*.png
# -----------------------------------------------------------------------------
# NOTA: com 143 preditores nao da p/ por s() em todos. Usa-se um CONJUNTO CURADO
#   (config COVS_*). Ajustar k/termos apos ver a saida (como nos scripts anteriores).
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "mgcv", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(mgcv); library(ggplot2)
source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
subsecao <- function(txt) cat("\n-- ", txt, " ", strrep("-", max(0, 70 - nchar(txt))), "\n", sep = "")
kv       <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))
pct      <- function(x) sprintf("%.1f%%", 100 * x)
set.seed(2026)

# --- CONFIG ------------------------------------------------------------------
LIMITES     <- c(30, 100)
ROCK_COMPLETO <- TRUE
# Conjunto de covariaveis contínuas (SEM SoilGrids; terreno + indices de contexto)
COVS_SUAVE  <- c("altitude", "slope", "cti", "hand", "convergence", "roughness",
                 "spi", "elev_stdev", "dev_magnitude", "northness", "eastness", "pcurv",
                 "rockyIndex", "sandyIndex")                                    # s()
COVS_FATOR  <- c("bioma_nome", "lulc")                                          # parametricas
K_DEPTH     <- 12    # base da profundidade
K_SPACE     <- 60    # base do espaco
USAR_TI     <- FALSE # TRUE = interacao 3D profund x espaco (ti); causou nao-convergencia
FAMILIA     <- gaussian()  # identidade: estavel. (link=log estoura c/ estoques grandes.)
NTHREADS    <- parallel::detectCores()
SUBAMOSTRA  <- NA    # NA = todos; ou N perfis p/ teste rapido

# --- 1. Dados ----------------------------------------------------------------
secao("1. Carregar dados")
camada <- readRDS(here("data", "processed", "camada_profvar.rds"))
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
if (!is.na(SUBAMOSTRA) && SUBAMOSTRA < nrow(perfil)) {
  ids <- perfil[sample(.N, SUBAMOSTRA), id]; perfil <- perfil[id %in% ids]; camada <- camada[id %in% ids]
}
# Garante fatores com os MESMOS niveis entre camada e perfil (predicao)
for (v in COVS_FATOR) {
  lv <- union(levels(as.factor(camada[[v]])), levels(as.factor(perfil[[v]])))
  camada[, (v) := factor(get(v), levels = lv)]; perfil[, (v) := factor(get(v), levels = lv)]
}
kv("Camadas (treino)", nrow(camada)); kv("Perfis (predicao)", nrow(perfil))
kv("Dobras", uniqueN(perfil$fold))

# --- 2. Formula GAM (profundidade variavel) ----------------------------------
secao("2. Formula GAM")
# covariaveis com poucos valores unicos (<10) nao suportam s() -> entram como termo linear
n_uni     <- vapply(COVS_SUAVE, function(v) length(unique(camada[[v]][!is.na(camada[[v]])])), integer(1))
suave_ok  <- COVS_SUAVE[n_uni >= 10]
suave_lin <- COVS_SUAVE[n_uni <  10]
if (length(suave_lin)) kv("Poucos valores unicos -> termo linear", paste(suave_lin, collapse = ", "))
sm  <- paste(sprintf("s(%s)", suave_ok), collapse = " + ")
lin <- if (length(suave_lin)) paste(" +", paste(suave_lin, collapse = " + ")) else ""
fac <- paste(COVS_FATOR, collapse = " + ")
termo_ti <- if (USAR_TI)
  sprintf(" + ti(profund_inf, coord_x_utm, coord_y_utm, d=c(1,2), k=c(6,%d))", min(20, K_SPACE)) else ""
form <- as.formula(sprintf(
  "soc_stock_gm2_cum_qmap ~ s(profund_inf, k=%d) + s(coord_x_utm, coord_y_utm, k=%d)%s + %s%s + %s",
  K_DEPTH, K_SPACE, termo_ti, sm, lin, fac))
cat("  ", deparse(form), "\n")

# --- 3. CV espacial: 1 GAM/dobra -> estoque nos dois limites ------------------
secao("3. GAM por dobra (bam, discrete) + predicao em profund_inf = L")
pred30 <- pred100 <- rep(NA_real_, nrow(perfil))
for (f in sort(unique(perfil$fold))) {
  cam_tr <- camada[fold != f]
  t0 <- Sys.time()
  m <- bam(form, data = as.data.frame(cam_tr), family = FAMILIA, discrete = TRUE, nthreads = NTHREADS)
  te <- which(perfil$fold == f)
  for (L in LIMITES) {
    nd <- copy(perfil[te]); nd[, profund_inf := L]
    pr <- as.numeric(predict(m, newdata = as.data.frame(nd), type = "response"))
    pr <- pmax(pr, 0)                                   # estoque nao-negativo
    if (L == 30) pred30[te] <- pr else pred100[te] <- pr
  }
  cat(sprintf("    dobra %2d: treino_camadas=%d teste_perfis=%d  (%.0fs)\n",
              f, nrow(cam_tr), length(te), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
perfil[, `:=`(pred30 = pred30, pred100 = pred100)]

# --- 4. Avaliacao por limite (nao-censurados) --------------------------------
secao("4. Avaliacao (nao-censurados)")
resultados <- list(); predicoes <- list()
for (L in LIMITES) {
  alvo <- perfil[[paste0("alvo_", L)]]
  ev_stock <- as.integer(perfil$tempo_espessura >= L | perfil$evento_rock == 1)
  pcol <- if (L == 30) perfil$pred30 else perfil$pred100
  keep <- is.finite(alvo) & ev_stock == 1 & is.finite(pcol)
  o <- alvo[keep]; p <- pcol[keep]
  res <- data.table(limite = L, n = sum(keep), metricas_resumo(o, p),
                    r_pearson = cor(o, p), sd_ratio = sd(p) / sd(o),
                    obs_medio = round(mean(o)), pred_medio = round(mean(p)))
  print(res); resultados[[as.character(L)]] <- res
  predicoes[[as.character(L)]] <- data.table(limite = L, id = perfil$id[keep], obs = o, pred = p)

  g <- ggplot(data.table(obs = o, pred = p), aes(obs, pred)) +
    geom_point(alpha = 0.2, size = 0.6, colour = "#542788") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "red") +
    labs(title = sprintf("GAM prof. variavel — obs vs pred (%d cm)", L),
         x = "estoque observado (g/m2)", y = "estoque predito (g/m2)") + theme_minimal()
  ggsave(file.path(dir_fig, sprintf("06_gam_obs_vs_pred_%dcm.png", L)), g, width = 6, height = 6, dpi = 150)
}

# --- 5. Salvar ---------------------------------------------------------------
secao("Resultados consolidados (GAM)")
tab <- rbindlist(resultados); print(tab)
fwrite(tab, file.path(dir_tab, "06_gam.csv"))
fwrite(rbindlist(predicoes), file.path(dir_tab, "06_gam_pred.csv"))
kv("[tabela salva]", "outputs/tabelas/06_gam.csv")
