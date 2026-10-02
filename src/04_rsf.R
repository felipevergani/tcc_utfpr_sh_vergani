# =============================================================================
# 04_rsf.R  —  Random Survival Forest — ESTOQUE COMO TEMPO (censura do estoque)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Estrutura de sobrevivencia CORRETA para o alvo (estoque de COS em profundidade):
#   tempo  = alvo_L  (estoque cumulativo observado ate min(prof_max, L))
#   evento = 1 se o estoque 0-L esta COMPLETO ; 0 se CENSURADO a direita
# Um perfil tem estoque 0-L COMPLETO (evento=1) se:
#   (i)  atingiu L (prof_max >= L)  -> estoque 0-L observado exatamente, OU
#   (ii) bateu na ROCHA antes de L  -> abaixo do contato litico nao ha COS,
#        logo o 0-L observado JA e o total (NAO e censura a direita).
# Censurado a direita (evento=0) apenas se a amostragem parou antes de L SEM
# rocha (o solo continua abaixo -> estoque subestimado). Isso da ~22% (30cm) e
# ~65% (100cm) de censura, coerente com o documento do TCC (61%/22%).
#
# NOTA (por que NAO 'profundidade + is_rock'): naquela estrutura, is_rock e o
# unico evento (~2% neste dado) e TODO o resto vira censura (98%) -> degenerado.
# Isso e o problema da ESPESSURA (Chen/Westhuizen), nao o do ESTOQUE: um perfil
# que bate rocha e uma observacao COMPLETA de estoque, nao um censurado.
#
# ELO SOBREVIVENCIA->ESTOQUE: 3 agregadores da curva S(s|x) comparados numa
# rodada. Media restrita (rmst) com horizonte no p95 (rmst_cap) foi o melhor; a
# mediana/quantil ficam como diagnostico. Avaliacao nos NAO-censurados (evento=1,
# verdade conhecida), mesma regua do 03.
# Entrada:  data/processed/perfil_sobrevivencia.rds, meta_preditores.rds
# Saida:    outputs/tabelas/04_rsf.csv, 04_rsf_pred.csv ; outputs/figuras/04_*.png
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "survival", "randomForestSRC", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(survival); library(randomForestSRC); library(ggplot2)
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
LIMITES        <- c(30, 100)
NTREE          <- 500
NODESIZE       <- 15
MTRY           <- NA          # NA = default rfsrc (Testar otimização para o melhor mytry, testar DynForest R package)
SPLITRULE      <- "logrank"
ROCK_COMPLETO  <- TRUE        # (ii): rocha antes de L conta como estoque completo
QNIVEL_MEDIANA <- 0.5         # elo mediana: menor s com S(s|x) <= 0.5
QNIVEL_EXTRA   <- 0.4         # elo quantil extra (identificavel sob censura ~60%)
HORIZONTE_PCT  <- 0.95        # elo rmst: horizonte no percentil dos eventos (nao no max)
SUBAMOSTRA     <- NA          # NA = todos; ou N p/ teste rapido
options(rf.cores = parallel::detectCores(), mc.cores = parallel::detectCores())

# --- 1. Dados ----------------------------------------------------------------
secao("1. Carregar dados")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
preditores <- meta$preditores
flags <- grep("^flag_na_", names(perfil), value = TRUE)
feats <- c(preditores, flags)
if (!is.na(SUBAMOSTRA) && SUBAMOSTRA < nrow(perfil)) perfil <- perfil[sample(.N, SUBAMOSTRA)]
kv("Perfis", nrow(perfil)); kv("Features", length(feats)); kv("Dobras", uniqueN(perfil$fold))

# --- 2. Funcoes: estrutura de sobrevivencia + elos ---------------------------
# monta a estrutura de sobrevivencia no limite L: tempo = estoque, evento = completo
montar_surv <- function(L) {
  alvo    <- perfil[[paste0("alvo_", L)]]
  atingiu <- as.integer(perfil$tempo_espessura >= L)          # (i)
  rocha   <- as.integer(perfil$evento_rock == 1)              # (ii)
  evento  <- if (ROCK_COMPLETO) as.integer(atingiu == 1 | rocha == 1) else atingiu
  data.table(id = perfil$id, fold = perfil$fold, tempo = alvo, evento = evento)
}
# media restrita: integral_0^H S(s) ds (horizonte H robusto)
rmst_matrix <- function(S, tt, H) {
  ord <- order(tt); tt <- tt[ord]; S <- S[, ord, drop = FALSE]
  sel <- tt <= H; tt <- tt[sel]; S <- S[, sel, drop = FALSE]
  K <- length(tt); if (K == 0) return(rep(0, nrow(S)))
  area <- rep(tt[1], nrow(S))
  if (K >= 2) area <- area + as.numeric(S[, 1:(K - 1), drop = FALSE] %*% diff(tt))
  area <- area + S[, K] * max(0, H - tt[K]); as.numeric(area)
}
# quantil: menor s com S(s|x) <= nivel (robusto a outliers)
quantil_link <- function(S, tt, nivel) {
  ord <- order(tt); tt <- tt[ord]; S <- S[, ord, drop = FALSE]
  below <- S <= nivel
  idx <- max.col(below, ties.method = "first")
  has <- below[cbind(seq_len(nrow(S)), idx)]
  out <- tt[idx]; out[!has] <- NA_real_; out
}

# --- 3. CV espacial com RSF por limite (3 elos numa rodada) ------------------
secao("2. RSF (estoque como tempo) + CV espacial")
col_map <- c(mediana = "pred_mediana", quantil = "pred_quantil", rmst_cap = "pred_rmst")
resultados <- list(); predicoes <- list()
for (L in LIMITES) {
  subsecao(sprintf("Limite %d cm", L))
  sv  <- montar_surv(L)
  dat <- cbind(sv, perfil[, ..feats])[is.finite(tempo)]
  kv("Perfis usados", nrow(dat))
  kv("Completo (evento=1)", sprintf("%d (%s)", dat[evento==1,.N], pct(mean(dat$evento))))
  kv("Censurado a direita (evento=0)", sprintf("%d (%s)", dat[evento==0,.N], pct(mean(dat$evento==0))))

  form <- as.formula(paste("Surv(tempo, evento) ~", paste(feats, collapse = " + ")))
  p_med <- p_ext <- p_rmst <- rep(NA_real_, nrow(dat))
  for (f in sort(unique(dat$fold))) {
    tr <- which(dat$fold != f); te <- which(dat$fold == f)
    if (dat[tr][evento == 1, .N] < 2) next
    m  <- rfsrc(form, data = as.data.frame(dat[tr]), ntree = NTREE, nodesize = NODESIZE,
                splitrule = SPLITRULE, mtry = if (is.na(MTRY)) NULL else MTRY,
                importance = "none", seed = 2026)
    pr <- predict(m, newdata = as.data.frame(dat[te]))
    S  <- pr$survival; tt <- pr$time.interest
    H  <- as.numeric(quantile(dat[tr][evento == 1, tempo], HORIZONTE_PCT))
    p_med[te]  <- quantil_link(S, tt, QNIVEL_MEDIANA)
    p_ext[te]  <- quantil_link(S, tt, QNIVEL_EXTRA)
    p_rmst[te] <- rmst_matrix(S, tt, H)
  }
  dat[, `:=`(pred_mediana = p_med, pred_quantil = p_ext, pred_rmst = p_rmst)]

  aval <- dat[evento == 1]
  linhas <- rbindlist(lapply(names(col_map), function(elo) {
    col <- col_map[[elo]]; ok <- is.finite(aval[[col]]); o <- aval$tempo[ok]; p <- aval[[col]][ok]
    cbind(elo = elo, data.table(metricas_resumo(o, p)),
          r_pearson = if (sd(p) > 0) cor(o, p) else NA_real_,
          sd_ratio  = sd(p) / sd(o),
          cobertura = round(mean(is.finite(aval[[col]])), 3),
          pred_medio = round(mean(p)))
  }))
  linhas <- cbind(limite = L, obs_medio = round(mean(aval$tempo)), linhas)
  print(linhas); resultados[[as.character(L)]] <- linhas
  predicoes[[as.character(L)]] <- cbind(limite = L,
    dat[, .(id, fold, tempo, evento, pred_mediana, pred_quantil, pred_rmst)])

  melhor <- linhas[which.min(rmse), elo]; colm <- col_map[[melhor]]
  dd <- aval[is.finite(get(colm)), .(tempo, pred = get(colm))]
  g <- ggplot(dd, aes(tempo, pred)) +
    geom_point(alpha = 0.2, size = 0.6, colour = "#1b7837") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "red") +
    labs(title = sprintf("RSF (estoque) — obs vs pred (%d cm, elo=%s)", L, melhor),
         x = "estoque observado (g/m2)", y = "estoque predito (g/m2)") + theme_minimal()
  ggsave(file.path(dir_fig, sprintf("04_rsf_obs_vs_pred_%dcm.png", L)), g, width = 6, height = 6, dpi = 150)
}

# --- 4. Salvar ---------------------------------------------------------------
secao("Resultados consolidados (RSF)")
tab <- rbindlist(resultados); print(tab)
fwrite(tab, file.path(dir_tab, "04_rsf.csv"))
fwrite(rbindlist(predicoes), file.path(dir_tab, "04_rsf_pred.csv"))
kv("[tabela salva]", "outputs/tabelas/04_rsf.csv")
