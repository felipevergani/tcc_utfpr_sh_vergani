# =============================================================================
# 04b_sensibilidade.R  —  Sensibilidade do RSF: nodesize x mtry (elo rmst_cap)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: testar se o RSF esta SUB-TUNADO. A 1a rodada mostrou predicoes
#           ACHATADAS (sd_pred/sd_obs ~0.3) e correlacao fraca (r~0.16-0.27),
#           derrubando o CCC. nodesize=15 pode estar suavizando demais.
#           Varremos nodesize x mtry e medimos, alem de RMSE/CCC/vies:
#             - r  = correlacao de Pearson (obs vs pred)  -> teto do CCC
#             - sd_ratio = sd(pred)/sd(obs)               -> grau de achatamento
# Elo fixo = rmst_cap (venceu no 04). Avaliacao nos NAO-censurados (evento=1).
# Pre-requisito: rodar antes 02_preprocess.R.
# Entrada:  data/processed/perfil_sobrevivencia.rds, meta_preditores.rds
# Saida:    outputs/tabelas/04b_sensibilidade.csv
# NOTA: RSF e caro. Use SUBAMOSTRA para o sweep; confirme o vencedor no 04 completo.
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "survival", "randomForestSRC")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(survival); library(randomForestSRC)
source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
kv       <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))

set.seed(2026)

# --- CONFIG ------------------------------------------------------------------
LIMITES        <- c(30, 100)
NTREE          <- 500
SPLITRULE      <- "logrank"
ROCK_COMO_COMPLETO <- TRUE
HORIZONTE_PCT  <- 0.95
NODESIZES      <- c(3, 15, 40)          # grade de nodesize (menor = arvore mais profunda)
MTRYS          <- c("default", "p3")    # default (sqrt p do rfsrc) vs p/3
SUBAMOSTRA     <- 3000                  # p/ velocidade; NA = todos (lento)
options(rf.cores = parallel::detectCores(), mc.cores = parallel::detectCores())

# --- 1. Dados ----------------------------------------------------------------
secao("1. Carregar dados")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
preditores <- meta$preditores
flags <- grep("^flag_na_", names(perfil), value = TRUE)
feats <- c(preditores, flags)
if (!is.na(SUBAMOSTRA) && SUBAMOSTRA < nrow(perfil)) perfil <- perfil[sample(.N, SUBAMOSTRA)]
p_feats <- length(feats)
kv("Perfis", nrow(perfil)); kv("Features", p_feats)
kv("Grade", sprintf("%d nodesize x %d mtry x %d limites = %d configs",
                    length(NODESIZES), length(MTRYS), length(LIMITES),
                    length(NODESIZES)*length(MTRYS)*length(LIMITES)))

# --- 2. Funcoes (elo rmst_cap) -----------------------------------------------
montar_surv <- function(L) {
  alvo    <- perfil[[paste0("alvo_", L)]]
  atingiu <- as.integer(perfil$tempo_espessura >= L)
  rocha   <- as.integer(perfil$evento_rock == 1)
  evento  <- if (ROCK_COMO_COMPLETO) as.integer(atingiu == 1 | rocha == 1) else atingiu
  data.table(fold = perfil$fold, tempo = alvo, evento = evento)
}
rmst_matrix <- function(S, tt, H) {
  ord <- order(tt); tt <- tt[ord]; S <- S[, ord, drop = FALSE]
  sel <- tt <= H; tt <- tt[sel]; S <- S[, sel, drop = FALSE]
  K <- length(tt); if (K == 0) return(rep(0, nrow(S)))
  area <- rep(tt[1], nrow(S))
  if (K >= 2) area <- area + as.numeric(S[, 1:(K - 1), drop = FALSE] %*% diff(tt))
  area <- area + S[, K] * max(0, H - tt[K])
  as.numeric(area)
}
resolver_mtry <- function(m) if (identical(m, "p3")) max(1L, floor(p_feats / 3)) else NULL

# --- 3. Sweep ----------------------------------------------------------------
secao("2. Sweep nodesize x mtry")
form_de <- function() as.formula(paste("Surv(tempo, evento) ~", paste(feats, collapse = " + ")))
grade <- CJ(limite = LIMITES, nodesize = NODESIZES, mtry = MTRYS, sorted = FALSE)
res <- list()
for (i in seq_len(nrow(grade))) {
  L <- grade$limite[i]; ns <- grade$nodesize[i]; mt <- grade$mtry[i]
  sv  <- montar_surv(L); dat <- cbind(sv, perfil[, ..feats])[is.finite(tempo)]
  form <- form_de(); mtry_v <- resolver_mtry(mt)
  pred <- rep(NA_real_, nrow(dat))
  t0 <- Sys.time()
  for (f in sort(unique(dat$fold))) {
    tr <- which(dat$fold != f); te <- which(dat$fold == f)
    if (dat[tr][evento == 1, .N] < 2) next
    m  <- rfsrc(form, data = as.data.frame(dat[tr]), ntree = NTREE, nodesize = ns,
                splitrule = SPLITRULE, mtry = mtry_v, importance = "none", seed = 2026)
    pr <- predict(m, newdata = as.data.frame(dat[te]))
    H  <- as.numeric(quantile(dat[tr][evento == 1, tempo], HORIZONTE_PCT))
    pred[te] <- rmst_matrix(pr$survival, pr$time.interest, H)
  }
  dat[, pred := pred]
  av <- dat[evento == 1 & is.finite(pred)]
  m  <- metricas_resumo(av$tempo, av$pred)
  res[[i]] <- data.table(limite = L, nodesize = ns, mtry = mt, n = m$n,
                         rmse = m$rmse, ccc = m$ccc, vies = m$vies, r2 = m$r2,
                         r_pearson = cor(av$tempo, av$pred),
                         sd_ratio  = sd(av$pred) / sd(av$tempo),
                         obs_medio = mean(av$tempo), pred_medio = mean(av$pred))
  cat(sprintf("  L=%3d nodesize=%2d mtry=%-7s  rmse=%.0f ccc=%.3f r=%.3f sd_ratio=%.2f  (%.0fs)\n",
              L, ns, mt, m$rmse, m$ccc, cor(av$tempo, av$pred),
              sd(av$pred)/sd(av$tempo), as.numeric(difftime(Sys.time(), t0, units="secs"))))
}

# --- 4. Consolidar -----------------------------------------------------------
secao("3. Resultados")
tab <- rbindlist(res)
setorder(tab, limite, -ccc)
print(tab)
fwrite(tab, file.path(dir_tab, "04b_sensibilidade.csv"))
kv("[tabela salva]", "outputs/tabelas/04b_sensibilidade.csv")
cat("\n  Leitura: se o CCC/r/sd_ratio subirem muito com nodesize menor, o RSF\n",
    "  estava sub-tunado (vale rodar o 04 completo com o vencedor). Se ficarem\n",
    "  parados, o limite e do elo/ framework -> considerar o pivo (RSF de profund.).\n\n")
