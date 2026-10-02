# =============================================================================
# 03_baseline_rf.R  —  Baseline: Random Forest ingenuo (IGNORA a censura)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: modelo de referencia a ser batido pelos metodos de censura (04-06).
#           RF de regressao prevendo o estoque cumulativo, tratando perfis
#           censurados como se fossem medicoes exatas (o "modelo errado").
# Entrada:  data/processed/perfil_sobrevivencia.rds  (ja imputado no 02)
#           data/processed/meta_preditores.rds
# Saida:    outputs/tabelas/03_baseline_rf.csv
#           outputs/figuras/03_obs_vs_pred_*.png
# Avaliacao: validacao cruzada ESPACIAL (coluna 'fold' do 02) — mesmas dobras
#            para todos os modelos do benchmark.
# -----------------------------------------------------------------------------
# DECISOES:
#  - Cenario 30 cm: perfis com alvo NA (sem camada <= 30) sao descartados (713).
#  - Alvo testado em escala BRUTA e LOG (log1p), pela forte assimetria (ver 01).
#  - Imputacao: usa a tabela ja imputada (mediana+flag) do 02. Refinamento
#    futuro: mover a imputacao para dentro de cada dobra (evitar vazamento).
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "ranger", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(ranger); library(ggplot2)
source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
kv       <- function(k, v) cat(sprintf("  %-40s %s\n", paste0(k, ":"), v))

set.seed(2026)
LIMITES  <- c(30, 100)
NTREE    <- 500

# --- 1. Carregar dados -------------------------------------------------------
secao("1. Carregar dados processados")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
preditores <- meta$preditores
flags      <- grep("^flag_na_", names(perfil), value = TRUE)
feats      <- c(preditores, flags)               # covariaveis + indicadores de faltante
kv("Perfis", nrow(perfil)); kv("Preditores", length(preditores)); kv("Flags de NA", length(flags))
kv("Dobras (fold)", uniqueN(perfil$fold))

# --- 2. Funcao: CV espacial com ranger ---------------------------------------
# Treina em fold != f, prediz em fold == f; retorna vetor de predicoes alinhado.
cv_ranger <- function(dados, y, feats, folds) {
  pred <- rep(NA_real_, nrow(dados))
  mtry <- max(1L, floor(length(feats) / 3))       # p/3 (melhor no 03b_sensibilidade)
  for (f in sort(unique(folds))) {
    tr <- folds != f; te <- folds == f
    m  <- ranger(x = dados[tr, ..feats], y = y[tr], num.trees = NTREE, mtry = mtry,
                 respect.unordered.factors = "order", seed = 2026)
    pred[te] <- predict(m, dados[te, ..feats])$predictions
  }
  pred
}

# --- 3. Loop por limite: metricas em TODOS e em NAO-CENSURADOS ----------------
# 'evento' = estoque 0-L completo (atingiu L OU bateu rocha), igual ao 04_rsf.R.
# A comparacao JUSTA com RSF/IPC-RF/GAM e no subconjunto nao-censurado (verdade
# conhecida). 'todos_vs_alvoL' fica como referencia (enviesada pela censura).
resultados <- list(); predicoes <- list()
for (L in LIMITES) {
  secao(sprintf("Baseline RF — limite %d cm", L))
  alvo_col <- paste0("alvo_", L)
  dat <- perfil[is.finite(get(alvo_col))]         # descarta alvo NA (cenario 30 cm)
  evento <- as.integer(dat$tempo_espessura >= L | dat$evento_rock == 1)
  kv("Perfis usados", nrow(dat))
  kv("Nao-censurados (evento=1)", sprintf("%d (%.1f%%)", sum(evento), 100 * mean(evento)))

  y_raw <- dat[[alvo_col]]
  p_raw <- cv_ranger(dat, y_raw, feats, dat$fold)                 # escala bruta
  p_log <- expm1(cv_ranger(dat, log1p(y_raw), feats, dat$fold))   # escala log

  uc  <- evento == 1
  # met(...): resume as metricas para uma escala e uma mascara de avaliacao
  met <- function(escala, pred, aval, mask) data.table(
    limite = L, escala = escala, avaliacao = aval,
    metricas_resumo(y_raw[mask], pred[mask]))
  res <- rbind(
    met("bruta", p_raw, "nao_censurados", uc),
    met("bruta", p_raw, "todos_vs_alvoL", rep(TRUE, length(uc))),
    met("log",   p_log, "nao_censurados", uc),
    met("log",   p_log, "todos_vs_alvoL", rep(TRUE, length(uc)))
  )
  print(res)
  resultados[[as.character(L)]] <- res
  predicoes[[as.character(L)]] <- data.table(limite = L, id = dat$id[uc],
                                             obs = y_raw[uc], pred = p_raw[uc])

  # figura observado vs predito (bruta, nao-censurados = mesma base do RSF)
  df <- data.table(obs = y_raw[uc], pred = p_raw[uc])
  g <- ggplot(df, aes(obs, pred)) +
    geom_point(alpha = 0.2, size = 0.6, colour = "#2c7fb8") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "red") +
    labs(title = sprintf("Baseline RF — obs vs pred (%d cm, nao-censurados)", L),
         x = "estoque observado (g/m2)", y = "estoque predito (g/m2)") +
    theme_minimal()
  ggsave(file.path(dir_fig, sprintf("03_obs_vs_pred_%dcm.png", L)), g, width = 6, height = 6, dpi = 150)
}

# --- 4. Consolidar e salvar --------------------------------------------------
secao("Resultados consolidados (baseline)")
tab <- rbindlist(resultados)
print(tab)
fwrite(tab, file.path(dir_tab, "03_baseline_rf.csv"))
fwrite(rbindlist(predicoes), file.path(dir_tab, "03_baseline_rf_pred.csv"))
kv("[tabela salva]", "outputs/tabelas/03_baseline_rf.csv")
kv("[predicoes salvas]", "outputs/tabelas/03_baseline_rf_pred.csv")
