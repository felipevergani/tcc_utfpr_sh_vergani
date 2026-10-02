# =============================================================================
# 05_ipc_rf.R  —  Regressao corrigida com IPCW (IPC-RF)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Ref.: van der Westhuizen et al. (2024). Metodo model-agnostic: em vez de passar
#   pela curva de sobrevivencia (RSF), mantem a REGRESSAO RF e corrige o vies de
#   censura PONDERANDO as observacoes completas por 1/G-hat.
#
# Estrutura de censura (IDENTICA ao 04):
#   tempo  = alvo_L ; evento = 1 (COMPLETO: atingiu L ou rocha antes de L)
#                             0 (CENSURADO a direita: parou antes de L sem rocha)
#
# IPCW: G-hat(t) = P(nao ser censurado ate o estoque t) via Kaplan-Meier da
#   CENSURA (Surv(tempo, 1-evento)), estimada DENTRO de cada dobra de treino
#   (sem vazamento). Peso do completo i: w_i = 1 / G-hat(tempo_i), truncado.
#   -> upweight de completos com estoque ALTO (raros, pois tendem a ser
#      censurados), corrigindo a subestimacao sistematica.
#
# Compara DOIS ajustes por limite:
#   complete_case : RF nos completos SEM pesos (analise de casos completos)
#   ipcw          : RF nos completos COM case.weights = w (IPC-RF)
# Avaliacao nos NAO-censurados (evento=1), mesma regua do 03/04.
#
# NOTA (van der Westhuizen): o ganho do IPCW cai quando a censura passa de ~60%.
#   No limite 100 cm estamos em ~65% -> regime de fronteira (pesos extremos).
# NOTA (avaliacao): corrigir a censura empurra as predicoes PARA CIMA (rumo a
#   media populacional). O conjunto nao-censurado e enviesado para baixo, entao
#   um vies POSITIVO do IPCW vs complete_case e ESPERADO, nao necessariamente pior.
# Entrada:  data/processed/perfil_sobrevivencia.rds, meta_preditores.rds
# Saida:    outputs/tabelas/05_ipc_rf.csv, 05_ipc_rf_pred.csv ; figuras/05_*.png
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "survival", "ranger", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(survival); library(ranger); library(ggplot2)
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
NTREE       <- 500
ROCK_COMPLETO <- TRUE     # evento = atingiu L ou rocha (igual ao 04)
G_FLOOR     <- 0.05       # piso de G-hat (evita divisao por ~0)
W_TRUNC_Q   <- 0.99       # truncamento dos pesos no quantil (estabiliza extremos)
SUBAMOSTRA  <- NA         # NA = todos; ou N p/ teste rapido

# --- 1. Dados ----------------------------------------------------------------
secao("1. Carregar dados")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
preditores <- meta$preditores
flags <- grep("^flag_na_", names(perfil), value = TRUE)
feats <- c(preditores, flags)
if (!is.na(SUBAMOSTRA) && SUBAMOSTRA < nrow(perfil)) perfil <- perfil[sample(.N, SUBAMOSTRA)]
MTRY <- max(1L, floor(length(feats) / 3))   # p/3 (melhor no baseline)
kv("Perfis", nrow(perfil)); kv("Features", length(feats)); kv("mtry", MTRY)

# --- 2. Funcoes --------------------------------------------------------------
# define tempo = estoque (alvo_L) e evento = completo (atingiu L ou rocha)
montar_surv <- function(L) {
  alvo    <- perfil[[paste0("alvo_", L)]]
  atingiu <- as.integer(perfil$tempo_espessura >= L)
  rocha   <- as.integer(perfil$evento_rock == 1)
  evento  <- if (ROCK_COMPLETO) as.integer(atingiu == 1 | rocha == 1) else atingiu
  data.table(id = perfil$id, fold = perfil$fold, tempo = alvo, evento = evento)
}
# G-hat(t): sobrevivencia da CENSURA (KM), avaliada em t (funcao escada)
Ghat_at <- function(km, t) {
  st <- km$time; sv <- km$surv
  idx <- findInterval(t, st)                 # n. de tempos de censura <= t
  g <- ifelse(idx == 0, 1, sv[idx])
  pmax(g, G_FLOOR)
}
# ranger de regressao (opcionalmente ponderado)
fit_rf <- function(dtr, y, w = NULL) {
  ranger(x = dtr[, ..feats], y = y, num.trees = NTREE, mtry = MTRY,
         case.weights = w, respect.unordered.factors = "order", seed = 2026)
}

# --- 3. CV espacial: complete_case vs ipcw -----------------------------------
secao("2. IPC-RF (CV espacial, KM da censura within-fold)")
resultados <- list(); predicoes <- list(); pesos_diag <- list()
for (L in LIMITES) {
  subsecao(sprintf("Limite %d cm", L))
  sv  <- montar_surv(L)
  dat <- cbind(sv, perfil[, ..feats])[is.finite(tempo)]
  kv("Completo (evento=1)", sprintf("%d (%s)", dat[evento==1,.N], pct(mean(dat$evento))))
  kv("Censurado a direita", sprintf("%d (%s)", dat[evento==0,.N], pct(mean(dat$evento==0))))
  if (mean(dat$evento==0) > 0.60) kv("AVISO", "censura > 60% (regime de fronteira do IPCW)")

  p_cc <- p_ip <- rep(NA_real_, nrow(dat)); wmax_fold <- numeric()
  for (f in sort(unique(dat$fold))) {
    tr <- which(dat$fold != f); te <- which(dat$fold == f)
    tr_c <- tr[dat$evento[tr] == 1]                       # completos do treino
    if (length(tr_c) < 5) next
    # KM da censura no TREINO -> pesos IPCW nos completos do treino
    km <- survfit(Surv(tempo, 1 - evento) ~ 1, data = dat[tr])
    w  <- 1 / Ghat_at(km, dat$tempo[tr_c])
    w  <- pmin(w, quantile(w, W_TRUNC_Q))                 # trunca extremos
    wmax_fold <- c(wmax_fold, max(w))
    ycc <- dat$tempo[tr_c]
    m_cc <- fit_rf(dat[tr_c], ycc, w = NULL)              # casos completos (sem peso)
    m_ip <- fit_rf(dat[tr_c], ycc, w = w)                 # IPC-RF (ponderado)
    p_cc[te] <- predict(m_cc, dat[te, ..feats])$predictions
    p_ip[te] <- predict(m_ip, dat[te, ..feats])$predictions
  }
  dat[, `:=`(pred_cc = p_cc, pred_ipcw = p_ip)]
  kv("Peso IPCW maximo (mediana entre dobras)", sprintf("%.1f", median(wmax_fold)))

  aval <- dat[evento == 1]
  met1 <- function(nome, col) {
    ok <- is.finite(aval[[col]]); o <- aval$tempo[ok]; p <- aval[[col]][ok]
    cbind(limite = L, ajuste = nome, obs_medio = round(mean(o)),
          data.table(metricas_resumo(o, p)),
          r_pearson = cor(o, p), sd_ratio = sd(p) / sd(o), pred_medio = round(mean(p)))
  }
  linhas <- rbind(met1("complete_case", "pred_cc"), met1("ipcw", "pred_ipcw"))
  print(linhas); resultados[[as.character(L)]] <- linhas
  predicoes[[as.character(L)]] <- cbind(limite = L, dat[, .(id, fold, tempo, evento, pred_cc, pred_ipcw)])

  dd <- aval[is.finite(pred_ipcw), .(tempo, pred = pred_ipcw)]
  g <- ggplot(dd, aes(tempo, pred)) +
    geom_point(alpha = 0.2, size = 0.6, colour = "#b35806") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "red") +
    labs(title = sprintf("IPC-RF — obs vs pred estoque (%d cm)", L),
         x = "estoque observado (g/m2)", y = "estoque predito (g/m2)") + theme_minimal()
  ggsave(file.path(dir_fig, sprintf("05_ipc_rf_obs_vs_pred_%dcm.png", L)), g, width = 6, height = 6, dpi = 150)
}

# --- 4. Salvar ---------------------------------------------------------------
secao("Resultados consolidados (IPC-RF)")
tab <- rbindlist(resultados); print(tab)
fwrite(tab, file.path(dir_tab, "05_ipc_rf.csv"))
fwrite(rbindlist(predicoes), file.path(dir_tab, "05_ipc_rf_pred.csv"))
kv("[tabela salva]", "outputs/tabelas/05_ipc_rf.csv")
cat("\n  Referencias (nao-cens., bruta): Baseline 30cm CCC~0.385 / 100cm 0.192;\n",
    "  RSF 0.140 / 0.080. Ler o IPCW pelo par (complete_case vs ipcw): a diferenca\n",
    "  mostra o efeito da correcao. Vies positivo do ipcw = correcao da censura.\n\n")
