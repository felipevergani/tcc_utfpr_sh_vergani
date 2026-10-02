# =============================================================================
# 07_benchmark.R  —  Consolidacao do benchmark + incerteza (ENTREGAVEL FINAL)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Parte A: reune RMSE/CCC/vies dos 4 modelos (03-06) numa tabela e figuras
#          comparativas (mesma regua: nao-censurados, escala bruta).
# Parte B: incerteza do modelo VENCEDOR (IPC-RF) via Quantile Regression Forest
#          (ranger quantreg) ponderado por IPCW -> intervalos de predicao 90%,
#          com cobertura (PICP) e largura media (MPIW).
# Entrada:  outputs/tabelas/03_baseline_rf.csv, 04_rsf.csv, 05_ipc_rf.csv, 06_gam.csv
#           data/processed/perfil_sobrevivencia.rds, meta_preditores.rds
# Saida:    outputs/tabelas/07_benchmark.csv, 07_ipcrf_intervalos.csv
#           outputs/figuras/07_ccc.png, 07_rmse.png, 07_ipcrf_intervalos_*.png
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "survival", "ranger", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(survival); library(ranger); library(ggplot2)
source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
.linha <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao  <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
kv     <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))
set.seed(2026)

# --- 1. PARTE A: consolidacao ------------------------------------------------
secao("A. Consolidacao do benchmark (nao-censurados, bruta)")
rd <- function(f) fread(file.path(dir_tab, f))   # le uma tabela de resultados (outputs/tabelas)
cols <- c("modelo", "limite", "ccc", "rmse", "mae", "vies", "r2")

b03 <- rd("03_baseline_rf.csv")[escala == "bruta" & avaliacao == "nao_censurados"][
  , .(modelo = "RF (baseline)", limite, ccc, rmse, mae, vies, r2)]
b04 <- rd("04_rsf.csv")[elo == "rmst_cap"][
  , .(modelo = "RSF", limite, ccc, rmse, mae, vies, r2)]
g05 <- rd("05_ipc_rf.csv")
b05cc <- g05[ajuste == "complete_case"][, .(modelo = "RF (casos completos)", limite, ccc, rmse, mae, vies, r2)]
b05   <- g05[ajuste == "ipcw"][, .(modelo = "IPC-RF", limite, ccc, rmse, mae, vies, r2)]
g06 <- rd("06_gam.csv")
b06 <- data.table(modelo = "GAM prof. variavel", limite = g06$limite,
                  ccc = g06$ccc, rmse = g06$rmse, mae = g06$mae, vies = g06$vies, r2 = g06$r2)

bench <- rbindlist(list(b03, b04, b05cc, b05, b06))[order(limite, -ccc)]
print(bench)
fwrite(bench, file.path(dir_tab, "07_benchmark.csv"))
kv("[tabela salva]", "outputs/tabelas/07_benchmark.csv")

# Figuras comparativas
ordem <- bench[limite == 100][order(ccc), modelo]
bench[, modelo := factor(modelo, levels = ordem)]
g_ccc <- ggplot(bench, aes(modelo, ccc, fill = modelo)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ limite, labeller = label_both) +
  geom_text(aes(label = sprintf("%.3f", ccc)), hjust = -0.1, size = 3) +
  labs(title = "Benchmark — CCC por modelo (nao-censurados)", x = NULL, y = "CCC") +
  theme_minimal()
ggsave(file.path(dir_fig, "07_ccc.png"), g_ccc, width = 9, height = 5, dpi = 150)
g_rmse <- ggplot(bench, aes(modelo, rmse, fill = modelo)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ limite, scales = "free_x", labeller = label_both) +
  labs(title = "Benchmark — RMSE por modelo (nao-censurados)", x = NULL, y = "RMSE (g/m2)") +
  theme_minimal()
ggsave(file.path(dir_fig, "07_rmse.png"), g_rmse, width = 9, height = 5, dpi = 150)
kv("[figuras salvas]", "07_ccc.png / 07_rmse.png")

# --- 2. PARTE B: incerteza do IPC-RF via QRF ---------------------------------
secao("B. Incerteza do IPC-RF (Quantile Regression Forest ponderado)")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
feats  <- c(meta$preditores, grep("^flag_na_", names(perfil), value = TRUE))
MTRY   <- max(1L, floor(length(feats) / 3))
G_FLOOR <- 0.05; W_TRUNC_Q <- 0.99; QUANTIS <- c(0.05, 0.50, 0.95)

# tempo = estoque (alvo_L); evento = completo (atingiu L ou rocha)
montar_surv <- function(L) {
  alvo <- perfil[[paste0("alvo_", L)]]
  ev   <- as.integer(perfil$tempo_espessura >= L | perfil$evento_rock == 1)
  data.table(id = perfil$id, fold = perfil$fold, tempo = alvo, evento = ev)
}
# Ghat_at(km, t): prob. de NAO ser censurado ate o estoque t (KM da censura), com piso
Ghat_at <- function(km, t) { idx <- findInterval(t, km$time)
  pmax(ifelse(idx == 0, 1, km$surv[idx]), G_FLOOR) }

res_inc <- list()
for (L in c(30, 100)) {
  sv  <- montar_surv(L)
  dat <- cbind(sv, perfil[, ..feats])[is.finite(tempo)]
  q05 <- q50 <- q95 <- rep(NA_real_, nrow(dat))
  for (f in sort(unique(dat$fold))) {
    tr <- which(dat$fold != f); te <- which(dat$fold == f)
    tr_c <- tr[dat$evento[tr] == 1]; if (length(tr_c) < 20) next
    km <- survfit(Surv(tempo, 1 - evento) ~ 1, data = dat[tr])
    w  <- 1 / Ghat_at(km, dat$tempo[tr_c]); w <- pmin(w, quantile(w, W_TRUNC_Q))
    m  <- ranger(x = dat[tr_c, ..feats], y = dat$tempo[tr_c], case.weights = w,
                 num.trees = 500, mtry = MTRY, quantreg = TRUE,
                 respect.unordered.factors = "order", seed = 2026)
    pq <- predict(m, dat[te, ..feats], type = "quantiles", quantiles = QUANTIS)$predictions
    q05[te] <- pq[, 1]; q50[te] <- pq[, 2]; q95[te] <- pq[, 3]
  }
  dat[, `:=`(q05 = q05, q50 = q50, q95 = q95)]
  av <- dat[evento == 1 & is.finite(q50)]
  picp <- mean(av$tempo >= av$q05 & av$tempo <= av$q95)     # cobertura do IP 90%
  mpiw <- mean(av$q95 - av$q05)                             # largura media
  pm   <- metricas_resumo(av$tempo, av$q50)                 # ponto = mediana QRF
  linha <- data.table(limite = L, n = nrow(av), picp_90 = round(picp, 3),
                      mpiw = round(mpiw), ccc_q50 = pm$ccc, rmse_q50 = pm$rmse)
  print(linha); res_inc[[as.character(L)]] <- linha

  # figura: amostra de intervalos ordenados pelo observado
  s <- av[order(tempo)][seq(1, .N, length.out = min(300, .N))]
  s[, ord := seq_len(.N)]
  g <- ggplot(s, aes(ord, tempo)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#fdae61", alpha = 0.5) +
    geom_point(size = 0.5, colour = "black") +
    geom_line(aes(y = q50), colour = "#d73027") +
    labs(title = sprintf("IPC-RF QRF — intervalos de predicao 90%% (%d cm)", L),
         subtitle = sprintf("PICP=%.2f  MPIW=%.0f g/m2", picp, mpiw),
         x = "perfis (ordenados pelo estoque observado)", y = "estoque (g/m2)") +
    theme_minimal()
  ggsave(file.path(dir_fig, sprintf("07_ipcrf_intervalos_%dcm.png", L)), g, width = 8, height = 5, dpi = 150)
}
inc <- rbindlist(res_inc)
fwrite(inc, file.path(dir_tab, "07_ipcrf_intervalos.csv"))

secao("Benchmark consolidado")
cat("  Tabela:   outputs/tabelas/07_benchmark.csv\n")
