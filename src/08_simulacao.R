# =============================================================================
# 08_simulacao.R  —  Validacao da correcao de censura com DADOS REAIS
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Foco nos limites do trabalho: 30 e 100 cm.
# Ideia (semi-sintetica, verdade MEDIDA): usa perfis cujo estoque 0-L e REAL
#   (completos: atingiram L ou bateram rocha). Censura ARTIFICIALMENTE uma fracao
#   deles — finge que a amostragem parou mais raso, entao o "observado" vira o
#   estoque real de uma camada mais rasa (alvo parcial), e evento=0. Os demais
#   ficam com o alvo_L real (evento=1). Nada de estoque inventado: so a censura
#   e imposta, sobre perfis onde a verdade 0-L existe.
# Avalia naive / complete_case / ipcw CONTRA o alvo_L REAL em todo o teste (o que
#   os censurados naturais nao permitem). Varia a proporcao de censura.
# Entrada: data/processed/perfil_sobrevivencia.rds, camada_profvar.rds, meta_preditores.rds
# Saida:   outputs/tabelas/08_simulacao.csv ; outputs/figuras/08_*.png
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "survival", "ranger", "ggplot2", "scales")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(survival); library(ranger); library(ggplot2)
source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
.linha <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao  <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
kv     <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))
set.seed(2026)

# --- CONFIG ------------------------------------------------------------------
LIMITES     <- c(30, 100)
CENS_RATES  <- c(0.20, 0.40, 0.60)   # proporcoes de censura artificial
MECANISMOS  <- c("aleatorio", "informativo")  # MCAR vs censura ~ estoque alto (realista)
NTREE       <- 400
G_FLOOR     <- 0.05; W_TRUNC_Q <- 0.99

# --- 1. Dados ----------------------------------------------------------------
secao("1. Carregar dados")
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
camada <- readRDS(here("data", "processed", "camada_profvar.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
feats  <- c(meta$preditores, grep("^flag_na_", names(perfil), value = TRUE))
MTRY   <- max(1L, floor(length(feats) / 3))
# Ghat_at(km, t): prob. de NAO ser censurado ate o estoque t (KM da censura), com piso
Ghat_at <- function(km, t) { idx <- findInterval(t, km$time)
  pmax(ifelse(idx == 0, 1, km$surv[idx]), G_FLOOR) }
kv("Perfis", nrow(perfil)); kv("mtry", MTRY)

# --- 2. Loop por limite e por proporcao de censura ---------------------------
secao("2. Censura artificial sobre perfis REAIS (verdade = alvo_L)")
res <- list()
for (L in LIMITES) {
  # Pool com verdade 0-L conhecida (completos)
  compl <- perfil$tempo_espessura >= L | perfil$evento_rock == 1
  pool  <- perfil[compl & is.finite(perfil[[paste0("alvo_", L)]])]
  pool[, alvo := get(paste0("alvo_", L))]                    # verdade real
  # Estoques parciais reais (camadas mais rasas que L) p/ censura artificial
  camL <- camada[id %in% pool$id & profund_inf < L,
                 .(id, cum = soc_stock_gm2_cum_qmap)]
  kv(sprintf("Limite %d cm: perfis no pool (verdade real)", L), nrow(pool))

  for (mec in MECANISMOS) for (cr in CENS_RATES) {
    set.seed(1000 + L + round(cr * 100))
    dat <- copy(pool[, c("id", "fold", "alvo", ..feats)])
    dat[, `:=`(obs = as.numeric(alvo), evento = 1L)]
    n <- nrow(dat); ncen <- round(cr * n)
    # informativo: chance de censura cresce com o estoque verdadeiro (solo fundo/rico)
    prob <- if (mec == "informativo") rank(dat$alvo) else NULL
    cen <- sample(seq_len(n), ncen, prob = prob)
    dat[cen, evento := 0L]
    # observado dos censurados = estoque parcial REAL de uma camada mais rasa
    partmap <- camL[id %in% dat$id[cen], .(cum = as.numeric(cum[sample(.N, 1)])), by = id]
    dat[partmap, obs_part := i.cum, on = "id"]
    dat[evento == 0 & !is.na(obs_part), obs := obs_part]
    dat[evento == 0 &  is.na(obs_part), obs := 0.5 * alvo]   # fallback (sem camada <L)
    dat[, obs_part := NULL]
    taxa <- mean(dat$evento == 0)

    p_na <- p_cc <- p_ip <- rep(NA_real_, nrow(dat))
    for (f in sort(unique(dat$fold))) {
      tr <- which(dat$fold != f); te <- which(dat$fold == f)
      tr_c <- tr[dat$evento[tr] == 1]; if (length(tr_c) < 20) next
      km <- survfit(Surv(obs, 1 - evento) ~ 1, data = dat[tr])
      w  <- 1 / Ghat_at(km, dat$obs[tr_c]); w <- pmin(w, quantile(w, W_TRUNC_Q))
      xa <- function(idx) dat[idx, ..feats]
      m_na <- ranger(x = xa(tr),   y = dat$obs[tr],   num.trees = NTREE, mtry = MTRY,
                     respect.unordered.factors = "order", seed = 2026)
      m_cc <- ranger(x = xa(tr_c), y = dat$obs[tr_c], num.trees = NTREE, mtry = MTRY,
                     respect.unordered.factors = "order", seed = 2026)
      m_ip <- ranger(x = xa(tr_c), y = dat$obs[tr_c], case.weights = w, num.trees = NTREE,
                     mtry = MTRY, respect.unordered.factors = "order", seed = 2026)
      p_na[te] <- predict(m_na, xa(te))$predictions
      p_cc[te] <- predict(m_cc, xa(te))$predictions
      p_ip[te] <- predict(m_ip, xa(te))$predictions
    }
    for (nm in c("naive", "complete_case", "ipcw")) {
      p <- switch(nm, naive = p_na, complete_case = p_cc, ipcw = p_ip)
      ok <- is.finite(p)
      m <- metricas_resumo(dat$alvo[ok], p[ok])            # vs VERDADE real
      res[[length(res) + 1]] <- data.table(
        limite = L, mecanismo = mec, censura_alvo = cr, censura_real = round(taxa, 3), metodo = nm,
        rmse = m$rmse, ccc = m$ccc, vies = m$vies, vies_rel = m$vies / mean(dat$alvo[ok]))
    }
    kv(sprintf("  L=%d %s censura ~%.0f%% (real %.0f%%)", L, mec, 100*cr, 100*taxa), "ok")
  }
}
tab <- rbindlist(res)[order(limite, mecanismo, censura_alvo, metodo)]
secao("Resultado (metricas vs alvo_L REAL)")
print(tab)
fwrite(tab, file.path(dir_tab, "08_simulacao.csv"))

# --- 3. Figuras (facet: mecanismo x limite) ----------------------------------
gv <- ggplot(tab, aes(censura_real, vies_rel, colour = metodo)) +
  geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(mecanismo ~ limite, labeller = label_both) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Vies relativo vs proporcao de censura (verdade real)",
       subtitle = "0 = sem vies. Informativo (realista): naive subestima, complete tambem, ipcw corrige.",
       x = "proporcao de censura", y = "vies relativo", colour = NULL) + theme_minimal()
ggsave(file.path(dir_fig, "08_vies_vs_censura.png"), gv, width = 9, height = 7, dpi = 150)
gc <- ggplot(tab, aes(censura_real, ccc, colour = metodo)) +
  geom_line() + geom_point() + facet_grid(mecanismo ~ limite, labeller = label_both) +
  labs(title = "CCC (vs verdade real) vs proporcao de censura",
       x = "proporcao de censura", y = "CCC", colour = NULL) + theme_minimal()
ggsave(file.path(dir_fig, "08_ccc_vs_censura.png"), gc, width = 9, height = 7, dpi = 150)

secao("Simulacao concluida")
cat("  outputs/tabelas/08_simulacao.csv | figuras 08_*.png\n",
    "  Verdade = alvo_L medido; so a censura e imposta. Foco em 30 e 100 cm.\n\n")
