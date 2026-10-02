# =============================================================================
# 03b_sensibilidade.R  —  Sensibilidade do baseline RF: imputacao x hiperparametros
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: responder DUAS perguntas, reusando SEMPRE as mesmas dobras espaciais:
#   (A) A escolha do metodo de imputacao (mediana / KNN / missForest) muda o
#       RMSE/CCC/vies do baseline RF? (variar imputacao, ranger fixo no "base")
#   (B) Mudar hiperparametros do ranger melhora ou piora? (variar UM de cada vez,
#       imputacao fixa)  -> analise OFAT (one-factor-at-a-time)
# Pre-requisito: rodar antes 02_preprocess.R (gera os .rds abaixo).
# Entrada:  data/processed/perfil_pre_imputacao.rds  (perfil c/ NAs + estrutura de sobrev.)
#           data/processed/meta_preditores.rds       (preditores, categoricas)
#           data/processed/dobras_espaciais.rds       (id -> fold)
# Saida:    outputs/tabelas/03b_sensibilidade.csv
#           outputs/figuras/03b_sensib_imputacao.png
#           outputs/figuras/03b_sensib_hiperparam.png
# -----------------------------------------------------------------------------
# >>> CUIDADOS METODOLOGICOS (ler antes de citar no TCC) <<<
#  C1. Imputacao GLOBAL (ajustada em todos os perfis). E uma TRIAGEM de
#      sensibilidade: se a imputacao ja nao muda nada aqui, o refinamento
#      within-fold (imputar so no treino de cada dobra) e irrelevante. Se mudar,
#      re-testar within-fold antes de concluir.
#  C2. Isto e SENSIBILIDADE, nao SELECAO. As flags de NA (flag_na_*) sao
#      identicas entre os metodos de imputacao -> o efeito medido isola apenas os
#      VALORES imputados. Nao reportar "o melhor config" como desempenho final
#      (isso seria vies otimista); reportar como robustez do baseline.
#  C3. Mesmas dobras espaciais (coluna fold do 02) em TODAS as execucoes.
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "ranger", "VIM", "missRanger", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(ranger); library(ggplot2)

source(here("R", "metricas.R"))

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
subsecao <- function(txt) cat("\n-- ", txt, " ", strrep("-", max(0, 70 - nchar(txt))), "\n", sep = "")
kv       <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))

set.seed(2026)
LIMITES     <- c(30, 100)
ESCALAS     <- c("bruta", "log")
IMP_FIXA    <- "mediana"                 # imputacao usada no experimento (B) de hiperparametros
NUM_THREADS <- max(1L, parallel::detectCores() - 1L)
RODAR_MISSFOREST <- TRUE                  # missForest e o mais lento; desligar p/ teste rapido

# --- 1. Carregar dados -------------------------------------------------------
secao("1. Carregar dados pre-imputacao + dobras")
pre    <- readRDS(here("data", "processed", "perfil_pre_imputacao.rds"))
meta   <- readRDS(here("data", "processed", "meta_preditores.rds"))
dobras <- readRDS(here("data", "processed", "dobras_espaciais.rds"))

preditores  <- meta$preditores
categoricas <- meta$categoricas
pre <- merge(pre, dobras[, .(id, fold)], by = "id", all.x = TRUE)
stopifnot(!anyNA(pre$fold))                                   # todos os perfis com dobra
kv("Perfis", nrow(pre)); kv("Preditores", length(preditores))
kv("Dobras", uniqueN(pre$fold)); kv("Threads", NUM_THREADS)

# Preditores com faltantes + flags de NA (CONSTANTES entre metodos -> ver C2)
com_na <- preditores[vapply(preditores, function(v) anyNA(pre[[v]]), logical(1))]
flags_dt <- as.data.table(lapply(com_na, function(v) as.integer(is.na(pre[[v]]))))
setnames(flags_dt, paste0("flag_na_", com_na))
flag_cols <- names(flags_dt)
feats <- c(preditores, flag_cols)
p_feats <- length(feats)
kv("Preditores com NA", length(com_na)); kv("Total de features (com flags)", p_feats)

# Template comum: pre + flags (predicoes serao sobrescritas por metodo de imputacao)
template <- copy(pre)
template[, (flag_cols) := flags_dt]

# --- 2. Funcoes de imputacao (GLOBAL, ver C1) --------------------------------
secao("2. Definir imputadores")
moda <- function(x){ ux <- unique(x[!is.na(x)]); ux[which.max(tabulate(match(x, ux)))] }

impute_mediana <- function() {
  out <- copy(pre[, ..preditores])
  for (v in preditores) if (anyNA(out[[v]])) {
    if (v %in% categoricas) out[is.na(get(v)), (v) := moda(pre[[v]])]
    else                    out[is.na(get(v)), (v) := median(pre[[v]], na.rm = TRUE)]
  }
  out
}

impute_knn <- function(k) {
  don <- intersect(c("coord_x","coord_y","altitude","slope","cti","hand","convergence",
                     "roughness","spi","elev_stdev","northness","eastness"), names(pre))
  sub <- pre[, unique(c(preditores, don)), with = FALSE]
  for (v in categoricas) if (v %in% names(sub)) sub[, (v) := as.factor(get(v))]
  out <- VIM::kNN(sub, variable = com_na, dist_var = don, k = k, imp_var = FALSE)
  setDT(out)
  out[, ..preditores]
}

impute_missforest <- function(num.trees, pmm.k) {
  sub <- copy(pre[, ..preditores])
  for (v in categoricas) if (v %in% names(sub)) sub[, (v) := as.factor(get(v))]
  out <- missRanger::missRanger(sub, num.trees = num.trees, pmm.k = pmm.k,
                                seed = 2026, verbose = 0)
  setDT(out)
  out
}

# Especificacoes de imputacao a comparar (experimento A)
imp_specs <- list(
  list(nome = "mediana",      tipo = "mediana"),
  list(nome = "knn_k5",       tipo = "knn", k = 5),
  list(nome = "knn_k10",      tipo = "knn", k = 10)
)
if (RODAR_MISSFOREST) imp_specs <- c(imp_specs, list(
  list(nome = "mf_t100_pmm5", tipo = "missforest", num.trees = 100, pmm.k = 5),
  list(nome = "mf_t200_pmm3", tipo = "missforest", num.trees = 200, pmm.k = 3)
))

# Cache de imputacoes (cada uma roda 1x, e cara p/ missForest)
imp_cache <- new.env(parent = emptyenv())
get_imp <- function(spec) {
  if (!is.null(imp_cache[[spec$nome]])) return(imp_cache[[spec$nome]])
  t0 <- Sys.time()
  imp <- switch(spec$tipo,
    mediana    = impute_mediana(),
    knn        = impute_knn(spec$k),
    missforest = impute_missforest(spec$num.trees, spec$pmm.k))
  kv(sprintf("Imputado '%s'", spec$nome), sprintf("%.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  imp_cache[[spec$nome]] <- imp
  imp
}

# Monta a tabela de modelagem (template + valores imputados), fatoriza categoricas
montar_dat <- function(imp) {
  d <- copy(template)
  for (v in preditores) d[, (v) := imp[[v]]]
  for (v in categoricas) if (v %in% names(d)) d[, (v) := as.factor(get(v))]
  stopifnot(!anyNA(d[, ..feats]))                            # imputacao nao deixou NA
  d
}

# --- 3. CV espacial com ranger (hiperparametros configuraveis) ---------------
secao("3. Definir CV espacial + grade de hiperparametros")
resolver_mtry <- function(m, p) {
  if (is.na(m))                return(NULL)                  # default do ranger
  if (identical(m, "sqrt"))    return(max(1L, floor(sqrt(p))))
  if (identical(m, "p3"))      return(max(1L, floor(p / 3)))
  as.integer(m)
}

cv_ranger <- function(dat, y, feats, folds, hp) {
  pred <- rep(NA_real_, nrow(dat))
  mtry <- resolver_mtry(hp$mtry, length(feats))
  for (f in sort(unique(folds))) {
    tr <- folds != f; te <- folds == f
    m <- ranger(x = dat[tr, ..feats], y = y[tr],
                num.trees = hp$num.trees, mtry = mtry,
                min.node.size = hp$min.node.size, splitrule = hp$splitrule,
                respect.unordered.factors = "order",
                num.threads = NUM_THREADS, seed = 2026)
    pred[te] <- predict(m, dat[te, ..feats], num.threads = NUM_THREADS)$predictions
  }
  pred
}

# Uma execucao (limite x escala) -> uma linha de metricas
run_config <- function(dat, feats, L, escala, hp) {
  alvo_col <- paste0("alvo_", L)
  d  <- dat[is.finite(get(alvo_col))]                        # descarta alvo NA (cenario 30 cm)
  y  <- d[[alvo_col]]
  if (escala == "bruta") p <- cv_ranger(d, y,        feats, d$fold, hp)
  else                   p <- expm1(cv_ranger(d, log1p(y), feats, d$fold, hp))
  m <- metricas_resumo(y, p)
  data.table(limite = L, escala = escala, n = m$n,
             rmse = m$rmse, mae = m$mae, ccc = m$ccc, vies = m$vies, r2 = m$r2)
}

# Grade OFAT do ranger (mtry=NA -> default; "sqrt"/"p3" resolvidos por p_feats)
hp_base  <- list(nome = "base", num.trees = 500, mtry = NA, min.node.size = 5, splitrule = "variance")
configs  <- list(
  hp_base,
  modifyList(hp_base, list(nome = "ntree1000",  num.trees = 1000)),
  modifyList(hp_base, list(nome = "mtry_sqrt",  mtry = "sqrt")),
  modifyList(hp_base, list(nome = "mtry_p3",    mtry = "p3")),
  modifyList(hp_base, list(nome = "node10",     min.node.size = 10)),
  modifyList(hp_base, list(nome = "node20",     min.node.size = 20)),
  modifyList(hp_base, list(nome = "extratrees", splitrule = "extratrees"))
)
kv("Especificacoes de imputacao (exp. A)", length(imp_specs))
kv("Configuracoes de ranger (exp. B)",     length(configs))
kv("Imputacao fixa no exp. B",             IMP_FIXA)

# --- 4. Experimento A: sensibilidade a IMPUTACAO (ranger fixo = base) --------
secao("4. Experimento A - imputacao (ranger no config 'base')")
resA <- rbindlist(lapply(imp_specs, function(spec) {
  dat <- montar_dat(get_imp(spec))
  grade <- CJ(L = LIMITES, escala = ESCALAS, sorted = FALSE)
  r <- rbindlist(Map(function(L, e) run_config(dat, feats, L, e, hp_base), grade$L, grade$escala))
  cbind(experimento = "imputacao", imputacao = spec$nome, config = "base", r)
}))
subsecao("Resultado (experimento A)")
print(resA[order(limite, escala, rmse)])

# --- 5. Experimento B: sensibilidade a HIPERPARAMETROS (imputacao fixa) ------
secao(sprintf("5. Experimento B - hiperparametros (imputacao = %s)", IMP_FIXA))
spec_fixa <- Filter(function(s) s$nome == IMP_FIXA, imp_specs)[[1]]
dat_fixa  <- montar_dat(get_imp(spec_fixa))
resB <- rbindlist(lapply(configs, function(hp) {
  grade <- CJ(L = LIMITES, escala = ESCALAS, sorted = FALSE)
  r <- rbindlist(Map(function(L, e) run_config(dat_fixa, feats, L, e, hp), grade$L, grade$escala))
  cbind(experimento = "hiperparam", imputacao = IMP_FIXA, config = hp$nome, r)
}))
subsecao("Resultado (experimento B)")
print(resB[order(limite, escala, rmse)])

# --- 6. Consolidar e salvar --------------------------------------------------
secao("6. Consolidar e salvar")
res <- rbindlist(list(resA, resB))
fwrite(res, file.path(dir_tab, "03b_sensibilidade.csv"))
kv("[tabela salva]", "outputs/tabelas/03b_sensibilidade.csv")

subsecao("Amplitude do efeito (max-min do RMSE por limite x escala)")
amp <- res[, .(rmse_min = min(rmse), rmse_max = max(rmse),
               amplitude = max(rmse) - min(rmse),
               amplitude_pct = (max(rmse) - min(rmse)) / min(rmse)),
           by = .(experimento, limite, escala)]
print(amp[order(experimento, limite, escala)])
cat("\n  Leitura: amplitude_pct pequena (ex.: < 2-3%) sugere que o fator NAO importa\n",
    "  muito para o baseline; grande sugere que vale investigar (e, p/ imputacao,\n",
    "  re-testar within-fold antes de concluir).\n")

# --- 7. Figuras --------------------------------------------------------------
secao("7. Figuras")
gA <- ggplot(resA, aes(reorder(imputacao, rmse), rmse, fill = escala)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ limite, scales = "free_y", labeller = label_both) +
  labs(title = "Sensibilidade do baseline RF ao metodo de imputacao",
       x = NULL, y = "RMSE (g/m2)", fill = "escala") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(dir_fig, "03b_sensib_imputacao.png"), gA, width = 9, height = 5, dpi = 150)

gB <- ggplot(resB, aes(reorder(config, rmse), rmse, fill = escala)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ limite, scales = "free_y", labeller = label_both) +
  labs(title = sprintf("Sensibilidade do baseline RF aos hiperparametros (imput.=%s)", IMP_FIXA),
       x = NULL, y = "RMSE (g/m2)", fill = "escala") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(dir_fig, "03b_sensib_hiperparam.png"), gB, width = 9, height = 5, dpi = 150)
kv("[figuras salvas]", "03b_sensib_imputacao.png / 03b_sensib_hiperparam.png")

secao("Sensibilidade concluida")
cat("  Ver 03b_sensibilidade.csv. Lembrar C1 (imputacao global = triagem) e\n",
    "  C2 (sensibilidade, NAO selecao de modelo final).\n\n")
