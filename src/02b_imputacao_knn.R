# =============================================================================
# 02b_imputacao_knn.R  —  Comparacao de imputacao: mediana vs KNN vs missForest
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: aplicar 3 metodos de imputacao aos MESMOS faltantes, cronometrar e
#           comparar os valores atribuidos (tabela + figuras).
# Pre-requisito: rodar antes o 02_preprocess.R.
# Entrada:  data/processed/perfil_pre_imputacao.rds  (perfil com NAs)
#           data/processed/meta_preditores.rds       (preditores, categoricas)
# Saida:    outputs/tabelas/02b_comparacao_imputacao.csv
#           outputs/tabelas/02b_tempos.csv
#           outputs/figuras/02b_densidade_imputacao.png
#           outputs/figuras/02b_knn_vs_missforest.png
# Nota: comparacao GLOBAL (descritiva). Na modelagem final, a imputacao deve ser
#       ajustada DENTRO de cada dobra de CV para evitar vazamento.
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "VIM", "missRanger", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(VIM); library(missRanger); library(ggplot2)

dir_tab <- here("outputs", "tabelas"); dir_fig <- here("outputs", "figuras")
dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
kv       <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))

set.seed(2026)
K_KNN     <- 5     # vizinhos (KNN)
MR_TREES  <- 100   # arvores (missRanger)

# --- 1. Carregar estado pre-imputacao ----------------------------------------
secao("1. Carregar dados pre-imputacao")
pre  <- readRDS(here("data", "processed", "perfil_pre_imputacao.rds"))
meta <- readRDS(here("data", "processed", "meta_preditores.rds"))
preditores  <- meta$preditores
categoricas <- meta$categoricas
# Categoricas como fator (necessario para missRanger; ok para os demais)
for (v in categoricas) if (v %in% names(pre)) pre[, (v) := as.factor(get(v))]

com_na <- preditores[vapply(preditores, function(v) anyNA(pre[[v]]), logical(1))]
kv("Preditores com faltantes", length(com_na))
kv("Total de celulas faltantes", sum(vapply(com_na, function(v) sum(is.na(pre[[v]])), integer(1))))

mascara <- lapply(com_na, function(v) which(is.na(pre[[v]])))
names(mascara) <- com_na

# Conjunto reduzido de doadores de distancia p/ KNN (coords COMPLETAS + terreno)
donores_dist <- intersect(
  c("coord_x","coord_y","altitude","slope","cti","hand","convergence",
    "roughness","spi","elev_stdev","northness","eastness"),
  names(pre))
kv("Doadores de distancia (KNN)", paste(donores_dist, collapse = ", "))

tempos <- data.table(metodo = character(), segundos = numeric())

# --- 2. Metodo A: mediana / moda ---------------------------------------------
secao("2. Metodo A - mediana / moda")
moda <- function(x){ ux <- unique(x[!is.na(x)]); ux[which.max(tabulate(match(x, ux)))] }
tA <- system.time({
  A <- copy(pre)
  for (v in com_na) {
    if (v %in% categoricas) A[is.na(get(v)), (v) := moda(pre[[v]])]
    else A[is.na(get(v)), (v) := median(pre[[v]], na.rm = TRUE)]
  }
})["elapsed"]
tempos <- rbind(tempos, data.table(metodo = "mediana", segundos = as.numeric(tA)))
kv("Tempo (s)", round(tA, 1))

# --- 3. Metodo B: KNN (VIM::kNN, dist_var reduzido) --------------------------
secao("3. Metodo B - KNN")
tB <- system.time({
  B <- VIM::kNN(pre, variable = com_na, dist_var = donores_dist, k = K_KNN, imp_var = FALSE)
  setDT(B)
})["elapsed"]
tempos <- rbind(tempos, data.table(metodo = "KNN", segundos = as.numeric(tB)))
kv("Tempo (s)", round(tB, 1))

# --- 4. Metodo C: missForest (missRanger, multithread) -----------------------
secao("4. Metodo C - missForest (missRanger)")
tC <- system.time({
  C <- missRanger::missRanger(
    pre[, ..preditores], num.trees = MR_TREES, pmm.k = 5, seed = 2026, verbose = 0)
  setDT(C)
})["elapsed"]
tempos <- rbind(tempos, data.table(metodo = "missForest", segundos = as.numeric(tC)))
kv("Tempo (s)", round(tC, 1))

subsecao_tempos <- tempos[order(segundos)]
cat("\n  -- Tempos por metodo --\n"); print(subsecao_tempos)
fwrite(subsecao_tempos, file.path(dir_tab, "02b_tempos.csv"))

# --- 5. Tabela comparativa (apenas celulas imputadas) ------------------------
secao("5. Tabela comparativa")
comp <- rbindlist(lapply(com_na, function(v) {
  idx <- mascara[[v]]
  a <- A[[v]][idx]; b <- B[[v]][idx]; cc <- C[[v]][idx]
  if (v %in% categoricas) {
    data.table(variavel = v, tipo = "categorica", n_na = length(idx),
               prop_na = length(idx)/nrow(pre),
               media_mediana = NA_real_, media_KNN = NA_real_, media_mForest = NA_real_,
               dif_KNN_sd = NA_real_, dif_mForest_sd = NA_real_,
               conc_KNN = mean(as.character(a) == as.character(b)),
               conc_mForest = mean(as.character(a) == as.character(cc)))
  } else {
    a <- as.numeric(a); b <- as.numeric(b); cc <- as.numeric(cc)
    esc <- sd(pre[[v]], na.rm = TRUE); esc <- ifelse(esc == 0 | is.na(esc), 1, esc)
    data.table(variavel = v, tipo = "numerica", n_na = length(idx),
               prop_na = length(idx)/nrow(pre),
               media_mediana = mean(a), media_KNN = mean(b), media_mForest = mean(cc),
               dif_KNN_sd = mean(b - a)/esc, dif_mForest_sd = mean(cc - a)/esc,
               conc_KNN = NA_real_, conc_mForest = NA_real_)
  }
}))[order(-n_na)]
print(comp)
fwrite(comp, file.path(dir_tab, "02b_comparacao_imputacao.csv"))
kv("[tabela salva]", "outputs/tabelas/02b_comparacao_imputacao.csv")

# --- 6. Figura 1: densidades observado vs 3 metodos --------------------------
secao("6. Figura - densidades")
num_na <- comp[tipo == "numerica"][order(-n_na)]$variavel
sel <- head(num_na, 6)
long <- rbindlist(lapply(sel, function(v) {
  idx <- mascara[[v]]
  rbind(
    data.table(variavel = v, fonte = "observado",  valor = as.numeric(pre[[v]][-idx])),
    data.table(variavel = v, fonte = "mediana",    valor = as.numeric(A[[v]][idx])),
    data.table(variavel = v, fonte = "KNN",        valor = as.numeric(B[[v]][idx])),
    data.table(variavel = v, fonte = "missForest", valor = as.numeric(C[[v]][idx]))
  )
}))
g1 <- ggplot(long, aes(valor, colour = fonte)) +
  geom_density(na.rm = TRUE, linewidth = 0.8) +
  facet_wrap(~ variavel, scales = "free", ncol = 2) +
  scale_colour_manual(values = c(observado = "grey40", mediana = "#de2d26",
                                 KNN = "#2c7fb8", missForest = "#31a354")) +
  labs(title = "Valores imputados vs observados (6 variaveis com mais faltantes)",
       colour = NULL, x = NULL, y = "densidade") +
  theme_minimal() + theme(legend.position = "top")
ggsave(file.path(dir_fig, "02b_densidade_imputacao.png"), g1, width = 9, height = 7, dpi = 150)
kv("[figura salva]", "outputs/figuras/02b_densidade_imputacao.png")

# --- 7. Figura 2: KNN vs missForest (variavel com mais NA) -------------------
secao("7. Figura - KNN vs missForest")
v1 <- num_na[1]; idx <- mascara[[v1]]
disp <- data.table(KNN = as.numeric(B[[v1]][idx]), missForest = as.numeric(C[[v1]][idx]))
g2 <- ggplot(disp, aes(KNN, missForest)) +
  geom_point(alpha = 0.3, size = 0.7, colour = "#756bb1") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "red") +
  labs(title = paste0("Valores imputados: KNN vs missForest (", v1, ")"),
       subtitle = "linha tracejada = concordancia perfeita") +
  theme_minimal()
ggsave(file.path(dir_fig, "02b_knn_vs_missforest.png"), g2, width = 7, height = 6, dpi = 150)
kv("[figura salva]", "outputs/figuras/02b_knn_vs_missforest.png")

secao("Comparacao concluida")
cat("  Leitura: dif_*_sd = (media imputada - mediana) em desvios-padrao;\n",
    "  conc_* = concordancia com a moda (categoricas). Ver 02b_tempos.csv p/ velocidade.\n\n")
