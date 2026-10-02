# =============================================================================
# 01_eda.R  —  Analise exploratoria com foco na censura
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: caracterizar os dados e a estrutura de censura ANTES de modelar.
# Entrada:  data/raw/c3_2026_03_27_soildata_soc_modeling_survival.csv
# Saida:    outputs/figuras/*.png  e  outputs/tabelas/*.csv
# Uso:      abrir TestesProjeto.Rproj e rodar este script (ordem 01 -> 07).
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "ggplot2")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here)
library(data.table)
library(ggplot2)

source(here("R", "helper.R"))

dir_fig <- here("outputs", "figuras")
dir_tab <- here("outputs", "tabelas")
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)

# --- Funcoes de output estruturado -------------------------------------------
.linha    <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao     <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
subsecao  <- function(txt) cat("\n-- ", txt, " ", strrep("-", max(0, 70 - nchar(txt))), "\n", sep = "")
kv        <- function(k, v) cat(sprintf("  %-42s %s\n", paste0(k, ":"), v))
pct       <- function(x) sprintf("%.1f%%", 100 * x)
salva_tab <- function(dt, nome) {
  fp <- file.path(dir_tab, nome)
  fwrite(dt, fp)
  cat(sprintf("  [tabela salva] %s\n", fp))
}
salva_fig <- function(plot, nome, w = 8, h = 5) {
  fp <- file.path(dir_fig, nome)
  ggsave(fp, plot, width = w, height = h, dpi = 150)
  cat(sprintf("  [figura salva] %s\n", fp))
}

# Limites de profundidade de interesse (cm)
LIMITES <- c(30, 100)

# --- 1. Leitura --------------------------------------------------------------
secao("1. Leitura dos dados")
arq <- here("data", "raw", "c3_2026_03_27_soildata_soc_modeling_survival.csv")
dt  <- fread(arq)
kv("Arquivo", basename(arq))
kv("Dimensoes (linhas x colunas)", sprintf("%d x %d", nrow(dt), ncol(dt)))

# --- 2. Visao geral ----------------------------------------------------------
secao("2. Visao geral")
subsecao("summary_soildata() (helper do coorientador)")
summary_soildata(dt)

subsecao("Contagens principais")
kv("Camadas (linhas)", format(nrow(dt), big.mark = "."))
kv("Perfis unicos (id)", format(uniqueN(dt$id), big.mark = "."))
kv("Datasets (dataset_id)", uniqueN(dt$dataset_id))
kv("Preditores/colunas", ncol(dt))
cpp <- dt[, .N, by = id]$N
kv("Camadas por perfil (min/mediana/max)", sprintf("%d / %.0f / %d", min(cpp), median(cpp), max(cpp)))

# --- 3. Controle de qualidade (integridade das camadas) ----------------------
secao("3. Controle de qualidade das camadas")
subsecao("Profundidades iguais (espessura zero)")
qc_eq  <- check_equal_depths(dt)
subsecao("Profundidades invertidas ou negativas")
qc_inv <- check_depth_inversion(dt)
subsecao("Camadas repetidas")
qc_rep <- check_repeated_layer(dt)
subsecao("Camadas faltantes (lacunas verticais)")
qc_miss <- any_missing_layer(dt)
subsecao("Coordenadas duplicadas")
suppressWarnings(check_equal_coordinates(dt))

qc_resumo <- data.table(
  verificacao = c("profund_sup == profund_inf", "profund invertida/negativa",
                  "camadas repetidas", "lacunas verticais"),
  n_ocorrencias = c(nrow(qc_eq), nrow(qc_inv), nrow(qc_rep),
                    if (is.null(qc_miss)) 0L else nrow(qc_miss))
)
subsecao("Resumo do QC")
print(qc_resumo)
salva_tab(qc_resumo, "01_qc_resumo.csv")

# --- 4. Distribuicao de profundidade -----------------------------------------
secao("4. Distribuicao de profundidade")
subsecao("profund_inf (limite inferior de cada camada)")
print(summary(dt$profund_inf))

prof_max <- dt[, .(prof_max = max(profund_inf)), by = id]
subsecao("Profundidade maxima por perfil")
print(summary(prof_max$prof_max))

g_hist <- ggplot(prof_max, aes(prof_max)) +
  geom_histogram(binwidth = 5, fill = "#2c7fb8", colour = "white") +
  labs(title = "Profundidade maxima amostrada por perfil",
       x = "Profundidade maxima (cm)", y = "N. de perfis") +
  theme_minimal()
salva_fig(g_hist, "01_hist_prof_max.png")

# --- 5. Quantificacao da censura ---------------------------------------------
secao("5. Quantificacao da censura a direita")
# Evento (contato litico) por perfil: algum is_rock == TRUE
evento_perfil <- dt[, .(tem_evento = any(is_rock == TRUE, na.rm = TRUE),
                        prof_max   = max(profund_inf)), by = id]

cens_tab <- rbindlist(lapply(LIMITES, function(L) {
  n_total    <- nrow(evento_perfil)
  n_censura  <- evento_perfil[prof_max < L, .N]   # nao atinge o limite = censurado
  data.table(limite_cm = L,
             n_perfis = n_total,
             n_censurados = n_censura,
             prop_censura = round(n_censura / n_total, 4))
}))
subsecao("Proporcao de perfis censurados por limite")
print(cens_tab)
salva_tab(cens_tab, "01_censura_por_limite.csv")

subsecao("Cruzamento evento (is_rock) x perfil")
kv("Perfis com evento (contato litico)", sprintf("%d (%s)",
   evento_perfil[tem_evento == TRUE, .N], pct(mean(evento_perfil$tem_evento))))
kv("Perfis sem evento (potencial censura)", sprintf("%d (%s)",
   evento_perfil[tem_evento == FALSE, .N], pct(mean(!evento_perfil$tem_evento))))

g_cens <- ggplot(melt(cens_tab, id.vars = "limite_cm",
                      measure.vars = c("n_censurados"))[, lim := factor(limite_cm)],
                 aes(lim, value)) +
  geom_col(fill = "#de2d26", width = 0.5) +
  geom_text(aes(label = value), vjust = -0.3) +
  labs(title = "Perfis censurados por limite de profundidade",
       x = "Limite (cm)", y = "N. de perfis censurados") +
  theme_minimal()
salva_fig(g_cens, "01_censura_barras.png")

# --- 6. Variavel resposta ----------------------------------------------------
secao("6. Variavel resposta (soc_stock_gm2_cum_qmap)")
subsecao("Distribuicao do estoque cumulativo (camada mais profunda por perfil)")
alvo_perfil <- dt[order(profund_inf), .SD[.N], by = id,
                  .SDcols = c("profund_inf", "soc_stock_gm2_cum_qmap")]
print(summary(alvo_perfil$soc_stock_gm2_cum_qmap))

g_alvo <- ggplot(alvo_perfil, aes(soc_stock_gm2_cum_qmap)) +
  geom_histogram(bins = 60, fill = "#31a354", colour = "white") +
  labs(title = "Estoque de COS cumulativo no fundo do perfil",
       x = "soc_stock_gm2_cum_qmap (g/m2)", y = "N. de perfis") +
  theme_minimal()
salva_fig(g_alvo, "01_hist_alvo.png")

# --- 7. Faltantes nas covariaveis --------------------------------------------
secao("7. Faltantes nas covariaveis")
na_tab <- data.table(
  variavel = names(dt),
  n_na     = vapply(dt, function(x) sum(is.na(x)), integer(1))
)[, prop_na := round(n_na / nrow(dt), 4)][order(-prop_na)]
subsecao("Top 20 variaveis com mais faltantes")
print(na_tab[prop_na > 0][1:20])
salva_tab(na_tab, "01_faltantes_por_variavel.csv")

subsecao("Sobreposicao dos faltantes ambientais (~17%)")
# Verifica se os faltantes de coords/terreno/SoilGrids sao os MESMOS perfis
amb <- c("altitude", "coord_x_utm", "clay_00_05cm")
amb <- amb[amb %in% names(dt)]
if (length(amb) >= 2) {
  flags <- dt[, lapply(.SD, function(x) is.na(x)), .SDcols = amb]
  kv("Linhas com NA em TODAS as ambientais", sprintf("%d (%s)",
     sum(rowSums(flags) == length(amb)), pct(mean(rowSums(flags) == length(amb)))))
  kv("Linhas com NA em ALGUMA ambiental", sprintf("%d (%s)",
     sum(rowSums(flags) > 0), pct(mean(rowSums(flags) > 0))))
}

# --- 8. Distribuicao espacial ------------------------------------------------
secao("8. Distribuicao espacial")
subsecao("Perfis por bioma")
if ("bioma_nome" %in% names(dt)) {
  bioma_tab <- unique(dt[, .(id, bioma_nome)])[, .N, by = bioma_nome][order(-N)]
  print(bioma_tab)
  salva_tab(bioma_tab, "01_perfis_por_bioma.csv")
}
pts <- unique(dt[!is.na(coord_x) & !is.na(coord_y), .(id, coord_x, coord_y, bioma_nome)])
g_map <- ggplot(pts, aes(coord_x, coord_y, colour = bioma_nome)) +
  geom_point(size = 0.4, alpha = 0.5) +
  coord_quickmap() +
  labs(title = "Distribuicao espacial dos perfis", x = "Longitude", y = "Latitude",
       colour = "Bioma") +
  theme_minimal()
salva_fig(g_map, "01_mapa_perfis.png", w = 8, h = 7)

# --- 9. Vies de subestimacao (objetivo especifico 1) -------------------------
secao("9. Vies de subestimacao por censura")
# Ilustracao com perfis COMPLETOS (atingem >= 100 cm): quanto do estoque 0-100
# seria perdido se a amostragem parasse antes (tratando o observado como final).
estoque_ate <- function(d, cap) {
  d[profund_inf <= cap, .(s = if (.N > 0) max(soc_stock_gm2_cum_qmap) else NA_real_), by = id]
}
completos <- prof_max[prof_max >= 100, id]
if (length(completos) > 0) {
  dc  <- dt[id %in% completos]
  s30  <- estoque_ate(dc, 30);  setnames(s30,  "s", "s30")
  s100 <- estoque_ate(dc, 100); setnames(s100, "s", "s100")
  comp <- merge(s30, s100, by = "id")[!is.na(s30) & !is.na(s100) & s100 > 0]
  comp[, frac_capturada_0_30 := s30 / s100]
  subsecao(sprintf("Entre %d perfis completos (>=100 cm)", nrow(comp)))
  kv("Fracao media do estoque 0-100 capturada em 0-30", pct(mean(comp$frac_capturada_0_30)))
  kv("Fracao mediana", pct(median(comp$frac_capturada_0_30)))
  kv("Subestimacao media ao parar em 30 cm", pct(1 - mean(comp$frac_capturada_0_30)))
  salva_tab(comp, "01_vies_subestimacao_completos.csv")
}

secao("EDA concluida")
cat("  Figuras em outputs/figuras/  |  Tabelas em outputs/tabelas/\n\n")
