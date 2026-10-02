# =============================================================================
# 02_preprocess.R  —  Preparacao dos dados e estrutura de sobrevivencia
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Objetivo: gerar os datasets de modelagem limpos + par (tempo, evento) + dobras
#           de CV espacial, prontos para os scripts 03-06.
# Entrada:  data/raw/c3_2026_03_27_soildata_soc_modeling_survival.csv
# Saida:    data/processed/perfil_sobrevivencia.rds  (1 linha por perfil)
#           data/processed/camada_profvar.rds        (1 linha por camada)
#           data/processed/dobras_espaciais.rds      (id -> fold)
#           outputs/tabelas/02_*.csv
# -----------------------------------------------------------------------------
# >>> DECISOES COM DEFAULT (revisar apos ver os resultados) <<<
#  D1. Preditores = apenas covariaveis AMBIENTAIS/ESPACIAIS (sem vazamento).
#      Excluidas as propriedades medidas em laboratorio da propria amostra
#      (carbono, argila, dsi, ph, ...) e a taxonomia observada (ORDER/SUBORDER),
#      pois nao existem em locais nao amostrados (contexto de mapeamento digital).
#  D2. Faltantes: perfis SEM coordenada sao removidos (necessarios p/ CV
#      espacial); demais NA em preditores -> imputacao KNN (k=5) + flag de NA.
#      (Decisao registrada em docs/decisoes_metodologicas.md; mediana e fallback.)
#  D3. Alvo por limite L: ultima camada com profund_inf <= L. Perfil e censurado
#      no limite L se profundidade maxima < L.
#  D4. CV espacial: dobras por dataset_id (dataset inteiro em uma unica dobra).
#  D5. Pseudo-amostras (sand-pseudo/rock-pseudo, sem covariaveis) removidas via
#      PSEUDOSAND_index/PSEUDOROCK_index (flag REMOVER_PSEUDO). Nao afeta os 317
#      eventos is_rock (todos em perfis reais).
#  DUAS variantes de evento sao geradas:
#    A) 'atingiu o limite L' (estoque de COS)  -> evento_lim_L / cens_L / tempo_lim_L
#    B) 'contato litico' (espessura, Chen 2019) -> evento_rock / tempo_espessura
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
pacotes <- c("here", "data.table", "VIM")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(VIM)
source(here("R", "helper.R"))

dir_tab  <- here("outputs", "tabelas"); dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
dir_proc <- here("data", "processed");  dir.create(dir_proc, showWarnings = FALSE, recursive = TRUE)

.linha   <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
secao    <- function(txt) { cat("\n"); .linha("="); cat("  ", toupper(txt), "\n", sep = ""); .linha("=") }
subsecao <- function(txt) cat("\n-- ", txt, " ", strrep("-", max(0, 70 - nchar(txt))), "\n", sep = "")
kv       <- function(k, v) cat(sprintf("  %-46s %s\n", paste0(k, ":"), v))
pct      <- function(x) sprintf("%.1f%%", 100 * x)

set.seed(2026)
LIMITES <- c(30, 100)
K_FOLDS <- 10
K_KNN   <- 5             # vizinhos para imputacao KNN (ver docs/decisoes_metodologicas.md)
IMPUTACAO <- "knn"       # "knn" (default, decidido) | "mediana" (fallback simples)
REMOVER_PSEUDO <- TRUE   # D5: remover pseudo-amostras (sand-pseudo/rock-pseudo)

# --- 1. Leitura --------------------------------------------------------------
secao("1. Leitura")
dt <- fread(here("data", "raw", "c3_2026_03_27_soildata_soc_modeling_survival.csv"))
kv("Camadas x colunas", sprintf("%d x %d", nrow(dt), ncol(dt)))
kv("Perfis unicos", uniqueN(dt$id))

# Pseudo-amostras sinteticas (sem covariaveis) -> D5
dt[, pseudo := as.integer((PSEUDOSAND_index %in% 1) | (PSEUDOROCK_index %in% 1))]
kv("Perfis pseudo (sand/rock)", uniqueN(dt[pseudo == 1, id]))
if (REMOVER_PSEUDO) {
  dt <- dt[pseudo == 0]
  kv("Pseudo removidas (REMOVER_PSEUDO=TRUE)", sprintf("restam %d perfis", uniqueN(dt$id)))
}

# --- 2. Selecao de preditores (D1) -------------------------------------------
secao("2. Selecao de preditores")
terreno   <- c("altitude","convergence","cti","dev_magnitude","eastness","elev_stdev",
               "geomorphon","hand","northness","pcurv","roughness","slope","spi")
espacial  <- c("coord_x_utm","coord_y_utm")
contexto  <- c("lulc","bioma_nome","rockyIndex","sandyIndex","black_soils")
classes   <- c("Acrisols","Alisols","Arenosols","Chernozems","Ferralsols","Gleysols",
               "Histosols","Leptosols","Lixisols","Luvisols","Nitisols","Phaeozems",
               "Planosols","Plinthosols","Podzols","Regosols","Stagnosols","Umbrisols","Vertisols")
provincias <- grep("Prov$", names(dt), value = TRUE)
#soilgrids <- grep("^(bdod|clay|sand|soc|cfvo)_[0-9]+_[0-9]+cm$", names(dt), value = TRUE)  # so camadas SoilGrids (evita capturar soc_stock/soc_density = alvo)

categoricas <- c("lulc","bioma_nome","geomorphon")   # tratar como fator
preditores  <- c(terreno, espacial, contexto, classes, provincias)   # SoilGrids removido (definicao comentada acima)
preditores  <- preditores[preditores %in% names(dt)]

kv("Terreno", length(terreno)); kv("Espacial", length(espacial))
kv("Contexto", length(contexto)); kv("Classes de solo (prob.)", length(classes))
kv("Provincias geologicas", length(provincias))
kv("TOTAL de preditores", length(preditores))
subsecao("Excluidos por vazamento (medidos na amostra) / meta")
excluidos_lab <- c("carbono","ctc","cec_clay_ratio","ph","dsi","dsi_upper","dsi_lower",
                   "argila","argila_upper","argila_lower","silte","silt_clay_ratio",
                   "areia","areia_upper","areia_lower","terrafina","esqueleto",
                   "coarse_upper","coarse_lower","vrc","STONY","ORGANIC","AHRZN","EHRZN",
                   "BHRZN","CHRZN","DENSIC","GLEY","ORDER","SUBORDER","STONESOL")
cat("  ", paste(excluidos_lab[excluidos_lab %in% names(dt)], collapse = ", "), "\n")

#inclusão de ORDER, SUBORDER, STONESOL (transformar em fator por estarem em categoricas nominais) (futuro trabalho)

# --- 3. Estrutura de sobrevivencia (D3) --------------------------------------
secao("3. Estrutura de sobrevivencia - DUAS variantes de evento")
# Variante B (espessura de solo, Chen 2019): evento = contato litico (is_rock)
dt[, is_rock_lgl := toupper(as.character(is_rock)) == "TRUE"]
perfil <- dt[, .(
  tempo_espessura = max(profund_inf),                          # prof. maxima amostrada
  evento_rock     = as.integer(any(is_rock_lgl, na.rm = TRUE)) # 1 = contato litico observado
), by = .(id, dataset_id)]
subsecao("Variante B (espessura): evento = is_rock")
kv("Perfis com evento (contato litico)", sprintf("%d (%s)", perfil[evento_rock==1,.N], pct(mean(perfil$evento_rock))))
kv("Perfis censurados (sem contato)",    sprintf("%d (%s)", perfil[evento_rock==0,.N], pct(mean(perfil$evento_rock==0))))
cat("  (Variante A - 'atingiu o limite L' - calculada por limite na secao 4)\n")

# --- 4. Alvo por limite de profundidade (D3) ---------------------------------
secao("4. Alvo e Variante A por limite de profundidade")
# estoque_ate(cap): estoque cumulativo de cada perfil ate a profundidade 'cap'
estoque_ate <- function(cap) {
  dt[profund_inf <= cap, .(s = max(soc_stock_gm2_cum_qmap)), by = id]
}
for (L in LIMITES) {
  s <- estoque_ate(L); setnames(s, "s", paste0("alvo_", L))
  perfil <- merge(perfil, s, by = "id", all.x = TRUE)
  perfil[, (paste0("tempo_lim_", L))  := pmin(tempo_espessura, L)]          # tempo truncado em L
  perfil[, (paste0("evento_lim_", L)) := as.integer(tempo_espessura >= L)]  # Variante A: atingiu L
  perfil[, (paste0("cens_", L))       := as.integer(tempo_espessura < L)]   # censurado no limite L
  perfil[, (paste0("logalvo_", L))    := log1p(get(paste0("alvo_", L)))]    # alvo em log
  kv(sprintf("Limite %d cm: censurados (Variante A)", L), sprintf("%d (%s)",
     perfil[get(paste0("cens_",L))==1,.N], pct(mean(perfil[[paste0("cens_",L)]]))))
  kv(sprintf("Limite %d cm: alvo NA (sem camada <= L)", L), perfil[is.na(get(paste0("alvo_",L))),.N])
}

# --- 5. Covariaveis ambientais por perfil ------------------------------------
secao("5. Anexar covariaveis (constantes por perfil)")
# Pega a primeira ocorrencia NAO-NA de cada preditor dentro do perfil
primeiro_naNA <- function(x) { i <- which(!is.na(x))[1]; x[if (is.na(i)) 1L else i] }  # preserva a classe da coluna (NA tipado se tudo NA)
cov_perfil <- dt[, lapply(.SD, primeiro_naNA), by = id, .SDcols = preditores]
# coordenadas geograficas para CV/plot (nao usadas como preditor)
geo <- dt[, .(coord_x = primeiro_naNA(coord_x), coord_y = primeiro_naNA(coord_y),
              bioma = primeiro_naNA(bioma_nome)), by = id]
perfil <- Reduce(function(a,b) merge(a,b,by="id",all.x=TRUE), list(perfil, cov_perfil, geo))
kv("Colunas na tabela de perfil", ncol(perfil))

# --- 6. Faltantes (D2) -------------------------------------------------------
secao("6. Tratamento de faltantes")
n0 <- nrow(perfil)
perfil <- perfil[!is.na(coord_x) & !is.na(coord_y)]   # sem coords -> fora (CV espacial)
kv("Perfis removidos por falta de coordenada", sprintf("%d (%s)", n0-nrow(perfil), pct((n0-nrow(perfil))/n0)))

# Salva estado PRE-imputacao + metadados (usados por 02b p/ comparar metodos)
saveRDS(perfil, file.path(dir_proc, "perfil_pre_imputacao.rds"))
saveRDS(list(preditores = preditores, categoricas = categoricas),
        file.path(dir_proc, "meta_preditores.rds"))

# Flags de faltante (criadas ANTES de imputar; independem do metodo) + imputacao
# moda(x): valor mais frequente (usada para imputar variaveis categoricas)
moda <- function(x){ ux <- unique(x[!is.na(x)]); ux[which.max(tabulate(match(x, ux)))] }
imputar_knn <- function(d, vars, k) {                       # imputacao KNN (VIM::kNN)
  cand <- c("coord_x","coord_y","coord_x_utm","coord_y_utm","altitude","slope","cti",
            "hand","convergence","roughness","spi","elev_stdev","northness","eastness")
  don  <- intersect(cand, names(d))
  don  <- don[vapply(don, function(v) !anyNA(d[[v]]), logical(1))]   # doadores completos
  sub  <- d[, unique(c(vars, don)), with = FALSE]
  for (v in intersect(categoricas, names(sub))) sub[, (v) := as.factor(get(v))]
  imp  <- VIM::kNN(sub, variable = vars, dist_var = don, k = k, imp_var = FALSE)
  setDT(imp)
  for (v in vars) d[, (v) := imp[[v]]]
  d
}
com_na <- preditores[vapply(preditores, function(v) anyNA(perfil[[v]]), logical(1))]
na_relatorio <- data.table(variavel = com_na,
                           n_na    = vapply(com_na, function(v) sum(is.na(perfil[[v]])), integer(1)),
                           prop_na = vapply(com_na, function(v) mean(is.na(perfil[[v]])), numeric(1)))
for (v in com_na) perfil[, (paste0("flag_na_", v)) := as.integer(is.na(get(v)))]

if (IMPUTACAO == "knn") {
  kv("Metodo de imputacao", sprintf("KNN (k=%d)", K_KNN))
  perfil <- imputar_knn(perfil, com_na, K_KNN)
} else {
  kv("Metodo de imputacao", "mediana/moda")
  for (v in com_na) {
    if (v %in% categoricas) perfil[is.na(get(v)), (v) := moda(perfil[[v]])]
    else                    perfil[is.na(get(v)), (v) := median(perfil[[v]], na.rm = TRUE)]
  }
}
stopifnot(!anyNA(perfil[, ..com_na]))
subsecao("Faltantes imputados (top 15)")
print(na_relatorio[order(-prop_na)][1:min(15,.N)])
fwrite(na_relatorio[order(-prop_na)], file.path(dir_tab, "02_faltantes_imputados.csv"))

# Fatores
for (v in categoricas) if (v %in% names(perfil)) perfil[, (v) := as.factor(get(v))]

# --- 7. Dobras de CV espacial (D4) -------------------------------------------
secao("7. Dobras de CV espacial (por dataset_id)")
# Cada dataset inteiro vai para uma unica dobra; balanceia por n. de perfis (greedy)
ds <- perfil[, .N, by = dataset_id][order(-N)]
ds[, fold := 0L]; carga <- integer(K_FOLDS)
for (i in seq_len(nrow(ds))) { f <- which.min(carga); ds$fold[i] <- f; carga[f] <- carga[f] + ds$N[i] }
perfil <- merge(perfil, ds[, .(dataset_id, fold)], by = "dataset_id", all.x = TRUE)
subsecao("Perfis por dobra")
print(perfil[, .N, by = fold][order(fold)])
saveRDS(perfil[, .(id, dataset_id, fold)], file.path(dir_proc, "dobras_espaciais.rds"))

# --- 8. Tabela camada-nivel (profundidade variavel: GAM/RF) ------------------
secao("8. Tabela camada-nivel (modelagem de profundidade variavel)")
# Preditores sao covariaveis de LOCAL (constantes por perfil, D1). Para garantir
# consistencia, a camada HERDA os mesmos valores ja imputados em 'perfil' (evita
# imputar de novo no nivel de camada e obter valores diferentes por duplicacao).
cols_base <- c("id","dataset_id","profund_inf","soc_stock_gm2_cum_qmap")
camada <- dt[id %in% perfil$id, ..cols_base]
camada <- merge(camada, perfil[, c("id","fold", preditores), with = FALSE], by = "id", all.x = TRUE)
for (v in categoricas) if (v %in% names(camada)) camada[, (v) := as.factor(get(v))]
stopifnot(!anyNA(camada[, ..preditores]))
kv("Camadas x colunas", sprintf("%d x %d", nrow(camada), ncol(camada)))

# --- 9. Salvar ---------------------------------------------------------------
secao("9. Salvar datasets processados")
saveRDS(perfil, file.path(dir_proc, "perfil_sobrevivencia.rds"))
saveRDS(camada, file.path(dir_proc, "camada_profvar.rds"))
kv("perfil_sobrevivencia.rds (linhas)", nrow(perfil))
kv("camada_profvar.rds (linhas)", nrow(camada))
cat("\n  Pronto. Revisar decisoes D1-D4 no cabecalho apos inspecionar as saidas.\n\n")
