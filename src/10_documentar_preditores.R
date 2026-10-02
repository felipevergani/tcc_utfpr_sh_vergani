# =============================================================================
# 10_documentar_preditores.R  —  Documenta os preditores usados (proveniencia)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Gera outputs/tabelas/preditores_usados.csv: cada preditor usado na modelagem,
# sua categoria e a descricao correspondente no dicionario docs/variables.txt.
# Observacao: o pipeline (02_preprocess.R) seleciona os preditores por categoria
# no proprio codigo; este script documenta essa selecao e a cruza com o
# variables.txt para rastreabilidade. Nao altera a modelagem.
# =============================================================================
pacotes <- c("here", "data.table")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table)

meta <- readRDS(here("data", "processed", "meta_preditores.rds"))
pred <- meta$preditores; categoricas <- meta$categoricas

terreno  <- c("altitude","convergence","cti","dev_magnitude","eastness","elev_stdev",
              "geomorphon","hand","northness","pcurv","roughness","slope","spi")
espacial <- c("coord_x_utm","coord_y_utm")
contexto <- c("lulc","bioma_nome","rockyIndex","sandyIndex","black_soils")
classes  <- c("Acrisols","Alisols","Arenosols","Chernozems","Ferralsols","Gleysols",
              "Histosols","Leptosols","Lixisols","Luvisols","Nitisols","Phaeozems",
              "Planosols","Plinthosols","Podzols","Regosols","Stagnosols","Umbrisols","Vertisols")
categoria <- function(v) {
  if (v %in% terreno)  "Terreno/relevo"
  else if (v %in% espacial) "Espacial (coordenadas UTM)"
  else if (v %in% contexto) "Contexto ambiental"
  else if (v %in% classes)  "Classe de solo (prob. SoilGrids)"
  else if (grepl("Prov$", v)) "Provincia geologica"
  else if (grepl("^(bdod|clay|sand|soc|cfvo)_[0-9]+_[0-9]+cm$", v)) "SoilGrids (por profundidade)"
  else "Outro"
}

# descricoes do dicionario variables.txt (linhas do tipo: "nome", # descricao)
lin <- readLines(here("docs", "variables.txt"), warn = FALSE)
mm  <- regmatches(lin, regexec('"([^"]+)"\\s*,?\\s*#\\s*(.*)$', lin))
dic <- rbindlist(lapply(mm, function(x) if (length(x) == 3)
  data.table(variavel = x[2], descricao = trimws(x[3])) else NULL))

out <- data.table(variavel = pred, categoria = vapply(pred, categoria, character(1)),
                  tipo = ifelse(pred %in% categoricas, "categorica", "numerica"))
out <- merge(out, unique(dic, by = "variavel"), by = "variavel", all.x = TRUE, sort = FALSE)
setorder(out, categoria, variavel)
fwrite(out, here("outputs", "tabelas", "preditores_usados.csv"))
cat(sprintf("Preditores documentados: %d -> outputs/tabelas/preditores_usados.csv\n", nrow(out)))
print(out[, .N, by = categoria])
