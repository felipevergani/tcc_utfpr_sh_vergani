# =============================================================================
# R/cv_espacial.R  —  Definicao das dobras de validacao cruzada ESPACIAL
# =============================================================================
# CRITICO: gere as dobras UMA vez, salve os indices, e reutilize em TODOS os
# modelos. Dobras diferentes por modelo invalidam a comparacao.
# Opcoes: agrupar por dataset_id  OU  blocos geograficos (blockCV/spatialsample).
# -----------------------------------------------------------------------------

# TODO: library(blockCV) ou library(spatialsample)
# criar_dobras_espaciais <- function(dados, k = 10) { ... }   # retorna indices por dobra
# TODO: salvar dobras em data/processed/dobras_espaciais.rds
