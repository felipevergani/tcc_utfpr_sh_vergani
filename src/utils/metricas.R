# =============================================================================
# R/metricas.R  —  Funcoes de avaliacao reutilizadas por todos os modelos
# =============================================================================
# Defina UMA vez para garantir que "RMSE"/"CCC"/"vies" signifiquem o mesmo
# em todos os scripts (03-07).
# -----------------------------------------------------------------------------

# Raiz do erro quadratico medio
rmse <- function(obs, pred) {
  sqrt(mean((pred - obs)^2, na.rm = TRUE))
}

# Erro absoluto medio
mae <- function(obs, pred) {
  mean(abs(pred - obs), na.rm = TRUE)
}

# Concordance Correlation Coefficient (Lin, 1989)
ccc <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]; pred <- pred[ok]
  mo <- mean(obs); mp <- mean(pred)
  vo <- mean((obs - mo)^2); vp <- mean((pred - mp)^2)
  cov_op <- mean((obs - mo) * (pred - mp))
  (2 * cov_op) / (vo + vp + (mo - mp)^2)
}

# Vies medio (positivo = superestima; negativo = subestima)
vies <- function(obs, pred) {
  mean(pred - obs, na.rm = TRUE)
}

# R2 (coeficiente de determinacao)
r2 <- function(obs, pred) {
  1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

# Resumo em uma linha (data.frame de 1 linha)
metricas_resumo <- function(obs, pred) {
  data.frame(n = sum(is.finite(obs) & is.finite(pred)),
             rmse = rmse(obs, pred), mae = mae(obs, pred),
             ccc = ccc(obs, pred), vies = vies(obs, pred), r2 = r2(obs, pred))
}
