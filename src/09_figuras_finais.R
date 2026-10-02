# =============================================================================
# 09_figuras_finais.R  —  Figuras finais (publicacao / leitores leigos)
# TCC: AM na Modelagem de Dados Censurados (Estoque de COS)
# -----------------------------------------------------------------------------
# Le as saidas dos scripts 01-08 e gera figuras caprichadas (titulos que afirmam
# a conclusao, paleta segura p/ daltonicos - Okabe-Ito, 200 dpi). Nao refaz
# modelagem: so visualizacao. Rodar por ultimo.
# Entrada: outputs/tabelas/{07_benchmark,08_simulacao,05_ipc_rf_pred}.csv,
#          outputs/tabelas/01_vies_subestimacao_completos.csv,
#          data/processed/perfil_sobrevivencia.rds
# Saida:   outputs/figuras_finais/*.png
# =============================================================================

pacotes <- c("here", "data.table", "ggplot2", "scales", "survival")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(here); library(data.table); library(ggplot2); library(scales); library(survival)
source(here("R", "metricas.R"))   # ccc() para a fig8

dir_tab <- here("outputs", "tabelas")
dir_ff  <- here("outputs", "figuras_finais"); dir.create(dir_ff, showWarnings = FALSE, recursive = TRUE)
salva <- function(g, nome, w, h) {
  ggsave(file.path(dir_ff, nome), g, width = w, height = h, dpi = 200, bg = "white")
  cat("  [figura]", nome, "\n")
}

# Paleta Okabe-Ito
OI <- c(ing = "#D55E00", comp = "#E69F00", ipcw = "#009E73", base = "#0072B2",
        base2 = "#56B4E9", rsf = "#999999", gam = "#CC79A7")

# Tema comum
theme_tcc <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", size = rel(1.22)),
          plot.subtitle = element_text(color = "grey35", size = rel(0.82)),
          plot.title.position = "plot",
          strip.text = element_text(face = "bold", size = rel(1.0)),
          panel.grid.minor = element_blank(),
          axis.title = element_text(color = "grey25"),
          legend.position = "bottom", legend.title = element_text(color = "grey25"))
}
lab_lim <- function(x) factor(paste0("Limite de ", x, " cm"),
                              levels = c("Limite de 30 cm", "Limite de 100 cm"))

# reorder_within (inline, evita dependencia do tidytext) --------------------
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}
scale_y_reordered <- function(..., sep = "___")
  ggplot2::scale_y_discrete(labels = function(x) gsub(paste0(sep, ".+$"), "", x), ...)

# ===========================================================================
# FIG 1 — Vies vs censura (simulacao, mecanismo informativo)
# ===========================================================================
s <- fread(file.path(dir_tab, "08_simulacao.csv"))[mecanismo == "informativo"]
s[, metodo_lab := factor(fifelse(metodo == "naive", "Ingênuo (ignora a censura)",
                          fifelse(metodo == "complete_case", "Casos completos",
                                  "IPC-RF (corrige a censura)")),
                         levels = c("Ingênuo (ignora a censura)", "Casos completos",
                                    "IPC-RF (corrige a censura)"))]
s[, limite_lab := lab_lim(limite)]
pal1 <- c("Ingênuo (ignora a censura)" = OI[["ing"]], "Casos completos" = OI[["comp"]],
          "IPC-RF (corrige a censura)" = OI[["ipcw"]])
ann1 <- data.table(limite_lab = lab_lim(100), censura_real = 0.42, vies_rel = -0.30,
                   metodo_lab = factor("Ingênuo (ignora a censura)", levels = levels(s$metodo_lab)),
                   txt = "quanto mais censura,\nmais o ingênuo subestima")
g1 <- ggplot(s, aes(censura_real, vies_rel, colour = metodo_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
  geom_text(data = ann1, aes(label = txt), color = OI[["ing"]], size = 3, hjust = 0, lineheight = 0.9) +
  facet_wrap(~ limite_lab) +
  scale_colour_manual(values = pal1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = c(.2, .4, .6)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Ignorar a censura subestima o carbono do solo — o IPC-RF corrige o viés",
       subtitle = "Verdade medida: perfis profundos foram censurados artificialmente e comparados ao valor real",
       x = "Proporção de dados censurados",
       y = "Erro de estimativa do estoque\n(negativo = subestima)", colour = NULL) +
  theme_tcc()
salva(g1, "fig1_vies_censura.png", 11, 5.4)

# ===========================================================================
# FIG 2 — Benchmark CCC por modelo
# ===========================================================================
b <- fread(file.path(dir_tab, "07_benchmark.csv"))
nomes <- c("IPC-RF" = "IPC-RF", "RF (baseline)" = "RF ingênuo",
           "RF (casos completos)" = "RF casos completos",
           "GAM prof. variavel" = "GAM prof. variável", "RSF" = "RSF")
fillmap <- c("IPC-RF" = OI[["ipcw"]], "RF (baseline)" = OI[["base"]],
             "RF (casos completos)" = OI[["base2"]], "GAM prof. variavel" = OI[["gam"]],
             "RSF" = OI[["rsf"]])
b[, modelo_lab := nomes[modelo]][, limite_lab := lab_lim(limite)]
g2 <- ggplot(b, aes(ccc, reorder_within(modelo_lab, ccc, limite), fill = modelo)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.3f", ccc),
                fontface = ifelse(modelo == "IPC-RF", "bold", "plain")),
            hjust = -0.15, size = 3.6) +
  facet_wrap(~ limite_lab, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = fillmap, guide = "none") +
  scale_x_continuous(limits = c(0, 0.47), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "IPC-RF tem a melhor concordância entre estoque predito e observado",
       subtitle = "CCC = coeficiente de concordância; cada modelo avaliado nos perfis não-censurados",
       x = "CCC — concordância (quanto maior, melhor)", y = NULL) +
  theme_tcc()
salva(g2, "fig2_benchmark_ccc.png", 11, 4.6)

# ===========================================================================
# FIG 3 — Subestimacao: quanto do estoque esta abaixo de 30 cm (objetivo 1)
# ===========================================================================
sub <- fread(file.path(dir_tab, "01_vies_subestimacao_completos.csv"))
fr30 <- mean(sub$frac_capturada_0_30, na.rm = TRUE)
d3 <- data.table(camada = factor(c("0–30 cm (raso)", "30–100 cm (profundo)"),
                                 levels = c("30–100 cm (profundo)", "0–30 cm (raso)")),
                 frac = c(fr30, 1 - fr30))
g3 <- ggplot(d3, aes(x = "", y = frac, fill = camada)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = percent(frac, accuracy = 1)), position = position_stack(vjust = 0.5),
            size = 5.5, color = "white", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("0–30 cm (raso)" = OI[["comp"]],
                               "30–100 cm (profundo)" = OI[["base"]])) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Metade do carbono do solo está abaixo de 30 cm",
       subtitle = "Distribuição média do estoque 0–100 cm entre os perfis amostrados até 100 cm",
       x = NULL, y = "Fração do estoque de 0–100 cm", fill = NULL) +
  theme_tcc() +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
salva(g3, "fig3_subestimacao.png", 9, 3.2)

# ===========================================================================
# FIG 4 — Observado vs predito do IPC-RF (vencedor)
# ===========================================================================
p <- fread(file.path(dir_tab, "05_ipc_rf_pred.csv"))[evento == 1 & is.finite(pred_ipcw)]
p[, limite_lab := lab_lim(limite)]
g4 <- ggplot(p, aes(tempo, pred_ipcw)) +
  geom_point(alpha = 0.10, size = 0.5, colour = OI[["ipcw"]]) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ limite_lab, scales = "free") +
  labs(title = "IPC-RF: estoque predito vs. observado",
       subtitle = "Cada ponto é um perfil; a linha tracejada vermelha é a predição perfeita",
       x = "Estoque observado (g/m²)", y = "Estoque predito (g/m²)") +
  theme_tcc()
salva(g4, "fig4_obs_vs_pred_ipcrf.png", 10, 5)

# ===========================================================================
# FIG 5 — Ilustracao do conceito de censura (perfis de exemplo)
# ===========================================================================
prof <- data.table(
  nome  = factor(c("P1", "P2", "P3", "P4", "P5"), levels = c("P1","P2","P3","P4","P5")),
  x     = 1:5,
  fundo = c(20, 45, 80, 130, 55),
  rocha = c(FALSE, FALSE, FALSE, FALSE, TRUE))
prof[, status := fifelse(fundo >= 100 | rocha, "Completo (30 e 100 cm)",
                  fifelse(fundo >= 30, "Completo em 30, censurado em 100", "Censurado (30 e 100 cm)"))]
prof[, status := factor(status, levels = c("Completo (30 e 100 cm)",
     "Completo em 30, censurado em 100", "Censurado (30 e 100 cm)"))]
palc <- c("Completo (30 e 100 cm)" = OI[["ipcw"]],
          "Completo em 30, censurado em 100" = OI[["comp"]],
          "Censurado (30 e 100 cm)" = OI[["ing"]])
g5 <- ggplot(prof) +
  geom_rect(aes(xmin = x - 0.32, xmax = x + 0.32, ymin = 0, ymax = fundo, fill = status)) +
  geom_hline(yintercept = 30, linetype = "dashed", color = OI[["comp"]]) +
  geom_hline(yintercept = 100, linetype = "dashed", color = OI[["base"]]) +
  geom_point(data = prof[rocha == TRUE], aes(x = x, y = fundo), shape = 17, size = 3) +
  annotate("text", x = 0.35, y = 30, label = "30 cm", vjust = -0.4, hjust = 0, size = 3.4, color = OI[["comp"]]) +
  annotate("text", x = 0.35, y = 100, label = "100 cm", vjust = -0.4, hjust = 0, size = 3.4, color = OI[["base"]]) +
  annotate("text", x = 5, y = 55, label = "rocha", vjust = -1.1, size = 3, fontface = "italic") +
  scale_y_reverse(breaks = c(0, 30, 100, 150)) +
  scale_x_continuous(breaks = 1:5, labels = levels(prof$nome)) +
  scale_fill_manual(values = palc) +
  labs(title = "O que é censura à direita no estoque de carbono",
       subtitle = "Cada barra é um perfil de solo; a amostragem que para antes do limite censura o estoque",
       x = "Perfis de exemplo", y = "Profundidade amostrada (cm)", fill = NULL) +
  theme_tcc() + theme(panel.grid.major.x = element_blank())
salva(g5, "fig5_conceito_censura.png", 9, 5)

# ===========================================================================
# FIG 6 — Distribuicao espacial dos perfis (contexto dos dados)
# ===========================================================================
perfil <- readRDS(here("data", "processed", "perfil_sobrevivencia.rds"))
mp <- perfil[!is.na(coord_x) & !is.na(coord_y), .(coord_x, coord_y, bioma)]
g6 <- ggplot(mp, aes(coord_x, coord_y, colour = bioma)) +
  geom_point(size = 0.35, alpha = 0.5) + coord_quickmap() +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Distribuição dos perfis de solo no Brasil",
       subtitle = sprintf("%s perfis usados na modelagem, coloridos por bioma", formatC(nrow(mp), format = "d", big.mark = ".")),
       x = "Longitude", y = "Latitude", colour = "Bioma") +
  theme_tcc()
salva(g6, "fig6_mapa_perfis.png", 8, 7)

# ===========================================================================
# Predicoes por perfil dos 4 modelos (nao-censurados) — base das figs 7-9
# ===========================================================================
read_pred <- function(file, predcol, modelo) {
  d <- fread(file.path(dir_tab, file))
  if ("evento" %in% names(d)) d <- d[evento == 1]
  ob <- if ("obs" %in% names(d)) d$obs else d$tempo
  data.table(modelo = modelo, limite = d$limite, obs = ob, pred = d[[predcol]])[is.finite(pred)]
}
preds <- rbindlist(list(
  read_pred("03_baseline_rf_pred.csv", "pred",      "RF ingênuo"),
  read_pred("04_rsf_pred.csv",         "pred_rmst", "RSF"),
  read_pred("05_ipc_rf_pred.csv",      "pred_ipcw", "IPC-RF"),
  read_pred("06_gam_pred.csv",         "pred",      "GAM prof. variável")))
preds[, modelo := factor(modelo, levels = c("IPC-RF","RF ingênuo","GAM prof. variável","RSF"))]
preds[, limite_lab := lab_lim(limite)]
palm <- c("RF ingênuo" = OI[["base"]], "RSF" = OI[["rsf"]],
          "IPC-RF" = OI[["ipcw"]], "GAM prof. variável" = OI[["gam"]])
# limite de eixo por limite (corta cauda extrema para leitura)
caps <- preds[modelo == "IPC-RF", .(cap = as.numeric(quantile(obs, 0.98))), by = limite]
preds <- merge(preds, caps, by = "limite")

# ===========================================================================
# FIG 7 — Compressao: distribuicao das predicoes vs verdade
# ===========================================================================
pf7 <- preds[obs <= cap & pred <= cap]
obs7 <- unique(pf7[modelo == "IPC-RF", .(limite_lab, valor = obs)])
g7 <- ggplot() +
  geom_density(data = obs7, aes(valor), fill = "grey82", colour = NA) +
  geom_density(data = pf7, aes(pred, colour = modelo), linewidth = 0.9) +
  facet_wrap(~ limite_lab, scales = "free") +
  scale_colour_manual(values = palm) +
  scale_x_continuous(labels = comma) +
  labs(title = "Cada modelo reproduz a distribuição real do estoque de forma diferente",
       subtitle = "Área cinza = distribuição real. A 100 cm o RF ingênuo desloca-se para baixo (subestima) e o RSF é o mais irregular; o IPC-RF é o mais próximo do real",
       x = "Estoque (g/m²)", y = "Densidade", colour = NULL) +
  theme_tcc()
salva(g7, "fig7_compressao_densidade.png", 11, 5)

# ===========================================================================
# FIG 8 — Decomposicao do CCC: correlacao x razao de dispersao
# ===========================================================================
met <- preds[, .(r = cor(obs, pred), sd_ratio = sd(pred) / sd(obs)), by = .(modelo, limite)]
met[, limite_lab := lab_lim(limite)]
g8 <- ggplot(met, aes(r, sd_ratio, colour = modelo)) +
  geom_point(size = 4.5) +
  geom_text(aes(label = modelo), vjust = -1.1, size = 3.1, show.legend = FALSE) +
  facet_wrap(~ limite_lab) +
  scale_colour_manual(values = palm, guide = "none") +
  expand_limits(y = c(0.15, 0.85), x = c(0.1, 0.55)) +
  labs(title = "CCC alto exige correlação E dispersão realista — o RSF falha nas duas",
       subtitle = "Direita = predições mais correlacionadas; cima = menos compressão (razão → 1 é o ideal)",
       x = "Correlação (r) entre observado e predito",
       y = "Razão de dispersão (sd predito / sd observado)") +
  theme_tcc()
salva(g8, "fig8_decomposicao_ccc.png", 11, 5)

# ===========================================================================
# FIG 9 — Observado vs predito dos 4 modelos (pequenos multiplos)
# ===========================================================================
pf9 <- preds[obs <= cap & pred <= cap * 1.4]
g9 <- ggplot(pf9, aes(obs, pred)) +
  geom_point(alpha = 0.05, size = 0.35, colour = "#333333") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  facet_grid(modelo ~ limite_lab, scales = "free") +
  labs(title = "Observado vs. predito — os quatro modelos",
       subtitle = "Linha vermelha = predição perfeita; nuvem achatada na horizontal indica compressão",
       x = "Estoque observado (g/m²)", y = "Estoque predito (g/m²)") +
  theme_tcc()
salva(g9, "fig9_obs_pred_todos.png", 9, 11)

# ===========================================================================
# FIG 10 — Mecanismo do IPCW (curva da censura + pesos)
# ===========================================================================
dc <- data.table(tempo = perfil$alvo_100,
                 evento = as.integer(perfil$tempo_espessura >= 100 | perfil$evento_rock == 1))
dc <- dc[is.finite(tempo)]
km <- survfit(Surv(tempo, 1 - evento) ~ 1, data = dc)
gd <- data.table(stock = km$time, G = km$surv)[stock <= as.numeric(quantile(dc$tempo, 0.97))]
gd[, peso := 1 / pmax(G, 0.05)]
long <- rbind(
  data.table(stock = gd$stock, painel = "Probabilidade de NÃO ser censurado", valor = gd$G),
  data.table(stock = gd$stock, painel = "Peso IPCW (1 / probabilidade)",       valor = gd$peso))
long[, painel := factor(painel, levels = c("Probabilidade de NÃO ser censurado", "Peso IPCW (1 / probabilidade)"))]
g10 <- ggplot(long, aes(stock, valor)) +
  geom_line(colour = OI[["ipcw"]], linewidth = 1) +
  facet_wrap(~ painel, scales = "free_y", ncol = 1) +
  scale_x_continuous(labels = comma) +
  labs(title = "Como o IPC-RF corrige a censura",
       subtitle = "Estoques altos são raros entre os perfis completos → recebem peso maior no ajuste",
       x = "Estoque de COS a 100 cm (g/m²)", y = NULL) +
  theme_tcc()
salva(g10, "fig10_mecanismo_ipcw.png", 9, 6)

cat("\n  Figuras finais em outputs/figuras_finais/\n\n")
