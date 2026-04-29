# 📊 Modelagem de Dados Censurados – Estoque de Carbono do Solo

Este repositório apresenta a aplicação de técnicas de aprendizado de máquina para modelagem do estoque de carbono do solo em profundidade, considerando dados censurados.

## 🧠 Variáveis do Dataset (Wise Variables)

As variáveis utilizadas no modelo estão organizadas em diferentes níveis:

- **Dataset-wise**: identificam a origem dos dados e características gerais (ex: `dataset_id`, presença de fragmentos grosseiros).
- **Event-wise**: descrevem cada perfil de solo, incluindo localização geográfica, classificação do solo e variáveis ambientais (ex: coordenadas, ordem do solo, relevo, clima).
- **Layer-wise**: representam propriedades específicas de cada camada do solo (ex: profundidade, teor de argila, areia, densidade, carbono orgânico).
- **SoilGrids e variáveis derivadas**: estimativas padronizadas de propriedades do solo em diferentes profundidades.
- **Índices e indicadores**: incluem variáveis booleanas e probabilísticas relacionadas a classes de solo, geologia e uso da terra.

## 🎯 Variáveis Alvo (Resultados)

O modelo tem como objetivo prever:

- `soc_density_gcm3_qmap`: densidade de carbono orgânico no solo  
- `soc_stock_gm2_qmap`: estoque de carbono por camada  
- `soc_stock_gm2_cum_qmap`: estoque acumulado até determinada profundidade  

Essas variáveis representam o comportamento do carbono no solo ao longo do perfil, sendo essenciais para análises ambientais e climáticas.

## ⚠️ Dados Censurados

Algumas variáveis (ex: distância de áreas rochosas ou arenosas) são censuradas, ou seja, possuem limites superiores definidos. O modelo leva isso em consideração durante o treinamento.

## 📌 Objetivo

Avaliar como diferentes variáveis ambientais, físicas e químicas do solo influenciam o estoque de carbono, utilizando abordagens de aprendizado de máquina adaptadas para dados censurados.
