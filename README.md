# 📊 Modelagem de Dados Censurados – Estoque de Carbono do Solo

Este repositório apresenta o Trabalho de Conclusão de Curso (TCC) intitulado **“Aprendizado de Máquina na Modelagem de Dados Censurados: Estoque de Carbono do Solo em Profundidade”**.

## 🧠 Variáveis do Dataset (Wise Variables)

As variáveis utilizadas no modelo estão organizadas em diferentes níveis:

- **Dataset-wise**: informações gerais sobre o conjunto de dados  
- **Event-wise**: características do perfil do solo (localização, clima, classificação)  
- **Layer-wise**: propriedades de cada camada (profundidade, textura, densidade, carbono)  
- **SoilGrids e derivadas**: estimativas padronizadas de atributos do solo  
- **Indicadores**: variáveis auxiliares relacionadas ao ambiente e uso do solo  

## 🎯 Variáveis Alvo (Resultados)

O modelo busca prever:

- `soc_density_gcm3_qmap`: densidade de carbono orgânico  
- `soc_stock_gm2_qmap`: estoque de carbono por camada  
- `soc_stock_gm2_cum_qmap`: estoque acumulado no perfil  

## ⚠️ Dados Censurados

O estudo considera a presença de dados censurados, ou seja, variáveis com limites de medição, incorporando isso na modelagem.



## 📁 Estrutura do Projeto

```bash
tcc_utfpr_sh_vergani/
├── data/
│   ├── raw/              # Dados brutos
│   ├── external/         # Dados externos (ex: SoilGrids)
│   ├── interim/          # Dados intermediários
│   └── processed/        # Dados prontos para modelagem
│
├── notebooks/
│   ├── exploratory/      # Análise exploratória (EDA)
│   ├── experiments/      # Testes de modelos
│   └── results/          # Resultados e visualizações
│
├── src/
│   ├── data/             # Processamento de dados
│   ├── features/         # Engenharia de atributos
│   ├── models/           # Treinamento e avaliação
│   ├── visualization/    # Geração de gráficos
│   └── utils/            # Funções auxiliares
│
├── models/
│   └── saved/            # Modelos treinados
│
├── reports/
│   ├── figures/          # Figuras do TCC
│   └── tables/           # Tabelas do TCC
│
├── docs/
│   └── tcc/              # Documentação e escrita do TCC
│
├── configs/
│   └── config.yaml       # Configurações do projeto
│
├── tests/                # Testes (opcional)
├── requirements.txt      # Dependências
├── main.py               # Pipeline principal
├── README.md
└── .gitignore
```
## 👨‍💻 Autor

**Felipe Brun Vergani**  
- Graduando em Ciência da Computação – Universidade Tecnológica Federal do Paraná (UTFPR), Campus Santa Helena  
- Bolsista de Iniciação Tecnológica (CNPq)  
- Integrante do Laboratório de Pedometria (UTFPR-SH)  
- Colaborador do Repositório de Dados do Solo Brasileiro (SoilData)  

Atua nas áreas de **Aprendizado de Máquina, padronização e harmonização de dados e modelagem ambiental**, com foco no desenvolvimento de bases de dados e mapas de estoque de carbono orgânico do solo.

🔗 Lattes: http://lattes.cnpq.br/4035704837425497  
🔗 ORCID: https://orcid.org/0009-0009-7597-4663  

## 📌 Objetivo

Investigar a influência de variáveis ambientais e do solo no estoque de carbono, utilizando modelos de aprendizado de máquina capazes de lidar com dados censurados.
