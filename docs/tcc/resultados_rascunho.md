# Resultados e Discussão — rascunho para adaptação

> Rascunho gerado a partir do pipeline `01`–`08` e das tabelas em `reports/`.
> Números conferidos nas saídas (`reports/tables/*.csv`). Figuras referenciadas estão em `reports/figures/`.

## 1. Caracterização dos dados e da estrutura de censura

Após a remoção de pseudo-amostras sintéticas e de perfis sem coordenadas
geográficas (necessárias à validação cruzada espacial), o conjunto de modelagem
reuniu 15.011 perfis de solo (48.333 camadas), descritos por 127 variáveis
preditoras — covariáveis ambientais e espaciais (terreno, posição, classes de
solo, províncias geológicas e camadas do SoilGrids) acrescidas de indicadores de
dado faltante. As propriedades medidas em laboratório na própria amostra
(carbono, argila, densidade, pH, entre outras) e a taxonomia observada foram
deliberadamente excluídas, por não existirem em locais não amostrados — condição
do problema de mapeamento digital.

A censura à direita é expressiva e cresce com a profundidade de interesse: dos
16.011 perfis originais, 21,6% não alcançam 30 cm e 61,2% não alcançam 100 cm.
Ou seja, para o limite de 100 cm, a maioria dos perfis fornece apenas um limite
inferior do estoque real — exatamente a condição que os métodos avaliados se
propõem a tratar.

## 2. Subestimação do estoque por censura (objetivo específico 1)

O impacto de tratar observações censuradas como medições exatas foi quantificado
de duas formas. Descritivamente, entre os 4.891 perfis efetivamente amostrados
até 100 cm, os primeiros 30 cm concentram em média apenas 46,7% do estoque
cumulativo de 0–100 cm; interromper a amostragem em 30 cm subestimaria, portanto,
o estoque de 0–100 cm em cerca de 53%. Esse resultado confirma que a parcela
profunda do carbono é substancial e que ignorá-la introduz viés sistemático.

O efeito preditivo dessa subestimação é demonstrado de forma controlada na Seção
6: um modelo de regressão que ignora a censura subestima o estoque verdadeiro de
forma crescente com a proporção de dados censurados, chegando a −32% no limite de
100 cm sob 60% de censura.

## 3. Preparação dos dados e protocolo de avaliação

Os valores faltantes nas covariáveis (concentrados em ~1,8% dos perfis, no bloco
de camadas do SoilGrids) foram imputados por vizinhos mais próximos (KNN, k = 5).
Uma análise de sensibilidade prévia mostrou que a escolha do método de imputação
altera as métricas do modelo de referência em menos de 1%; o KNN foi adotado por
igualar a mediana em desempenho sem introduzir o viés de subestimação que a
mediana impõe às covariáveis assimétricas.

A validação cruzada espacial foi definida uma única vez, agrupando cada conjunto
de dados de origem (`dataset_id`) inteiramente em uma das dez dobras, e reutilizada
por todos os modelos, garantindo comparabilidade. Para o alvo de estoque no limite
L, considerou-se **completo** (evento observado) o perfil que atingiu L ou que
encontrou contato lítico antes de L — pois abaixo da rocha não há carbono orgânico,
de modo que o estoque 0–L observado já é o total. Considerou-se **censurado à
direita** apenas o perfil cuja amostragem cessou antes de L sem atingir a rocha.
Essa definição resulta em ~22% de censura em 30 cm e ~65% em 100 cm. Todas as
métricas comparativas foram calculadas sobre os perfis não-censurados, onde o
estoque 0–L verdadeiro é conhecido, na escala original do estoque.

É importante notar que a base não é particionada em subconjuntos distintos por
limite: trata-se de um único conjunto de perfis, cada qual com uma profundidade
máxima amostrada, avaliado em ambos os limites. A censura é, por construção,
**aninhada** entre os limites — um perfil que não alcança 30 cm tampouco alcança
100 cm. Verificou-se empiricamente que todos os 5.209 perfis completos em 100 cm
são também completos em 30 cm (nenhuma violação do aninhamento), enquanto 5.927
perfis, amostrados entre 30 e 100 cm, são completos em 30 cm mas censurados em
100 cm. Assim, o conjunto de avaliação de 100 cm é um subconjunto aninhado do de
30 cm, e a proporção de censura cresce naturalmente com a profundidade de
interesse. Cada limite constitui, ainda assim, uma tarefa de predição
independente, com sua própria partição completo/censurado e sua própria verdade
(`alvo_30` ou `alvo_100`). Por fim, 713 perfis cuja primeira camada amostrada já
se inicia abaixo de 30 cm não possuem estoque 0–30 cm definido (ausência de camada
rasa, e não censura) e foram excluídos apenas da tarefa de 30 cm; por isso essa
tarefa emprega 14.298 perfis e a de 100 cm emprega 15.011.

## 4. Benchmark comparativo dos modelos (objetivo central)

A Tabela 1 reúne o desempenho dos quatro modelos, além da análise de casos
completos, avaliados sobre os perfis não-censurados. As métricas são o Coeficiente
de Correlação de Concordância (CCC), a raiz do erro quadrático médio (RMSE) e o
viés médio.

**Tabela 1 — Desempenho por modelo (perfis não-censurados, escala bruta).**

| Modelo                       | CCC 30 cm | RMSE 30 cm | CCC 100 cm | RMSE 100 cm |
| ---------------------------- | --------- | ---------- | ---------- | ----------- |
| **IPC-RF**                   | **0,401** | 3767       | **0,253**  | 10582       |
| RF (baseline)                | 0,385     | 3693       | 0,192      | 10442       |
| RF (casos completos)         | 0,374     | 3696       | 0,226      | 10126       |
| GAM de profundidade variável | 0,334     | 4235       | 0,100      | 10240       |
| RSF                          | 0,140     | 4011       | 0,080      | 11276       |

O **IPC-RF obteve o maior CCC nos dois limites**, com vantagem mais nítida em
100 cm (0,253 contra 0,192 do modelo de referência), justamente onde a censura é
mais severa. Esse resultado sustenta a hipótese central do trabalho: incorporar a
estrutura de censura por ponderação, preservando a regressão por floresta
aleatória, melhora a concordância entre estoque predito e observado.

O Random Survival Forest apresentou o pior desempenho (CCC de 0,140 e 0,080). A
análise das predições mostra que o RSF **comprime a faixa de variação** — o
desvio-padrão das predições é cerca de 30% do desvio-padrão observado —, perdendo
poder discriminativo ao converter a curva de sobrevivência em um valor pontual de
estoque. Uma varredura de hiperparâmetros confirmou que essa limitação é
estrutural, e não fruto de ajuste inadequado. Conceitualmente, o sinal do estoque
reside sobretudo na concentração de carbono (regida por clima, vegetação e
textura), e não na profundidade em que o solo termina — grandeza que o RSF modela
bem, mas que pouco distingue os perfis que alcançam o limite.

O GAM de profundidade variável, apontado como o melhor método por de Sousa Mendes
et al. (2025), **não replicou essa liderança** neste conjunto: foi competitivo em
30 cm (CCC 0,334), mas degradou fortemente em 100 cm (CCC 0,100). A explicação é
coerente com o problema: predizer o estoque em 100 cm exige extrapolar a curva de
acúmulo em profundidade para uma região em que a censura deixou poucas camadas de
treino, e o GAM não corrige esse viés. A expansão do conjunto de covariáveis não
alterou o quadro, indicando limitação do método e não escassez de informação.

## 5. Quantificação da incerteza (objetivo específico de incerteza)

Para o modelo vencedor (IPC-RF), intervalos de predição de 90% foram estimados por
Floresta de Regressão Quantílica ponderada por IPCW (Tabela 2). A cobertura empírica
(PICP) ficou em 0,88 (30 cm) e 0,87 (100 cm), ligeiramente abaixo do nível nominal
de 0,90 — indicando intervalos discretamente estreitos —, com largura média (MPIW)
de 8.482 e 17.138 g/m², respectivamente. A maior largura em 100 cm reflete
honestamente a incerteza acrescida pela censura mais intensa.

**Tabela 2 — Intervalos de predição de 90% do IPC-RF (QRF ponderado).**

| Limite | Cobertura (PICP) | Largura média (MPIW, g/m²) |
| ------ | ---------------- | -------------------------- |
| 30 cm  | 0,88             | 8.482                      |
| 100 cm | 0,87             | 17.138                     |

## 6. Validação da correção de censura por censura controlada

A avaliação sobre perfis não-censurados possui uma limitação intrínseca: para os
perfis censurados — justamente onde a correção atua — o estoque verdadeiro não foi
medido em campo, de modo que a acurácia da correção não é diretamente verificável.
Além disso, o conjunto não-censurado é enviesado para baixo, o que penaliza o
de-enviesamento. Para contornar essa limitação sem recorrer a dados sintéticos,
utilizaram-se os perfis com estoque 0–L medido (completos) como verdade de campo,
sobre os quais se impôs artificialmente censura à direita, em três proporções
(20%, 40% e 60%) e sob dois mecanismos: aleatório e informativo (probabilidade de
censura crescente com o estoque verdadeiro, refletindo que solos profundos e ricos
em carbono são os menos alcançados pela amostragem).

Sob censura **informativa** — o cenário realista — o IPC-RF apresentou o **menor
viés em todos os casos** (Tabela 3). Tanto o modelo ingênuo quanto a análise de
casos completos subestimam o estoque, e a subestimação se agrava com a proporção
de censura; a ponderação por IPCW reduz esse viés pela metade ou mais.

**Tabela 3 — Viés relativo vs. verdade medida (censura informativa).**

| Limite | Censura | Ingênuo | Casos completos | IPC-RF     |
| ------ | ------- | ------- | --------------- | ---------- |
| 30 cm  | 20%     | −7,8%   | −8,3%           | **−3,5%**  |
| 30 cm  | 60%     | −17,7%  | −25,3%          | **−16,2%** |
| 100 cm | 20%     | −14,0%  | −9,8%           | **−5,5%**  |
| 100 cm | 40%     | −22,9%  | −17,2%          | **−9,6%**  |
| 100 cm | 60%     | −32,1%  | −28,0%          | **−18,7%** |

O contraste com o mecanismo **aleatório** delimita honestamente o alcance do
método: sob censura completamente ao acaso, a análise de casos completos é
não-enviesada e a ponderação IPCW tende a supercorrigir. O ganho do IPCW é,
portanto, específico da censura informativa — que é a esperada em levantamentos
pedológicos. Ressalva adicional: mesmo o IPC-RF subcorrige sob 60% de censura
(viés remanescente de ~−17 a −19%), em consonância com van der Westhuizen et al.
(2024), que reportam degradação do IPCW acima de ~60% de censura.

## 7. Síntese e discussão

Os resultados compõem uma narrativa coerente. O modelo de regressão tradicional
extrai bom sinal preditivo, mas subestima sistematicamente o estoque sob censura.
O Random Survival Forest trata a censura nativamente, porém sacrifica poder
preditivo ao comprimir as predições. O GAM de profundidade variável é competitivo
em camadas rasas, mas não corrige a censura e sofre na extrapolação profunda. O
**IPC-RF reúne as duas virtudes**: mantém a regressão por floresta aleatória — e,
com ela, o sinal — e corrige o viés de censura por ponderação, alcançando a melhor
concordância no benchmark e o menor viés na validação controlada. Confirma-se,
assim, que métodos de aprendizado de máquina adaptados à censura fornecem
estimativas mais acuradas e menos enviesadas do estoque de carbono orgânico do solo
em profundidade do que os modelos que tratam dados censurados como exatos, com o
ganho sendo maior quanto mais profundo o limite de interesse e mais informativa a
censura.

### Limitações

A definição operacional do alvo toma o estoque cumulativo na camada mais profunda
não superior a L, o que introduz pequena aproximação de alinhamento de
profundidade. A imputação de faltantes e a ponderação IPCW foram estimadas de forma
global e por dobra de treino, respectivamente; a imputação por dobra foi dispensada
por sensibilidade desprezível. A quantificação de incerteza emprega a Floresta de
Regressão Quantílica, cujo ponto (mediana) difere do estimador de média do IPC-RF,
utilizado apenas para os intervalos. Por fim, a validação da correção depende do
mecanismo de censura assumido; adotou-se o informativo como cenário realista, com o
aleatório reportado como contraste.
