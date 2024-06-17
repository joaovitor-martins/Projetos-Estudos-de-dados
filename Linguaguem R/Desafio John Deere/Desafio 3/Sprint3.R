# Challenge - IntelliDeere - Disciplina: STATISTICAL COMPUTING WITH R - 1TIAR 2023:
# Arthur Coutinho Santos --- RM: 97804
# Camilly Alves --- RM: 550210 
# Guilherme Garcia Paschoalinoto --- RM: 99221
# João Vitor de Andrade Martins --- RM: 98744
# Murilo Krauss --- RM: 98262

# Carregar a biblioteca
library(readxl)

# Especificar o nome do arquivo Excel com barras invertidas duplas
Pre_Venda <- "C:\\Users\\Joao\\Downloads\\ChallengeR\\ChallengePre_Venda.xlsx"
Pos_Venda <- "C:\\Users\\Joao\\Downloads\\ChallengeR\\ChallengePos_Venda.xlsx"

# Importar o arquivo Excel
Pre_Venda <- read_excel(Pre_Venda)
Pos_Venda <- read_excel(Pos_Venda)

# Ex 1:
#A)
# Amostra 1
amCusto <- sample(Pos_Venda$Custo, 1)
amCusto
mediaCusto <- mean(Pos_Venda$Custo)
mediaCusto
dpCusto <- sd(Pos_Venda$Custo)
dpCusto

a1 <- pnorm(amCusto, mediaCusto, dpCusto, lower.tail = TRUE)
a1
# Evento pouco provável(Amostra = 100)

# Amostra 2
amTMA <- sample(Pos_Venda$`Tempo de Atendimento (em horas)`, 1)
amTMA
mediaTMA <- mean(Pos_Venda$`Tempo de Atendimento (em horas)`)
mediaTMA
dpTMA <- sd(Pos_Venda$`Tempo de Atendimento (em horas)`)
dpTMA

a2 <- pnorm(amTMA, mediaTMA, dpTMA, lower.tail = TRUE)
a2
# Evento pouco provável(Amostra = 3)

# Amostra 3
amArea <- sample(Pre_Venda$`Área Plantada`, 1)
amArea
mediaArea <- mean(Pre_Venda$`Área Plantada`)
mediaArea
dpArea <- sd(Pre_Venda$`Área Plantada`)
dpArea

a3 <- pnorm(amArea, mediaArea, dpArea, lower.tail = TRUE)
a3
# Evento muito provável(AMostra = 1120)

#B)
# Amostra 1
amCusto <- sample(Pos_Venda$Custo, 1)
amCusto
medianaCusto <- median(Pos_Venda$Custo)
medianaCusto
dpCusto <- sd(Pos_Venda$Custo)
dpCusto

b1 <- pnorm(amCusto, medianaCusto, dpCusto, lower.tail = FALSE)
b1
# Evento muito provável(Amostra = 100)

# Amostra 2
amTMA <- sample(Pos_Venda$`Tempo de Atendimento (em horas)`, 1)
amTMA
medianaTMA <- median(Pos_Venda$`Tempo de Atendimento (em horas)`)
medianaTMA
dpTMA <- sd(Pos_Venda$`Tempo de Atendimento (em horas)`)
dpTMA

b2 <- pnorm(amTMA, medianaTMA, dpTMA, lower.tail = FALSE)
b2
# Evento pouco provável(Amostra = 8)

# Amostra 3
amArea <- sample(Pre_Venda$`Área Plantada`, 1)
amArea
medianaArea <- median(Pre_Venda$`Área Plantada`)
medianaArea
dpArea <- sd(Pre_Venda$`Área Plantada`)
dpArea

b3 <- pnorm(amArea, medianaArea, dpArea, lower.tail = FALSE)
b3
# Evento pouco provável(Amostra = 1200)

#C)
# Amostra 1
amCusto <- sample(Pos_Venda$Custo, 1)
amCusto
mediaCusto <- mean(Pos_Venda$Custo)
mediaCusto
dpCusto <- sd(Pos_Venda$Custo)
dpCusto

limite_inferior1 <- mediaCusto - 2 * dpCusto
limite_superior1 <- mediaCusto + 2 * dpCusto

a <- pnorm(limite_inferior1, mediaCusto, dpCusto, lower.tail = TRUE)
b <- pnorm(limite_superior1, mediaCusto, dpCusto, lower.tail = TRUE)

pc1 <- b - a
pc1
# Evento muito provável(Amostra = 150)

# Amostra 2
amTMA <- sample(Pos_Venda$`Tempo de Atendimento (em horas)`, 1)
amTMA
mediaTMA <- mean(Pos_Venda$`Tempo de Atendimento (em horas)`)
mediaTMA
dpTMA <- sd(Pos_Venda$`Tempo de Atendimento (em horas)`)
dpTMA

limite_inferior2 <- mediaTMA - 2 * dpTMA
limite_superior2 <- mediaTMA + 2 * dpTMA

c <- pnorm(limite_inferior2, mediaTMA, dpTMA, lower.tail = TRUE)
d <- pnorm(limite_superior2, mediaTMA, dpTMA, lower.tail = TRUE)

pc2 <- d - c
pc2
# Evento muito provável(Amostra = 3)

# Amostra 3
amArea <- sample(Pre_Venda$`Área Plantada`, 1)
amArea
mediaArea <- mean(Pre_Venda$`Área Plantada`)
mediaArea
dpArea <- sd(Pre_Venda$`Área Plantada`)
dpArea

limite_inferior3 <- mediaArea - 2 * dpArea
limite_superior3 <- mediaArea + 2 * dpArea

e <- pnorm(limite_inferior3, mediaArea, dpArea, lower.tail = TRUE)
f <- pnorm(limite_superior3, mediaArea, dpArea, lower.tail = TRUE)

pc3 <- f - e
pc3
# Evento muito provável(Amostra = 1000)

# Ex 2:
# A análise probabilística dos dados é uma fonte crucial de informações para 
# orientar a tomada de decisão. Com ela, podemos estimar a probabilidade de 
# eventos ocorrerem, como valores abaixo da média, desempenho superior à mediana 
# e a probabilidade de observações dentro de um intervalo específico. Esses 
# resultados têm aplicações em diversas áreas, desde finanças até o controle de 
# qualidade do atendimento. Eles desempenham um papel fundamental na avaliação de 
# riscos, na alocação eficiente de recursos e na identificação de cenários 
# excepcionais. Utilizando o R e suas ferramentas estatísticas e de probabilidade, 
# podemos embasar decisões informadas em uma ampla gama de contextos, fornecendo 
# suportevalioso para o processo decisório.

# Ex 3:
# A)
na <- 20
alphaa <- 0.05

#V1 Pre_Venda Área Plantada
amostra120 <- c(1200, 1000, 900, 800, 1500, 700, 1100, 1300, 950, 1800, 1000, 1350, 850, 1120, 1200, 950, 1100, 1000, 1200, 1500)
media1 <- mean(Pre_Venda$`Área Plantada`)
media1
dp1 <- sd(Pre_Venda$`Área Plantada`)
dp1

'
H0: u >= 1085.07
H1: u < 1085.07
'
Zc1a <- (mean(amostra120) - media1)/(dp1/sqrt(na))
Zc1a

Za1a <- qnorm(alphaa)
Za1a

ifelse(Zc1a < Za1a, "Rejeita H0", "Aceita H0")
# Aceita H0

#V2 Pre_Venda Número de Vendas
amostra220 <- c(9, 8, 12, 7, 15, 8, 12, 9, 15, 9, 13, 8, 10, 7, 18, 16, 9, 11, 10, 7)
media2 <- mean(Pre_Venda$`Número de Vendas`)
media2
dp2 <- sd(Pre_Venda$`Número de Vendas`)
dp2

'
H0: u >= 11.26923
H1: u < 11.26923
'

Zc2a <- (mean(amostra220) - media2)/(dp2/sqrt(na))
Zc2a

Za2a <- qnorm(alphaa)
Za2a

ifelse(Zc2a < Za2a, "Rejeita H0", "Aceita H0")
# Aceita H0

#V3 Pos_Venda Custo
amostra320 <- c(200, 100, 100, 150, 200, 400, 200, 400, 500, 500, 350, 300, 300, 400, 300, 100, 350, 300, 200, 200)
media3 <- mean(Pos_Venda$Custo)
media3
dp3 <- sd(Pos_Venda$Custo)
dp3

'
H0: u >= 265.3509
H1: u < 265.3509
'

Zc3a <- (mean(amostra320) - media3)/(dp3/sqrt(na))
Zc3a

Za3a <- qnorm(alphaa)
Za3a

ifelse(Zc3a < Za3a, "Rejeita H0", "Aceita H0")
# Aceita H0

# B)
alphab <- 0.1
nb <- 25

# V1
amostra125 <- c(1200, 1000, 900, 800, 1500, 700, 1100, 1300, 950, 1800, 1000, 1350, 850, 1120, 1200, 950, 1100, 1000, 1200, 1500, 900, 800, 1350, 850, 1120)
media1 <- mean(Pre_Venda$`Área Plantada`)
media1
dp1 <- sd(Pre_Venda$`Área Plantada`)
dp1

'
H0: u <= 1085.07
H1: u > 1085.07
'
Zc1b <- (mean(amostra125) - media1)/(dp1/sqrt(nb))
Zc1b

Za1b <- qnorm(alphab)
Za1b

ifelse(Zc1b > Za1b, "Rejeita H0", "Aceita H0")
# Rejeita H0

# V2
amostra225 <- c(13, 12, 10, 9, 15, 8, 7, 9, 10, 15, 18, 11, 12, 10, 14, 13, 7, 16, 16, 15, 8, 9, 8, 11, 12)
media2 <- mean(Pre_Venda$`Número de Vendas`)
media2
dp2 <- sd(Pre_Venda$`Número de Vendas`)
dp2

'
H0: u <= 11.26923
H1: u > 11.26923
'

Zc2b <- (mean(amostra225) - media2)/(dp2/sqrt(nb))
Zc2b

Za2b <- qnorm(alphab)
Za2b

ifelse(Zc2b > Za2b, "Rejeita H0", "Aceita H0")
# Rejeita H0

# V3
amostra325 <- c(200, 100, 100, 150, 200, 400, 200, 400, 500, 500, 350, 300, 300, 400, 300, 100, 350, 300, 200, 200, 200, 150, 150, 500, 500)
media3 <- mean(Pos_Venda$Custo)
media3
dp3 <- sd(Pos_Venda$Custo)
dp3

'
H0: u <= 265.3509
H1: u > 265.3509
'

Zc3b <- (mean(amostra325) - media3)/(dp3/sqrt(nb))
Zc3b

Za3b <- qnorm(alphab)
Za3b

ifelse(Zc3b > Za3b, "Rejeita H0", "Aceita H0")
# Rejeita H0

# C)
alphac <- 0.01
nc <- 15

# V1
amostra115 <- c(1200, 1000, 900, 800, 1500, 700, 1100, 1300, 950, 1800, 1000, 1350, 850, 1120, 1200)
media1 <- mean(Pre_Venda$`Área Plantada`)
media1
dp1 <- sd(Pre_Venda$`Área Plantada`)
dp1

'
H0: u = 1085.07
H1: u <> 1085.07
'
Zc1c <- (mean(amostra115) - media1)/(dp1/sqrt(nc))
Zc1c

Za1c <- qnorm(alphac/2)
Za1c

ifelse(Zc1c < Za1c | Zc1c > (-Za1c), "Rejeita H0", "Aceita H0")
# Aceita H0

# V2
amostra215 <- c(11, 7, 9, 8, 12, 15, 13, 15, 7, 16, 10, 12, 8, 13, 10)
media2 <- mean(Pre_Venda$`Número de Vendas`)
media2
dp2 <- sd(Pre_Venda$`Número de Vendas`)
dp2

'
H0: u = 11.26923
H1: u <> 11.26923
'

Zc2c <- (mean(amostra215) - media2)/(dp2/sqrt(nc))
Zc2c

Za2c <- qnorm(alphac/2)
Za2c

ifelse(Zc2c < Za2c | Zc2c > (-Za2c), "Rejeita H0", "Aceita H0")
# Aceita H0

# V3
Amostra315 <- c(200, 100, 100, 150, 200, 400, 200, 400, 500, 500, 350, 300, 300, 400, 300)
media3 <- mean(Pos_Venda$Custo)
media3
dp3 <- sd(Pos_Venda$Custo)
dp3

'
H0: u = 265.3509
H1: u <> 265.3509
'

Zc3c <- (mean(Amostra315) - media3)/(dp3/sqrt(nc))
Zc3c

Za3c <- qnorm(alphac/2)
Za3c

ifelse(Zc3c < Za3c | Zc3c > (-Za3c), "Rejeita H0", "Aceita H0")
# Aceita H0

# Ex 4:

# Os testes de hipótese para a média são vitais na tomada de decisões. Eles validam 
# hipóteses estatísticas, avaliam a significância das diferenças nos dados e controlam 
# os riscos associados às decisões. Quando rejeitamos a hipótese nula, estamos 
# indicando que as diferenças observadas são estatisticamente significativas, o que
# orienta ações seguintes. Por outro lado, se aceitamos a hipótese nula, significa 
# que as variações nos dados são provavelmente devidas ao acaso, economizando recursos 
# e evitando ações desnecessárias. 
# A escolha do nível de significância é importante, pois afeta a probabilidade de 
# cometer erros ao interpretar os resultados. Isso ajuda na comunicação das descobertas 
# estatísticas de forma compreensível, facilitando a tomada de decisões baseadas 
# em evidências. Em resumo, os testes de hipótese são ferramentas essenciais para 
# tomar decisões informadas com confiança.

