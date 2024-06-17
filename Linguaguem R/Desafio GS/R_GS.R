# Carregar a biblioteca
library(readxl)
library(corrplot)
library(ggplot2)
library(lmtest)

# Especificar o nome do arquivo Excel com barras invertidas duplas
caminho_arquivo <- "C:\\Users\\marti_ezke8vn\\Downloads\\DatasetGS.xlsx"

# Importar o arquivo Excel
GS <- read_excel(caminho_arquivo)

# Variáveis aleatórias 1
amostra_V1 <- sample(GS$`Investimento (em milhões de dólares)`, 1)

# Dados para o pnorm()
media_V1 <- mean(GS$`Investimento (em milhões de dólares)`) 
dp_V1 <- sd(GS$`Investimento (em milhões de dólares)`)
mediana_V1 <- median(GS$`Investimento (em milhões de dólares)`)

# 1A) 
prob1 <- pnorm(amostra_V1, media_V1, dp_V1, lower.tail = TRUE)
prob1

# Evento pouco provavel (amostra = 10)

# B)
prob2 <- pnorm(amostra_V1, mediana_V1, dp_V1, lower.tail = FALSE)
prob2

# Evento muito provável (amostra = 10)

# C)
limite_inferior <- media_V1 - 3 * dp_V1
limite_superior <- media_V1 + 3 * dp_V1

a <- pnorm(limite_inferior, media_V1, dp_V1, lower.tail = TRUE)
b <- pnorm(limite_superior, media_V1, dp_V1, lower.tail = TRUE)

prob3 <- b - a
prob3

# Evento muito provável (amostra = 10)

# 2A)
na <- 20
alphaa <- 0.06
amostra_20 <- sample(GS$`Usuários Atuais (em milhares)`, 20)

media_Usuarios <- mean(GS$`Usuários Atuais (em milhares)`)
media_Usuarios

dp_Usuarios <- sd(GS$`Usuários Atuais (em milhares)`)
dp_Usuarios

'
H0: u >= 247.9487
H1: u < 247.9487
'

Zca <- (mean(amostra_20) - media_Usuarios)/(dp_Usuarios/sqrt(na))
Zca

Zaa <- qnorm(alphaa)
Zaa

ifelse(Zca < Zaa, "Rejeita H0", "Aceita H0")
# Aceita H0

# B)
nb <- 25
alphab <- 0.09
amostra_25 <- sample(GS$`Usuários Atuais (em milhares)`, 25)

media_Usuarios <- mean(GS$`Usuários Atuais (em milhares)`)
media_Usuarios

dp_Usuarios <- sd(GS$`Usuários Atuais (em milhares)`)
dp_Usuarios

'
H0: u <= 247.9487
H1: u > 247.9487
'

Zcb <- (mean(amostra_25) - media_Usuarios)/(dp_Usuarios/sqrt(nb))
Zcb

Zab <- qnorm(alphab)
Zab

ifelse(Zcb < Zab, "Rejeita H0", "Aceita H0")
# Aceita H0

# C)
nc <- 15
alphac <- 0.02
amostra_15 <- sample(GS$`Usuários Atuais (em milhares)`, 15)

media_Usuarios <- mean(GS$`Usuários Atuais (em milhares)`)
media_Usuarios

dp_Usuarios <- sd(GS$`Usuários Atuais (em milhares)`)
dp_Usuarios

'
H0: u = 247.9487
H1: u <> 247.9487
'

Zcc <- (mean(amostra_15) - media_Usuarios)/(dp_Usuarios/sqrt(nc))
Zcc

Zac <- qnorm(alphac)
Zac

ifelse(Zcc < Zac | Zcc > (-Zac), "Rejeita H0", "Aceita H0")
# Aceita H0

# 3A)
plot(GS$`Investimento (em milhões de dólares)`, GS$`Eficácia (em %)`,
     xlab="Investimento",
     ylab="Eficácia",
     main="Gráfico de dispersão entre as variáveis Invest e eficácia")

# Analisando o gráfico podemos concluir que, quanto maior o investimento($$$) em tecnologias
# para a area de saude, maior é o valor da eficácia daquela tecnologia, mas simultaneamente,
# não podemos afirmar que um alto investimento é certeza de uma eficácia boa, um exemplo, temos
# um caso em que foi investido uns 15M(+-) e a eficacia foi de 33%, por outro lado temos um caso
# que foi investido menos de 10M e teve uma eficacia de 80%, ou seja alem do valor investido
# deve ser necessario mão de obra qualificada.

# B)
# Covariância
cov(GS$`Investimento (em milhões de dólares)`, GS$`Eficácia (em %)`)

# A covariância entre Inestimento e Eficácia é positiva, o que indica que,
# em geral, a medida que o um dos campos aumenta, o outro também tende a aumentar. 

# Correlação Linear de Pearson
cor(GS$`Investimento (em milhões de dólares)`, GS$`Eficácia (em %)`)

# Indica uma relação positiva moderada entre as duas variáveis. Quanto mais próximo o 
# valor da correlação estiver de 1, mais forte será a relação linear positiva, ou seja
# ou seja se um dos campos aumenta, o outro também tende a aumentar, mas não de 
# maneira totalmente proporcional.

# C)
GS$Invest <- GS$`Investimento (em milhões de dólares)`
GS$Efi <- GS$`Eficácia (em %)`
DB <- data.frame(GS$Invest, GS$Efi)

# Gráfico de correlação linear de Pearson
corrplot(cor(DB), method = 'number')

# O grafico da correlação linear de pearson fica deixa mais claro a relaação entre as variaveis,
# elas possuem interação direta mas não proporcional, ou seja se uma varia a outra tambem varia
# no mesmo sentido mas não uniformemente

# D)
# Reta da Regressão Linear Simples 
# Plot do gráfico de dispersão
# Gráfico de dispersão e Regressão Linear Simples
plot(GS$Efi, GS$Invest,
     xlab="Eficácia",
     ylab="Investimento",
     main="Gráfico de dispersão entre as variáveis Eficácia e Investimento")

modelo <- lm(Invest ~ Efi, data = GS)

# Adicionando a linha de regressão ao gráfico
abline(modelo, col="red")

# Exibindo a equação da regressão linear simples
summary(modelo)
# Y = alpha + beta*X + Erro
# `Investimento (em milhões de dólares)` = -24.5763 + 0.5322 * `Eficácia (em %)` + E

# F)
summary(modelo)

# R**2 = 0.1811
# R**2 ajustado = 0.159

# F-Score = 8.184  --- p-value = 0.00691

# O R² e o R² ajustado indicam que o modelo não explica uma proporção muito grande 
# da variabilidade na variável dependente. O F-statistic e o p-value associado sugerem 
# que o modelo como um todo é estatisticamente significativo, o que significa que pelo menos
# uma das variáveis independentes é útil na previsão da variável dependente.

# Criar um novo conjunto de dados de amostra
amostra1 <- data.frame(Efi = 65)
amostra2 <- data.frame(Efi = 90)

# Realizar a previsão usando o modelo ajustado
previsao1 <- predict(modelo, newdata = amostra1)
previsao1

previsao2 <- predict(modelo, newdata = amostra2)
previsao2

# Resultado da previsao 1: 10.01938 
# Resultado da previsao 2: 23.3254

# 4)
GS$Invest <- GS$`Investimento (em milhões de dólares)`
GS$Efi <- GS$`Eficácia (em %)`
GS$Usuarios <- GS$`Usuários Atuais (em milhares)`

DB2 <- data.frame(GS$Invest, GS$Efi, GS$Usuarios)

# Criando um gráfico de dispersão
ggplot(DB2, aes(x = GS$Invest, y = GS$Efi, color = GS$Usuarios)) +
  geom_point(size = 3) +
  labs(title = "Gráfico de Dispersão entre Investimento, Eficácia e Usuários",
       x = "Investimento (em milhões de dólares)",
       y = "Eficácia (em %)",
       color = "Usuários Atuais (em milhares)")

# Analisando o grafico de disprsão entre essas 3 variaveis(Invest, Eficácia, Usuarios), podemos
# observar que maior o numero de usuarios, menor o investimento e a eficacia, ou seja podemos intepretar
# que os tipos de tratamentos com maior investimento devem seer mais caros para os usuarios
# ou são para tratamentos mais precisos.

# B)
#Matriz de Covariância
matriz_cov <- cov(DB2)
matriz_cov

# Investimento e Eficácia: Covariância positiva (80.5695), indicando uma tendência de 
# aumento conjunto.
# Investimento e Usuários: Covariância negativa (-2067.0985), indicando uma tendência de 
# variação inversa. Quando o investimento aumenta, o número de usuários tende a diminuir.
# Eficácia e Usuários: Covariância negativa (-986.3225), indicando uma tendência de variação
# inversa. À medida que a eficácia aumenta, o número de usuários atuais tende a diminuir.

#Matriz de Correlação Linear de Pearson
matriz_corr <- cor(DB2, method = "pearson")
matriz_corr

# Investimento vs. Eficácia:
# Coeficiente de Correlação: 0.4256
# Interpretação: Existe uma correlação positiva moderada entre o investimento e a eficácia. 
# Aumentos no investimento estão associados a aumentos na eficácia, mas a relação não é 
# muito forte.

# Investimento vs. Usuários:
# Coeficiente de Correlação: -0.7346
# Interpretação: Existe uma correlação negativa forte entre o investimento e o número 
# de usuários atuais. Aumentos no investimento estão associados a uma diminuição no número 
# de usuários atuais.

# Eficácia vs. Usuários:
# Coeficiente de Correlação: -0.4383
# Interpretação: Existe uma correlação negativa moderada entre a eficácia e o número de 
# usuários atuais. Aumentos na eficácia estão associados a uma diminuição no número de 
# usuários atuais, mas a relação não é muito forte.

# C)
corrplot(cor(DB2), method = 'number')

# O grafico ele deixa claro as relações comentados no exercicio anterior, mostrando que estão
# relacionados positivamentes e eficaciaxinvest. Já eficaciaXusuaios e usuariosXinvest estão relacionadas
# negativamnete, ou seja são inversos

# D)
# Equação da Regressão Linear Múltipla
modelo2 <- lm(Invest ~ Efi+Usuarios, data = GS)
summary(modelo2)

# Y = alpha + beta*X1 + beta*X2 + Erro
# Invest = 21.50543 + 0.16039 * Efi - 0.05707 * Usuarios + E

# E)
summary(modelo2)

# R**2 = 0.5529
# R**2 ajustado = 0.528

# F-Score = 22.26  ---  p-value = 5.101e-07

# Em resumo, o modelo de regressão linear múltipla é estatisticamente significativo, 
# mas a variável Eficácia não contribui significativamente para a explicação da variabilidade 
# em Investimento, enquanto a variável Usuários Atuais é estatisticamente significativa. 
# O modelo explica aproximadamente 55.29% da variabilidade na variável dependente.

#F)
amostra3 <- data.frame(Efi = c(45, 65), 
                      Usuarios = c(400, 500))

previsao3 <- predict(modelo2, newdata = amostra3)
previsao3

# Resultado da previsao: 5.895069; 3.395918