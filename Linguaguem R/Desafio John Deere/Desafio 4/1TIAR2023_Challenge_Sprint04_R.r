# Challenge - IntelliDeere
# Arthur Coutinho Santos - RM 97804
# Camilly Souza Alves - RM 550210 
# Guilherme Garcia Paschoalinoto - RM 99221
# João Vitor de Andrade Martins - RM 98744
# Murilo Krauss - RM 98262

# Carregar as bibliotecas necessárias
library(readxl)
library(corrplot)
library(lmtest)

#Importar base de dados
Base <- "C:/Users/Camilly/Documents/FIAP/Challenge John Deere/Sprint1/ChallengePre_Venda.xlsx"

Pre <- read_excel(Base)


# 1)
Pre$Preco <- Pre$`Preço Médio`
Pre$nVendas <- Pre$`Número de Vendas`

Db <- data.frame(Pre$Preco,Pre$nVendas)

# A)
plot(Pre$nVendas, Pre$Preco,
     xlab="Numero de Vendas",
     ylab="Preço Medio",
     main="Gráfico de dispersão entre as variáveis Vendas e Preço")

# B)
# Covariância
cov(Pre$nVendas, Pre$Preco)

# A covariância entre Número de Vendas e Preço Médio é positiva, o que indica que,
# em geral, a medida que o Número de Vendas aumenta, o Preço Médio também tende a aumentar. 

# Correlação Linear de Pearson
cor(Pre$nVendas, Pre$Preco)

# A correlação de Pearson, de aproximadamente 0.38, indica que existe uma ligação entre as variaveis,
# podendo dizer que estão relacionadas, mas não de forma perfeita, ou seja se o Numero de Vendas
# aumenta, o Preço também tende a aumentar, mas não de maneira totalmente proporcional.

# C)
# Gráfico de correlação linear de Pearson
corrplot(cor(Db), method = 'number')

# D)
# Os pré-requisitos da variável Numero de Vendas e da variável Preço são:
# Gráfico de dispersão: Indica que, na maioria das situações, à medida que o  
# Numero de Vendas aumenta, o Preço também aumenta.

# Covariância: A covariância de 134495.9 entre Numero de Vendas e Preço Médio 
# indica uma relação positiva geral, onde um aumento no Numero de Vendas 
# tende a coincidir com um aumento no Preço Médio.

# Correlação linear de Pearson: A correlação de Pearson entre Numero de Vendas e Preço Médio é, 
# aproximadamente, 0.38, o que indica uma correlação positiva moderada , mas não é forte
# pois está abaixo de 50%.Portanto, as duas variáveis estão relacionadas de alguma forma,
# mas a relação não é linear e perfeita. 

# Gráfico de Correlação linear de Pearson
# A matriz de correlação mostra que a correlação entre Numero de Vendas e a Preço Médio é de, aproximadamente,
# 0.39, indicando uma correlação positiva moderada, mas não forte (acima de 0%, mas abaixo de 50%). 
# Isso significa que, em geral, à medida que o Numero de Vendas aumenta, o Preço Médio tende a aumentar também.

# E)
# Reta da Regressão Linear Simples 
# Plot do gráfico de dispersão
plot(Pre$nVendas, Pre$Preco,
     xlab="Num Vendas",
     ylab="Preço",
     main="Gráfico de dispersão entre as variáveis Vendas e Preço")

modelo <- lm(nVendas ~ Preco, data = Pre)
abline(modelo, col="blue")

# Equação da Regressão Linear Simples
summary(modelo)

# Y = alpha + beta*X + Erro
# NVendas = -12.78 + 0.00001149 * Preco + E

# F)
summary(modelo)

# R**2 = 0.1506
# R**2 ajustado = 0.1423

# O coeficiente de determinação R² mostra o quanto o X explica o Y, no caso
# 0.1506(R²), que significa que cerca de 15.06% da variação do nVendas é explicado pelo Preco,
# já o R² ajustado é de 0.1423(14,23%) que é o R² mais refinado. Tendo um olhar de toda a 
# regressão, esses valores significam que o modelo de regressão linear simples se ajustou.

# G)
amostra1 <- data.frame(Preco=2050000)
amostra2 <- data.frame(Preco=1950000)


previsão1 <- predict(modelo, newdata=amostra1)
previsão1

previsão2 <- predict(modelo, newdata=amostra2)
previsão2

# Resultado da previsao 1: 10.76998 
# Resultado da previsao 2: 9.621254 

#2)

Pre$Preco <- Pre$`Preço Médio`
Pre$nVendas <- Pre$`Número de Vendas`
Pre$Area <- Pre$`Área Plantada`

Db2 <- data.frame(Pre$Preco,Pre$nVendas,Pre$Area)

#A)

#Matriz de Covariância
matriz_cov <- cov(Db2)
matriz_cov

#Matriz de Correlação Linear de Pearson
matriz_corr <- cor(Db2, method = "pearson")
matriz_corr

# Matriz de Covariância: A covariância de 3774346.5273 entre Area e Preço 
# indica uma relação positiva geral, onde um aumento na area do terreno 
# tende a coincidir com um aumento no Preço. Porém, por refletir um valor alto, 
# mostra que não é uma relação muito forte. Também é possível analisar esse comportamento
# na relação  Preço com nVendas.

# Matriz de Correlação linear de Pearson: A relação entre as variáveis nVendas e Area
# tem uma maior taxa correlação (78.52%),e nVendas com Preco (38.81%). O que indicam
# um correlação positiva e moderada, mas não perfeita.
# Já a relação entre as variáveis Area e Preco também se enquadram em positiva e moderada,
# mas apresentam uma tava baixa de 16.76%.

#B)
# Gráfico de correlação linear de Pearson
corrplot(cor(Db2), method = 'number')

#C)
#  Os pré-requisitos da Regressão Linear Múltipla são:
# Gráfico de dispersão: Indica que, na maioria das situações, à medida que o  
# Numero de Vendas aumenta, o Preço também aumenta.

# Covariância: A covariância de 134495.9 entre Numero de Vendas e Preço Médio 
# indica uma relação positiva geral, onde um aumento no Numero de Vendas 
# tende a coincidir com um aumento no Preço Médio.

# Correlação linear de Pearson: A correlação de Pearson entre Numero de Vendas e Preço Médio é, 
# aproximadamente, 0.38, o que indica uma correlação positiva moderada , mas não é forte
# pois está abaixo de 50%.Portanto, as duas variáveis estão relacionadas de alguma forma,
# mas a relação não é linear e perfeita. 

# Gráfico de Correlação linear de Pearson
# A matriz de correlação mostra que a correlação entre Numero de Vendas e Area é de, aproximadamente,
# 0.79, indicando uma correlação positiva forte. Isso significa que, em geral, à medida que
# o Area do terrenos aumenta, o Numero de Vendas tende a aumentar também. Diferente do valor da 
# da correlação entre Numero de Vendas e Preço que ficou, aproximadamente, 0.39, indicando
# uma correlação positiva moderada, porém não muito forte.

# Teste Estatísticos como o Durbin-Watson para autocorrelação serial
modelo <- lm(nVendas ~ Preco+Area, data = Pre)
resultado_dw <- dwtest(modelo)
resultado_dw
# Há indícios de autocorrelação positiva nos resíduos do modelo, mas ela não é forte 
# o suficiente para considerar o modelo seriamente violador da suposição de independência 
# dos erros. O modelo ainda pode ser considerado válido, mesmo com a 
# autocorrelação positiva presente, ela não é um problema grave que invalidaria as 
# conclusões derivadas do modelo.

#D)
# Equação da Regressão Linear Múltipla
modelo <- lm(nVendas ~ Preco+Area, data = Pre)
summary(modelo)

# Y = alpha + beta*X1 + beta*X2 + Erro
# NVendas = -17.46 + 0.00000781 * Preco + 0.01141 * Area + E

#E)
# Coeficiente de Determinação Múltiplo (R²)
summary(modelo)

# R**2 = 0.6844
# R**2 ajustado = 0.6781

# O coeficiente de determinação R² mostra o quanto o meu X1 e X2 explicam o Y, no caso
# 0.6844(R²), que significa que cerca de 68.44% da variação do nVendas é explicado pelo 
# Preco e Area do terreno. Já o R² ajustado é de 0.6781(67,81%) que é o R² mais refinado.
# Tendo um olhar de toda a regressão, esses valores significam que o modelo de regressão 
# linear múltipla se ajustou.

#F)
amostra <- data.frame(Area = c(1200, 1000, 900, 800), 
                      Preco = c(2050000, 1950000, 2100000, 2150000))

previsao <- predict(modelo, newdata = amostra)
previsao

# Resultado da previsao: 12.233837 ; 9.171547 ; 9.202449 ; 8.452327 