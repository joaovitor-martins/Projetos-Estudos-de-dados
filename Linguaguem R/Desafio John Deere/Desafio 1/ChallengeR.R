# Challenge - IntelliDeere - Disciplina: STATISTICAL COMPUTING WITH R - 1TIAR 2023:
# Arthur Coutinho Santos --- RM: 97804
# Camilly Alves --- RM: 550210 
# Guilherme Garcia Paschoalinoto --- RM: 99221
# João Vitor de Andrade Martins --- RM: 98744
# Murilo Krauss --- RM: 98262

# Bibliotecas
library(readxl)
library(psych)
library(ggplot2)
library(dplyr)
library(forecast)

# 1C) Montar a base de dados em Excel e/ou no RStudio
# Carregar os dados
ChallengePre_Venda <- read_excel("C:/Users/Joao/Downloads/ChallengePre_Venda.xlsx")
ChallengePos_Venda <- read_excel("C:/Users/Joao/Downloads/ChallengePos_Venda.xlsx")

# Ajeitando Pré-Venda
# Verificar o tipo de cada coluna do dataset de Pré-Venda
print(class(ChallengePre_Venda$`Data da venda`))
print(class(ChallengePre_Venda$Localização))
print(class(ChallengePre_Venda$`Área Plantada`))
print(class(ChallengePre_Venda$`Tipo de Cultura`))
print(class(ChallengePre_Venda$`Preço Médio`))
print(class(ChallengePre_Venda$`Método de Pagamento`))
print(class(ChallengePre_Venda$`Número de Vendas`))
print(class(ChallengePre_Venda$`Condição do Veículo`))

# Transformar data em número
ChallengePre_Venda$`Data da venda` <- as.numeric(as.Date(ChallengePre_Venda$`Data da venda`))

# Transformar caracteres em numero
ChallengePre_Venda$`Tipo de Cultura` <- as.numeric(factor(ChallengePre_Venda$`Tipo de Cultura`))
ChallengePre_Venda$`Método de Pagamento` <- as.numeric(factor(ChallengePre_Venda$`Método de Pagamento`))
ChallengePre_Venda$Localização <- as.numeric(factor(ChallengePre_Venda$Localização))

# Transformar caracteres em numeros pré-definidos
ChallengePre_Venda <- ChallengePre_Venda %>%
  mutate(
    `Condição do Veículo` = recode(`Condição do Veículo`,
                                   "Novo" = 1,
                                   "Usado" = 0)
  )

# Verificar novamente o tipo de cada coluna
print(class(ChallengePre_Venda$`Data da venda`))
print(class(ChallengePre_Venda$Localização))
print(class(ChallengePre_Venda$`Área Plantada`))
print(class(ChallengePre_Venda$`Tipo de Cultura`))
print(class(ChallengePre_Venda$`Preço Médio`))
print(class(ChallengePre_Venda$`Método de Pagamento`))
print(class(ChallengePre_Venda$`Número de Vendas`))
print(class(ChallengePre_Venda$`Condição do Veículo`))


# Ajeitando Pós-Venda
# Verificar o tipo de cada coluna do dataset de Pré-Venda
print(class(ChallengePos_Venda$`Data da compra`))
print(class(ChallengePos_Venda$Localização))
print(class(ChallengePos_Venda$Equipamento))
print(class(ChallengePos_Venda$`Tipo de Serviço`))
print(class(ChallengePos_Venda$`Tempo de Atendimento (em horas)`))
print(class(ChallengePos_Venda$`Peças Substituídas`))
print(class(ChallengePos_Venda$Custo))
print(class(ChallengePos_Venda$Cliente))

# Transformar data em número
ChallengePos_Venda$`Data da compra` <- as.numeric(as.Date(ChallengePos_Venda$`Data da compra`, origin = "1970-01-01"))


# Transformar caracteres em numero
ChallengePos_Venda$Equipamento <- as.numeric(factor(ChallengePos_Venda$Equipamento))
ChallengePos_Venda$Cliente <- as.numeric(factor(ChallengePos_Venda$Cliente))
ChallengePos_Venda$`Tipo de Serviço` <- as.numeric(factor(ChallengePos_Venda$`Tipo de Serviço`))
ChallengePos_Venda$`Peças Substituídas` <- as.numeric(factor(ChallengePos_Venda$`Peças Substituídas`))
ChallengePos_Venda$Localização <- as.numeric(factor(ChallengePos_Venda$Localização))

# Verificar novamente o tipo de cada coluna
print(class(ChallengePos_Venda$`Data da compra`))
print(class(ChallengePos_Venda$Localização))
print(class(ChallengePos_Venda$Equipamento))
print(class(ChallengePos_Venda$`Tipo de Serviço`))
print(class(ChallengePos_Venda$`Tempo de Atendimento (em horas)`))
print(class(ChallengePos_Venda$`Peças Substituídas`))
print(class(ChallengePos_Venda$Custo))
print(class(ChallengePos_Venda$Cliente))

# 2A) Montar um Gráfico de Dispersão no RStudio entre todas as variáveis do item 01) a)
# Criar o gráfico de dispersão
pairs(ChallengePre_Venda)

# 2B) Montar um Gráfico de Dispersão no RStudio entre todas as variáveis do item 01) b)
# Criar o gráfico de dispersão
pairs(ChallengePos_Venda)

# 2C) Aplicar a Correlação Linear de Pearson no RStudio entre todas as variáveis do item 01) a).
# Calculando a correlação linear de Pearson
cor(ChallengePre_Venda, method = "pearson")

# 2D) Aplicar a Correlação Linear de Pearson no RStudio entre todas as variáveis do item 01) b).
# Calculando a correlação linear de Pearson
cor(ChallengePos_Venda, method = "pearson")

# 3A) Montar o gráfico de Tendência, Sazonalidade e Ruido Branco (fator aleatório) no RStudio para cada variável do item 01) a)
# Converter a coluna "Data da venda" em formato de data
ChallengePre_Venda$`Data da venda` <- as.Date(ChallengePre_Venda$`Data da venda`, format = "%d/%m/%Y", origin = "2022-09-26")

# Criar uma série temporal a partir da coluna "Área Plantada"
serie_temporalAreaPlantada <- ts(ChallengePre_Venda$`Área Plantada`, frequency = 12, start = c(2021, 7), end = c(2023, 6))
# Decompor a série temporal em tendência, sazonalidade e ruído
decomposedAreaPlantada <- decompose(serie_temporalAreaPlantada)
# Plotar o gráfico de tendência, sazonalidade e ruído branco
plot(decomposedAreaPlantada, type = "l")

# Criar uma série temporal a partir da coluna "Preço Médio"
serie_temporalPreco <- ts(ChallengePre_Venda$`Preço Médio`, frequency = 12, start = c(2021, 7), end = c(2023, 6))
# Decompor a série temporal em tendência, sazonalidade e ruído
decomposedPreco <- decompose(serie_temporalPreco)
# Plotar o gráfico de tendência, sazonalidade e ruído branco
plot(decomposedPreco, type = "l")

# Criar uma série temporal a partir da coluna "Número de Vendas"
serie_temporalVendas <- ts(ChallengePre_Venda$`Número de Vendas`, frequency = 12, start = c(2021, 7), end = c(2023, 6))
# Decompor a série temporal em tendência, sazonalidade e ruído
decomposedVendas <- decompose(serie_temporalVendas)
# Plotar o gráfico de tendência, sazonalidade e ruído branco
plot(decomposedVendas, type = "l")

# 3B) Montar o gráfico de Tendência, Sazonalidade e Ruido Branco (fator aleatório) no RStudio para cada variável do item 01) b)
# Converter a coluna "Data da compra" em formato de data
ChallengePos_Venda$`Data da compra` <- as.Date(ChallengePos_Venda$`Data da compra`, format = "%d/%m/%Y", origin = "2022-01-04")

# Criar uma série temporal a partir da coluna "Tempo de Atendimento (em horas)"
serie_temporalTempoAtendimento <- ts(ChallengePos_Venda$`Tempo de Atendimento (em horas)`, frequency = 12, start = c(2021, 7), end = c(2023, 6))
# Decompor a série temporal em tendência, sazonalidade e ruído
decomposedTempoAtendimento <- decompose(serie_temporalTempoAtendimento)
# Plotar o gráfico de tendência, sazonalidade e ruído branco com limites de eixo Y ajustados
plot(decomposedTempoAtendimento, type = "l", ylim = c(min(decomposedTempoAtendimento$random), max(decomposedTempoAtendimento$random)))

# Criar uma série temporal a partir da coluna "Custo"
serie_temporalCusto <- ts(ChallengePos_Venda$Custo, frequency = 12, start = c(2021, 7), end = c(2023, 6))
# Decompor a série temporal em tendência, sazonalidade e ruído
decomposedCusto <- decompose(serie_temporalCusto)
# Plotar o gráfico de tendência, sazonalidade e ruído branco
plot(decomposedCusto, type = "l")

