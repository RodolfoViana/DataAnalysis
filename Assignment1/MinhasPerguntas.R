library(dplyr)
library(ggplot2)

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro

# Diferença entre o 9 e o 999 ? 
# Despesas no exterior ?

# Qual é empresa aérea preferida pelos nossos parlamentares ?

file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")
fileBeneficiario <- select(file, txtDescricao, numSubCota, txtCNPJCPF, txtDescricaoEspecificacao, txtDescricao, txtBeneficiario)

# Verificar o cnpj dessas empresas
itemDescricao <- filter(fileBeneficiario, numSubCota == 999)

ggplot(itemDescricao, aes(x=reorder(Tipo, -Descricao), y=Descricao)) + geom_bar(stat = "identity") + labs(x='Tipo da Despesa', y='Valor dos Gastos em Reais') + geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') + scale_y_continuous(breaks = c(0,2000000,4000000,6000000))

