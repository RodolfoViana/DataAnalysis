library(dplyr)
library(ggplot2)

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro

# Diferença entre o 9 e o 999 ? 
# Despesas no exterior ?

# Qual é empresa aérea preferida pelos nossos parlamentares ?

file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")
fileBeneficiario <- select(file, txtDescricao, numSubCota, txtCNPJCPF, txtDescricaoEspecificacao, txtDescricao, txtBeneficiario)

verificar o cnpj dessas empresas
itemDescricao <- filter(fileBeneficiario, numSubCota == 999)

numSubCota9 <- filter(fileBeneficiario, numSubCota == 9)

test <- filter(fileBeneficiario, numSubCota == 9)
tabelaCNPJAviao <- select(test, txtCNPJCPF, txtBeneficiario)
tabelaCNPJAviaoUnique <- unique(tabelaCNPJAviao)
cnpjcpg <- unique(test$txtCNPJCPF)

