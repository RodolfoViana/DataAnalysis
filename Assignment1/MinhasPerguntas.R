library(dplyr)
library(ggplot2)

# Qual é empresa aérea preferida pelos nossos parlamentares ?

file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")
fileBeneficiario <- select(file, txtDescricao, numSubCota, txtCNPJCPF, txtDescricaoEspecificacao, txtDescricao, txtBeneficiario)
itemDescricao <- filter(fileBeneficiario, numSubCota == 999)
empresaAerea <- itemDescricao$txtBeneficiario
empresaAerea <- as.character(empresaAerea)
frameEmpresaAerea <- as.data.frame(table(empresaAerea))
ggplot(frameEmpresaAerea, aes(x=reorder(empresaAerea, -Freq), y=Freq)) + geom_bar(stat="identity") + labs(x='Empresas Aéreas', y='Quantidade de bilhetes emitidos') 
# geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') 

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro


#################################################################################################3##########

# Qual o parlamentar que mais tem bilhete aéreo ?

txNomeParlamentar
fileparlamentar <- select(file, numSubCota, ideCadastro, txNomeParlamentar)
bilheteaereo <- filter(fileparlamentar, numSubCota == 999)

bilheteaereo <- bilheteaereo$txNomeParlamentar
bilheteaereo <- as.character(bilheteaereo)
framebilheteaereo <- as.data.frame(table(bilheteaereo))
newdata2 <- framebilheteaereo[order(-framebilheteaereo$Freq),] 
x <- head(newdata2, 10)
ggplot(x, aes(x=reorder(bilheteaereo2, -Freq), y=Freq)) + geom_bar(stat="identity") + labs(x='Nome do parlamentar', y='Quantidade de bilhetes emitidos') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

groupedByNumSubCota <- group_by(file, numSubCota)
summary(newdata2$Freq)


# Diferença entre o 9 e o 999 ? 
# Despesas no exterior ?


