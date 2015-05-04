library(dplyr)
library(ggplot2)


empresasAerea <- data.frame(Rank = c(1, 2, 3, 4, 5),  Empresa = c("TAM", "GOL", "Azul", "Avianca Brasil", "Trip"), "Faturamento" = c(13.2,  8.7,  3.7, 1.7, 1.4))
print(empresasAerea, row.names = FALSE)




file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Qual é empresa aérea preferida pelos nossos parlamentares ?

fileIdBeneficiario <- select(file, numSubCota, ideCadastro, txtBeneficiario)
itemDescricao <- filter(fileIdBeneficiario, numSubCota == 999)
empresaAerea <- itemDescricao$txtBeneficiario
empresaAerea <- as.character(empresaAerea)
frameEmpresaAerea <- as.data.frame(table(empresaAerea))
ggplot(frameEmpresaAerea, aes(x=reorder(empresaAerea, -Freq), y=Freq)) + geom_bar(stat="identity") + labs(x='Empresas Aéreas', y='Quantidade de bilhetes emitidos') + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') 
#grafico de pizza para comparar azul com avianca e as D+

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro

ggplot(frameEmpresaAerea, aes(x = empresaAerea, fill = Freq)) +geom_bar(width = 1) + coord_polar(theta = "y")

