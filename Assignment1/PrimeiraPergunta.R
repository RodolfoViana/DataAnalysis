library(dplyr)
library(ggplot2)


empresaAereas <- data.frame(Rank = c(1, 2, 3, 4, 5),  Empresa = c("TAM", "GOL", "Azul", "Avianca Brasil", "Trip"), "Faturamento" = c(13.2,  8.7,  3.7, 1.7, 1.4))

file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Qual é empresa aérea preferida pelos nossos parlamentares ?

fileIdBeneficiario <- select(file, numSubCota, ideCadastro, txtBeneficiario)
itemDescricao <- filter(fileIdBeneficiario, numSubCota == 999)
empresaAerea <- itemDescricao$txtBeneficiario
empresaAerea <- as.character(empresaAerea)
frameEmpresaAerea <- as.data.frame(table(empresaAerea))
ggplot(frameEmpresaAerea, aes(x=reorder(empresaAerea, -Freq), y=Freq, fill = empresaAerea)) + geom_bar(stat="identity") + labs(x='Empresas Aéreas', y='Quantidade de bilhetes emitidos') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_blank(),  
        legend.title=element_blank(), axis.text.x=element_blank(), axis.line=element_blank()) 
# geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') 
#grafico de pizza para comparar azul com avianca e as D+

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro
frameEmpresaAerea$Freq <- as.numeric(frameEmpresaAerea$Freq)
ggplot(frameEmpresaAerea, aes(x = "", y = Freq, fill = empresaAerea)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 90, direction = 1) +
  labs(title = "Empresas Aérea") +
  theme(axis.text.x=element_blank(), legend.title=element_blank(), axis.title.x=element_blank(),  axis.title.y=element_blank(),
        panel.background=element_blank())

