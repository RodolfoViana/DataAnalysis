library(dplyr)
library(ggplot2)


file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

fileEscritorio <- select(file, numSubCota, txtBeneficiario)
escritorio <- filter(fileEscritorio, numSubCota == 1)
escritorio$numSubCota <- NULL
escritorio$txtBeneficiario <- as.character(escritorio$txtBeneficiario)
frameEscritorio <- as.data.frame(table(escritorio))
frameEscritorio <- frameEscritorio[order(-frameEscritorio$Freq),] 
frameEscritorioHead <- head(frameEscritorio, 10)
frameEscritorioHead["Id"] <- c(1:10)
ggplot(frameEscritorioHead, aes(x=reorder(Id, -Freq), y=Freq)) + geom_bar(stat="identity") + 
  labs(x='Tipo da Despesa Pro Escritorio', y='Quantidade de Vezes Dessa Despesa') 
frameEscritorioId <- select(frameEscritorioHead, Id, escritorio)
print(frameEscritorioId, row.names = FALSE)


# Valor em reais dos gastos no item manutenção de escritórios
fileEscritorioValor <- select(file, numSubCota, txtBeneficiario, vlrLiquido)
fileEscritorioValor <- filter(fileEscritorioValor, numSubCota == 1)

groupbyEscritorioValor <-  group_by(fileEscritorioValor, txtBeneficiario)
tipoDespesaVlrliquido <- summarise(groupbyEscritorioValor, sum(vlrLiquido))
colnames(tipoDespesaVlrliquido) <- c("Beneficiario", "Valor")
tipoDespesaVlrliquido <- tipoDespesaVlrliquido[order(-tipoDespesaVlrliquido$Valor),] 
tipoDespesaVlrliquidoHead <- head(tipoDespesaVlrliquido, 10)
tipoDespesaVlrliquidoHead["Id"] <- c(1:10)

frameBeneficiarioId <- select(tipoDespesaVlrliquidoHead, Id, Beneficiario)
print(frameBeneficiarioId, row.names = FALSE)

ggplot(tipoDespesaVlrliquidoHead, aes(x=reorder(Id, -Valor), y=Valor)) + geom_bar(stat="identity") + labs(x='Tipo da Despesa Pro Escritorio', y='Valor em reais gasto') +
  theme(panel.background=element_blank())


empresasAerea <- data.frame(Rank = c(1, 2, 3, 4, 5),  Empresa = c("TAM", "GOL", "Azul", "Avianca Brasil", "Trip"))
print(empresasAerea, row.names = FALSE)

library(gridExtra)

qplot(1:5, 1:5, geom = "blank") + 
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank()) +
 annotation_custom(grob = tableGrob(empresasAerea,
                                   # change font sizes:
                                   gpar.coltext = gpar(cex = 1.2),
                                   gpar.rowtext = gpar(cex = 1.2)),
                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + coord_fixed()


