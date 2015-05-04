library(dplyr)
library(ggplot2)



file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Qual é empresa aérea preferida pelos nossos parlamentares ?


fileBeneficiario <- select(file, txtDescricao, numSubCota, txtCNPJCPF, txtDescricaoEspecificacao, txtDescricao, txtBeneficiario)
itemDescricao <- filter(fileBeneficiario, numSubCota == 999)
empresaAerea <- itemDescricao$txtBeneficiario
empresaAerea <- as.character(empresaAerea)
frameEmpresaAerea <- as.data.frame(table(empresaAerea))
ggplot(frameEmpresaAerea, aes(x=reorder(empresaAerea, -Freq), y=Freq)) + geom_bar(stat="identity") + labs(x='Empresas Aéreas', y='Quantidade de bilhetes emitidos') 
# geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') 
#grafico de pizza para comparar azul com avianca e as D+

# Descobrir se os parlamentares compram para outras pessoas
txtPassageiro


#################################################################################################3##########

# Qual o parlamentar que mais tem bilhete aéreo ?

fileparlamentar <- select(file, numSubCota, txNomeParlamentar)
bilheteaereo <- filter(fileparlamentar, numSubCota == 999)

bilheteaereo <- bilheteaereo$txNomeParlamentar
bilheteaereo <- as.character(bilheteaereo)
bilheteaereo <- as.data.frame(table(bilheteaereo))
bilheteaereo <- bilheteaereo[order(-bilheteaereo$Freq),] 
bilheteaereohead <- head(bilheteaereo, 10)
ggplot(bilheteaereohead, aes(x=reorder(bilheteaereo, -Freq), y=Freq)) + 
  geom_bar(stat="identity") + 
  labs(x='Nome do parlamentar', y='Quantidade de bilhetes emitidos') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.x=element_blank())
#groupedByNumSubCota <- group_by(bilheteaereo, numSubCota)
summary(bilheteaereo$Freq)
ggplot(bilheteaereo, aes(x=bilheteaereo, y=Freq)) + geom_boxplot( stat = "boxplot")

boxbilhete <- bilheteaereo
boxbilhete["Col"] <- "Parlamentar"
boxbilhete$bilheteaereo <- NULL
boxplot(Freq~Col,data=boxbilhete)
ggplot(boxbilhete, aes(y=Freq, x=Col)) + geom_boxplot( stat = "boxplot")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   6.000   9.267  13.000  69.000 


# Diferença entre o 9 e o 999 ? 
# Despesas no exterior ?

# Qual o parlamentar que gasta mais dinheiro do seu proprio bolso?

filevlrGlosa <- select(file, vlrGlosa, txNomeParlamentar)
groupbyParlamentar <-  group_by(filevlrGlosa, txNomeParlamentar)
parlamentarVlrGlosa <- summarise(groupbyParlamentar, sum(vlrGlosa))
colnames(parlamentarVlrGlosa) <- c("Parlamentar", "glosa")

parlamentarVlrGlosaOrder <- parlamentarVlrGlosa[order(-parlamentarVlrGlosa$glosa),] 
parlamentarVlrGlosaHead <- head(parlamentarVlrGlosaOrder, 10)
ggplot(parlamentarVlrGlosaHead, aes(x=reorder(Parlamentar, -glosa), y=glosa)) + geom_bar(stat="identity") + 
  labs(x='Nome do Parlamentar', y='Valor Pago Pelo Parlamentar') + theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(parlamentarVlrGlosa, aes(x=Parlamentar, y=glosa)) + geom_boxplot( stat = "boxplot") +
  theme(panel.background=element_blank(),  
        legend.title=element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank(), axis.ticks=element_blank()) +
  scale_y_continuous(breaks = round(seq(min(bilheteaereo$Freq), max(bilheteaereo$Freq))))

boxvlrGlosa <- parlamentarVlrGlosa
boxvlrGlosa["Col"] <- "Parlamentar"
boxvlrGlosa$bilheteaereo <- NULL
boxplot(glosa~Col,data=boxvlrGlosa)
ggplot(boxvlrGlosa, aes(y=glosa, x=Col)) + geom_boxplot( stat = "boxplot") 

summary(parlamentarVlrGlosa$glosa)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   34.01  272.70  246.40 5181.00 


# Como um parlamentar gasta no item manutenção de escritórios de apoio à atividade parlamentar?

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
ggplot(tipoDespesaVlrliquidoHead, aes(x=reorder(Id, -Valor), y=Valor)) + geom_bar(stat="identity") + labs(x='Tipo da Despesa Pro Escritorio', y='Valor em reais gasto') 
frameBeneficiarioId <- select(tipoDespesaVlrliquidoHead, Id, Beneficiario)
print(frameBeneficiarioId, row.names = FALSE)

