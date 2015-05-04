library(dplyr)
library(ggplot2)


file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Qual o parlamentar que gasta mais dinheiro do seu proprio bolso?

filevlrGlosa <- select(file, vlrGlosa, txNomeParlamentar)
groupbyParlamentar <-  group_by(filevlrGlosa, txNomeParlamentar)
parlamentarVlrGlosa <- summarise(groupbyParlamentar, sum(vlrGlosa))
colnames(parlamentarVlrGlosa) <- c("Parlamentar", "glosa")

parlamentarVlrGlosaOrder <- parlamentarVlrGlosa[order(-parlamentarVlrGlosa$glosa),] 
parlamentarVlrGlosaHead <- head(parlamentarVlrGlosaOrder, 15)
ggplot(parlamentarVlrGlosaHead, aes(x=reorder(Parlamentar, -glosa), y=glosa)) + geom_bar(stat="identity") + 
  labs(x='Nome do Parlamentar', y='Valor Pago Pelo Parlamentar') + 
  theme(panel.background=element_blank()) +
  coord_flip()


ggplot(parlamentarVlrGlosa, aes(x=Parlamentar, y=glosa)) + geom_boxplot( stat = "boxplot")

boxvlrGlosa <- parlamentarVlrGlosa
boxvlrGlosa["Col"] <- "Parlamentar"
boxvlrGlosa$bilheteaereo <- NULL
boxplot(glosa~Col,data=boxvlrGlosa)
ggplot(boxvlrGlosa, aes(y=glosa, x=Col)) + geom_boxplot( stat = "boxplot") 

summary(parlamentarVlrGlosa$glosa)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   34.01  272.70  246.40 5181.00 
