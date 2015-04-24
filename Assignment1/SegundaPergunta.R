library(dplyr)
library(ggplot2)


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
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_blank())#groupedByNumSubCota <- group_by(bilheteaereo, numSubCota)
summary(bilheteaereo$Freq)
ggplot(bilheteaereo, aes(x=bilheteaereo, y=Freq)) + geom_boxplot( stat = "boxplot") +
  theme(panel.background=element_blank(),  
        legend.title=element_blank(), axis.text.x=element_blank(), axis.line=element_blank(), axis.title.x=element_blank(), axis.ticks=element_blank()) +
  scale_y_continuous(breaks = round(seq(min(boxbilhete$Freq), max(boxbilhete$Freq), by = 10),))

boxbilhete <- bilheteaereo
boxbilhete["Col"] <- "Parlamentar"
boxbilhete$bilheteaereo <- NULL
boxplot(Freq~Col,data=boxbilhete)
ggplot(boxbilhete, aes(y=Freq, x=Col)) + geom_boxplot( stat = "boxplot")  + 
  theme(panel.background=element_blank(),  
        legend.title=element_blank(), axis.text.x=element_blank(), axis.title.y=element_blank(), axis.line=element_blank(), axis.title.x=element_blank(), axis.ticks=element_blank()) +
  scale_y_continuous(breaks = round(seq(min(boxbilhete$Freq), max(boxbilhete$Freq), by = 10),))
