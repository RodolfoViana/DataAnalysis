

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
ggplot(bilheteaereo, aes(x=bilheteaereo, y=Freq)) + geom_boxplot( stat = "boxplot")

boxbilhete <- bilheteaereo
boxbilhete["Col"] <- "Parlamentar"
boxbilhete$bilheteaereo <- NULL
boxplot(Freq~Col,data=boxbilhete)
ggplot(boxbilhete, aes(y=Freq, x=Col)) + geom_boxplot( stat = "boxplot")
