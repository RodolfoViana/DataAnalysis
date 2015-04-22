library(dplyr)
library(ggplot2) 

# Carregando o arquivo Ano Atual 
file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Agrupa a tabela pela a coluna numSubCota, o conteúdo deste dado representa o código do 
# Tipo de Despesa referente à despesa realizada pelo deputado e comprovada por 
# meio da emissão de um documento fiscal, a qual é debitada na cota do 
# deputado.
groupedByNumSubCota <- group_by(file, numSubCota)

# Selecionando apenas a coluna de item e descriçao

itemDescricao <- select(file, txtDescricao, numSubCota)
itemDescricao <- itemDescricao[!duplicated(itemDescricao),]
itemDescricaoOrder <- itemDescricao[order(itemDescricao$numSubCota), ]
colnames(itemDescricaoOrder) <- c("Tipo", "Despesa")
itemDescricaoOrder$Tipo <- as.character(itemDescricaoOrder$Tipo)
itemDescricaoOrder$Despesa <- as.character(itemDescricaoOrder$Despesa)
itemDescricaoOrder


# Somando o valor liquido agrupado pelo item
valorLiquido <- summarise(groupedByNumSubCota, sum(vlrLiquido))
colnames(valorLiquido) <- c("Tipo", "Descricao")
valorLiquido <- valorLiquido[order(valorLiquido$Descricao), ]
valorLiquido$Tipo <- as.character(valorLiquido$Tipo)

# Plotando os valores liquidos
ggplot(valorLiquido, aes(x=reorder(Tipo, -Descricao), y=Descricao)) + geom_bar(stat = "identity") + labs(x='Tipo da Despesa', y='Valor dos Gastos em Reais') + geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') + scale_y_continuous(breaks = c(0,2000000,4000000,6000000))
# ggplot(valorLiquido, aes(x=reorder(Tipo, -Descricao), y=Descricao)) + geom_bar(stat = "identity") + labs(x='Tipo da Despesa', y='Valor dos Gastos em Reais') + geom_bar(stat='identity',data=subset(valorLiquido,valorLiquido$Tipo==5),fill='blue') + scale_y_continuous(trans = "log")


# Quais tipos de despesas têm despesas que mais variam, e que são mais desiguais? 
desvio <- summarise(groupedByNumSubCota, sd(vlrLiquido))
#colnames(desvio) <- c("Tipo", "Desvio")
desvio$Tipo <- as.character(desvio$Tipo)
ggplot(desvio, aes(x=reorder(Tipo, -Desvio), y=Desvio)) + geom_bar(stat = "identity") + labs(x='Tipo da Despesa', y='Variação em reais') 

# + scale_y_continuous(trans = "log")
