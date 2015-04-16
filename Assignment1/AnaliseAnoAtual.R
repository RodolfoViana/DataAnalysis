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
itemDescricao <- select(file, numSubCota, txtDescricao)
itemDescricao <- itemDescricao[!duplicated(itemDescricao),]
itemDescricao <- itemDescricao[order(itemDescricao$numSubCota), ]

# Somando o valor liquido agrupado pelo item
valorLiquido <- summarise(groupedByNumSubCota, sum(vlrLiquido))
colnames(valorLiquido) <- c("Tipo", "Despesa")
valorLiquido$Tipo <- as.character(valorLiquido$Tipo)

# Plotando no mundo da imaginaçao
qplot(valorLiquido$Tipo, valorLiquido$Despesa)

# Quais tipos de despesas têm despesas que mais variam, e que são mais desiguais? 
desvio <- summarise(groupedByNumSubCota, max(vlrLiquido), min(vlrLiquido), sd(vlrLiquido))
colnames(desvio) <- c("Tipo", "Valor Maximo", "Valor Minimo", "Desvio Padrao")
desvio
desvio$Tipo <- as.character(desvio$Tipo)
desvio <- desvio[order(desvio$"Desvio Padrao"), ]
qplot(desvio$Tipo, desvio$"Desvio Padrao")
