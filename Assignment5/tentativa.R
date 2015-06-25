library(dplyr)
test <- read.csv("~/Projetos/DataAnalysis/Assignment5/test_second_round_kaggle_sem_acento.csv")


target <- c("Reprovado por Falta")
n_aprovado <- summarise(group_by(filter(test, SITUACAO %in% target), MATRICULA), n())
names(n_aprovado) <- c("MATRICULA", "NAPROVADO")

test <- merge(test, n_aprovado, by = "MATRICULA", all = TRUE)

test[is.na(test)] <- 0

a <- filter(test, NAPROVADO == 0)
b <- filter(test, NAPROVADO > 1)
set.seed(12345)
testSub <- b[order(runif(nrow(b)*0.01)), ]

b["COD_EVASAO"] <- 1

test <- merge(test, select(b, ID, COD_EVASAO), by = "ID", all = TRUE)

test[is.na(test)] <- 0
prop.table(table(test$COD_EVASAO))

write.csv(select(test, ID, COD_EVASAO), file="~/Projetos/DataAnalysis/Assignment5/submissao/submissaoRound17.csv", row.names = FALSE)





Modelo com matrix de confusão melhor:
  
  ```{r}
model <- C5.0(treino2[,c(3,5,7,9,10,11,15,16)], treino2$COD_EVASAO)
pred <- predict(model, test2[,c(3,5,7,9,10,11,15,16)])

true_eva <- test2$COD_EVASAO == 1
table(pred, true_eva)
```

Esse foi o modelo que foi usado para fazer a classificação e submissão para o kaggle. 

```{r}
test_kaggle <- read.csv("~/Projetos/DataAnalysis/Assignment5/test_first_round_kaggle_sem_acento.csv")

#Transformação para factor
test_kaggle$DEPARTAMENTO <- as.factor(test_kaggle$DEPARTAMENTO)
test_kaggle$COD_CURSO <- as.factor(test_kaggle$COD_CURSO)
test_kaggle$MEDIA <- as.character(test_kaggle$MEDIA)
test_kaggle$MEDIA[is.na(test_kaggle$MEDIA)] <- -10
test_kaggle$MEDIA <- as.numeric(test_kaggle$MEDIA)

#Missing
levels(test_kaggle$DEPARTAMENTO)[1] = NA
levels(test_kaggle$DISCIPLINA)[1] = NA
levels(test_kaggle$SITUACAO)[1] = NA

test_kaggle$PERIODO_INGRESSO <- as.factor(test_kaggle$PERIODO_INGRESSO)

test_kaggle_group <- group_by(test_kaggle, MATRICULA)

media <- summarise(test_kaggle_group, mean(MEDIA))
names(media) <- c("MATRICULA", "MEDIATOTAL")

reprovacao <- summarise(group_by(filter(test_kaggle_group, SITUACAO=="Reprovado"), MATRICULA), n())
names(reprovacao) <- c("MATRICULA", "REPROVACAO")

aprovado <- summarise(group_by(filter(test_kaggle_group, SITUACAO=="Aprovado"), MATRICULA), n())
names(aprovado) <- c("MATRICULA", "APROVADO")

target <- c("Reprovado por Falta", NA)
filter(dat, name %in% target)

reprovado_falta <- summarise(group_by(filter(test_kaggle_group, SITUACAO %in% target), MATRICULA), n())
names(reprovado_falta) <- c("MATRICULA", "REPROVADOFALTA")

test_kaggle_group <- merge(test_kaggle_group, media, by = "MATRICULA", all = TRUE)
test_kaggle_group <- merge(test_kaggle_group, reprovacao, by = "MATRICULA", all = TRUE)
test_kaggle_group <- merge(test_kaggle_group, aprovado, by = "MATRICULA", all = TRUE)
test_kaggle_group <- merge(test_kaggle_group, reprovado_falta, by = "MATRICULA", all = TRUE)

test_kaggle_group$COD_EVASAO <- NULL

pred <- predict(credit_boost10, test_kaggle_group[,c(3,5,7,9,10,11,14,15,16,17)])
summary(pred)

test_kaggle_group["COD_EVASAO"] <- pred

write.csv(select(test_kaggle_group, ID, COD_EVASAO),file="submissao6.csv")

```
