library(dplyr)

training <- read.csv("~/Projetos/DataAnalysis/Assignment5/training_sem_acento.csv")

training <- filter(training, PERIODO_RELATIVO == 1)

#Transformação para factor
training$COD_CURSO <- as.factor(training$COD_CURSO)
training$MEDIA <- as.numeric(as.character(training$MEDIA))
training$COD_EVASAO <- as.factor(training$COD_EVASAO)
training$CURSO <- as.factor(training$CURSO)
training$DEPARTAMENTO <- as.factor(training$DEPARTAMENTO)
training$DISCIPLINA <- as.factor(training$DISCIPLINA)

#Missing
levels(training$SITUACAO)[1] = NA
levels(training$PERIODO_INGRESSO)[1] = NA
levels(training$CURSO)[1] = NA
levels(training$CREDITOS)[1] = NA
levels(training$DEPARTAMENTO)[1] = NA
levels(training$DISCIPLINA)[1] = NA
levels(training$MEDIA)[1] = NA

#Primeiro periodo
set.seed(12345) 
primeiro_periodo <- filter(training, PERIODO_RELATIVO == 1)

primeiro_training <- primeiro_periodo[order(runif(nrow(primeiro_periodo))), ]

#Divisao de treino e teste
training_train <- primeiro_training[1:round(0.9*nrow(primeiro_periodo)), ]
training_test <- primeiro_training[round(0.9*nrow(primeiro_periodo)):nrow(primeiro_periodo), ]
prop.table(table(training_train$COD_EVASAO))
prop.table(table(training_test$COD_EVASAO))

library("C50")

model <- C5.0(training_train[,-14], training_train$COD_EVASAO)
model
summary(model)