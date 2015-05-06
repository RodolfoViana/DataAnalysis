library(dplyr)
library(ggplot2)
library("Rmisc", quietly = T)


file <- read.table("Projetos/DataAnalysis/Assignment2/students.data", header=TRUE)

# Qual é a média de tentativas das 10 questões (top 10 de submissão)?

mediaQuestoes <- file %>% 
  select(question) %>%
  table() %>%
  as.data.frame()

colnames(mediaQuestoes) <- c("Questao", "Frequencia")
newdata <- mediaQuestoes[order(-mediaQuestoes$Frequencia),] 

condition <- head(newdata, n = 10)$Questao

subset <- filter(file, question %in% condition)

CI(mediaQuestoes, ci = 0.95)


# Qual é a média de acerto de cada grupo de aluno (dividir os alunos em 3 grupos)?

alunos <- file %>% 
  select(student) %>%
  table() %>%
  as.data.frame()

summary(alunos$Freq)

mediaAluno <- file %>% 
  select(question, attempt, result, student) %>% 
  group_by(student) %>%
  summarise(median(as.numeric(attempt)))


# Qual questão que teve a maior variação nas submissões?

variacaoAluno <- file %>% 
  select(question, attempt, result, student) %>% 
  group_by(question) %>%
  summarise(sd(as.numeric(attempt)))

# Qual questão que teve o maior número de acertos de primeira?

maisAcertos <- file %>% 
  select(question, attempt, result, student) %>% 
  filter(attempt == 0) %>%
  filter(result == "True") %>%
  select(question)  

acertos <- as.data.frame(table(maisAcertos))


