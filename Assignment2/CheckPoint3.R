library(dplyr)
library(ggplot2)


file <- read.table("Projetos/DataAnalysis/Assignment2/students.data", header=TRUE)


# Qual é a média de acerto de cada questão?

mediaQuestoes <- file %>% 
  select(question, attempt, result, student) %>% 
  group_by(question) %>%
  summarise(median(attempt))

ggplot(mediaQuestoes, aes(x=question, y="median(attempt)")) + geom_point(position = "jitter") 

# Qual é a média de acerto de cada aluno?

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


