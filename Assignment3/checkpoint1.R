library(dplyr)
library(ggplot2)


file <- read.csv("~/Projetos/DataAnalysis/Assignment3/brasileiro2013.csv", sep=";", header=TRUE)

fileSemNone <- file %>%
  filter(tempo_gol != "None")

fileSemNone$tempo_gol <- as.numeric(fileSemNone$tempo_gol)

primeiroTempo <- fileSemNone %>%
  filter(tempo_gol <= 45) %>%
  mutate(tempo_id = '1')

segundoTempo <- fileSemNone %>%
  filter(tempo_gol > 45) %>%
  mutate(tempo_id = '2')

segundoTempo$tempo_gol <- segundoTempo$tempo_gol - 45

mean(segundoTempo$tempo_gol)
mean(primeiroTempo$tempo_gol)

se = sd(primeiroTempo$tempo_gol) / sqrt(nrow(primeiroTempo))
lower = mean(primeiroTempo$tempo_gol) - 1.96 * se
upper = mean(primeiroTempo$tempo_gol) + 1.96 * se
ic_1 = c(lower,upper)
mean(primeiroTempo$tempo_gol)

se1 = sd(segundoTempo$tempo_gol) / sqrt(nrow(segundoTempo))
lower = mean(segundoTempo$tempo_gol) - 1.96 * se1
upper = mean(segundoTempo$tempo_gol) + 1.96 * se1
ic_2 = c(lower,upper)
mean(segundoTempo$tempo_gol)

samples = rbind(primeiroTempo,segundoTempo)

toPlot = summarise(group_by(samples, tempo_id), mean = mean(tempo_gol))
toPlot = mutate(toPlot, lower = ifelse(toPlot$tempo_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$tempo_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = tempo_id, y=mean, colour = tempo_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)

t.test

--------------------------------------------------------------------------------------
  
wilcox.test(primeiroTempo$tempo_gol, segundoTempo$tempo_gol, alternative = "two.sided") 

median(primeiroTempo$tempo_gol)
median(segundoTempo$tempo_gol)




