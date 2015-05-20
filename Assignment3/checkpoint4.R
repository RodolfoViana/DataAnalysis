install.packages("data.table") 


library(data.table)
library(dplyr)
library(ggplot2)

file <- read.csv("~/Projetos/DataAnalysis/Assignment3/brasileiro2013.csv", sep=";", header=TRUE)

Existe diferença significativa entre a média de tempo dos gols de times do 
estado do Rio de Janeiro com os times do estado de São Paulo?

Times do Rio de Janeiro: Vasco da Gama, Fluminense, Botafogo, Flamengo.
Times de São Paulo: Corinthians, Ponte Preta, Santos, São Paulo, Portuguesa


timesRJ <- c("Vasco da Gama", "Fluminense", "Botafogo", "Flamengo")
timesSP <- c("Corinthians", "Ponte Preta", "Santos", "São Paulo", "Portuguesa")


file$time_a <- as.character(file$time_a)
file$time_b <- as.character(file$time_b)


#fileRj_a <- file %>%
#  filter(time_a %in% timesRJ | time_b %in% timesRJ)

fileRj_a <- file %>%
  filter(time_a %in% timesRJ) %>%
  as.data.table()

golsRj_a <- fileRj_a[, .SD[.N], by=id_jogo]$placar_time_a

fileRj_b <- file %>%
  filter(time_b %in% timesRJ) %>%
  as.data.table()
  
golsRj_b <- fileRj_b[, .SD[.N], by=id_jogo]$placar_time_b

golsRj <- c(golsRj_a, golsRj_b)

hist(golsRj)

fileSP_a <- file %>%
  filter(time_a %in% timesSP) %>%
  as.data.table()
  
golsSP_a <- fileSP_a[, .SD[.N], by=id_jogo]$placar_time_a
  
fileSP_b <- file %>%
  filter(time_b %in% timesSP) %>%
  as.data.table()

golsSP_b <- fileSP_b[, .SD[.N], by=id_jogo]$placar_time_b

golsSP <- c(golsSP_a, golsSP_b)

hist(golsSP)

Com base nessa amostra temos essa distribuição de gols para os times do estado do Rio de Janeiro:
  
  ```{r, warning=FALSE, fig.align='center', message=FALSE}
hist(golsRj, main="Frequência dos gols do Rio")
```

Temos essa distribuição dos gols dos times do estado de São Paulo:
  
  ```{r, warning=FALSE, fig.align='center', message=FALSE}
hist(golsSP, main="Frequência dos gols de SP")
```


t.test(golsSP, golsRj, alternative = "two.sided")$p.value


testRj <- t.test(golsRj)
testSP <- t.test(golsSP)
ic_1 = c(testRj$conf.int[1],testRj$conf.int[2])
ic_2 = c(testSP$conf.int[1],testSP$conf.int[2])

toPlot <- data.frame(estado = c("RJ","SP"), media = c(mean(golsRj), mean(golsSP)))
toPlot = mutate(toPlot, lower = ifelse(toPlot$estado == "RJ",ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$estado == "RJ",ic_1[2],ic_2[2]))

ggplot(to, aes(x = estado, y=media, colour = estado )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  labs(y='Média do tempo dos gols (em min)', x='Tempo da partida') +
  theme(panel.background=element_blank())

------------------------------------------------------------------------------------------------

Existe diferença significativa entre a média de gols do primeiro turno e do segundo turno?

primeiroTurno <- file %>%
  filter(id_jogo <= 190) %>%
  as.data.table()

segundoTurno <- file %>%
  filter(id_jogo > 190) %>%
  as.data.table()

primeiroTurno <- primeiroTurno[, .SD[.N], by=id_jogo]
segundoTurno <- segundoTurno[, .SD[.N], by=id_jogo]

golsPrimeiroTurno = c(primeiroTurno$placar_time_a, primeiroTurno$placar_time_b)

hist(golsPrimeiroTurno)

golsSegundoTurno = c(segundoTurno$placar_time_a, segundoTurno$placar_time_b)

hist(golsSegundoTurno)

t.test(golsPrimeiroTurno, golsSegundoTurno, alternative = "two.sided")


----------------------------------------------------------------------------------------
  
Existe diferença significativa entre a média de gols dos times fora e dentro de casa

Eu imagino que os times fazem mais gols dentro de casa do que fora de casa, 
por esse motivo acredito que a média dos gols será diferente. 

partidasResultados <- as.data.table(file)[, .SD[.N], by=id_jogo]

(quando eu mudo para less eu aceito a h0 então? Mostrando que eles são iguais?)

t.test(partidasResultados$placar_time_a, partidasResultados$placar_time_b, alternative = "two.sided")

dentroDeCasa <- t.test(partidasResultados$placar_time_a)
foraDeCasa <- t.test(partidasResultados$placar_time_b)
ic_1 = c(dentroDeCasa$conf.int[1],dentroDeCasa$conf.int[2])
ic_2 = c(foraDeCasa$conf.int[1],foraDeCasa$conf.int[2])

toPlot <- data.frame(estado = c("Em casa","Fora de Casa"), media = c(mean(partidasResultados$placar_time_a), mean(partidasResultados$placar_time_b)))
toPlot = mutate(toPlot, lower = ifelse(toPlot$estado == "Em casa",ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$estado == "Em casa",ic_1[2],ic_2[2]))

ggplot(toPlot, aes(x = estado, y=media, colour = estado )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  labs(y='Média dos gols', x='') +
  theme(panel.background=element_blank())
  

