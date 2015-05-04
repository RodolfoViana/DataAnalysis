library(dplyr)
library(ggplot2)


# Dados da câmara dos deputados
file <- read.csv("Projetos/DataAnalysis/Assignment1/AnoAtual.csv")

# Selecionando apenas os gastos de combustíveis e lubrificantes
vlrCombustiveis <- file %>% 
  filter(txtDescricao == "COMBUSTÍVEIS E LUBRIFICANTES.") %>% 
  select(vlrLiquido, numSubCota)

# Media do valor de combustíveis e lubrificantes
mediaVlrCombustiveis <- median(vlrCombustiveis$vlrLiquido)

# função de distribuição de probabilidade da variavel combustíveis e lubrificantes
ggplot(vlrCombustiveis, aes(vlrLiquido)) + 
  geom_density() + 
  labs(x='Valor liquido', y='Probablidade') + 
  theme_bw()

ggplot(vlrCombustiveis, aes(vlrLiquido)) + 
  xlim(0, 1000) +
  geom_density() + 
  labs(x='Valor liquido', y='Probablidade') + 
  theme_bw()



# Selecionar 10 amostras de tamanho 100 (valores escolhidos aleatoriamente) e plotar a função 
# de distribuição de probabilidade das médias dessas amostras


# Média de 10 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 10


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) +
  geom_density() + 
  labs(x='Valor liquido', y='Probablidade') + 
  theme_bw()


# Repetir o passo 2 aumentando o número de amostras. Cuidado! 
# É para aumentar o número de amostras, não o de observações dentro das amostras. Plote cada iteração.

# Média de 20 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 20


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) + geom_histogram()


# Média de 40 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 40


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) + geom_histogram()


# Média de 80 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 80


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) + geom_histogram()


# Média de 160 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 160


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) + geom_histogram()


# Média de 320 amostras com n = 100
dist_original <- vlrCombustiveis$vlrLiquido
sample_size <- 100
num_samples <- 320


samples_means <- c()
for(i in seq(1, num_samples)){
  a_sample <- sample(dist_original, sample_size)
  samples_means[i] <- mean(a_sample)
}
ggplot(data.frame(samples_means), aes(samples_means)) + geom_histogram()
