# Instala e carrega a biblioteca extraDistr
install.packages("extraDistr")  # Execute apenas se não estiver instalada
library(extraDistr)

# Dados da distribuição bivariada de Poisson
data <- matrix(c(
  24, 28, 12, 6, 0, 1, 0, 0,
  19, 39, 41, 23, 1, 0, 0, 0,
  12, 45, 47, 29, 12, 3, 1, 0,
  7, 17, 32, 21, 14, 1, 0, 0,
  2, 2, 3, 11, 13, 1, 2, 1,
  0, 0, 3, 6, 4, 2, 2, 1,
  0, 0, 2, 0, 2, 4, 0, 1,
  0, 0, 0, 1, 2, 0, 0, 0
), nrow = 8, byrow = TRUE)

# Frequências marginais
marginal_x <- rowSums(data)  # Soma das linhas
marginal_y <- colSums(data)   # Soma das colunas

# Preparar os dados para o cálculo da covariância
X <- rep(0:(length(marginal_x) - 1), marginal_x)
Y <- rep(0:(length(marginal_y) - 1), marginal_y)

# Calcular a covariância
cov_xy <- cov(X, Y)
# Exibir o resultado
cov_xy

c <- cov_xy
b <- 62.5
a <- 62.5

######

# P(X = 2, Y = 3)
prob_model <- dbvpois(2, 3, a, b, c)
print(paste("Probabilidade pela Poisson Bivariada: ", prob_model))


# proporção amostral
n_23 <- data[3, 4]
N <- sum(data)
prob_sample <- n_23 / N
print(paste("Proporção amostral P(X=2, Y=3): ", prob_sample))

difference <- abs(prob_model - prob_sample)
print(paste("Diferença entre o modelo e a proporção amostral: ", difference))


#######

total_prob <- 0
for (x in 0:2) {  # X <= 2
  for (y in 3:7) {  # Y >= 3
    total_prob <- total_prob + dbvpois(x, y, a, b, c)
  }
}
print(paste("Probabilidade pelo modelo de Poisson Bivariada P(X <= 2, Y >= 3): ", total_prob))


total_count <- sum(data[1:3, 4:8])  # Somando para X <= 2 e Y >= 3
N <- sum(data)

prob_sample <- total_count / N
print(paste("Proporção amostral P(X <= 2, Y >= 3): ", prob_sample))

difference <- abs(total_prob - prob_sample)
print(paste("Diferença entre o modelo e a proporção amostral: ", difference))

