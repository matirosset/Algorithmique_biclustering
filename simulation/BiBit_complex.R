source("R/resultat.R")
source("R/BiBit.R")

library(microbenchmark)
library(ggplot2)

# Simulation sur le nombre de lignes

mean_bibit_n <- c()
for(i in seq(1,30)){
  m <- summary(microbenchmark(bibit(binaryMatrix(20*i,50,10,1,0)$matR), unit = "ms", times = 200))$mean
  mean_bibit_n  <- c(mean_bibit_n, m)
}

n <- 20*seq(1,30)

ggplot(as.data.frame(cbind(n, mean_bibit_n)), aes(n, mean_bibit_n)) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Nombre de lignes n") + 
  ylab("Temps moyen en millisecondes") + 
  ggtitle("Temps moyen d'exécution sur 200 simulations pour chaque n")

ggplot(as.data.frame(cbind(n, mean_bibit_n)), aes(log(n), log(mean_bibit_n))) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Log nombre de lignes n") + 
  ylab("Log temps moyen en millisecondes") + 
  ggtitle("Log temps moyen d'exécution sur 200 simulations pour chaque n")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, color = "lightgreen")


reg <- lm(log(mean_bibit_n) ~ log(n))
reg$coefficients



# Simulation sur le nombre de colonnes

mean_bibit_m <- c()
for(i in seq(1,30))
{
  m <- summary(microbenchmark(bibit(binaryMatrix(50,50*i,10,1,0)$matR), unit = "ms", times = 200))$mean
  mean_bibit_m  <- c(mean_bibit_m, m)
}

m <- 50*seq(1,30)

ggplot(as.data.frame(cbind(m, mean_bibit_m)), aes(m, mean_bibit_m)) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Nombre de colonnes m") + 
  ylab("Temps moyen en millisecondes") + 
  ggtitle("Temps moyen d'exécution sur 200 simulations pour chaque m")

ggplot(as.data.frame(cbind(m, mean_bibit_m)), aes(log(m), log(mean_bibit_m))) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Log nombre de colonnes m") + 
  ylab("Log temps moyen en millisecondes") + 
  ggtitle("Log temps moyen d'exécution sur 200 simulations pour chaque m")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, color = "lightgreen")

reg <- lm(log(mean_bibit_m) ~ log(m))
reg <- lm(log(mean_bibit_m[10:30]) ~ log(m[10:30]))



# Simulation sur le nombre de biclusters

mean_bibit_c <- c()
for(i in seq(4,100,8))
{
  m <- summary(microbenchmark(bibit(binaryMatrix(150,150,i,1,0)$matR), unit = "ms", times = 200))$mean
  mean_bibit_c  <- c(mean_bibit_c, m)
}

cluster <- seq(4,100,8)

ggplot(as.data.frame(cbind(cluster, mean_bibit_c)), aes(cluster, mean_bibit_c)) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Nombre de clusters nbClust") + 
  ylab("Temps moyen en millisecondes") + 
  ggtitle("Temps moyen d'exécution sur 200 simulations pour chaque nbClust")

ggplot(as.data.frame(cbind(cluster, mean_bibit_c)), aes(log(cluster), log(mean_bibit_c))) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Log nombre de clusters nbClust") + 
  ylab("Log temps moyen en millisecondes") + 
  ggtitle("Log temps moyen d'exécution sur 200 simulations pour chaque nbClust")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, color = "lightgreen")

reg <- lm(log(mean_bibit_c) ~ log(cluster))



# Simulation sur le nombre de lignes et colonnes

mean_bibit_both <- c()
for(i in seq(40,1000,50))
{
  m <- summary(microbenchmark(bibit(binaryMatrix(i,i,30,1,0)$matR), unit = "ms", times = 100))$mean
  mean_bibit_both  <- c(mean_bibit_both, m)
}

both <- seq(40,1000,50)

ggplot(as.data.frame(cbind(both, mean_bibit_both)), aes(both, mean_bibit_both)) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Nombre de lignes/colonnes") + 
  ylab("Temps moyen en millisecondes") + 
  ggtitle("Temps moyen d'exécution sur 100 simulations")

ggplot(as.data.frame(cbind(both, mean_bibit_both)), aes(log(both), log(mean_bibit_both))) + 
  geom_line(linetype = "dashed", color = "steelblue") +
  geom_point(color = "steelblue") + 
  xlab("Log nombre de lignes/colonnes") + 
  ylab("Log temps moyen en millisecondes") + 
  ggtitle("Log temps moyen d'exécution sur 100 simulations")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, color = "lightgreen")

reg <- lm(log(mean_bibit_both) ~ log(both))
reg <- lm(log(mean_bibit_both[10:20]) ~ log(both[10:20]))
