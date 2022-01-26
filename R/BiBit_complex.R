library(microbenchmark)
library(ggplot2)

## Temps exécution ligne

mean_bibit_n <- c()
for(i in seq(1,30))
{
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