library(dplyr)
library(ggplot2)

dchi <- function(x, k){
  log.denominator <- lgamma(k/2)
  log.numerator <- (1 - k/2)*log(2) + (k-1)*log(x) - x^2/2
  return(exp(log.numerator - log.denominator))
}



x.seq <- seq(0.1, 40, 0.1)
k.seq <- c(1, 10, 100, 1000)


chi.df <- data.frame(
  density = c(sapply(k.seq, function(k){
    sapply(x.seq, dchi, k = k)})),
  d = rep(x.seq, length(k.seq)),
  n = rep(k.seq, each = length(x.seq))) %>%
  filter(density > 0.001) %>% mutate(n = as.factor(n))

chi.plot <- qplot(d, density, color = n, geom = "line", data = chi.df, size = I(2))

ggsave("chiplot.png", chi.plot, width = 8, height = 2)
