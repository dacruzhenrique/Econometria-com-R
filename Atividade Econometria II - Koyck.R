library(tidyverse)

#Dataset utilizado
library(readxl)
df <- read_excel("DDKOYCK.xlsx")
View(df)

# Modelo de Defasagens Distribuídas (DD)

  # I) Ad hoc:

#1ª rodada:
modelo1 <- lm(df$`Consumo das Famílias` ~ df$PIB)
summary(modelo1)

#2ª rodada:
modelo2 <- lm(df$`Consumo das Famílias` ~ df$PIB + lag(df$PIB, 1))
summary(modelo2)

#3ª rodada:
modelo3 <- lm(df$`Consumo das Famílias` ~ df$PIB + lag(df$PIB, 1) +
                lag(df$PIB, 2))
summary(modelo3)

#Como na 3ª rodada um dos coeficientes mudou de sinal e, portanto, paramos

#Utilizando os resultados da melhor rodada (2ª rodada), temos:
#multiplicador de CP = 0,4222
#multiplicador de LP = 0,7449
#a0 = -0,0002455

  #II) Abordagem de Koyck:

#Yt=a0(1-lamb)+b0Xt+(lamb)Y(t-1)+vt -> vt=ut-(lamb)u(t-1)
#Como esse modelo faz parte do caso 3 de autocorrelação, não podemos usar MQO

## Variáveis Instrumentais ##
library(ivreg)

koyck <- ivreg(df$`Consumo das Famílias` ~ df$PIB + lag(df$`Consumo das Famílias`, 1)| df$PIB + 
              lag(df$PIB, 1))

summary(koyck)

#Para esse modelo, temos: (lambda = 0,3618)
#multiplicador de CP = 0,4779
#multiplicador de LP = 0,4779 / (1 - 0,3618) = 0,7488
#a0 original = -0,0001623 / (1 - 0,3618) = -0,0002543