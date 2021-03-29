library(tidyverse)

## IEE vs IBOV ##
modelo1 <- lm(ieevar ~ ibovvar, data = indices)
summary(modelo1)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo1)

#Teste de Durbin-Watson manual
erro <- modelo1$residuals[-1]
defasagem <- modelo1$residuals[-248]
d = sum((erro - defasagem)**2)/sum(erro**2)
print(d)

## ICON vs IBOV ##
modelo2 <- lm(iconvar ~ ibovvar, data = indices)
summary(modelo2)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo2)

## IMOB vs IBOV ##
modelo3 <- lm(imobvar ~ ibovvar, data = indices)
summary(modelo3)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo3)

## IMAT vs IBOV ##
modelo4 <- lm(imatvar ~ ibovvar, data = indices)
summary(modelo4)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo4)

#Teste de Durbin-Watson manual
erro <- modelo4$residuals[-1]
defasagem <- modelo4$residuals[-248]
d = sum((erro - defasagem)**2)/sum(erro**2)

#Correção da autocorrelação
library(orcutt)
cochrane.orcutt(modelo4)

## IFNC vs IBOV ##
modelo5 <- lm(ifncvar ~ ibovvar, data = indices)
summary(modelo5)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo5)

## INDX vs IBOV ##
modelo6 <- lm(indxvar ~ ibovvar, data = indices)
summary(modelo6)

#Análise de autocorrelação nos resíduos
library(lmtest)
dwtest(modelo6)
