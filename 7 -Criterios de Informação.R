                       #Aula 7 - Critérios de Informação

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Varia��o do PIB brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -Função de Autocorrelação (ACF)
pacf(var_PIB)        #cria a FACP - Funçao de Autocorrelação Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
AR1
MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de médias móveis ordem q=1 , MA(1)
MA1
ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de médias móveis ordem p=1 e q=1 ARMA(1,1)
ARMA11

AIC(AR1) #Extrai a estatística AIC do modelo AR1
BIC(AR1) #Extrai a estatística BIC Ddo modelo AR1


AR2 <- arima(var_PIB, order = c(2,0,0))
AR2
AIC(AR2)
BIC(AR2)
MA2 <- arima(var_PIB, order = c(0,0,2))
MA2
AIC(MA2)
BIC(MA2)
MA3 <- arima(var_PIB, order = c(0,0,3))
MA3
AIC(MA3)
BIC(MA3)
MA4 <- arima(var_PIB, order = c(0,0,4))
MA4
AIC(MA4)
BIC(MA4)
MA5 <- arima(var_PIB, order = c(0,0,5))
MA5
AIC(MA5)
BIC(MA5)
MA6 <- arima(var_PIB, order = c(0,0,6))
MA6
AIC(MA6)
BIC(MA6)
MA7 <- arima(var_PIB, order = c(0,0,7))
MA7
AIC(MA7)
BIC(MA7)
MA8 <- arima(var_PIB, order = c(0,0,8))
MA8
AIC(MA8)
BIC(MA8)
MA9 <- arima(var_PIB, order = c(0,0,9))
MA9
AIC(MA9)
BIC(MA9)

ARMA12 <- arima(var_PIB, order = c(1,0,2))
ARMA12
AIC(ARMA12)
BIC(ARMA12)
ARMA13 <- arima(var_PIB, order = c(1,0,3))
ARMA13
AIC(ARMA13)
BIC(ARMA13)
ARMA14 <- arima(var_PIB, order = c(1,0,4))
ARMA14
AIC(ARMA14)
BIC(ARMA14)
ARMA15 <- arima(var_PIB, order = c(1,0,5))
ARMA15
AIC(ARMA15)
BIC(ARMA15)
ARMA16 <- arima(var_PIB, order = c(1,0,6))
ARMA16
AIC(ARMA16)
BIC(ARMA16)
ARMA17 <- arima(var_PIB, order = c(1,0,7))
ARMA17
AIC(ARMA17)
BIC(ARMA17)
ARMA18 <- arima(var_PIB, order = c(1,0,8))
ARMA18
AIC(ARMA18)
BIC(ARMA18)
ARMA19 <- arima(var_PIB, order = c(1,0,9))
ARMA19
AIC(ARMA19)
BIC(ARMA19)

ARMA21 <- arima(var_PIB, order = c(2,0,1))
ARMA21
AIC(ARMA21)
BIC(ARMA21)
ARMA22 <- arima(var_PIB, order = c(2,0,2))
ARMA22
AIC(ARMA22)
BIC(ARMA22)
ARMA23 <- arima(var_PIB, order = c(2,0,3))
ARMA23
AIC(ARMA23)
BIC(ARMA23)
ARMA24 <- arima(var_PIB, order = c(2,0,4))
ARMA24
AIC(ARMA24)
BIC(ARMA24)
ARMA25 <- arima(var_PIB, order = c(2,0,5))
ARMA25
AIC(ARMA25)
BIC(ARMA25)
ARMA26 <- arima(var_PIB, order = c(2,0,6))
ARMA26
AIC(ARMA26)
BIC(ARMA26)
ARMA27 <- arima(var_PIB, order = c(2,0,7))
ARMA27
AIC(ARMA27)
BIC(ARMA27)
ARMA28 <- arima(var_PIB, order = c(2,0,8))
ARMA28
AIC(ARMA28)
BIC(ARMA28)
ARMA29 <- arima(var_PIB, order = c(2,0,9))
ARMA29
AIC(ARMA29)
BIC(ARMA29)

#Exemplo aplicação múltipla - Extra (Deve-se completar as estimações antes de executar esse código)

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)