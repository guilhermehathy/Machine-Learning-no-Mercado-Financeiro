rm(list=ls())

#Carrega funções e pacotes
source("funcoes.R")
library(caret)

#---------------------------------------------------------------------------------

getSymbols(Symbols = "BRL=X", src = "yahoo", from = '2015-01-01', to = '2015-12-01')

x <- `BRL=X`

dados2015 <- x[,4]


#-----------------------------------------------------------------------------------

getSymbols(Symbols = "BRL=X", src = "yahoo", from = '2016-01-01', to = '2016-12-01')
x <- `BRL=X`
dados2016 <- x[,4]

rm(x)

#----------------------------------------------------------------------------------
#Dados Treinamento
x <-  cbind(dados2015,EMA(dados2015, 8), EMA(dados2015, 17),EMA(dados2015, 34),RSI(dados2015,17),stoch(dados2015,8), 
            MACD(dados2015, 12,26,9))
x <- x[c(34:237),]
y <- as.numeric(dados2015[35:238,])

dados <- cbind(x,y)

#-----------------------------------------------------------------------------------
#Dados Validação

x <-  cbind(dados2016,EMA(dados2016, 8), EMA(dados2016, 17),EMA(dados2016, 34),RSI(dados2016,17),stoch(dados2016,8), 
            MACD(dados2016, 12,26,9))


x <- x[c(34:237),]
y <- as.numeric(dados2016[35:238,])
dadosValida <- cbind(x,y)



#--------------------------------------------------------------


resultadoRMSE <- data.frame(Neuronio1 = numeric(),
                            Neuronio2 = numeric(),
                            Resultado = numeric())



neuronio1 <-5
neuronio2 <- 0  
while(neuronio1 <= 5){
  while(neuronio2 <= 0){
    if(neuronio2 == 0){
      form <- as.formula(paste(sprintf("dados[, %d] ~ ", ncol(dados)),
                               paste(sprintf("dados[, %d]", 
                                             1:(ncol(dados)-1)), collapse = "+")))
      maxit<-as.integer(10000000)
      rede <- neuralnet(form, data = dados, hidden = neuronio1, 
                        stepmax = maxit)
    }
    else{
      maxit<-as.integer(1000000000)
      form <- as.formula(paste(sprintf("dados[, %d] ~ ", ncol(dados)),
                               paste(sprintf("dados[, %d]", 
                                             1:(ncol(dados)-1)), collapse = "+")))
      rede <- neuralnet(form, data = dados, hidden = c(neuronio1,neuronio2), 
                        stepmax = maxit)
    }
    resultado <- compute(rede, dadosValida[,1:(ncol(dadosValida)-1)])
    
    generico <- c(neuronio1, neuronio2, RMSE(dadosValida[,ncol(dadosValida)], resultado$net.result))
    print(generico)
    resultadoRMSE[nrow(resultadoRMSE) + 1 ,] <- generico
    neuronio2 = neuronio2 + 5
  }
  neuronio2 = 0
  neuronio1 = neuronio1 + 5
}



#----------------------------------------------------------------------------------

tickers = 'BRL=X'

precos = new.env()
getSymbols(tickers, src = 'yahoo', from = '2016-02-17',to = '2016-11-28', env = precos, auto.assign = T)	

#Ajustando os valores

for(i in ls(precos)) precos[[i]] = adjustOHLC(precos[[i]], use.Adjusted=T)  
bt.prep(precos, align='remove.na') 

prices = precos$prices  
models = list()

#Rede
precos$weight <- ifelse(dadosValida[,1] < resultado$net.result &  dadosValida[,2] > dadosValida[,3], 1, 0)

models$rede = bt.run.share(precos, clean.signal=T)

strategy.performance.snapshoot(models, T)
plotbt.custom.report.part2(models)
