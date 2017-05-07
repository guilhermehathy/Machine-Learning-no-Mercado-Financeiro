rm(list=ls())

#Carrega funções e pacotes
source("funcoes.R")
load("redeneural.RData")

tickers = 'PETR4.SA'

precos = new.env()
getSymbols(tickers, src = 'yahoo', from = '2016-01-01', to = '2016-12-31',
           env = precos, auto.assign = T)

#Ajustando os valores

for(i in ls(precos)) precos[[i]] = adjustOHLC(precos[[i]], use.Adjusted=T)  
bt.prep(precos, align='remove.na') 

prices = precos$prices  
models = list()

dadostreino <-  cbind(prices,EMA(prices, 8), EMA(prices, 17),EMA(prices, 34),
                      RSI(prices,17),stoch(prices,8), MACD(prices, 12,26,9))

redesn <- compute(rede, dadostreino)

arvore <- cbind(iif(prices > SMA(prices, 20), 1, 0),
                iif(cross.up(SMA(prices, 8), SMA(prices, 20)), 1, 0),
                iif(50 > RSI(prices, 16), 1, 0),
                iif(prices < redesn$net.result, 1, 0))

x <- as.data.frame(apply(arvore,1, sum)/ncol(arvore))
x
precos$weight <- iif(x >= 0.5, 1, 0)

models$modelo = bt.run.share(precos, clean.signal=T)

strategy.performance.snapshoot(models, T)



