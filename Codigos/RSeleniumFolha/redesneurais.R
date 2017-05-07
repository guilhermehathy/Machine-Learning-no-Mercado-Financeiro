rm(list=ls())

#Carrega funções e pacotes
source("funcoes.R")
  
#---------------------------------------------------------------------------------

getSymbols(Symbols = "BRL=X", src = "yahoo", from = '2015-01-01', to = '2015-12-01')

x <- `BRL=X`

dados2015 <- x[,4]


#-----------------------------------------------------------------------------------

getSymbols(Symbols = "BRL=X", src = "yahoo", from = '2016-01-01', to = '2016-12-01')
x <- `BRL=X`
dados2016 <- x[,4]

rm(x)

#------------------------------------------------------------------------------------
#Criando algoritimo com o melhor resultado

dados165 <- separadia(dados2015, 5)  
maxit<-as.integer(10000000)
nn <-  neuralnet(dados165[,5]~dados165[,1] + dados165[,2] +
                   dados165[,3] + dados165[,4], data = dados165, 
                 hidden = c(10,5), stepmax = maxit)

plot(nn)
dados175 <- separadia(dados2016,5)

teste <-  compute(nn, dados175[,1:4])


res <- cbind(dados175[,4], teste$net.result)
colnames(res) <- c("quartodia", "previsto")

res



#------------------------------------------------------------------------------------
#Importanto ultimos preços.


tickers = 'BRL=X' 

precos = new.env()
getSymbols(tickers, src = 'yahoo', from = '2016-01-06', to = '2016-11-30', env = precos, auto.assign = T)	

#Ajustando os valores

for(i in ls(precos)) precos[[i]] = adjustOHLC(precos[[i]], use.Adjusted=T)  
bt.prep(precos, align='remove.na') 

#------------------------------------------------------------------------------------


prices = precos$prices  
models = list()

# Testando a rede neural 

precos$weight[] = NA
precos$weight[] = iif(prices < res[,2], 1, 0)
models$neural = bt.run.share(precos, clean.signal=T)

strategy.performance.snapshoot(models, T)
