source("funcoes.R")
load("redeneural.RData")

cota <- read.table("bovespa")
compra <- data.frame(Nome = character())


for(i in cota[,1]){
  
  ti <- paste0(i,".SA")
  tickers = ti
  print(ti)
  
  precos = new.env()
  getSymbols(tickers, src = 'yahoo', from = '2017-01-01', to = '2018-12-31',
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
  
  if(last(precos$weight) == 1){
    
  }
}

