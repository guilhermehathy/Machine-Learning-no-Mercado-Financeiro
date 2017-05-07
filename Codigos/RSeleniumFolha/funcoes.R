

#Carregando Bibiotecas
library(neuralnet)
library(SIT)

load.packages('quantmod')


#------------------------------------------------------------------------------------

separadia <- function(dados,dias){
  
  
  prices <- dados
  
  
  diasgenerico = dias -1
  
  t0<-as.numeric(prices)            
  t0<-t0[-((length(t0)-diasgenerico):length(t0))]
  
  i = 1
  coluna = 2
  while(i < dias){
    diasgenerico = diasgenerico - 1
    
    t2<-as.numeric(prices) [-seq(1,i,1)] 
    t2<-t2[-((length(t2)-diasgenerico):length(t2))]
    
    t0 <- cbind(t0,t2)
    
    coluna = coluna + 1
    i = i + 1
  }
  
  colnames(t0) <- sprintf("x%d", 1:length(colnames(t0)))
  return(t0)
}
#---------------------------------------------------------------------------------

RMSE <- function(x, y){
  res <- sqrt((sum(x - y)^2) / length(x) )
  return(res)
  
}

#---------------------------------------------------------------------------------

algoritmo <- function(dadostreino, dadosvalidacao, dias, n1, n2, resultado){
  
  dadostreinodividido <- separadia(dados = dadostreino, dias = dias)
  
  x <- as.formula(paste(sprintf("dadostreinodividido[, %d] ~ ", dias),
                        paste(sprintf("dadostreinodividido[, %d]", 
                                      1:(dias-1)), collapse = "+")))
  
  if(n1 == 0  & n2 > 0){
    
    generico <- c(dias, n1, n2, 999999)
    
    return(generico)
    
  }

  
  if(n1 > 0  & n2 == 0){
    
    maxit<-as.integer(10000000)
    #Criando algoritmo
    algoritmo <- neuralnet(x,data=dadostreinodividido, hidden=n1, 
                           stepmax = maxit)
    
    #Testando com dados de 2017
    dadosvalidacaodividido <- separadia(dados = dadosvalidacao ,dias = dias)
    
    teste <- compute(algoritmo, dadosvalidacaodividido[,1:(dias - 1)])
    
    #Calculando RMSE
    resultadoRMSE <-  RMSE(x = teste$net.result, y = dadosvalidacaodividido[,dias])
    
    
    generico <- c(dias, n1, 0, resultadoRMSE)
    
  
    
  
    return(generico)
    
  }  
  
  
  if(n1 > 0  & n2 > 0){
    #Criando algoritmo
    maxit<-as.integer(10000000)
    algoritmo <- neuralnet(x,data=dadostreinodividido, hidden=c(n1, n2), 
                           stepmax = maxit)
    
    #Testando com dados de 2017
    dadosvalidacaodividido <- separadia(dados = dadosvalidacao ,dias = dias)
    
    teste <- compute(algoritmo, dadosvalidacaodividido[,1:(dias - 1)])
    
    #Calculando RMSE
    resultadoRMSE <-  RMSE(x = teste$net.result, y = dadosvalidacaodividido[,dias])
    
    
    generico <- c(dias, n1, n2, resultadoRMSE)
    
    
    return(generico)
  }  
  


}



ultimopreco <- function(i){
  f <- i
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
  x <- last(prices)
  y <- as.numeric(x)
  
  return(y)
}
