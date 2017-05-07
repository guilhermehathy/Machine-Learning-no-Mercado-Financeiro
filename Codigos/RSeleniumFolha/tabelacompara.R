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

#----------------------------------------------------------------------------------


dias = 5
neuronio1 = 5
neuronio2 = 0
resultado <- data.frame(Dias = numeric(),Neuronio1 = numeric(),
                        Neuronio2 = numeric(),
                        Resultado = numeric())



while(dias <= 5){
  while(neuronio1 <=20){
    while(neuronio2<=5){
      
      
      x <- algoritmo(dadostreino = dados2015, dadosvalidacao = dados2016,
                     dias = dias, n1 = neuronio1, n2 = neuronio2, 
                     resultado = resultado)
      resultado[nrow(resultado) + 1 ,] <- x
      
      
      neuronio2 = neuronio2+5
    }
    neuronio2 = 0
    neuronio1 = neuronio1 + 5
  }
  neuronio1 = 5
  dias = dias + 5
}



head(resultado[order(resultado$Resultado),])
