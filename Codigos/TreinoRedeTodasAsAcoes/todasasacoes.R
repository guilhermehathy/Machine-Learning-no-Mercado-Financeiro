setwd("/home/guihathy/Pesquisa/Machine/SIT/TreinoRedeTodasAsAcoes")
source("funcoes.R")
library(SIT)
load.packages('quantmod')




cota <- read.table("bovespa")


dados <-  data.frame()
for (i in cota[,1]){
 tickers <- paste0(i,".SA")
 x <- getSymbols(tickers,from='2016-01-01', to = '2016-02-31', auto.assign = F)
 teste <- separadia(x[,4], 5)
 dados <- rbind(dados, teste)
}




library(caret)
trainIndex <- createDataPartition(dados$x1, p=.75, list=F)
dados.train <- dados[trainIndex, ]
dados.test <- dados[-trainIndex, ]


redeneural <- train(x5 ~.,data = dados.train, method = "nnet", maxit = 100000)


library(devtools)

source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')


redeneural <- train(x5 ~.,data = dados.train, method = "nnet", maxit = 100000)


plot.nnet(redeneural ,nid=T)
