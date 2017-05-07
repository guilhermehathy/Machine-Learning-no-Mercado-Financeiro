
library("RSelenium")
rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]
remDr$navigate("https://login.folha.com.br/login?done=http%3A%2F%2Ffolhainvest.folha.uol.com.br%2Fcarteira&service=folhainvest")

login <- remDr$findElement("name", "email")
login$sendKeysToElement(list("guilhermehathy@hotmail.com"))
senha <- remDr$findElement("name", "password")
senha$sendKeysToElement(list("9895iyij", key = "enter"))

Sys.sleep(10)
#--------------------------------------------------------------------------------------
remDr <- rD[["client"]]
remDr$navigate("http://folhainvest.folha.uol.com.br/comprar")


#A Ação
option <- remDr$findElement(using = 'xpath', "//select[@id='quoteBox']/option[@value='BBDC4']")
option$clickElement()

#Preço a mercado
amercado <- remDr$findElement(using = 'id',"pricing_market")
amercado$clickElement()

#Selecionando a quantidade
quantidade <- remDr$findElement("name", "quantity")
quantidade$sendKeysToElement(list("100", key = "enter"))

Sys.sleep(10)

#Confirmando Ordem

confirma <- remDr$findElement(using = 'name',"confirm")
confirma$clickElement()

remDr$close()
