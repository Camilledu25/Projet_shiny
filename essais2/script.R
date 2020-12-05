library(readr)
library(dplyr)
library("forecast")
library("fpp2")
library(lubridate)
library(ggplot2)
library(caschrono)
require(graphics)


bilan_electrique_transpose <- read_delim("C:/Users/Camille/Documents/Projet_shiny/projet/bilan-electrique-transpose.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

# renommer les variables
colnames(bilan_electrique_transpose) <- c("jour","categorie_client","puissance")
bilan <- bilan_electrique_transpose[order(bilan_electrique_transpose$jour),]
bilan$categorie_client=as.factor(bilan$categorie_client)

bilan3=bilan%>%filter(categorie_client == "PME / PMI")#%>%filter(jour > as.Date ("2020-07-15"))

plot(puissance~jour,type="l",data=bilan3)

bilan1=bilan%>%filter(categorie_client == "Entreprises")%>%filter(jour > as.Date ("2020-01-15"))

plot(puissance~jour,type="l",data=bilan1)

bilan4=bilan%>%filter(categorie_client == "Professionnels")%>%filter(jour > as.Date ("2018-01-15"))

plot(puissance~jour,type="l",data=bilan4)

##############################################################################

bilan2=bilan%>%filter(categorie_client == "Résidentiels")#%>%filter(jour > as.Date ("2020-07-15"))

plot(puissance~jour,type="l",data=bilan2)

(bilan22=bilan2%>%select(-categorie_client))

autoplot(bilan22)
set.seed(96683)
date <- ymd(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01"))
mesure <- runif(length(date), 1, 10)
mesure_ts <- ts(mesure, start = date[1], frequency = 4)
mesure_ts
autoplot(mesure_ts)
bilan22=as.ts(bilan22)
class(bilan22)
class(mesure_ts)

date1=bilan22%>%select(jour)
date1=c(date1$jour)
class(date1)
puissance1=bilan22%>%select(puissance)
puissance1=c(puissance1$puissance)
class(puissance1)
serie_ts=ts(puissance1,date1[1],frequency = 2)
autoplot(serie_ts)
ggAcf(serie_ts, ci = 0.95) + ggtitle("Lynx: Autocorrélation")
acf2y(serie_ts,lag.max=20)

flow_hw <- hw(serie_ts, damped = TRUE, h = 12*3, seasonal = "additive")
autoplot(flow_hw) + autolayer(fitted(flow_hw))
m <- decompose(serie_ts,,type="multiplicative")
plot(m)
co2
