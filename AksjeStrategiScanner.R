
###### Biblioteker

#install.packages("ReporteRs")
#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("xlsx")


#skal innholde momentum triggers,trend, VOLum, standard avvik
#Først og fremst trendfølging


library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyverse)
library(xlsx)

######

symbols <- c("ATEA.OL","VEI.OL","MHG.OL","YAR.OL", "AFG.OL", "AKER.OL","AXA.OL",
             "BAKKA.OL","NRS.OL","LSG.OL","GSF.OL","SALM.OL","DNB.OL","STB.OL","SRBANK.OL",
             "REC.OL","NAS.OL","NHY.OL", "TGS.OL","STL.OL","TEL.OL","AKSO.OL","AKERBP.OL","AVM.OL","DNB.OL","EMGS.OL"
             , "FRO.OL","FUNCOM.OL","SONG.OL","PGS.OL","AUSS.OL","RCL.OL","OPERA.OL","BWLPG.OL")

getSymbols(symbols, from = "2014-01-01")

prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
View(prices)

# momentum -> Definer som rate of change


roc <- ROC(prices, n = 1)
roc <- na.omit(roc)
class(roc)
meanroc <- sapply(roc, mean)

meanROC <- apply(roc, 1, mean)

head (meanroc)
head(meanROC)

MomSignal10 <- ifelse(rollapply(roc, width = 10, FUN = mean) < meanroc, TRUE, FALSE)
MomSignal5 <- ifelse ( rollapply(roc, width = 5, FUN = mean) < meanroc, TRUE, FALSE)



prices
##<- overt dett fucker opp fsma og SUMMER testene


## VOLUM kriterie

 volume <- list()
 for(a in 1:length(symbols)) {
   volume[[a]] <- Vo(get(symbols[a]))  
 }
 volume <- do.call(cbind, volume)
 colnames(volume) <- gsub("\\.[A-z]*", "", colnames(volume))

 
meanVolume <- sapply(volume,mean)


#Få disse til å gi en mer oversiktiglig output, som navnet på aksjene som tilfredstiller kravet. samme gjelder alle signalene



Volumsignal10 <- t(tail(ifelse( rollapply(volume, width = 10, FUN = mean) < meanVolume , TRUE, FALSE),1))
Volumsignal5 <- t(tail(ifelse( rollapply(volume, width = 5, FUN = mean) < meanVolume , TRUE, FALSE),1))


### TREND definer som Pris > 100 SMA legg til en kortsiktig 20 ema også. 

SMA100f <- function (x) { sma<- SMA(x, n = 100)
  
return (sma)
na.omit(sma)
}

EMA20f <- function(x) { ema <- EMA(x, n = 20)
        return(ema)
}


head(SMA100f(prices$TEL))
head(EMA20f(prices$TEL))

#priceNA <- na.omit(prices)
#str(priceNA)

SMA100 <- apply(prices,2, SMA100f)
tail(SMA100)

TrendSignal <- ifelse(prices > SMA100, TRUE, FALSE)
tail(TrendSignal)

EMA20 <- apply(prices, 2, EMA20f)
TrendSignalEMA <- ifelse(prices > EMA20, TRUE, FALSE)
tail(TrendSignalEMA)

###SUMMER signaler

AlleSignal10 <- t(tail(ifelse( MomSignal10 == TRUE & Volumsignal10 == TRUE & TrendSignal == TRUE, TRUE,FALSE),1))

MomTrendSignal100 <- t(tail(ifelse( MomSignal10 == TRUE & TrendSignal == TRUE, TRUE,FALSE),1))
Mom10TrendSignal20EMA <- t(tail(ifelse( MomSignal10 == TRUE & TrendSignalEMA20 == TRUE, TRUE,FALSE),1))
Mom5TrendSignal20EMA  <- t(tail(ifelse( MomSignal5 == TRUE & TrendSignalEMA20 == TRUE, TRUE,FALSE),1))                
                       
#TrendSignalSMA100 <- t(tail(TrendSignal),1)
#TrendSignalEMA20 <- t(tail(TrendSignalEMA),1)


MomSignal10 <- t(tail((MomSignal10),1))
MomSignal5 <- t(tail((MomSignal5),1))
MomSignal10

MOM<-data.frame(cbind(MomSignal5,MomSignal10))
View(MOM)
colnames(MOM) <- c("Momentum 5", "Momentum 10")
MOM

mom <- tbl_df(MOM)
mom<-mom %>%
 mutate(id = rownames(symbols)) #%>%
 #filter(`Momentum 5` ==  TRUE, `Momentum 10` == TRUE)
   mom
   str(MOM)

#View(mom)


###### Revert to mean signal 
{
SDF <- function(x) { sd(x)
}

rolingSD <- function(x) {rollapplyr(x, 20, sd)
}

#tail(apply(prices,2,rolingSD),1)

Sd <- apply(prices, 2, SDF)
Sd
tail(prices)
SDSignal1x <- t(tail(ifelse(prices > mean(prices)+Sd,TRUE,FALSE ),1))
SDSignal2x <- t(tail(ifelse(prices >2*Sd,TRUE,FALSE ),1))
SDSignal3x <- t(tail(ifelse(prices > 3*Sd,TRUE,FALSE ),1))

SDSignal1x
SDSignal2x
SDSignal3x
}
#####Final greir

Final <- cbind(MomSignal10, MomSignal5,Volumsignal10,Volumsignal5,MomTrendSignal100 ,Mom10TrendSignal20EM,Mom5TrendSignal20EMA )
colnames(Final) <- c("10 dagers" , "5 dagers", "Volum 10", "Volum 5","10mom+SMA","10mom+EMA","5mom + EMA" )
Final 

### Send til document ######

write.xlsx(data.frame(MOM), file = "C:\\Users\\Simen\\Desktop\\Strategi.xlsx", sheetName = "Momentum Compression v0.1")

