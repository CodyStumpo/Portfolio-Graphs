#assumes every ticker has a quote for every day of the period.  
setwd("~/Box Sync/cms/rport/")
#date1=Sys.Date()-183
#date1="2015-12-31"
#Can run over specified period or for example, last 90 days
date1=Sys.Date()-90
date2=Sys.Date()

msharpe=.85 #market sharpe, from creditedge was .55 until late '11, then climbing, historically 0.6
libor=.011 #1-year libor

library(hash)
portfolio=hash(
        #US STOCKS
           "AAPL"=5, "COST"=50, "GOOGL"=6, "FBT"=50, "RGI"=50, "RYU"=100+25, "PSJ"=75, "SCHA"=150+50,
           "BRK-B"=50, "SIRI"=1000, "V"=54-16, 
           "TSLA"=10, "EWRS"=200+50, "PBS"=100, "RTM"=50,
          "AMZN"=4,
  #looks like 200 EWRM becomes 200 EWMC on 29 Jan 2016 - add handling for symbol change
        #WORLD STOCKS
           "VWO"=150+100, "YAO"=150, "VTI"=109, "EWGS"=200, "VEA"=50+100, "IXJ"=100, "IXP"=125,
           "JPP"=150, "SCHC"=250, "ACIM"=50+50, "FNDE"=200+200, "SCHF"=300, "LIT"=300/2,
       "ICLN"= 500,
        # US BONDS   
           "HYD"=100, "PZA"=200, "LQD"=50-50+20, "BKLN"=300,
        # WORLD BONDS   
           "PCY"=200, "WIP"=100, "BWX"=100-50, 
        # ALT   
           "RJI"=700, "GCC"=250, "VNQ"=100, "USCI"=100)
shares=as.numeric(values(portfolio))
tickers=names(values(portfolio))



itickers<-c("vt","fwdb","usci")

# for financial engines beta=1, mix 55%, 45%, 0. 
# I'll say benchmark for me is 70,20,10.  
iweights=c(.7,.2,.1)


source("portHelper.R")

df[with(df, order(-retrisk)), ]

summary(rir*100)

table(sign(simyears-1))/numsims


#pretty mispricing
library("ggplot2")
library("ggrepel")
ggplot(df) +
  coord_cartesian(xlim=c(min(rc,0),max(rc)+.02)) +
  coord_cartesian(ylim=c(0,max(ereturns)+.02)) +
  geom_point(aes(rc, ereturns), color = 'red') +
  geom_text_repel(aes(rc, ereturns, label = tickers)) +
  geom_abline(intercept=libor, slope=((ereturn-libor)/stdev)) + #absolute return
  xlab("Non-Diversified Risk") +
  ylab("E[return]") +
  ggtitle("Mispricing")
#can't put in helper because ??
