library("tseries")
library("zoo")
library("calibrate")
require(plotrix)
require(quantmod)
library(corrplot)


#msharpe=.85 #market sharpe, from creditedge was .55 until late '11, then climbing. Historically 0.6
#libor=.01 #1-year libor
tradingDaysPerYear = 251

temp<-get.hist.quote(instrument=tickers[1],date1,date2,quote="AdjClose", provider="yahoo", quiet=TRUE)


# TODO - add some robustness here. If symbol returns no data, remove row and proceed. 
# If symbol returns data of wrong length, interpolate.

#get quotes
quotes <- zoo(matrix(, length(temp),length(tickers)), time(temp))
for(i in 1:length(tickers)) quotes[,i]<-get.hist.quote(instrument=tickers[i],date1,date2,quote="AdjClose", provider="yahoo", quiet=TRUE)

iquotes <- zoo(matrix(, length(temp),length(itickers)), time(temp))
for(i in 1:length(itickers)) iquotes[,i]<-get.hist.quote(instrument=itickers[i],date1,date2,quote="AdjClose", provider="yahoo", quiet=TRUE)

#should compute ishares given iweights (use latest values)!
lasti=as.numeric(tail(iquotes,1))
ishares=iweights/lasti

# multiply by shares to values
values<-quotes
for (i in 1:length(quotes[1])) values[,i]=quotes[,i]*shares[i]

ivalues=iquotes
for (i in 1:length(iquotes[1])) ivalues[,i]=iquotes[,i]*ishares[i]


#portfolio values
value=values[,1]
for(i in 1:length(values[,1])) value[i]=sum(values[i])

ivalue=ivalues[,1]
for(i in 1:length(ivalues[,1])) ivalue[i]=sum(ivalues[i])


# compute return of portfolio
return=tail(value, length(value)-1)
for(i in 1:length(return)) return[i]=log(coredata(value[i+1])/coredata(value[i])) 

ireturn=tail(ivalue, length(ivalue)-1)
for(i in 1:length(ireturn)) ireturn[i]=log(coredata(ivalue[i+1])/coredata(ivalue[i])) 

#compute cumulative returns
cumreturn = return
for (i in 1:length(cumreturn)) cumreturn[i]=exp(sum(head(return,i)))-1

icumreturn=ireturn
for (i in 1:length(icumreturn)) icumreturn[i]=exp(sum(head(ireturn,i)))-1


#compute returns of each ticker
returns=tail(values, length(values[,1])-1)
for (j in 1:length(returns[1])) 
  for(i in 1:length(returns[,j])) returns[i,j]=log(coredata(values[i+1,j])/coredata(values[i,j])) 

#compute corr2port, stdevs, & risk contribution
corr2port=shares
for(i in 1:length(corr2port)) corr2port[i]=cor(returns[,i],return)
stdevs=corr2port
for(i in 1:length(stdevs)) stdevs[i]=sd(returns[,i]) * sqrt(tradingDaysPerYear)
rc = corr2port * stdevs

#compute betas
istdev=sd(ireturn)*sqrt(tradingDaysPerYear)
corr2market=corr2port
for(i in 1:length(corr2market)) corr2market[i]=cor(returns[,i],ireturn)
betas = stdevs * corr2market / istdev

#compute E[return]  & return/risk
ereturns=stdevs
for(i in 1:length(ereturns)) ereturns[i]=betas[i]*(istdev*msharpe)+libor #absolute return
retrisk=(ereturns-libor)/rc #sharpe is excess return


#compute ytd, risk weight, & exposure weight
ytd=rep(0,length(tickers))
for(i in 1:length(tickers)) ytd[i]=exp(sum(returns[,i]))-1
riskweights=coredata(tail(values,1)*rc/sum(tail(values,1)*rc))[1,]
expweights=coredata(tail(values,1)/sum(tail(values,1)))[1,]



#compute portfolio metrics
stdev=sd(return)*sqrt(tradingDaysPerYear)
beta=cor(return,ireturn) * stdev / istdev
ereturn=beta*(istdev*msharpe)+libor #absolute return
sharpe=(ereturn-libor)/stdev #excess return

alpha=coef(glm(return ~ ireturn))[[1]]*tradingDaysPerYear
r2=cor(ireturn, return)^2

isofar=as.numeric(tail(icumreturn,1))
sofar=as.numeric(tail(cumreturn,1))

#summary (quartiles etc. of daily returns, port & index)
rir=data.frame(return,ireturn)
#doesn't print from helper, moved to main
#summary(rir*100)

#produce table to print for each ticker
df<-data.frame(tickers,round(data.frame(ytd, stdevs, corr2port, betas, ereturns, rc, retrisk, expweights, riskweights),2))
#moved to main
#df[with(df, order(-retrisk)), ]


#start plotting

#risk contributions vs. ytd
plot(rc, ytd)
abline(v=0, h=0)
textxy(rc, ytd, tickers)

#E[return] vs. ytd
plot(ereturns, ytd)
textxy(ereturns, ytd, tickers)
abline(a=0, b=1)



#scatter of ireturn vs. return
plot(ireturn, return); abline(v=0, h=0)

# plot (rc, ereturns-libor); abline(a=0, b=((ereturn-libor)/stdev)) #excess return

#ugly mispricing
#plot (c(rc,stdev), c(ereturns,ereturn),xlab = "Non-Diversified Risk", ylab="E[return]",xlim = c(0,max(rc)+.02),ylim=c(0,max(ereturns)+.02) ); abline(a=libor, b=((ereturn-libor)/stdev)) #absolute return
#textxy(c(rc,stdev), c(ereturns,ereturn), c(tickers,"PORT"), cex=.8)


#10-day moving average volatility of index
i10DayAvgVol=tail(ireturn, length(ireturn)-10)
for(i in 1:length(i10DayAvgVol)) i10DayAvgVol[i]=mean(abs(ireturn[seq(i,i+10)]))
plot(i10DayAvgVol)
#plot(rollapply(ireturn,10,sd), ylab="10-day return.SD")
#lines(rollapply(return, 10, sd), col="green")

#backtest cumulative return vs. index
plot(cumreturn, col="green"); lines(icumreturn, col="blue"); abline(a=0, b=0, col="black")

#backtest delta cumreturn from index
plot(cumreturn-icumreturn)
abline(h=0)



#simulate years of portfolio returns sampling daily returns
numsims=1000
simyears=replicate(numsims,exp(sum(sample(coredata(return),tradingDaysPerYear,replace = TRUE))))
#show histogram of yearly performance of that simulation
hist(simyears)
#this doesn't print from helper file, moved to main (how many + vs - years)
#table(sign(simyears-1))/numsims

#produce histogram based on assuming normally distributed returns
simavg=mean(simyears); simsd=sd(simyears)
hist(rlnorm(numsims,ereturn, stdev))



#produce angle plot
x=rc * coredata(tail(values,1))
y=sin(acos(corr2port))*stdevs*coredata(tail(values,1))
theta=acos(corr2port)
awidth=stdevs*50

#colors.r<-colorRamp(c("red", "blue"))
#colors.g<-colors.r(ereturns)
#colors.gr<-rgb(colors.g, maxColorValue=1)
maxColorValue <- 100
palette <- colorRampPalette(c("purple","green"))(maxColorValue)
x0=rep(0,length(tickers)); axmax=max(c(x,y))
plot(NA, xlim=c(0,axmax), ylim=c(0,axmax),xlab="kept risk", ylab="diversified risk", col="red")
mapply("arrows", x0,x0,x,y, col = palette[cut(ereturns, maxColorValue)], lwd=awidth)
text(x,y,tickers, cex=0.7)
text(max(x)/2,axmax, "width = riskiness, color=return")


#correlation matrix, clustered and visualized
dfr=data.frame(returns)
colnames(dfr)<-tickers
corrplot.mixed(cor(dfr), upper="circle", lower="number", order="FPC")
corrplot(cor(dfr), method="square", order="FPC")


#not clear dividends are being adjusted for correctly.  Get raw price and add in dividends yourself.

divs = values-values
for(i in 1:length(tickers)) {tmpo<-shares[i]*as.zoo(getDividends(tickers[i],date1,date2));
if(sum(tmpo)>0){
  tmp=na.locf(cumsum(tmpo), na.rm=TRUE, xout=index(value));
  tmp[index(tmp)<index(tmpo[1])]=0;
  divs[,i]<-tmp}
}
div=zoo(rowSums(divs), time(divs))

rawQuotes=values-values
for(i in 1:length(tickers)) rawQuotes[,i]<-get.hist.quote(instrument=tickers[i],date1,date2,quote="Close", provider="yahoo", quiet=TRUE)

rawValues<-rawQuotes
for (i in 1:length(rawQuotes[1])) rawValues[,i]=rawQuotes[,i]*shares[i]
rawValue=zoo(rowSums(rawValues), time(rawValues))
HistValue = rawValue + div

#plot the backtested portfolio price using the raw + dividend method.
plot(HistValue, xlab ="", ylab="Portfolio Value")
#Doesn't take into account splits, let alone buy & sell, but does add div to port correctly

