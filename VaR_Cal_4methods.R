data =read.csv("/Users/Larry/Google Drive/Risk Management/HW2/580HW2.csv",header=T)

#thousand.000
VaR <- vector("numeric", length=246)

#totally 2000 + 246 days, we need to derive return distribution from the first 2000 days
for (i in 1:246)
{
PLGBP = (1000/(data$GBPUSD[i:(i+1999)]*data$UKX[i:(i+1999)]))*data$GBPUSD[(i+1):(i+2000)]*data$UKX[(i+1):(i+2000)]-1000
PLEUR = (1000/(data$EURUSD[i:(i+1999)]*data$DAX[i:(i+1999)]))*data$EURUSD[(i+1):(i+2000)]*data$DAX[(i+1):(i+2000)]-1000
PLUSD = (1000/data$SPX[i:(i+1999)])*data$SPX[(i+1):(i+2000)]-1000
Total = PLGBP + PLEUR + PLUSD
VaR[i]<-abs(unname(quantile(Total,0.01,na.rm=T)))
}
VaR

#VaR stores the historical VaR
VaRdelta <- vector("numeric", length=246)
for (i in 1:246)
{
  RFTGBP = (1/(data$GBPUSD[i:(i+1999)]*data$UKX[i:(i+1999)]))*data$GBPUSD[(i+1):(i+2000)]*data$UKX[(i+1):(i+2000)]-1
  RDAXEUR = (1/(data$EURUSD[i:(i+1999)]*data$DAX[i:(i+1999)]))*data$EURUSD[(i+1):(i+2000)]*data$DAX[(i+1):(i+2000)]-1
  RSP500 = (1/data$SPX[i:(i+1999)])*data$SPX[(i+1):(i+2000)]-1
  stdRSP500 = sqrt(var(RSP500))
  stdRDAXEUR = sqrt(var(RDAXEUR))
  stdRFTGBP = sqrt(var(RFTGBP))
  covSPFT = cov(RSP500,RFTGBP)
  covSPDA = cov(RSP500,RDAXEUR)
  covFTDA = cov(RFTGBP,RDAXEUR)
  stdPortfolio=sqrt(var(RSP500)+var(RDAXEUR)+var(RFTGBP)+2*cov(RSP500,RFTGBP)+2*cov(RSP500,RDAXEUR)+2*cov(RFTGBP,RDAXEUR))
  VaRdelta[i]=2.33*stdPortfolio*1000
}
VaRdelta
VaRdeltabook <- vector("numeric", length=246)

for (i in 1:246)
{
  RFTGBP = (1/(data$GBPUSD[i:(i+1999)]*data$UKX[i:(i+1999)]))*data$GBPUSD[(i+1):(i+2000)]*data$UKX[(i+1):(i+2000)]-1
  RDAXEUR = (1/(data$EURUSD[i:(i+1999)]*data$DAX[i:(i+1999)]))*data$EURUSD[(i+1):(i+2000)]*data$DAX[(i+1):(i+2000)]-1
  RSP500 = (1/data$SPX[i:(i+1999)])*data$SPX[(i+1):(i+2000)]-1
  stdRSP500 = sqrt((sum(RSP500^2))/2000)
  stdRDAXEUR = sqrt((sum(RDAXEUR^2))/2000)
  stdRFTGBP = sqrt((sum(RFTGBP^2))/2000)
  covSPFT = (sum(RSP500*RFTGBP))/2000
  covSPDA = (sum(RSP500*RDAXEUR))/2000
  covFTDA = (sum(RFTGBP*RDAXEUR))/2000
  stdPortfolio=sqrt((sum(RSP500^2))/2000+(sum(RDAXEUR^2))/2000+(sum(RFTGBP^2))/2000+2*covSPFT+2*covSPDA+2*covFTDA)
  VaRdeltabook[i]=2.33*stdPortfolio*1000
}
VaRdeltabook
VaRdeltaexpo <- vector("numeric", length=246)
weight <- vector("numeric", length=100)
weight[100]=0.06
for (i in 1:99)
{
  weight[i]=(0.94^(100-i))*0.06
}
 
for (i in 1:246)
{
  RFTGBP = (1/(data$GBPUSD[(i+1900):(i+1999)]*data$UKX[(i+1900):(i+1999)]))*data$GBPUSD[(i+1901):(i+2000)]*data$UKX[(i+1901):(i+2000)]-1
  RDAXEUR = (1/(data$EURUSD[(i+1900):(i+1999)]*data$DAX[(i+1900):(i+1999)]))*data$EURUSD[(i+1901):(i+2000)]*data$DAX[(i+1901):(i+2000)]-1
  RSP500 = (1/data$SPX[(i+1900):(i+1999)])*data$SPX[(i+1901):(i+2000)]-1
  stdRSP500 = sqrt(sum((RSP500^2)*weight))
  stdRDAXEUR = sqrt(sum((RDAXEUR^2)*weight))
  stdRFTGBP = sqrt(sum((RFTGBP^2)*weight))
  covSPFT = sum((RSP500*RFTGBP)*weight)
  covSPDA = sum((RSP500*RDAXEUR)*weight)
  covFTDA = sum((RDAXEUR*RFTGBP)*weight)
  stdPortfolio=sqrt(sum((RSP500^2)*weight)+sum((RDAXEUR^2)*weight)+sum((RFTGBP^2)*weight)+2*sum((RSP500*RFTGBP)*weight)+2*sum((RSP500*RDAXEUR)*weight)+2*sum((RDAXEUR*RFTGBP)*weight))
  VaRdeltaexpo[i]=2.33*stdPortfolio*1000
}
VaRdeltaexpo

data2 =read.csv("/Users/Larry/Desktop/580HW2(100lag).csv",header=T)
VaRexpohist <- vector("numeric", length=246)
sigmaFT <- vector("numeric", length=2246)
sigmaDAX<- vector("numeric", length=2246)
sigmaSP <- vector("numeric", length=2246)

for (i in 1:2247)
{
  RFTGBP = (1/(data2$GBPUSD[i:(i+99)]*data2$UKX[(i):(i+99)]))*data2$GBPUSD[(i+1):(i+100)]*data2$UKX[(i+1):(i+100)]-1
  RDAXEUR = (1/(data2$EURUSD[(i):(i+99)]*data2$DAX[(i):(i+99)]))*data2$EURUSD[(i+1):(i+100)]*data2$DAX[(i+1):(i+100)]-1
  RSP500 = (1/data2$SPX[(i):(i+99)])*data2$SPX[(i+1):(i+100)]-1
  sigmaSP[i] = sqrt(sum((RSP500^2)*weight))
  sigmaDAX[i] = sqrt(sum((RDAXEUR^2)*weight))
  sigmaFT[i] = sqrt(sum((RFTGBP^2)*weight))
}

for (i in 1:246)
{
  PLGBP = ((1000/(data$GBPUSD[i:(i+1999)]*data$UKX[i:(i+1999)]))*data$GBPUSD[(i+1):(i+2000)]*data$UKX[(i+1):(i+2000)]-1000)*sigmaFT[i+2000]/sigmaFT[(i+1):(i+1999)]
  PLEUR = ((1000/(data$EURUSD[i:(i+1999)]*data$DAX[i:(i+1999)]))*data$EURUSD[(i+1):(i+2000)]*data$DAX[(i+1):(i+2000)]-1000)*sigmaDAX[i+2000]/sigmaDAX[(i+1):(i+1999)]
  PLUSD = ((1000/data$SPX[i:(i+1999)])*data$SPX[(i+1):(i+2000)]-1000)*sigmaSP[i+2000]/sigmaSP[(i+1):(i+1999)]
  Total = PLGBP + PLEUR + PLUSD
  VaRexpohist[i]<-abs(unname(quantile(Total,0.01,na.rm=T)))
}
VaRexpohist
Date=as.Date(data$Date,"%m/%d/%Y")
plot(Date[2002:2247],VaR,type="l")
plot(Date[2002:2247],VaRdeltabook,type="l",add=T)
plot(Date[2002:2247],VaRdeltaexpo, type = "l",lty=1,pch=19, col = "red", main="VaR", xlab="time",ylab = "Loss($1000)")
lines(Date[2002:2247],VaRdeltabook, type = "l",lty=1,pch=19, col = "blue")
lines(Date[2002:2247],VaR, type = "l",lty=1,pch=19, col = "black")
lines(Date[2002:2247],VaRexpohist, type = "l",lty=1,pch=19, col = "yellow")
