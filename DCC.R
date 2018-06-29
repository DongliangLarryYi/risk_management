# Use NGARCH(1,1) and DCC(1,1) models and Monte Carlo simulation to estimate the value of the securities.

# Assignment. It is toward the end of the trading day on Tuesday, August 5, 2014. For
# simplicity, assume that it is just after the close of trading, so that you know prices and interest
# rates from the close of trading on August 5, 2014. Morgan Stanley, a major U.S. investment
# bank, is selling an issue of Fixed Income Buffered Securities due August 10, 2015, with
# Payments on the Securities Based on the Worst Performing of the iShares® Russell 2000® ETF
# and the iShares® MSCI EAFE ETF. The buffered securities are debt instruments issued by
# Morgan Stanley, but have cash flows based on the performance of the two ETFs. Your job is to
# estimate the fair market value of the securities as of the trade date August 5, 2014.
# http://www.sec.gov/Archives/edgar/data/895421/000095010314005563/dp48559_424b2-1554.htm

Data = read.csv("/Users/Larry/Documents/R_Code/DCC/HW8.csv")
# two underlying assets' historical return
Return_IWM = log(Data$IWM.P[2:nrow(Data)] / Data$IWM.P[1:(nrow(Data)-1)])
Return_EFA = log(Data$EFA.P[2:nrow(Data)] / Data$EFA.P[1:(nrow(Data)-1)])
library(foreach)
t = length(Return_IWM)

# Build log likelihood function
Loglikelihood <- function(Return, Theta)
{
  if ( sum(Theta[1:4] <0) != 0 || Theta[1] * (1+Theta[5]^2) + Theta[2] > 1)
  {
    return (10000)
  }
  t = length(Return)
  Var = rep(0,t)
  Var[1] = Theta[4]^2
  for(i in 2:t)
  {
    Var[i] = (1- Theta[1] * (1+ Theta[5]^2) - Theta[2]) * Theta[3]^2 + Theta[1] * (Return[i-1] - Theta[5] * sqrt(Var[i-1]))^2 + Theta[2] * Var[i-1]
  }
  LN = 0.5* sum(log(Var)+Return^2/Var)
  return(LN)
}

iIWM = c(0.1,0.88,sd(Return_IWM),sd(Return_IWM),0)
result_IWM = optim(iIWM, Loglikelihood, Return=Return_IWM, method = "BFGS")
result_IWM
iEFA = c(0.08, 0.91, sd(Return_EFA), sd(Return_EFA),0)
result_EFA = optim(iEFA, Loglikelihood, Return = Return_EFA, method = "BFGS")
result_EFA
Par_IWM = result_IWM$par
Par_EFA = result_EFA$par

# build DCC function
DCCLN <- function(Return1, Return2, Theta){
  if (Theta[1]<=0 || Theta[2]<=0 || Theta[1]+Theta[2]>1)
  {return(10000)}
  t = length(Return1)
  rho12_hist = mean(Return1*Return2)
  Q11=rep(0,t)
  Q12=rep(0,t)
  Q22=rep(0,t)
  Q11[1]=1
  Q12[1]=rho12_hist
  Q22[1]=1
  
  for (i in 2:t)
  {
    Q11[i] = 1 + Theta[1] * (Return1[i-1]^2 - 1) + Theta[2] * (Q11[i-1] - 1)
    Q12[i] = rho12_hist + Theta[1] * (Return1[i-1] * Return2[i-1] - rho12_hist) + Theta[2] * (Q12[i-1] - rho12_hist)
    Q22[i] = 1 + Theta[1] * (Return2[i-1]^2-1)+ Theta[2] * (Q22[i-1]-1)
  }
  rho12 = Q12/sqrt(Q11*Q22)
  temp = 1 - rho12^2
  LN = 0.5* sum(log(temp)+(Return1^2 + Return2^2 - 2 * rho12 * Return1*Return2) / temp)
  return(LN)
}

#Standadizing
z_IWM = rep(0,t)
z_EFA = rep(0,t)
Vol_IWM = rep(0,t)
Vol_EFA = rep(0,t)
Vol_IWM[1] = Par_IWM[4]
Vol_EFA[1] = Par_EFA[4]
for (i in 2:t){
  Vol_IWM[i] = sqrt((1 - Par_IWM[1] *(1+ Par_IWM[5]^2)-Par_IWM[2]) * Par_IWM[3]^2 + Par_IWM[1] * (Return_IWM[i-1] - Par_IWM[5] * Vol_IWM[i-1])^2 + Par_IWM[2] * Vol_IWM[i-1]^2)
  Vol_EFA[i] = sqrt((1 - Par_EFA[1] *(1+ Par_EFA[5]^2)-Par_EFA[2]) * Par_EFA[3]^2 + Par_EFA[1] * (Return_EFA[i-1] - Par_EFA[5] * Vol_EFA[i-1])^2 + Par_EFA[2] * Vol_EFA[i-1]^2)
}

z_IWM = Return_IWM/Vol_IWM
z_EFA = Return_EFA/Vol_EFA
length(Vol_IWM)
#DCC MLE
iDCC = c(0.05, 0.9)
result_DCC = optim(iDCC, DCCLN, Return1= z_IWM, Return2 = z_EFA, method = "BFGS")
Par_DCC = result_DCC$par
result_DCC
#DCC of Rho
hist_rho = mean(z_IWM*z_EFA)
q11=1
q12=hist_rho
q22=1
for (i in 2:t){
  q11 = 1 + Par_DCC[1] * (z_IWM[i-1]^2-1) + Par_DCC[2]*(q11-1)
  q12 = hist_rho + Par_DCC[1]*(z_IWM[i-1]*z_EFA[i-1]-hist_rho) + Par_DCC[2] * (q12 - hist_rho)
  q22 = 1 + Par_DCC[1] * (z_EFA[i-1]^2-1) + Par_EFA[2]*(q22-1)
}
rho12 = q12/sqrt(q11*q22)

#Monte Carlo
#Working days number
WD = 256
# Price of Aug. 5 2014
IWM_ini = Data$IWM.P[t+1]
EFA_ini = Data$EFA.P[t+1]
MCs = foreach(i = 1:5000) %do%
{
  D = diag(c(Vol_IWM[t],Vol_EFA[t]))
  Upsilon = matrix(c(1,rho12,rho12,1),2,2)
  QQ11 = q11
  QQ12 = q12
  QQ22 = q22
  acumu_IWM = rep(0,WD)
  acumu_EFA = rep(0,WD)
  acumu_IWM[1]=Return_IWM[t]
  acumu_EFA[1]=Return_EFA[t]
  for (i in 2:WD)
  {
    rho = Upsilon[1,2]
    cholesky_Upsilon = matrix(c(1,rho, 0, sqrt(1-rho^2)),2,2)
    
    r1 = acumu_IWM[i-1]
    r2 = acumu_EFA[i-1]
    v1 = D[1,1]
    v2 = D[2,2]
    z1 = r1/v1
    z2 = r2/v2
    
    # two models prediction
    v1 = sqrt((1-Par_IWM[1] * (1 + Par_IWM[5]^2) - Par_IWM[2]) * Par_IWM[3]^2 + Par_IWM[1] * (r1 - Par_IWM[5] * v1)^2 + Par_IWM[2] * v1^2)
    v2 = sqrt((1-Par_EFA[1] * (1 + Par_EFA[5]^2) - Par_EFA[2]) * Par_EFA[3]^2 + Par_EFA[1] * (r2 - Par_EFA[5] * v2)^2 + Par_EFA[2] * v2^2)
    
    QQ11 = 1 + Par_DCC[1] * (z1^2 -1) + Par_DCC[2] *(QQ11 -1)
    QQ12 = hist_rho + Par_DCC[1]*(z1*z2 - hist_rho) + Par_DCC[2]*(QQ12 - hist_rho)
    QQ22 = 1 + Par_DCC[1] * (z2^2 -1) + Par_DCC[2] *(QQ22 -1)    
    rho = QQ12/sqrt(QQ11*QQ22)
    
    D = diag(c(v1,v2))
    Upsilon = matrix(c(1,rho,rho,1),2,2)
    r= D %*% cholesky_Upsilon %*% rnorm(2)
    acumu_IWM[i] = r[1]
    acumu_EFA[i] = r[2]
  }
    Estimate_IWM = IWM_ini * exp(sum(acumu_IWM))
    Estimate_EFA = EFA_ini * exp(sum(acumu_EFA))
    
  #Buffers
    Butter_IWM = (IWM_ini-Estimate_IWM)/IWM_ini
    Butter_EFA = (EFA_ini-Estimate_EFA)/EFA_ini
    Worst = max(Butter_EFA,Butter_IWM)
    if (Worst <= 0.15)
    {
      P=1000
    }
    else
    {
      P = 1000 - 1000* (Worst -0.15) * 1.1765
    }
  return (P)
}

#Pricing, discount of each coupon payment, 12 month libor : 0.0056
PV_coupon = 0
MCs = unlist(MCs)
for (i in 1:12){
  PV_coupon = PV_coupon + 4.1833/(1+0.0056*(3+i*30)/360)
  }
Value_Present = PV_coupon + mean(MCs)/(1+0.0056*370/360)
Value_Present

