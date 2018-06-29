# This code fit Garch and NGarch models with historical S&P returns
# 1000 historical S&P price is used here

library(fGarch)
# import S&P 500 historical price
SP500 = read.csv("/Users/Larry/Documents/R_Code/Garch/SP500.csv")
Index= SP500$AdjClose
# calculate log return of S&P 500
Return = log(Index[3:1002]/Index[2:1001])

# use garchFit to fit a model which can be used for comparison with our estimated parameters under MLE
mdl = garchFit(formula = ~ garch(1, 1), data = Return)
summary(mdl)

#estimating parameters of GARCH, four different methods as below.
#1: Four parameters to estimate
# this is sum of pdf, which will be used to optimize
sum_pdf <- function(theta)
  {
    var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]#sigma is long term variance
    var_fore=rep(0,1000)
    var_fore[1]=var1
    for (i in 2:1000)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  sum = 0
  for (i in 1:1000)
  {
    sum = sum - 0.5*(log(var_fore[i]*2*pi)+Return[i]^2/var_fore[i])
  }
  return (-sum)
  }
# initial value
initialvalue=c(9.614052e-05,0.5,0.2,9.614052e-05)
# optimization process
result1 = optim(initialvalue, fn=function(theta){sum_pdf(theta)})
# show result
sqrt(result1$par[1])
result1$par[2]
result1$par[3]
sqrt(result1$par[4])
result1

#2: Three parameters to estimate. Set long-run variance equalt to sample variance over the last 1000 days.
longtermvar=var(Return)
sum_pdf <- function(Return,theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=longtermvar#sigma is long term variance
  var_fore=rep(0,1000)
  var_fore[1]=var1
  for (i in 2:1000)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  sum = 0
  for (i in 1:1000)
  {sum = sum - 0.5*(log(var_fore[i]*2*pi)+Return[i]^2/var_fore[i])}
  return (-sum)
}
# initial value
initialvalue=c(9.614052e-05,0.5,0.2)
# optimization process
result2 = optim(initialvalue, fn=function(theta){sum_pdf(Return,theta)})
# show result
sqrt(result2$par[1])
result2$par[2]
result2$par[3]
sqrt(longtermvar)
result2

#3: Three parameters to estimate. Set initial variance equalt to sample variance over the last 1000 days.
initialvar=var(Return)
sum_pdf <- function(Return,theta)
{
  long_var=theta[1];alpha=theta[2];beta=theta[3];var1=initialvar
  var_fore=rep(0,1000)
  var_fore[1]=var1
  for (i in 2:1000)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  sum = 0
  for (i in 1:1000)
  {sum = sum - 0.5*(log(var_fore[i]*2*pi)+Return[i]^2/var_fore[i])}
  return (-sum)
}
# initial value
initialvalue=c(9.614052e-05,0.5,0.2)
# optimization process
result3 = optim(initialvalue, fn=function(theta){sum_pdf(Return,theta)})
# show result
sqrt(longtermvar)
result3$par[2]
result3$par[3]
sqrt(result3$par[1])
result3

#4: Two parameters to estimate. Set initial and long-run variance equalt to sample variance over the last 1000 days.
sum_pdf <- function(Return,theta)
{
  long_var=longtermvar;alpha=theta[1];beta=theta[2];var1=initialvar
  var_fore=rep(0,1000)
  var_fore[1]=var1
  for (i in 2:1000)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  sum = 0
  for (i in 1:1000)
  {sum = sum - 0.5*(log(var_fore[i]*2*pi)+Return[i]^2/var_fore[i])}
  return (-sum)
}
# initial value
initialvalue=c(0.5,0.2)
# optimization process
result4 = optim(initialvalue, fn=function(theta){sum_pdf(Return,theta)})
# show result
sqrt(longtermvar)
result4$par[1]
result4$par[2]
sqrt(longtermvar)
result4

#5: forecase the return variance and standared deviation for Friday, August 15, 2014
#a: forecast under model 1
estimation1=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]
  var_fore=rep(0,1001)
  var_fore[1]=var1
  for (i in 2:1001)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  return(var_fore[1001]^0.5)
}
estimationA=estimation1(c(result1$par[1],result1$par[2],result1$par[3],result1$par[4]))
estimationA

#b: forecast under model 2
estimation2=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=longtermvar
  var_fore=rep(0,1001)
  var_fore[1]=var1
  for (i in 2:1001)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  return(var_fore[1001]^0.5)
}
estimationB=estimation2(c(result2$par[1],result2$par[2],result2$par[3]))
estimationB

#c: forecast under model 3
estimation3=function(theta)
{
  var1=longtermvar;alpha=theta[2];beta=theta[3];long_var=theta[1]
  var_fore=rep(0,1001)
  var_fore[1]=var1
  for (i in 2:1001)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  return(var_fore[1001]^0.5)
}
estimationC=estimation3(c(result3$par[1],result3$par[2],result3$par[3]))
estimationC

#d: forecast under model 4
estimation4=function(theta)
{
  var1=longtermvar;alpha=theta[1];beta=theta[2];long_var=longtermvar#sigma is long term variance
  var_fore=rep(0,1001)
  var_fore[1]=var1
  for (i in 2:1001)
  {
    var_fore[i] = long_var*(1-alpha-beta)+alpha*Return[i-1]^2+beta*var_fore[i-1]
  }
  return(var_fore[1001]^0.5)
}
estimationD=estimation4(c(result4$par[1],result4$par[2]))
estimationD

#2: forecase of the realized variance from Aug. 15, 2014 to Sept. 19, 2014 (24 days)
#a: Model 1
estimation2_1=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]
  var_fore=rep(0,24)
  var_fore[1] = long_var*(1-alpha-beta)+(alpha+beta)*var1
  for (i in 2:24)
  {
    var_fore[i] = long_var*(1-alpha-beta)+(alpha+beta)*var_fore[i-1]
  }
  sum=sum(var_fore)
  return(sum)
}
estimation2_A=estimation2_1(c(estimationA^2,result1$par[2],result1$par[3],result1$par[4]))
estimation2_A
sqrt(estimation2_A*252/24)

#b: Model 2
estimation2_2=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]#sigma is long term variance
  var_fore=rep(0,24)
  var_fore[1] = long_var*(1-alpha-beta)+(alpha+beta)*var1
  for (i in 2:24)
  {
    var_fore[i] = long_var*(1-alpha-beta)+(alpha+beta)*var_fore[i-1]
  }
  sum=sum(var_fore)
  return(sum)
}
estimation2_B=estimation2_2(c(estimationB^2,result2$par[2],result2$par[3],longtermvar))
estimation2_B
sqrt(estimation2_B*252/24)

#c: Model 3
estimation2_3=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]#sigma is long term variance
  var_fore=rep(0,24)
  var_fore[1] = long_var*(1-alpha-beta)+(alpha+beta)*var1
  for (i in 2:24)
  {
    var_fore[i] = long_var*(1-alpha-beta)+(alpha+beta)*var_fore[i-1]
  }
  sum=sum(var_fore)
  return(sum)
}
estimation2_C=estimation2_3(c(estimationC^2,result3$par[2],result3$par[3],result3$par[1]))
estimation2_C
sqrt(estimation2_C*252/24)

#d: Model 4
estimation2_4=function(theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4]#sigma is long term variance
  var_fore=rep(0,24)
  var_fore[1] = long_var*(1-alpha-beta)+(alpha+beta)*var1
  for (i in 2:24)
  {
    var_fore[i] = long_var*(1-alpha-beta)+(alpha+beta)*var_fore[i-1]
  }
  sum=sum(var_fore)
  return(sum)
}
estimation2_D=estimation2_4(c(estimationD^2,result4$par[1],result4$par[2],longtermvar))
estimation2_D
sqrt(estimation2_D*252/24)

#3a: fit the NGarch model. There are one more variable Theta to estimate
sum_pdf3a <- function(Return,theta)
{
  var1=theta[1];alpha=theta[2];beta=theta[3];long_var=theta[4];sita=theta[5]
  var_fore=rep(0,1000)
  var_fore[1] = var1
  for (i in 2:1000)
  {
    var_fore[i] = long_var*(1-alpha*(1+sita*sita)-beta)+alpha*(Return[i-1]-sita*sqrt(var_fore[i-1]))^2+beta*(var_fore[i-1])
  }
  sum = 0
  for (t in 1:1000)
  {sum = sum - 0.5*(log(var_fore[t]*2*pi)+Return[t]^2/var_fore[t])}
  return (-sum)
}
#initial value matters!
c(result1$par[1],result1$par[2],result1$par[3],result1$par[4],0)
initialvalue3=c(9.614052e-05,0.09,0.8,9.614052e-05,0)
result3_1 = optim(initialvalue3, fn=function(theta){sum_pdf3a(Return,theta)})
sqrt(result3_1$par[1])
result3_1$par[2]
result3_1$par[3]
sqrt(result3_1$par[4])
result3_1$par[5]
result3_1

#3b: select model based on log likelihook calculation
log_value=2*(result1$value[1]-result3_1$value[1])
qchisq(.99, df=1)
#Garch(2,2)
mdl2 = garchFit(formula = ~ garch(2, 2), data = Return)
summary(mdl2)
sum_pdf_garch22 <- function(Return,theta)
{
  if (sum(theta < 0) != 0 || theta[3] + theta[4]+theta[5] + theta[6] > 1) {
    return(0)
  } 
  var1=theta[1];var2=theta[2];alpha1=theta[3];alpha2=theta[4];beta1=theta[5];beta2=theta[6];long_var=theta[7]#sigma is long term variance
  var_fore=rep(0,1000)
  var_fore[1]=var1
  var_fore[2]=var2
  for (i in 3:1000)
  {
    var_fore[i] = long_var*(1-alpha1-alpha2-beta1-beta2)+alpha1*Return[i-1]^2+alpha2*Return[i-2]^2
    +beta1*var_fore[i-1]+beta2*var_fore[i-2]
  }
  sum = 0
  for (i in 1:1000)
  {sum = sum - 0.5*(log(var_fore[i]*2*pi)+Return[i]^2/var_fore[i])}
  return (-sum)
}
initialvalue=c(9.614052e-05,9.614052e-05,0.06,0.15,0.000005,0.6,0.000001)
result_garch22 = optim(initialvalue, fn=function(theta){sum_pdf_garch22(Return,theta)})
result_garch22


