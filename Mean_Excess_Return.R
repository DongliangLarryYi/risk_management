SP500 = read.csv("/Users/Larry/Documents/UIUC Schedule/FIN 580/HW/HW4.data.csv")

SP500$loss = -SP500$Return

#test = c(1,2, NA, 3,4,NA,5,6)
#test1 = test[!is.na(test)] 


loss <- SP500$loss[!is.na(SP500$loss)] # make sure "NA" is not output to loss

#test2 =(loss)[loss>=0]
#test3 = (loss)[loss>=0.05]

# 1
steps = (0.05-0.01)/0.002+1
i = 0
meanexcess = matrix(nrow = steps, ncol = 2)
for (u in seq(0.01, 0.05, by = 0.002))
{
  i = i+1
  meanexcess[i,1] = u
  meanexcess[i,2] = mean((loss-u)[loss>=u])  #特别赞的用法
}

matplot(meanexcess[,1], meanexcess[,2], type = "l", 
        xlab = "Threshold (u)", ylab = "Mean Excess Loss (e(u))", col = "black")
#plot(meanexcess[,1], meanexcess[,2], type = "l", xlab = "Threshold (u)", ylab = "Mean Excess Loss (e(u))")


#calculate number with loss higher than 0.022
excess = (loss-0.022)[loss>=0.022]
length(excess)

# MLE for GPD
# GPD fitting, theta[1]=kesai, theta[2]=beta
initialvalue = c(0.1, 0.05)
gpd = function(excess,theta){(1/theta[2])*((1+theta[1]*excess/theta[2])^(-(1+1/theta[1])))}
result = optim(initialvalue, fn=function(theta){-sum(log(gpd(excess,theta)))}, 
               method="BFGS")
kesai = result$par[1]
beta = result$par[2]

#conditional desity
pdf_gpd = function(x,theta){(1/theta[2])*((1+theta[1]*x/theta[2])^(-(1+1/theta[1])))}
threshold_new = seq(from = 0.022, to = 0.1, by = 0.002) - 0.022
density = rep(0,length(threshold_new))
density = pdf_gpd(threshold_new,result$par)
threshold_new = threshold_new + 0.022
plot(threshold_new,density, type = "l")

#probability (need to check later)
prob22=length(excess)/length(loss)


condprob <- function(x){(1+kesai*x/beta)^(-1/kesai)}
  
series = seq(0.022 , 0.10, by = 0.001)

valueofprob <- vector("numeric", length=79)

for (j in 1: 79){
valueofprob[j] = prob22*condprob(series[j]-0.022)
}
length(series)
length(valueofprob)
plot(series,valueofprob, type ="l")

#var
VaR_EVT = 0.022+(beta/kesai)*((0.01/prob22)^(-kesai)-1)
VaR_EVT
prob22
