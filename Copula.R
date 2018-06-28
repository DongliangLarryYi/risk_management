#1.1 Independent situation
set.seed(1)
Trials = 200000
Total_Loss = rep(0,Trials)
Total_Loss_lambda = rep(0,Trials)
for (i in 1:Trials){
# loan: 10 Millions each
RV1 = rnorm(80)
U1 = pnorm(RV1)
loss1 = 10 * length(U1[U1<0.02])

# loan: 20 Millions each
RV2 = rnorm(20)
U2 = pnorm(RV2)
loss2 = 20 * length(U2[U2<0.02])
Total_Loss[i] = (loss1+loss2)/(10*80+20*20)
#standard exponential
RV1 = rnorm(80)
U1 = pnorm(RV1)
tao1 = -log(1-U1)/0.02;
loss1 = 10 * length(tao1[tao1<1])

RV2 = rnorm(20)
U2 = pnorm(RV2)
tao2 = -log(1-U2)/0.02;
loss2 = 20 * length(tao2[tao2<1])
Total_Loss_lambda[i] = (loss1+loss2)/(10*80+20*20)
###############
}
h = hist(Total_Loss,breaks = 40, plot= FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, col = "gray")
h_lambda = hist(Total_Loss_lambda,breaks = 40, plot= FALSE)
h_lambda$counts=h_lambda$counts/sum(h_lambda$counts)
plot(h_lambda, col = "gray")

mean(Total_Loss)
mean(Total_Loss_lambda)

#1.2
set.seed(1)
Total_Loss2 = rep(0,Trials)
for (i in 1:Trials){
  m = rnorm(1)
  rho = 0.3
  RV = sqrt(rho)*m + sqrt(1-rho)*rnorm(100)
  RV
  U = pnorm(RV)
  U1 = U[1:80]
  U2 = U[81:100]
  
  loss1 = 10 * length(U1[U1<0.02])
  loss2 = 20 * length(U2[U2<0.02])
  Total_Loss2[i] = (loss1+loss2)/(10*80+20*20)
}
h2 = hist(Total_Loss2,breaks = 200, plot= FALSE)
h2$counts=h2$counts/sum(h2$counts)
plot(h2, col = "gray",xlim = c(0,0.1))

#1.3
set.seed(1)
Trials3=500000
sequence_A = rep(0,Trials3)
sequence_B = rep(0,Trials3)
rho = 0.3
for (i in 1:Trials3){
m = rnorm(1)
RV = sqrt(rho)*m + sqrt(1-rho)*rnorm(2)
RV = pnorm(RV)
if (RV[1] > 0.98)
  RV[1] = 1
if (RV[1] <= 0.98)
  RV[1] = 0

if (RV[2] > 0.98)
  RV[2] = 1
if (RV[2] <= 0.98)
  RV[2] = 0

sequence_A[i] = RV[1]
sequence_B[i] = RV[2]
}
#mean(sequence_B)
cov(sequence_B,sequence_A)/sqrt(var(sequence_B)*var(sequence_A))

#1.4
set.seed(1)
Trials3=200000
sequence_A = rep(0,Trials3)
sequence_B = rep(0,Trials3)
cal_cor = rep(0,10)
for (k in 1:10){
rho = 0.20+ k*0.01
for (i in 1:Trials3){
  m = rnorm(1)
  RV = sqrt(rho)*m + sqrt(1-rho)*rnorm(2)
  RV = pnorm(RV)
  if (RV[1] > 0.98)
    RV[1] = 1
  if (RV[1] <= 0.98)
    RV[1] = 0
  if (RV[2] > 0.98)
    RV[2] = 1
  if (RV[2] <= 0.98)
    RV[2] = 0
  
  sequence_A[i] = RV[1]
  sequence_B[i] = RV[2]
}
#mean(sequence_B)
cal_cor[k] = cov(sequence_B,sequence_A)/sqrt(var(sequence_B)*var(sequence_A))
}

#2.1
EL1 = mean(Total_Loss)*(80*10+20*20)
Q999_1 = (unname(quantile(Total_Loss,0.999,na.rm=T)))*(80*10+20*20)
EC1 = Q999_1 - EL1
EC1

#2.2
EL2 = mean(Total_Loss2)*(80*10+20*20)
Q999_2 = (unname(quantile(Total_Loss2,0.999,na.rm=T)))*(80*10+20*20)
EC2 = Q999_2 - EL2
EC2

#3.1
set.seed(1)
Total_Loss3 = rep(0,Trials)
for (i in 1:Trials){
  # loan: 10 Millions each
  RV1 = rnorm(80)
  U1 = pnorm(RV1)
  loss1 = 10 * length(U1[U1<0.02])
  # loan: 20 Millions each
  RV2 = rnorm(20)
  U2 = pnorm(RV2)
  
  if(length(U2[U2<0.02]) >= 4)
    loss2 = 20* (length(U2[U2<0.02])-2)
  if(length(U2[U2<0.02]) < 4 & length(U2[U2<0.02]) > 0)
    loss2 = 20 * (length(U2[U2<0.02]))/2
  if(length(U2[U2<0.02]) == 0)
    loss2 = 0
  Total_Loss3[i] = (loss1+loss2)/(10*80+20*20)
}
h3 = hist(Total_Loss3,breaks = 40, plot= FALSE)
h3$counts=h3$counts/sum(h3$counts)
plot(h3, col = "gray")

#3.2
EL3 = mean(Total_Loss3)*(80*10+20*20)
Q999_3 = (unname(quantile(Total_Loss3,0.999,na.rm=T)))*(80*10+20*20)
EC3 = Q999_3 - EL3
EL3
EC3

#3.3
set.seed(1)
Total_Loss4 = rep(0,Trials)
for (i in 1:Trials){
  m = rnorm(1)
  rho = 0.3
  RV = sqrt(rho)*m + sqrt(1-rho)*rnorm(100)
  RV
  U = pnorm(RV)
  U1 = U[1:80]
  U2 = U[81:100]
  loss1 = 10 * length(U1[U1<0.02])  
  if(length(U2[U2<0.02]) >= 4)
    loss2 = 20* (length(U2[U2<0.02])-2)
  if(length(U2[U2<0.02]) < 4)
    loss2 = 20 * (length(U2[U2<0.02]))/2
  Total_Loss4[i] = (loss1+loss2)/(10*80+20*20)
}
h4 = hist(Total_Loss4,breaks = 200, plot= FALSE)
h4$counts=h4$counts/sum(h4$counts)
plot(h4, col = "gray",xlim = c(0,0.1))
EL4 = mean(Total_Loss4)*(80*10+20*20)
Q999_4 = (unname(quantile(Total_Loss4,0.999,na.rm=T)))*(80*10+20*20)
EC4 = Q999_4 - EL4
EL4
EC4

#4.1
set.seed(1)
Total_Loss5 = rep(0,Trials)
for (i in 1:Trials){
  # loan: 10 Millions each
  RV1 = rnorm(80)
  U1 = pnorm(RV1)
  beta1 = rbeta(length(U1[U1<0.02]),2,2)
  loss_beta1 = 1-beta1
#  loss_ration = sum(loss_beta)/length(U1[U1<0.02])
  loss1 = 10 * sum(loss_beta1)
  
  # loan: 20 Millions each
  RV2 = rnorm(20)
  U2 = pnorm(RV2)
  if (length(U2[U2<0.02]) >0){
  beta2=rbeta(length(U2[U2<0.02]),2,2)
  loss_beta2 = 1-beta2}
  if (length(U2[U2<0.02]) ==0)
    loss_beta2 = 0
  if(sum(loss_beta2) >= 4)
    loss2 = 20* (sum(loss_beta2)-2)
  if(sum(loss_beta2) < 4)
    loss2 = 20 * (sum(loss_beta2))/2
  Total_Loss5[i] = (loss1+loss2)/(10*80+20*20)
}
h5 = hist(Total_Loss5,breaks = 40, plot= FALSE)
h5$counts=h5$counts/sum(h5$counts)
plot(h5, col = "gray")

#4.2
set.seed(1)
Total_Loss6 = rep(0,Trials)
for (i in 1:Trials){
  m = rnorm(1)
  rho = 0.3
  RV = sqrt(rho)*m + sqrt(1-rho)*rnorm(100)
  RV
  U = pnorm(RV)
  U1 = U[1:80]
  U2 = U[81:100]
  
  beta1 = rbeta(length(U1[U1<0.02]),2,2)
  loss_beta1 = 1-beta1
  loss1 = 10 * sum(loss_beta1)
  
  if (length(U2[U2<0.02]) >0){
    beta2=rbeta(length(U2[U2<0.02]),2,2)
    loss_beta2 = 1-beta2}
  if (length(U2[U2<0.02]) ==0)
    loss_beta2 = 0
  if(sum(loss_beta2) >= 4)
    loss2 = 20* (sum(loss_beta2)-2)
  if(sum(loss_beta2) < 4)
    loss2 = 20 * (sum(loss_beta2))/2
  
  Total_Loss6[i] = (loss1+loss2)/(10*80+20*20)
}
h6 = hist(Total_Loss6,breaks = 120, plot= FALSE)
h6$counts=h6$counts/sum(h6$counts)
plot(h6, col = "gray",xlim = c(0,0.1))
