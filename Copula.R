# Using the Gaussian Copula Model of Correlated Default Times

#1 Using the Gaussian copula model to simulate losses due to default
# A bank loan portfolio consisting of 80 loans each with a loan amount of $10 million and
# 20 large loans each with an amount of $20 million.
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
}
h = hist(Total_Loss,breaks = 40, plot= FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, col = "gray")
h_lambda = hist(Total_Loss_lambda,breaks = 40, plot= FALSE)
h_lambda$counts=h_lambda$counts/sum(h_lambda$counts)
plot(h_lambda, col = "gray")

mean(Total_Loss)
mean(Total_Loss_lambda)

#1.2 assume that the copula correlation between each pair of loans is 0.30 or 30%.
# Using this copula correlation, estimate the distribution of possible losses due to the possible
# defaults of the 100 loans.
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

#1.3 If the copula correlation is 30%, what is your estimate of the default correlation
# between different obligors?
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

#1.4 Assume that the default correlation is 0.05. What copula correlation is consistent
# with a default correlation of 0.05?
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

#2.1 Economic capital when losses are independent
EL1 = mean(Total_Loss)*(80*10+20*20)
Q999_1 = (unname(quantile(Total_Loss,0.999,na.rm=T)))*(80*10+20*20)
EC1 = Q999_1 - EL1
EC1

#2.2 Economic Capital if the copula correlation is 30%?
EL2 = mean(Total_Loss2)*(80*10+20*20)
Q999_2 = (unname(quantile(Total_Loss2,0.999,na.rm=T)))*(80*10+20*20)
EC2 = Q999_2 - EL2
EC2

#3. Hedging “excess” concentrations using a first-loss tranche (total 3 points) Your bank is
# unhappy with the distributions of default losses that you estimated in Question 1, and is
# considering hedging the possible default losses using a kind of credit default swap (CDS) known
# as a first-loss tranche. (The counterparty of the CDS will be a hedge fund.) If your bank enters
# into the CDS, it will synthetically transfer the credit exposure of one-half of each of the 20 large
# loans to a special purpose vehicle. Thus, a total of 20 × (1/2) × $20 million = $200 million of
# credit exposure will be transferred to the SPV. The hedge fund will insure the first $40 million
# of losses on the $200 million of exposure transferred to the SPV. Your bank will be responsible
# for any losses greater than $40 million on the exposure transferred to the SPV. Your bank will
# of course also be responsible for the losses on the exposures not transferred to the SPV.
#3.1: estimate the distribution of possible loss with assumption that 100 loans are independent
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

#3.2 What are the expected losses on the loan portfolio, taking account of the hedge?
EL3 = mean(Total_Loss3)*(80*10+20*20)
Q999_3 = (unname(quantile(Total_Loss3,0.999,na.rm=T)))*(80*10+20*20)
EC3 = Q999_3 - EL3
EL3
EC3

#3.3 Please estimate the distribution of possible losses due to the possible defaults of the
# 100 loans, taking account of the protection provided by the hedge and assuming that the copula
# correlation is 0.30 or 30%.
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

# 4. Allowing for random recovery. (total 2 points) Questions 1, 2, and 3 make the simplifying
# assumption that LGD = 100%. A more reasonable assumption is that LGD is random, with an
# expected value that will differ depending on the terms of the loan. (For example, secured loans
# will typically have higher recoveries). For this question, assume that recovery is given by $F ×
# x, where $F is the amount of the loan (either $10 million or $20 million) and x is a random
# variable. The distribution of x is beta, with parameters  =  = 2. Assume that recoveries on
# the different loans are independent.
#4.1: Assume random recovery given by the beta distribution and that defaults on the 100
# loans are independent. Please estimate the distribution of possible losses due to the possible
# defaults of the 100 loans.
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

# 4.2: assume that the copula correlation between each pair of loans is 0.30 or 30%.
# Using this copula correlation, estimate the distribution of possible losses due to the possible defaults of the 100 loans.
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
