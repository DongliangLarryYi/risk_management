# Option Parameters
t = 35 / 365 # Maturity
r = 0.0025
sSPX = 1864.78
sDJX = 159.74

qSPX = 0.0222
qDJX = 0.0248
q = c(qSPX, qSPX, qDJX, qDJX) # Dividend Yields

kSPX = 1865
kDJX = 160
K = c(kSPX, kSPX, kDJX, kDJX) # Strikes

callSPX = (49.50 + 50.10) / 2
putSPX = (55.50 + 56.10) / 2
callDJX = (3.85 + 4.05) / 2
putDJX = (4.70 + 4.90) / 2

P = t(c(callSPX, putSPX, callDJX, putDJX)) # Prices
P
colnames(P) =  c("SPX.Call", "SPX.Put", "DJX.Call", "DJX.Put")

# Black-Scholes Formulas
BSC = function(S, K, r, q, t, sigma) {
  d1 = (log(S / K) + (r - q) * t + sigma^2 * t / 2) / (sigma * sqrt(t))
  d2 = (log(S / K) + (r - q) * t - sigma^2 * t / 2) / (sigma * sqrt(t))
  result = exp(-q * t) * pnorm(d1) * S - exp(-r * t) * pnorm(d2) * K
  
  return(result)
}
BSP = function(S, K, r, q, t, sigma) {
  d1 = (log(S / K) + (r - q) * t + sigma^2 * t / 2) / (sigma * sqrt(t))
  d2 = (log(S / K) + (r - q) * t - sigma^2 * t / 2) / (sigma * sqrt(t))
  result = exp(-r * t) * pnorm(-d2) * K - exp(-q * t) * pnorm(-d1) * S 
  
  return(result)
}
Vega = function(S, K, r, q, t, sigma) {
  d1 = (log(S / K) + (r - q) * t + sigma^2 * t / 2) / (sigma * sqrt(t))
  result = S * sqrt(t) * exp(-d1^2 / 2) / sqrt(2 * pi) #pdf
  
  return(result)
}

# Parameters of BS Formula
iV = rep(0.2, 4) # Implied Volatilities
pNR = t(P)
rNR = rep(r, 4)
tNR = rep(t, 4)
sNR = c(sSPX, sSPX, sDJX, sDJX)
kNR = c(kSPX, kSPX, kDJX, kDJX)
qNR = c(qSPX, qSPX, qDJX, qDJX)

# Newton-Raphson
for (i in 1:100) {
  c = BSC(sNR, kNR, rNR, qNR, tNR, iV)
  p = BSP(sNR, kNR, rNR, qNR, tNR, iV)
  pBS = rbind(c[1], p[2], c[3], p[4]) #there is resource waste on calculation
  iV = iV - (pBS - pNR) / Vega(sNR, kNR, rNR, qNR, tNR, iV)
}
iV = t(iV)
colnames(iV) = colnames(P)
iV