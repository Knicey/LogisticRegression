X = c(.32, .18, .26, .21, .29)
y = c(1, 0, 1, 0, 0)

betarange = seq(-5, 5, by = .01)

loglikelihood = vector(length = length(betarange))
betastore = vector(length = length(betarange))

xyproduct = y * X

for(i in 1:length(betarange)) {
  betastore[i] = c(betarange[i])
  beta = betarange[i]
  
  loglikelihood[i] = sum(xyproduct * beta) - sum(log(1 + exp(X* beta)))
}

mleindex = which(loglikelihood == max(loglikelihood), arr.ind=T)

betastore[mleindex]

deriv = vector(length = length(x))

for(i in 1:length(x)) {
  deriv[i] = exp(-0.61 * x[i]) * x[i] / ( 1 + exp(-0.61 * x[i]) )
}

sum(xyproduct) - sum(deriv)

