JB.dis.two.pts = function(latstart, latend, longstart, longend) {
  #***************************************************************************
  # Dave Maxwell distance (geodesic) in metres between the two locations
  # Banerjee, Carlin and Gelfand (2004) Hierarchical Modeling and Analysis for
  # Spatial Data eqn 1.10.
  # Modified by JB to allow vectors
  #***************************************************************************
  
  R = 6371 # radius of Earth approx 6371km
  
  # degrees to radians
  theta1 = 2*pi*latstart / 360
  theta2 = 2*pi*latend / 360
  lambda1 = 2*pi*longstart / 360
  lambda2 = 2*pi*longend / 360
  
  dis = 1000 * R * acos (sin(theta1)*sin(theta2) + cos(theta1)*cos(theta2)*cos(lambda1-lambda2))
  dis
}


bootstrap = function(X,alpha=0.05,nreps=1000) {
  #******************************************************************************
  # Calculates three different types of confidence interval for the
  # population mean:
  # Standard;
  # Percentile;
  # Bias corrected 100(1-alpha).
  #
  # Text used is Bryan F J Manly (1998) , Randomization, Bootstrap and Monte Carlo Methods in
  # Biology, 2nd edition, Chapman and Hall.
  #******************************************************************************
  #
  lenX = length(X)
  Xmean = rep(0,nreps)
  Xstan = rep(0,nreps)
  #boot=matrix(0,nrow=nreps,ncol=lenX)
  #
  # Generate the bootstrap samples and store them in boot[,]
  #
  for (j in 1:nreps) {
    boot = sample(X,lenX,replace=TRUE)
    Xmean[j] = mean(boot)
  }
  boot.mean = mean(Xmean)
  boot.stan = sd(Xmean)
  #
  # standard confidence interval
  #
  zalpha = qnorm(1-(alpha/2))
  upper.stan = boot.mean + zalpha*boot.stan
  lower.stan = boot.mean - zalpha*boot.stan
  #
  # percentile confidence interval
  # I have left out the standard 0.5 correction for percentiles as this
  # correction shouldn't be needed as nreps gets big
  #
  sort.mean = sort(Xmean)
  i.lower = round(nreps*alpha/2)
  i.upper = round(nreps*(1-(alpha/2)))
  upper.per = sort.mean[i.upper]
  lower.per = sort.mean[i.lower]
  #
  # Bias-corrected method
  #
  # Find proportion of time that bootstrap mean exceeds sample mean
  #
  diff = Xmean - mean(X)
  p.exceed = length(diff[diff > 0])/nreps
  #
  # calculate value of standard normal (x0) that is exceeded with prob p
  #
  z0 = qnorm(p.exceed)
  upper.bias = sort.mean[round(pnorm(2*z0 + zalpha) * nreps)]
  lower.bias = sort.mean[round(pnorm(2*z0 - zalpha) * nreps)]
  list(standard=c(lower.stan,upper.stan),percentile=c(lower.per,upper.per),bias=c(lower.bias,upper.bias))
}



bootstrap.mod = function(X, Area, alpha=0.05, nreps=1000) {
  #******************************************************************************
  # Calculates three different types of confidence interval for the
  # population mean:
  # Standard;
  # Percentile;
  # Bias corrected 100(1-alpha).
  # Modified to take account fo the area of each still / video
  #
  # Text used is Bryan F J Manly (1998) , Randomization, Bootstrap and Monte Carlo Methods in
  # Biology, 2nd edition, Chapman and Hall.
  #******************************************************************************
  #
  lenX = length(X)
  inds = 1:lenX
  Xmean = rep(0,nreps)
  Xstan = rep(0,nreps)
  #boot=matrix(0,nrow=nreps,ncol=lenX)
  #
  # Generate the bootstrap samples and store them in boot[,]
  #
  for (j in 1:nreps) {
    boot = sample(inds,lenX,replace=TRUE)
    Xmean[j] = 1000000 * sum(X[boot]) / sum(Area[boot])
  }
  boot.mean = mean(Xmean)
  boot.stan = sd(Xmean)
  #
  # standard confidence interval
  #
  zalpha = qnorm(1-(alpha/2))
  upper.stan = boot.mean + zalpha*boot.stan
  lower.stan = boot.mean - zalpha*boot.stan
  #
  # percentile confidence interval
  # I have left out the standard 0.5 correction for percentiles as this
  # correction shouldn't be needed as nreps gets big
  #
  sort.mean = sort(Xmean)
  i.lower = round(nreps*alpha/2)
  i.upper = round(nreps*(1-(alpha/2)))
  upper.per = sort.mean[i.upper]
  lower.per = sort.mean[i.lower]
  list(standard=c(lower.stan,upper.stan),percentile=c(lower.per,upper.per))
}
