#function to generate bootstrap replicates
#INPUT:
#x is vector of team ranking/ other measure of 
#team power in different simulations
#B is number of replicates to generate
#estimator is the statistic we want, set as mean by
#default
#l is the length of bootstrap resample in each run
#set to length of x by default
#OUTPUT
#a list of bootstrap replicates, bias ans standard error
boot_replicate <- function(x, B = 5000, estimator = mean, l = length(x)){
  boots <- replicate(B, estimator(sample(x,l,replace=TRUE)))
  sd_b <- sd(boots)
  bias_b <- mean(boots - estimator(x))
  list(boot = boots, bias = bias_b, se = sd_b)
}

#function to generate jackknife replicates
#INPUT:
#x is vector of team ranking/ other measure of 
#team power in different simulations
#estimator is the statistic we want, set as mean by
#default
#OUTPUT
#a list of jackknife replicates, bias ans standard error
jack_replicate <- function(x, estimator = mean){
  n <- length(x)
  jack <- numeric(n)
  #perform jackknife
  for(i in 1:n)
    jack[i] <- estimator(x[-i])
  #bias using jackknife
  bias_j <- (n-1)*(estimator(jack)-estimator(x))
  sumsq=sum((jack-mean(jack))^2)
  se_j <- sqrt((n-1)/n)*sqrt(sumsq)
  list(jack = jack,bias = bias_j, se = se_j)
}
