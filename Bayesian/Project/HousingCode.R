library(splitstackshape)
# Cleaning 2007 
# Only desireable columns
h2007 <- read.csv("HG2007.csv")
h2007 <- h2007[,c(1,5,11,13,15,17)]

# Make dollar values into numerics
h2007$Shared.FW <- as.numeric(gsub('[$]', '', h2007$Shared.FW))
h2007$Nonref.FW <- as.numeric(gsub('[$]', '', h2007$Nonref.FW))
h2007$SS.Shared <- as.numeric(gsub('[$]', '', h2007$SS.Shared))
h2007$SS.NonRef <- as.numeric(gsub('[$]', '', h2007$SS.NonRef))

# take out bad data - only apartments with year-round shared rooms
h2007 <- h2007[!is.na(h2007$Shared.FW),]
h2007 <- h2007[!is.na(h2007$SS.Shared),]

# make na nonrefundable deposits 0
h2007[is.na(h2007$Nonref.FW), 3] <- 0.00
h2007[is.na(h2007$SS.NonRef), 5] <- 0.00

# make year-long rent column
h2007[,7] <- h2007[,3] + h2007[,4]*8 + h2007[,5] + h2007[,6]*4
names(h2007)[7] <- "Total"

# remove others
h2007 <- h2007[,c(1,2,7)]

# duplicate rows based on number of units (not exact since there are private rooms)
h2007.ex <- expandRows(h2007, "Units")




# Cleaning 2019
# Only desireable columns
h2019 <- read.csv("HG2019.csv")
h2019 <- h2019[,c(1,5,12,14,16,18,20)]

# Make dollar values into numerics
h2019$Year.Shared <- as.numeric(gsub('[$]', '', h2019$Year.Shared))
h2019$FW.Shared <- as.numeric(gsub('[$]', '', h2019$FW.Shared))
h2019$FW.Year.Non.Ref <- as.numeric(gsub('[$]', '', h2019$FW.Year.Non.Ref))
h2019$SS.Shared <- as.numeric(gsub('[$]', '', h2019$SS.Shared))
h2019$SS.Non.Ref <- as.numeric(gsub('[$]', '', h2019$SS.Non.Ref))
h2019

#multiply col 3
h2019[,3] <- h2019[,3]*12
h2019[is.na(h2019[,3]), 3] <- h2019[is.na(h2019[,3]),4]*8 + h2019[is.na(h2019[,3]),6]*4

# make na nonrefundable deposits 0
h2019[is.na(h2019$FW.Year.Non.Ref), 5] <- 0.00
h2019[is.na(h2019$SS.Non.Ref), 7] <- 0.00

# take out bad data - only apartments with year-round shared rooms
h2019 <- h2019[!is.na(h2019$Year.Shared),]

# make total column 
h2019[,8] <- h2019[,3] + h2019[,5]
names(h2019)[8] <- "Total"

# remove others
h2019 <- h2019[,c(1,2,8)]
# duplicate rows based on number of units (not exact since there are private rooms)
h2019.ex <- expandRows(h2019, "Units")

hist(h2007[,3])
hist(h2007.ex[,2])
hist(h2019[,3])
hist(h2019.ex[,2])

# Gibbs Sampling function
gibbsSampling <- function(lambda, tau, gamma, phi, nrep, data) {
  mu <- numeric(nrep+1)
  sigsq <- numeric(nrep+1)
  
  mu[1] <- lambda
  sigsq[1] <- phi/(gamma - 1)
  
  for (i in 1:nrep){
    lambda1 <- (tau^2*sum(data) + sigsq[i]*lambda)/(tau^2*length(data) + sigsq[i])
    tau1 <- sqrt((sigsq[i]*tau^2)/(tau^2*length(data) + sigsq[i]))
    mu[i+1] <- rnorm(1, lambda1, tau1)
    
    gamma1 <- gamma + length(data)/2
    phi1 <- phi + sum((data-mu[i])^2)/2
    sigsq[i+1] <- 1/rgamma(1,gamma1,phi1)
  }
  
  return (list(mu, sigsq))
}

# set prior parameters
prior_lambda_07 <- 2500
prior_tau_07 <- 50
prior_gamma_07 <- 6
prior_phi_07 <- 1.25e6
prior_lambda_19 <- 3600
prior_tau_19 <- 75
prior_gamma_19 <- 6
prior_phi_19 <- 1.75e6

# run gibbs sampling
gs2007 <- gibbsSampling(prior_lambda_07,prior_tau_07,prior_gamma_07,prior_lambda_19,
                        1e5, h2007.ex[,2])
gs2019 <- gibbsSampling(prior_lambda_19,prior_tau_19,prior_gamma_19,prior_phi_19,
                        1e5, h2019.ex[,2])

mu07 <- gs2007[[1]]
var07 <- gs2007[[2]]
mu19 <- gs2019[[1]]
var19 <- gs2019[[2]]
d <- mu19 - mu07

#2007 mean
plot(density(mu07),xlim=c(2320,2700), xlab = expression(mu), col = "red")
curve(dnorm(x,prior_lambda_07,prior_tau_07),ylab = "Density", add = T, col = "blue")
legend("topleft", legend = c("2007 Prior", "2007 Posterior"), 
       col = c("blue","red"), lty = 1, cex = 0.7)

#2019 mean
plot(density(mu19),xlim=c(3400,4200), xlab = expression(mu), col = "red")
curve(dnorm(x,prior_lambda_19,prior_tau_19),ylab = "Density", add = T, col = "blue")
legend("topleft", legend = c("2019 Prior", "2019 Posterior"), 
       col = c("blue","red"), lty = 1)

# difference
plot(density(d),xlim=c(1600,1700), xlab = expression(mu))

# 2007 prior variance
curve(dgamma(1/x,prior_gamma_07,prior_phi_07), xlim = c(0,2e6), xlab = expression(sigma[u]^2), col = "blue")

# 2007 posterior variance
plot(density(var07), xlim = c(1e5,1.4e5),xlab = expression(sigma^2), col = "red")

# 2019 prior variance
curve(dgamma(1/x,prior_gamma_19,prior_phi_19), xlim = c(0,2e6), xlab = expression(sigma[u]^2), col = "blue")

# 2019 posterior variance
plot(density(var19), xlim = c(3.4e5,4.3e5),xlab = expression(sigma^2), col = "red")

     