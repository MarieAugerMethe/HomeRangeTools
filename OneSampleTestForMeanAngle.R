library(CircStats)

# One sample-test according to Zar (2014)
# Zar, J.H. (2014) Biostatistical Analysis, 5th ed, Pearson Education Limited, Essex, UK  
# H0 = pop has mean of size a_critical
# HA = pop mean is not a_critical
meanAngle1Sample <- function(simAngle, alpha, mu0){
  r <- est.rho(simAngle) # Mean resultant length
  a <- circ.mean(simAngle) # Mean angle
  n <- length(simAngle)
  R <- r*n 
  chisqVal <- qchisq(alpha, 1, lower.tail=FALSE) # For 0.05, df =2
  d <- acos(sqrt(n^2 - (n^2 - R^2) * exp(chisqVal/n))/R)
  ci <- c(a - d, a + d)
  ci <- (ci + 2*pi) %% (2*pi)
  res <- list(ci, paste("reject null hypothesis of mua =", round(mu0,2),
                        ": ", ci[1] > mu0 | ci[2] < mu0))
  return(res)
}

# simulate data with one peak using von mises distribution
set.seed(658)
simAngle <- rvm(20, pi, 5)
circ.plot(simAngle, bins=32)
meanAngle1Sample(simAngle,0.05,pi)

# Flatter von Mises
simAngle <- rvm(20, 0, 2)
circ.plot(simAngle, bins=32)
meanAngle1Sample(simAngle,0.05,pi)

simAngle <- rvm(20, pi/2, 2)
circ.plot(simAngle, bins=32)
meanAngle1Sample(simAngle,0.05,pi)

