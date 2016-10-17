library(VGAM)
library(MASS)
library(ggplot2)
# Load the data.
fp.Metal <- read.csv("Metal.csv")
fp.Punk <- read.csv("Punk.csv")
fp.Romance <- read.csv("Romance.csv")

# Get a first impression about each columns of data.
plot(fp.Metal)
hist(fp.Metal$Tonality, breaks = 30)
hist(fp.Metal$Aggressiveness, breaks = 50)
hist(fp.Metal$Timbre, breaks = 50)
hist(fp.Metal$Instrumental, breaks = 50)
hist(fp.Metal$Relaxedness, breaks = 50)
hist(fp.Metal$Acoustics, breaks = 50)
plot(density(fp.Metal$Acoustics), main = "Acoustics Feature Data Density Distribution")
hist(fp.Metal$Danceability, breaks = 50)

# Finding correlations between each high-level features
pairs(fp.Metal, cex.labels=2)
boxplot(fp.Metal, main = "Metal Playlist Output Box Plot", xlab = "Values", las=1, horizontal = TRUE)
boxplot(fp.Punk, main = "Punk Playlist Output Box Plot", xlab = "Values", las=1, horizontal = TRUE)
boxplot(fp.Romance, main = "Romance Playlist Output Box Plot", xlab = "High-level features", xlab = "Values", las=1, horizontal = TRUE)

par(mfrow = c(2, 1))
par(mfrow = c(1, 1))

# Trimming data
trim <- function(x,prop=.05) {
  trimx <- x[x < quantile(x,prob=(1-prop))]
  return(trimx) 
}

fp.Metal.Ac.trim <- trim(fp.Metal$Acoustics)
fp.Metal.To.trim <- trim(fp.Metal$Tonality)
fp.Metal.Ag.trim <- trim(fp.Metal$Aggressiveness)
fp.Metal.Ti.trim <- trim(fp.Metal$Timbre)
fp.Metal.In.trim <- trim(fp.Metal$Instrumental)
fp.Metal.Re.trim <- trim(fp.Metal$Relaxedness)
fp.Metal.Da.trim <- trim(fp.Metal$Danceability)
plot(density(fp.Metal.Ac.trim), main = "Acoustics Feature Data Density Distribution(Trimmed)")
plot(density(fp.Metal.To.trim))
plot(density(fp.Metal.Ag.trim))
plot(density(fp.Metal.Ti.trim))
plot(density(fp.Metal.In.trim))
plot(density(fp.Metal.Re.trim))
plot(density(fp.Metal.Da.trim))

# Comparative Observation
fp.Punk.Ag.trim <- trim(fp.Punk$Aggressiveness)
hist(fp.Punk.Ag.trim, breaks = 50)

fp.Punk.Ac.trim <- trim(fp.Punk$Acoustics)
hist(fp.Punk.Ac.trim)

fp.Romance.Ag.trim <- trim(fp.Romance$Aggressiveness)
hist(fp.Romance.Ag.trim, breaks = 50)

hist(fp.Romance$Relaxedness, breaks = 50)

# Complementary cumulative distribution function, checking tails
plot.ccdf(fp.Metal$Acoustics)
plot.ccdf(fp.Metal.Ac.trim)

## Moment estimation
# var(fp.Metal$Acoustics) 
# mean(fp.Metal$Acoustics) 
# shape = (mean(fp.Metal$Acoustics))**2/var(fp.Metal$Acoustics)
# scale = mean(fp.Metal$Acoustics)/var(fp.Metal$Acoustics)
var(fp.Metal.Ac.trim) 
mean(fp.Metal.Ac.trim) 
shape = (mean(fp.Metal.Ac.trim))**2/var(fp.Metal.Ac.trim)
scale = mean(fp.Metal.Ac.trim)/var(fp.Metal.Ac.trim) 

# Maximum Likelihood Estimation
fitdistr(fp.Metal$Acoustics, "normal")
fitdistr(fp.Metal$Acoustics, "gamma")
fitdistr(fp.Metal$Acoustics, "weibull")
fitdistr(fp.Metal$Acoustics, "lognormal")
fitdistr(fp.Metal.Ac.trim, "normal")
fitdistr(fp.Metal.Ac.trim, "gamma")
fitdistr(fp.Metal.Ac.trim, "weibull")
fitdistr(fp.Metal.Ac.trim, "lognormal")

# Fitting Normal distribution
fp.Metal.Ac.norm <- rnorm(1071, mean = 0.0538245463, sd = 0.0422302624)
hist(fp.Metal.Ac.norm, breaks = 50)
plot(ecdf(fp.Metal.Ac.norm))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.norm, xlab = "Metal - Acoustics Features Distribution", ylab = "Normal Distribution", main = "Fitting Normal")
qqline(fp.Metal.Ac.trim, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

# Fitting Poisson distribution
fp.Metal.Ac.pois <- rpois(1071, lambda=mean(fp.Metal.Ac.trim))
hist(fp.Metal.Ac.pois, breaks = 50)
plot(ecdf(fp.Metal.Ac.pois))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.pois, xlab = "Metal - Acoustics Features Distribution", ylab = "Poisson Distribution", main = "Fitting Poisson")
qqline(fp.Metal.Ac.trim, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

# Fitting Gamma distribution 
fp.Metal.Ac.gam <- rgamma(1071, shape = 0.38292408, rate = 7.11434080) #scale = 0.11231382459653
hist(fp.Metal.Ac.gam, breaks = 50)
plot(ecdf(fp.Metal.Ac.gam))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.gam, xlab = "Metal - Acoustics Features Distribution", ylab = "Gamma Distribution", main = "Fitting Gamma")
qqline(fp.Metal.Ac.trim, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

# Fitting Weibull distribution 
fp.Metal.Ac.wei <- rweibull(1071, shape = 0.515418797, scale = 0.039576890)
hist(fp.Metal.Ac.wei, breaks = 50)
plot(ecdf(fp.Metal.Ac.wei))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.wei, xlab = "Metal - Acoustics Features Distribution", ylab = "Weibull Distribution", main = "Fitting Weibull")
qqline(fp.Metal.Ac.trim, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

# Fitting Pareto distribution 
fp.Metal.Ac.par <- rpareto(1071, 10, 3)
hist(fp.Metal.Ac.par, breaks = 50)
plot(ecdf(fp.Metal.Ac.par))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.par, xlab = "Metal - Acoustics Features Distribution", ylab = "Pareto Distribution", main = "Fitting Pareto")
qqline(fp.Metal.Ac.par, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

# Fitting Log-normal distribution
fp.Metal.Ac.log <- rlnorm(1000, meanlog = -4.65264120, sdlog = 3.65929099)
hist(fp.Metal.Ac.log, breaks = 50)
plot(ecdf(fp.Metal.Ac.log))
qqplot(fp.Metal.Ac.trim, fp.Metal.Ac.log, xlab = "Metal - Acoustics Features Distribution", ylab = "Log-normal Distribution", main = "Fitting Log-normal")
qqline(fp.Metal.Ac.trim, col = 2, lwd = 2, lty = 2)
abline(1 , 1, col = 2, lwd = 2, lty = 2)

## Hypothesis Testing
# Kolmogorov-Smirnov Test
ks.test(fp.Metal$Acoustics, rnorm, mean = 0.066967088, sd = 0.086685105)
ks.test(fp.Metal$Acoustics, pgamma, rate = 5.58373288, shape = 0.37392532)
ks.test(fp.Metal$Acoustics, pweibull, scale = 0.046418200, shape = 0.509333182)
ks.test(fp.Metal$Acoustics, plnorm, meanlog = -4.65264120, sdlog = 3.65929099)
ks.test(fp.Metal.Ac.trim, rnorm, mean = 0.0538245463, sd = 0.0422302624)
ks.test(fp.Metal.Ac.trim, pgamma, rate = 5.58373288, shape = 0.37392532)
ks.test(fp.Metal.Ac.trim, pweibull, scale = 0.046418200, shape = 0.509333182)
ks.test(fp.Metal.Ac.trim, plnorm, meanlog = -4.65264120, sdlog = 3.65929099)

# Shapiro-Wilk Test
shapiro.test(fp.Metal$Acoustics)
shapiro.test(fp.Metal.Ac.trim)
