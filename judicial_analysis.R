rm(list=ls())
lapply(c('RCurl', 'superdiag', 'mice', 'R2jags', 'xtable', 'boot'), library, character.only=TRUE)

#setwd('/home/david/MLM/')

dat_url <- getURL('https://raw.githubusercontent.com/carlson9/MLM/master/judicial_data.txt')
data <- read.table(text = dat_url, row.names=NULL)
data <- data[,c('country', 'year', 'LJI', 'system', 'colonial', 'polity', 'years', 'frac', 'contest', 'vol')]
drop <- apply(data, 1, function(x) all(is.na(x)))
data <- data[!drop,]

#xtable(t(as.matrix(apply(data, 2, function(x) sum(is.na(x)))[8:10])), caption = 'Missingness')

m <- 10
#data.impute <- mice(data, m)
#for(i in 1:m){
#  write.table(complete(data.impute, i), file=paste0('/home/david/MLM/Data', i))
#}


data.impute <- list()
for(i in 1:m){
  dat_url <- getURL(paste0('https://raw.githubusercontent.com/carlson9/MLM/master/Data', i))
  data.impute[[i]] <- read.table(text = dat_url, row.names=NULL)
}


judicial <- 'model {
  for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- a[country[i]] + b[1]*system[i] + b[2]*polity[i] + b[3]*years[i] + b[4]*interest[i]
  }
  b[1] ~ dnorm (0, .0001)
  b[2] ~ dnorm (0, .0001)
  b[3] ~ dnorm (0, .0001)
  b[4] ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif(0, 100)
  for(j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    a.hat[j] <- g.0 + g.1*colonial.esp[j] + g.2*colonial.brit[j] + g.3*colonial.port[j] +g.4*colonial.fran[j]
  }
  g.0 ~ dnorm (0, .0001)
  g.1 ~ dnorm (0, .0001)
  g.2 ~ dnorm (0, .0001)
  g.3 ~ dnorm (0, .0001)
  g.4 ~ dnorm (0, .0001)
  tau.a <- pow (sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}'

judicial.null <- 'model {
  for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- a[country[i]] + b[1]*system[i] + b[2]*polity[i] + b[3]*years[i]
  }
  b[1] ~ dnorm (0, .0001)
  b[2] ~ dnorm (0, .0001)
  b[3] ~ dnorm (0, .0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif(0, 100)
  for(j in 1:J){
    a[j] ~ dnorm (a.hat[j], tau.a)
    a.hat[j] <- g.0 + g.1*colonial.esp[j] + g.2*colonial.brit[j] + g.3*colonial.port[j] +g.4*colonial.fran[j]
  }
  g.0 ~ dnorm (0, .0001)
  g.1 ~ dnorm (0, .0001)
  g.2 ~ dnorm (0, .0001)
  g.3 ~ dnorm (0, .0001)
  g.4 ~ dnorm (0, .0001)
  tau.a <- pow (sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}'



judicial.mods <- list()
interest.vars <- c('frac', 'contest', 'vol')
set.seed(1319)
for(k in 1:3){
  for(i in 1:m){
    temp.data <- data.impute[[i]]
    y <- logit(temp.data$LJI)
    n <- dim(temp.data)[1]
    country.names <- unique(temp.data$country)
    J <- length(country.names)
    colonial <- character()
    country <- numeric()
    for(j in 1:J){
      colonial[j] <- temp.data[temp.data$country == country.names[j], 'colonial'][1]
      country[temp.data$country == country.names[j]] <- j
    }
    colonial <- as.factor(colonial)
    colonial.esp <- ifelse(colonial == 2, 1, 0)
    colonial.brit <- ifelse(colonial == 3, 1, 0)
    colonial.port <- ifelse(colonial == 4, 1, 0)
    colonial.fran <- ifelse(colonial == 5, 1, 0)
    polity <- temp.data$polity
    years <- temp.data$years
    system <- temp.data$system
    interest <- temp.data[, interest.vars[k]]
    frac.data <- list('n', 'J', 'y', 'colonial.esp', 'colonial.brit', 'colonial.port', 'colonial.fran', 'system', 'polity', 'years', 'interest', 'country')
    frac.inits <- function(){
      list(a=rnorm(J), b=rnorm(4), g.0=rnorm(1), g.1=rnorm(1), g.2=rnorm(1), g.3=rnorm(1), g.4=rnorm(1), sigma.y=runif(1), sigma.a=runif(1))
    }
    frac.parameters <- c('a', 'b', 'g.0', 'g.1', 'g.2', 'g.3', 'g.4', 'sigma.y', 'sigma.a')
    judicial.mods[[i+m*(k-1)]] <- jags(frac.data, frac.inits, frac.parameters, textConnection(judicial), n.chains=3, n.iter=30000, n.burnin=10000, n.thin=1, DIC=TRUE)
  }
}

y <- logit(data$LJI)
n <- dim(data)[1]
country.names <- unique(data$country)
J <- length(country.names)
colonial <- character()
country <- numeric()
for(j in 1:J){
  colonial[j] <- data[data$country == country.names[j], 'colonial'][1]
  country[data$country == country.names[j]] <- j
}
colonial <- as.factor(colonial)
colonial.esp <- ifelse(colonial == 2, 1, 0)
colonial.brit <- ifelse(colonial == 3, 1, 0)
colonial.port <- ifelse(colonial == 4, 1, 0)
colonial.fran <- ifelse(colonial == 5, 1, 0)
polity <- data$polity
years <- data$years
system <- data$system
frac.data <- list('n', 'J', 'y', 'colonial.esp', 'colonial.brit', 'colonial.port', 'colonial.fran', 'system', 'polity', 'years', 'country')
frac.inits <- function(){
  list(a=rnorm(J), b=rnorm(3), g.0=rnorm(1), g.1=rnorm(1), g.2=rnorm(1), g.3=rnorm(1), g.4=rnorm(1), sigma.y=runif(1), sigma.a=runif(1))
}
frac.parameters <- c('a', 'b', 'g.0', 'g.1', 'g.2', 'g.3', 'g.4', 'sigma.y', 'sigma.a')
judicial.mods[[31]] <- jags(frac.data, frac.inits, frac.parameters, textConnection(judicial.null), n.chains=3, n.iter=30000, n.burnin=10000, n.thin=1, DIC=TRUE)


#save(judicial.mods, file='judicial2.mods.Rdata')
#
#load('judicial2.mods.Rdata')

avg.DIC <- numeric()
out.table <- list()
for(k in 1:3){
  coef <- numeric()
  coef.sd <- numeric()
  DIC <- numeric()
  for(i in 1:m){
    coef <- cbind(coef, unlist(judicial.mods[[i+m*(k-1)]]$BUGSoutput$mean))
    coef.sd <- cbind(coef.sd, unlist(judicial.mods[[i+m*(k-1)]]$BUGSoutput$sd))
    DIC <- c(DIC, judicial.mods[[i+m*(k-1)]]$BUGSoutput$DIC)
  }
  avg.DIC <- c(avg.DIC, mean(DIC))
  coef.mean <- apply(coef, 1, mean)
  between.var <- apply(coef, 1, var)
  within.var <- apply(coef.sd^2, 1, mean)
  impute.se.vec <- sqrt(within.var + ((m+1)/m)*between.var)
  impute.df <- (m-1)*(1 + (m/(m+1)) * within.var/between.var)^2
  gamma <- (m/(m+1)) * between.var/(within.var + ((m+1)/m)*between.var)
  full.df <- n - 1
  adj.full.df <- ((full.df+1)/(full.df+3)) * full.df*(1-gamma)
  adj.imp.df <- 1/(1/impute.df + 1/adj.full.df)
  out.table[[k]] <- round(cbind(coef.mean, impute.se.vec,
                             coef.mean/impute.se.vec, 
                             1-pt(abs(coef.mean/impute.se.vec),adj.imp.df),
                             coef.mean-1.96*impute.se.vec,
                             coef.mean+1.96*impute.se.vec),6)
  colnames(out.table[[k]]) <- c('mean', 'se', 't value', 'Pr(>|t|)', '2.5%', '97.5%')
  rownames(out.table[[k]])[-(1:44)] <- c('system', 'polity', 'years', ifelse(k==1, 'frac',
                                                                               ifelse(k==2, 'contest', 'vol')),
                                           'deviance', 'g.0', 'Colonial_Spain', 'Colonial_UK', 'Colonial_Portugal',
                                           'Colonial_France', 'sigma.a', 'sigma.y')
}

#save(out.table, file='out.table.Rdata')
#load('out.table.Rdata')

#xtable(out.table[[1]][-(1:44),], caption='Model with frac')
#xtable(out.table[[2]][-(1:44),], caption='Model with contest')
#xtable(out.table[[3]][-(1:44),], caption='Model with vol')

coef <- unlist(judicial.mods[[31]]$BUGSoutput$mean)
se <- unlist(judicial.mods[[31]]$BUGSoutput$sd)
null.table <- round(cbind(coef, se,
            coef-1.96*se,
            coef+1.96*se),6)[-(1:44), ]
colnames(null.table) <- c('mean', 'se', '2.5%', '97.5%')
rownames(null.table) <- c('system', 'polity', 'years',
                                       'deviance', 'g.0', 'Colonial_Spain', 'Colonial_UK', 'Colonial_Portugal',
                                       'Colonial_France', 'sigma.a', 'sigma.y')
#xtable(null.table, caption='Null Model')

judicial.mods[[31]]$BUGSoutput$DIC

diagnostic.list <- list()
for(mod in judicial.mods){
  diagnostic.list <- c(diagnostic.list, superdiag(as.mcmc(mod)))
}

#save(diagnostic.list, file='diagnostic.list.Rdata')
#load('diagnostic.list.Rdata')

jm1 <- judicial.mods[[1]]
jm2 <- judicial.mods[[11]]
jm3 <- judicial.mods[[21]]
rm(judicial.mods)
#save(jm1, jm2, jm3, file='subset.mods.Rdata')
#load('subset.mods.Rdata')

pdf('mod1_traceplot.pdf')
traceplot(jm1, mfrow=c(4, 3), ask=FALSE, varname = c('b', 'g.0', 'g.1', 'g.2', 'g.3', 'g.4', 'sigma.a', 'sigma.y'))
dev.off()

pdf('mod2_traceplot.pdf')
traceplot(jm2, mfrow=c(4, 3), ask=FALSE, varname = c('b', 'g.0', 'g.1', 'g.2', 'g.3', 'g.4', 'sigma.a', 'sigma.y'))
dev.off()

pdf('mod3_traceplot.pdf')
traceplot(jm3, mfrow=c(4, 3), ask=FALSE, varname = c('b', 'g.0', 'g.1', 'g.2', 'g.3', 'g.4', 'sigma.a', 'sigma.y'))
dev.off()

pdf('mod1_autocorr.pdf')
autocorr.plot(as.mcmc(jm1), ask=FALSE)
dev.off()

pdf('mod2_autocorr.pdf')
autocorr.plot(as.mcmc(jm2), ask=FALSE)
dev.off()

pdf('mod3_autocorr.pdf')
autocorr.plot(as.mcmc(jm3), ask=FALSE)
dev.off()

pdf('mod1_gelmanplot.pdf')
gelman.plot(as.mcmc(jm1), ask=FALSE)
dev.off()

pdf('mod2_gelmanplot.pdf')
gelman.plot(as.mcmc(jm2), ask=FALSE)
dev.off()

pdf('mod3_gelmanplot.pdf')
gelman.plot(as.mcmc(jm3), ask=FALSE)
dev.off()


##Model 1 coef plot
pdf('mod1_coefplot.pdf')
par(mar=c(4,8,2,2), family='Times')
mod1.coef <- out.table[[1]][-c(1:44, 49), c(1,5,6)]
plot(mod1.coef[,1], 1:dim(mod1.coef)[1], pch=18, xlab='estimates', ylab='', yaxt='n', xlim=c(min(mod1.coef[,2])-.3, max(mod1.coef[,3])+.3))
axis(2, at=1:dim(mod1.coef)[1], labels=rownames(mod1.coef), las=2, cex=.6)
segments(mod1.coef[,2], 1:dim(mod1.coef)[1], mod1.coef[,3], 1:dim(mod1.coef)[1])
abline(v=0, lty=2)
dev.off()

pdf('mod2_coefplot.pdf')
par(mar=c(4,8,2,2), family='Times')
mod2.coef <- out.table[[2]][-c(1:44, 49), c(1,5,6)]
plot(mod2.coef[,1], 1:dim(mod2.coef)[1], pch=18, xlab='estimates', ylab='', yaxt='n', xlim=c(min(mod2.coef[,2])-.3, max(mod2.coef[,3])+.3))
axis(2, at=1:dim(mod2.coef)[1], labels=rownames(mod2.coef), las=2, cex=.6)
segments(mod2.coef[,2], 1:dim(mod2.coef)[1], mod2.coef[,3], 1:dim(mod2.coef)[1])
abline(v=0, lty=2)
dev.off()

pdf('mod3_coefplot.pdf')
par(mar=c(4,8,2,2), family='Times')
mod3.coef <- out.table[[3]][-c(1:44, 49), c(1,5,6)]
plot(mod3.coef[,1], 1:dim(mod3.coef)[1], pch=18, xlab='estimates', ylab='', yaxt='n', xlim=c(min(mod3.coef[,2])-.3, max(mod3.coef[,3])+.3))
axis(2, at=1:dim(mod3.coef)[1], labels=rownames(mod3.coef), las=2, cex=.6)
segments(mod3.coef[,2], 1:dim(mod3.coef)[1], mod3.coef[,3], 1:dim(mod3.coef)[1])
abline(v=0, lty=2)
dev.off()


