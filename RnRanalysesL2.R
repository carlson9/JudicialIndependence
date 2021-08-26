rm(list=ls())
library(RCurl)
m = 10
data.impute <- list()
for(i in 1:m){
  dat_url = getURL(paste0('https://raw.githubusercontent.com/carlson9/JudicialIndependence/master/Data', i))
  data.impute[[i]] = read.table(text = dat_url, row.names=NULL)
}

library(parallel)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

judicial = '
data {
  int<lower=1> N;
  int<lower=1> N_coun;
  real y[N];
  int<lower=1, upper=N_coun> coun[N]; //country indicator
  matrix[N, 4] X; //variables (interest, polity, years, system)
  matrix[N_coun, 4] cln; //colonial history
}
parameters {
  vector[4] B;
  real b0;
  vector[N_coun] a;
  real g0;
  vector[4] G;
  real<lower=0> sigmay;
  real<lower=0> sigmaa;
}
model {
  B ~ normal(0, 3);
  b0 ~ std_normal();
  g0 ~ normal(0, 3);
  G ~ normal(0, 3);
  sigmaa ~ inv_gamma(2,.5);
  sigmay ~ inv_gamma(2,.5);
  for(cn in 1:N_coun) a[cn] ~ normal(g0 + cln[cn,]*G, sigmaa);
  for(n in 1:N) y[n] ~ normal(b0 + a[coun[n]] + X[n,]*B, sigmay);
}
'

model = stan_model(model_code = judicial)

#lag two time periods

for(dd in 1:m){
  hold = data.impute[[dd]]
  toBind = data.frame()
  for(cn in unique(hold$country)){
    hold.cn = hold[hold$country == cn,]
    hold.cn = hold.cn[order(hold.cn$year),]
    hold.cn[3:nrow(hold), 5:ncol(hold.cn)] = hold.cn[1:(nrow(hold)-2), 5:ncol(hold.cn)]
    hold.cn[1:2,] = NA
    hold.cn = na.omit(hold.cn)
    toBind = rbind(toBind, hold.cn)
  }
  data.impute[[dd]] = toBind
}

interest.vars <- c('frac', 'contest', 'vol')

library(boot)

for(k in 1:3){
  cl = makeCluster(20)
  clusterExport(cl, varlist = list('k', 'model', 'data.impute', 'interest.vars'))
  parLapply(cl, 1:m, function(i){
    temp.data <- data.impute[[i]]
    y <- boot::logit(temp.data$LJI)
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
    
    to_stan = list(N = n,
                   N_coun = J,
                   y = y,
                   coun = country,
                   X = cbind(interest, polity, years, system),
                   cln = cbind(colonial.esp,
                               colonial.brit,
                               colonial.port,
                               colonial.fran))
    
    set.seed(100*k + i)
    fit = rstan::sampling(model, data = to_stan, iter = 1500, chains = 2, cores = 2)
    assign(paste0('fitSum.', k, '.', i),
           rstan::summary(fit)$summary)
    rm(fit)
    write.csv(get(paste0('fitSum.', k, '.', i)), file = paste0('~/Dropbox/JudicialAnalysis/JudicialIndependence/fitSumL2.', k, '.', i, '.csv'))
    
    NULL
  })
  stopCluster(cl)
}

