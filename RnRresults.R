rm(list=ls())

poolResults = function(k, n, m = 10, myPath = '~/Dropbox/JudicialAnalysis/JudicialIndependence/fitSum.'){ #k variable of interest (1, 2, or 3 for this), n data size (changes based on lag), m number of imputed data sets
  coef <- numeric()
  coef.sd <- numeric()
  for(d in 1:m){
    hold = read.csv(paste0(myPath, k, '.', d, '.csv'))
    coef = cbind(coef, hold$mean)
    coef.sd = cbind(coef.sd, hold$sd)
  }
  estNames = hold$X
  coef.mean <- apply(coef, 1, mean)
  between.var <- apply(coef, 1, var)
  within.var <- apply(coef.sd^2, 1, mean)
  impute.se.vec <- sqrt(within.var + ((m+1)/m)*between.var)
  impute.df <- (m-1)*(1 + (m/(m+1)) * within.var/between.var)^2
  gamma <- (m/(m+1)) * between.var/(within.var + ((m+1)/m)*between.var)
  full.df <- n - 1
  adj.full.df <- ((full.df+1)/(full.df+3)) * full.df*(1-gamma)
  adj.imp.df <- 1/(1/impute.df + 1/adj.full.df)
  toReturn = round(cbind(coef.mean, impute.se.vec,
                         coef.mean/impute.se.vec, 
                         1-pt(abs(coef.mean/impute.se.vec),adj.imp.df),
                         coef.mean-1.96*impute.se.vec,
                         coef.mean+1.96*impute.se.vec),6)
  colnames(toReturn) <- c('mean', 'se', 't value', 'Pr(>|t|)', '2.5%', '97.5%')
  rownames(toReturn) = estNames
  return(toReturn)
}

n = c(936, 892, 848, 805)

out.table <- list()
for(k in 1:3) out.table[[k]] <- poolResults(k, n[1])

out.tableL1 <- list()
for(k in 1:3) out.tableL1[[k]] <- poolResults(k, n[2], 10, '~/Dropbox/JudicialAnalysis/JudicialIndependence/fitSumL1.')

out.tableL2 <- list()
for(k in 1:3) out.tableL2[[k]] <- poolResults(k, n[3], 10, '~/Dropbox/JudicialAnalysis/JudicialIndependence/fitSumL2.')

out.tableL3 <- list()
for(k in 1:3) out.tableL3[[k]] <- poolResults(k, n[4], 10, '~/Dropbox/JudicialAnalysis/JudicialIndependence/fitSumL3.')

plotResults = function(lag = ''){
  toPlot = get(paste0('out.table', lag), envir = .GlobalEnv)
  
  coefs = toPlot[[1]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), 'mean']
  lower = toPlot[[1]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '2.5%']
  upper = toPlot[[1]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '97.5%']
  
  pdf(paste0('~/Dropbox/JudicialAnalysis/JudicialIndependence/figures/mod1_coefplot', lag, '.pdf'))
  par(mar=c(4,6,2,2), family='Times')
  plot(coefs,10:1,pch=18,ylab='',xlab='',yaxt='n',xlim=c(-3.2,1.2),xaxt='n')
  abline(v=0,lty=2)
  axis(2, at=10:1, labels=c('Fraction', 'System', 'Polity', 'Years', 
                            'Spain', 'UK', 'Portugal', 'France', expression(sigma~'a'), expression(sigma~'y')),
       las=2,tick=FALSE, cex.axis=1.5)
  axis(1,at=-3:1,tick=FALSE,padj=1, cex.axis=1.5)
  segments(x0=lower,x1=upper,y0=10:1,y1=10:1)
  dev.off()
  
  coefs = toPlot[[2]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), 'mean']
  lower = toPlot[[2]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '2.5%']
  upper = toPlot[[2]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '97.5%']
  
  
  pdf(paste0('~/Dropbox/JudicialAnalysis/JudicialIndependence/figures/mod2_coefplot', lag, '.pdf'))
  par(mar=c(4,6,2,2), family='Times')
  plot(coefs,10:1,pch=18,ylab='',xlab='',yaxt='n',xlim=c(-3.2,1.2),xaxt='n')
  abline(v=0,lty=2)
  axis(2, at=10:1, labels=c('Contest', 'System', 'Polity', 'Years', 
                            'Spain', 'UK', 'Portugal', 'France', expression(sigma~'a'), expression(sigma~'y')),
       las=2,tick=FALSE, cex.axis=1.5)
  axis(1,at=-3:1,tick=FALSE,padj=1, cex.axis=1.5)
  segments(x0=lower,x1=upper,y0=10:1,y1=10:1)
  dev.off()
  
  coefs = toPlot[[3]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), 'mean']
  lower = toPlot[[3]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '2.5%']
  upper = toPlot[[3]][c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay'), '97.5%']
  
  pdf(paste0('~/Dropbox/JudicialAnalysis/JudicialIndependence/figures/mod3_coefplot', lag, '.pdf'))
  par(mar=c(4,6,2,2), family='Times')
  plot(coefs,10:1,pch=18,ylab='',xlab='',yaxt='n',xlim=c(-3.2,1.2),xaxt='n')
  abline(v=0,lty=2)
  axis(2, at=10:1, labels=c('Volatility', 'System', 'Polity', 'Years', 
                            'Spain', 'UK', 'Portugal', 'France', expression(sigma~'a'), expression(sigma~'y')),
       las=2,tick=FALSE, cex.axis=1.5)
  axis(1,at=-3:1,tick=FALSE,padj=1, cex.axis=1.5)
  segments(x0=lower,x1=upper,y0=10:1,y1=10:1)
  dev.off()
  
  NULL
}

lags = c('', 'L1', 'L2', 'L3')

lapply(lags, plotResults)
