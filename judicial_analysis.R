library(RCurl)
dat_url <- getURL('https://raw.githubusercontent.com/carlson9/MLM/master/judicial_data.txt')
data <- read.table(text = dat_url, row.names=NULL)
data <- data[,c('country', 'year', 'LJI', 'system', 'colonial', 'polity', 'years', 'frac', 'contest', 'vol')]

