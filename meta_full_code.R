> install.packages("meta")
> library(meta)
> dat<-read.csv("K:\\Cancer Research\\Meta Anik\\new paper\\Cancer.csv", header =TRUE)
> str(dat)

###Fitting the fixed effect model
Y <- with(dat, log(Gender*Area*p/(n*N)))
V <- with(dat, 1/Area + 1/p + 1/n + 1/N)
> cbind(Y,V)
> result.or <- rma(yi = Y, vi = V, method = "FE")
> summary(result.or)

###Confidence Interval

> confint(result.or)

### Fitting Random Effects Model

> re <- rma(yi = Y, vi = V, data=dat)
> summary(re)

### Forest plotting

> forest(re, slab = paste(dat$Author, as.character(dat$Year), sep = ", "))

### Funnel Plotting. A common way to investigate potential publication bias in a meta-analysis is the funnel plot. 
### Asymmetrical distribution indicates potential publication bias.

> funnel(re)

### Differences measure

> boxplot(Y ~ Area, data = dat)

### Mesuring tau2 for different method

> estimators <- c("DL", "REML", "HE", "HS", "SJ", "ML", "EB")
> taus <- sapply(estimators, function(method){re$tau2})
> plot(y = taus, x = 1:length(taus),type = "h", pch = 19,axes = FALSE, xlab = "Estimators")
> axis(2, las = 1)
> axis(1, at = 1:length(taus), lab = estimators)
