# 1	Preliminaries

#install packages
install.packages('tseries')
# use packages
library(tseries)



# 2 Basic commands
# set working directory
setwd('/Users/guoliliu/ISOM4530_2020') # try your own path
SP500 <- read.table("DSP500.csv",header = T, sep=",")
attach(SP500)


# Simple Manipulations
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
print(x)

v <-( 2*x^2+ 1)/2
print(v)

sqrt(v)
sum(v)

mean(v) 
sd(v)
length(v)

min(v)   
max(v)
summary(v)


# Sequences
seq(from=1, to=2, by=0.1)  
seq(from=1, to=2, length.out = 11)  

x <- 3
rep(x, times = 10)

# Logical vectors
temp <- (x > 13)
temp


#3.1
f_normal<-dnorm(seq(-3,3,length=100), mean=0, sd=1)
plot(seq(-3,3,length=100),f_normal)

plot(dnorm(seq(-3,3,length=100), mean=0, sd=1)) # PDF
plot(pnorm(seq(-3,3,length=100), mean=0, sd=1)) # CDF
plot(qnorm(seq(0,1,length=100), mean=0, sd=1)) # quantile
plot(rnorm(n = 100, mean=0, sd=1)) # generate random samples


#4 histogram
DSP<-(SP500$Close)
DSPLR <- diff(log(DSP))     # diff(x, lag = 1)   
diff(c(1,2,5,7))
plot(DSPLR,type="l",main="Daily log return of S&P500 from Jan 1960 to Aug 2020")
hist(DSPLR, breaks=50, freq = F,main="Histogram of DSPLR, #bins = 50")
par(mfrow=c(1,2))  
hist(DSPLR, breaks = 20, freq = F, main="Histogram of DSPLR, #bins = 20")   
hist(DSPLR, breaks=50, freq = F,main="Histogram of DSPLR, #bins = 50")



#5 KDE
par(mfrow=c(1,1))
hist(DSPLR,breaks=50, freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.01",ylim=c(0,65))   
KD <- density(DSPLR, kernel = "gaussian", bw = .01) 
lines(KD, col="red",lty=2,lwd=3)



