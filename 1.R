library(dplyr)
pollutantmean <- function(directory,pollutant,id=1:332){
  temp <- 0
  trows <- 0
  for(i in id){
    t1 <- data.frame()
    if(i<10)
      t1 <- read.csv(paste("00",i,".csv",sep=""))
    else if(i<100)
      t1 <- read.csv(paste("0",i,".csv",sep=""))
    else
      t1 <- read.csv(paste(i,".csv",sep=""))
    vec <- t1[,pollutant]
    temp<-temp+sum(vec,na.rm = T)
    trows<-trows+length(vec)-sum(is.na(vec))
  }
  temp<-temp/trows
  temp
}

#pollutantmean("a","sulfate",1:20)

complete <- function(directory, id = 1:332){
  nobs <- vector()
  for(i in id){
    t1 <- data.frame()
    if(i<10)
      t1 <- read.csv(paste("00",i,".csv",sep=""))
    else if(i<100)
      t1 <- read.csv(paste("0",i,".csv",sep=""))
    else
      t1 <- read.csv(paste(i,".csv",sep=""))
    nobs<-append(nobs,sum(complete.cases(t1)))
  }
  res <- data.frame(id,nobs)
  res
}

#complete("specdata", c(2, 4, 8, 10, 12))

corr<-function(directory,threshold=0){
  vec<-vector()
  comp <- complete("specdata")
  comp <- select(filter(comp,nobs>=threshold),everything())
  #print(comp)
  index<-vector()
  index<-comp[,1]
  #print(index)
  for(i in index){
    t1 <- data.frame()
    if(i<10)
      t1 <- read.csv(paste("00",i,".csv",sep=""))
    else if(i<100)
      t1 <- read.csv(paste("0",i,".csv",sep=""))
    else
      t1 <- read.csv(paste(i,".csv",sep=""))
    
    vec<-append(vec,cor(t1[,"sulfate"],t1[,"nitrate"],use="complete.obs"))
  }
  vec
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))