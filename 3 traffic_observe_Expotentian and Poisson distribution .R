

# Set up the data
require(UsingR)              # Load the UsingR add-on package
#2-a

carIntervalTimes<-as.integer(c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 3, 14, 5, 3, 4,
                    5, 1, 3, 16, 2))
totTimes=sum(carIntervalTimes)
#diffInter<-tabulate(carIntervalTimes)
#Observedinterval<-table(carIntervalTimes)
Observedinterval=numeric(40)
for(x in 1:length(carIntervalTimes)){
  Observedinterval[carIntervalTimes[x]]=Observedinterval[carIntervalTimes[x]]+1
}

Observedinterval=matrix(Observedinterval,ncol = 40)
avgIntervalTime<-mean(carIntervalTimes)
SimuTime<-dexp(1:40,1/avgIntervalTime)*40
obsResult<-rbind(Observedinterval,SimuTime)
colnames(obsResult)<-c(1:40)

barplot(obsResult,main="Distribution of Car Passed a Fixed Point per One-Second Block", 
        xlab="Interval of Car Passed (Second)", 
        ylab="Empirical Count (of One-Seconds Blocks)", col=c("lightblue","pink"), 
        border=c("darkblue","red"),names=1:40, beside=TRUE,
        legend=c("Observed","Expected"))
#qq plot

exponential.quantiles = qexp(ppoints(length(carIntervalTimes)))  # quantiles of standard exponential distribution (rate=1)
qqplot(exponential.quantiles, carIntervalTimes,main="Exponential Q-Q Plot of Car Passed Intervals",
       xlab = "Theoretical Exponential Quantiles", ylab = "Empirical Quantiles")
lines(exponential.quantiles,exponential.quantiles*mean(carIntervalTimes)) # Overlay a line

#K-S test

cumObs=numeric(40)  #for save observation Cumulative distribution function
cumlative=0  # for save probability
for(x in 1:length(carIntervalTimes)){
  cumlative=Observedinterval[x]+cumlative
  cumObs[x]=cumlative/40
}

ks.test(cumObs,pexp(1:40,1/avgIntervalTime))

#2-b

#SimuTime<-dexp(1:40,1/avgIntervalTime)*312  #using exponential distribution to sample interval time
SimuTime<-rexp(40,1/avgIntervalTime)  #using exponential distribution to sample interval time
carPassTime<-numeric(40) #to store car pass exactly time
carPassTime[1]= SimuTime[1]
for(x in 2:length(SimuTime)){
  carPassTime[x]=carPassTime[x-1]+SimuTime[x]
}
carPassTime
NumberofPassCar=ceiling(carPassTime/15) #find the different interval
expSampleCount=numeric(max(NumberofPassCar))

for(x in 1:length(NumberofPassCar)){
  expSampleCount[NumberofPassCar[x]]=expSampleCount[NumberofPassCar[x]]+1
}
expSampleCount=matrix(expSampleCount,ncol = length(expSampleCount))
avginterval= sum(expSampleCount)/length(expSampleCount)
pasSimu=dpois(1:length(expSampleCount),avginterval)*length(expSampleCount)

obsResult<-rbind(expSampleCount,pasSimu)
colnames(obsResult)<-c(1:length(expSampleCount))

barplot(obsResult,main="Distribution of Car Passed a Fixed Point per One-Second Block", 
        xlab="Interval of Car Passed (Second)", 
        ylab="Empirical Count (of One-Seconds Blocks)", col=c("lightblue","pink"), 
        border=c("darkblue","red"),names=1:length(expSampleCount), beside=TRUE,
        legend=c("Observed","Expected"))

pois.quantiles = qpois(ppoints(length(expSampleCount)),avginterval) 


qqplot(pois.quantiles, SimuTime,main="Poison Q-Q Plot of car pass Intervals",
       xlab = "Theoretical Exponential Quantiles", ylab = "Empirical Quantiles")
lines(pois.quantiles,pois.quantiles*avginterval) # Overlay a line

#K-S test

cumObs=numeric(40)  #for save observation Cumulative distribution function
cumlative=0  # for save probability
for(x in 1:length(expSampleCount)){
  cumlative=expSampleCount[x]+cumlative
  cumObs[x]=cumlative/40
}

ks.test(cumObs,pexp(1:40,1/avgIntervalTime))
#c

theta <- seq(length=20,from=0.2,to=4.0)
carIntervalTimes<-as.integer(c(12, 2, 6, 2, 19, 5, 34, 4, 1, 4, 8, 7, 1, 21, 6, 11, 8, 28, 6, 4, 5, 1, 18, 9, 5, 1, 21, 1, 1, 5, 3, 14, 5, 3, 4,
                               5, 1, 3, 16, 2))
totTimes=sum(carIntervalTimes)
#diffInter<-tabulate(carIntervalTimes)
#Observedinterval<-table(carIntervalTimes)
Observedinterval=numeric(length(carIntervalTimes))
Observedinterval[1]= carIntervalTimes[1]
for(x in 2:length(carIntervalTimes)){
  Observedinterval[x]=Observedinterval[x-1]+carIntervalTimes[x]
}
NumberofPassCar=ceiling(Observedinterval/15) #find the different interval

expSampleCount=numeric(max(NumberofPassCar))

for(x in 1:length(NumberofPassCar)){
  expSampleCount[NumberofPassCar[x]]=expSampleCount[NumberofPassCar[x]]+1
}
priorprob=expSampleCount/sum(expSampleCount[1:20])
priorprob=priorprob[1:20]
likelyhood=dpois(1:20, theta, log = FALSE)
postprob=priorprob*likelyhood
postmean=0
for(x in 1:length(postprob)){
  postmean=postprob[x]*x+postmean
  
}
postvar=0
for(x in 1:length(postprob)){
  postvar=(x-postmean)^2*postprob[x]+postvar
  
}

