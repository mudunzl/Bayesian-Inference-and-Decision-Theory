#1
#a
alpha=1
beta=3
theta=seq(length=100, from=0.05, to=1)

priordata= dbeta(theta,alpha,beta)

plot(theta,data)

mean =  alpha/(alpha+beta)
#sum(data*theta)/100
variance= (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
sd= variance^0.5
creinterval= qbeta(0.975,alpha,beta)-qbeta(0.025,alpha,beta)
qbeta(0.975,alpha,beta)
qbeta(0.025,alpha,beta)

#I think this is not a reasonable priror distribution because of assigment 2 data.
#In assignement 2, the data tell us 82% people choose B and 83% people choose c, so the mean value is 0.82*0.83
#=0.6806. However, in out prror data is 0.25 

#b
alphastar = alpha+ 19
betastar= beta+47-19
posterdata= dbeta(theta,alphastar,betastar)
postmean =  alphastar/(alphastar+betastar)
postvariance=(alphastar*betastar)/((alphastar+betastar)^2*(alphastar+betastar+1))
postsd= postvariance^0.5
creinterval= qbeta(0.975,alphastar,betastar)-qbeta(0.025,alphastar,betastar)
qbeta(0.025,alphastar,betastar)
qbeta(0.975,alphastar,betastar)
plot(theta,posterdata,type = "l",col= "red")
lines(theta,priordata,col="green")
#lik=array(1,length(theta))  

normLikC=dbeta(theta,shape1=19+1,shape2=47+1-19)    # Normalized Likelihood 

lines(theta,normLikC)
legend("topright",                                # 
       lty = 1,                                   # 
       lwd = 5,
       col = c("green", "red", "black"),           # col
       legend = c("Prior", "posterior", "Normalize Liklyhood") #
)

#c

alphastar = alphastar+ 20
betastar= betastar+47-20
posterdata1= dbeta(theta,alphastar,betastar)
postmean =  alphastar/(alphastar+betastar)
postvariance=(alphastar*betastar)/((alphastar+betastar)^2*(alphastar+betastar+1))
postsd= postvariance^0.5
creinterval= qbeta(0.975,alphastar,betastar)-qbeta(0.025,alphastar,betastar)
qbeta(0.975,alphastar,betastar)
qbeta(0.025,alphastar,betastar)
plot(theta,posterdata1,type = "l",col= "red")
lines(theta,posterdata,col="green")
#lik=array(1,length(theta))  

normLikC=dbeta(theta,shape1=20+1,shape2=47+1-20)    # Normalized Likelihood 
lines(theta,normLikC)


legend("topright",                                # 
       lty = 1,                                   # 
       lwd = 5,
       col = c("green", "red", "black"),           # col
       legend = c("Prior", "posterior", "Normalize Liklyhood") #
)

#d




#2
#a
lambda=seq(length=200,from=0.03,to=5)  # This is the range of lambda values for plot
alpha0=1
beta0=10000
priorDens=dgamma(lambda,shape=alpha0,scale=beta0)  # Prior
obsdata=c(2,2,1,1,0,4,3,0,2,1)
interval=length(obsdata)
carcount=sum(obsdata)

alpha1=alpha0+carcount
beta1=1/(1/beta0 + interval)
postDens=dgamma(lambda,shape=alpha1,scale=beta1)  # Posterior
plot(lambda,postDens,type = "l")

credibleintervallower = qgamma(0.05,shape=alpha1,scale=beta1)
credibleintervalhigher= qgamma(0.95,shape=alpha1,scale=beta1)


#normLikC=dgamma(lambda,shape=1+carcount,            
#                scale=1/interval)                    #normalize likly hood
#lines(lambda,normLikC,col="red")

#b
totalexpactcar=0
expectcar=dgamma(0:5,shape=alpha1,scale=beta1)*11 #next 11 time period 
for(x in 1:5){
  totalexpactcar=x*expectcar[x+1]+totalexpactcar
}
totalexpactcar
#expectcar[6]=(1-pgamma(5,shape=alpha1,scale=beta1))*11

#What type of distribution is it?
#it is gamma distribution
#alpha = 17
#beta = 0.09999


barplot(expectcar,main="Distribution of Arrivals per 15-second Block", 
        xlab="Number of Cars (in 15-second Block)", 
        ylab="Empirical Count (of 15-second Blocks)", col="lightblue", 
        border=c("darkblue"),names=c(0:4,"5+"), beside=TRUE,
        legend=c("Expected"))

#c

passcar=10:30
mean=passcar/11  #10 event to 30 in 11 interval
prober=dgamma(mean,shape=alpha1,scale=beta1)/sum(dgamma(mean,shape=alpha1,scale=beta1))  # Posterior
sum(prober)
barplot(prober,main="Distribution of Arrivals 10 to 30 per 15-second Block", 
        xlab="Number of Cars (in 15-second Block)", 
        ylab="probability", col="lightblue", 
        border=c("darkblue"),names=c(10:30), beside=TRUE,
        legend=c("Expected"))

#d
totalexpactcar=0
possramda = alpha1*beta1*11              # Posterior mean

posiexpect = dpois(10:30,possramda)/sum(dpois(10:30,possramda))
sum(posiexpect)

barplot(posiexpect,main="Distribution of Arrivals 10 to 30 per 15-second Block", 
        xlab="Number of Cars (in 15-second Block)", 
        ylab="probability", col="lightblue", 
        border=c("darkblue"),names=c(10:30), beside=TRUE,
        legend=c("Expected"))



#e




