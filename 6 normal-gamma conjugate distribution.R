#1
surfacedata=c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48)
bottomdata=c(5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)

mu0 = 0
k0=0
alpha0= -0.5
beta0=Inf


mus1=(k0*mu0+sum(surfacedata))/(k0+10)
mub1=(k0*mu0+sum(bottomdata))/(k0+10)
k1=k0+length(surfacedata)
alphas1=alpha0+length(surfacedata)/2
alphab1=alpha0+length(bottomdata)/2
betas1=( 1/beta0 + 0.5*sum((surfacedata-mean(surfacedata))^2) + 
              0.5*k0*10/(k0+10)*(mean(surfacedata)-mu0)^2 )^-1
betab1=( 1/beta0 + 0.5*sum((bottomdata-mean(bottomdata))^2) + 
            0.5*k0*5/(k0+10)*(mean(bottomdata)-mu0)^2 )^-1

spreads=1/sqrt(k1*alphas1*betas1)
spreadb=1/sqrt(k1*alphab1*betab1)


xbar=seq(length=200,from=3,to=9)
pred.known.vars=dt((xbar-mus1)/spreads,df=alphas1*2)/spreads  
pred.known.varb=dt((xbar-mub1)/spreadb,df=alphab1*2)/spreadb  

plot(xbar,pred.known.vars,type="l",col="blue",ylim=c(0,2.0),xlim=c(3,9),
     main=paste("Predict Obs - Unknown Variance"),
     xlab="theta",
     ylab="Probability Density")

lines(xbar,pred.known.varb,col= 'red')

# Posterior credible intervals for mean, variance, precision and standard deviation
# Note: NaNs produced for prior quantiles
theta025=mub1+qt(0.025,2*alphab1)/sqrt(k1*alphab1*betab1)  # 0.025 quantile for mean
theta975=mub1+qt(0.975,2*alphab1)/sqrt(k1*alphab1*betab1)  # 0.975 quantile for mean
rho025=qgamma(0.025,alphab1,scale=betab1)             # 0.025 quantile for precision
rho975=qgamma(0.975,alphab1,scale=betab1)             # 0.975 quantile for precision
var025=1/rho975                                   # 0.025 quantile for variance
var975=1/rho025                                   # 0.975 quantile for variance
std025=sqrt(var025)                               # 0.025 quantile for std dev
std975=sqrt(var975)                               # 0.975 quantile for std dev

thetas025=mus1+qt(0.025,2*alphas1)/sqrt(k1*alphas1*betas1)  # 0.025 quantile for mean
thetas975=mus1+qt(0.975,2*alphas1)/sqrt(k1*alphas1*betas1)  # 0.025 quantile for mean
rhos025=qgamma(0.025,alphas1,scale=betas1)             # 0.025 quantile for precision
rhos975=qgamma(0.975,alphas1,scale=betas1)             # 0.975 quantile for precision
vars025=1/rho975                                   # 0.025 quantile for variance
vars975=1/rho025                                   # 0.975 quantile for variance
stds025=sqrt(var025)                               # 0.025 quantile for std dev
stds975=sqrt(var975)                               # 0.975 quantile for std dev


gs<-200
theta<-seq(0,6.5,length=gs)
is2<-seq(0,6 ,length=gs)       # precision
s2g<-seq(0.008,4,length=gs)    # variance

ld.th.is2<-array(0,dim=c(200,200))

for(i in 1:gs) { 
  for(j in 1:gs) {
    ld.th.is2[i,j]<- pred.known.vars[i]*pred.known.varb[j]
  }} 

min(ld.th.is2)


persp(theta,is2,ld.th.is2,theta=30,phi=5,xlab="surface",
      ylab="bottom",zlab="mean")
#      ,xlim=c(3,8) ,ylim=c(3,8)) 


#--------------------------------------------------------

#mub2=(k1*mus1+sum(bottomdata))/(k1+10)
#k2=k1+length(bottomdata)
#alphab2=alphas1+length(bottomdata)/2
#betab2=( 1/betas1 + 0.5*sum((bottomdata-mean(bottomdata))^2) + 
#           0.5*k1*10/(k1+10)*(mean(bottomdata)-mus1)^2 )^-1

#spreadb2=1/sqrt((k2*10/(k2+10))*alphab2*betab2)
#xbar=seq(length=101,from=3,to=8)
#pred.known.varb2=dt((xbar-mub2)/spreadb2,df=alphab2*2)/spreadb2  

#plot(xbar,pred.known.varb2,type="l",col="blue",ylim=c(0,1.5),xlim=c(3,8),
#     main=paste("Predict Obs - Unknown Variance"),
#     xlab="Sample Mean",
#     ylab="Probability Density")





#2

# the xbar,k*,a*,b* is from the question 1 postorior of normal-gamma(mub2,k2,alphab2,betab2)


mustar=mub2
kstar = 10
astar = alphab2
bstar = betab2


#----
#test= c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48,5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)
#mean(test)
#bstar =1/(0.5*sum((test-mub2)^2))
#spread1 <- sqrt(1/(kstar*astar*bstar))
#spread1=1/sqrt((k2*10/(k2+10))*alphab2*betab2)
#xbar2=seq(length=101,from=3,to=8)
#stdValsb <- (xbar2 - mub2)/spread1
#test2 <- dt(stdValsb,df=2*alpha1)/spread1
#-----
mus1=(k0*mu0+sum(surfacedata))/(k0+10)
mub1=(k0*mu0+sum(bottomdata))/(k0+10)
k1=k0+length(surfacedata)
alphas1=alpha0+length(surfacedata)/2
alphab1=alpha0+length(bottomdata)/2
betas1=( 1/beta0 + 0.5*sum((surfacedata-mean(surfacedata))^2) + 
           0.5*k0*10/(k0+10)*(mean(surfacedata)-mu0)^2 )^-1
betab1=( 1/beta0 + 0.5*sum((bottomdata-mean(bottomdata))^2) + 
           0.5*k0*5/(k0+10)*(mean(bottomdata)-mu0)^2 )^-1

spreads=1/sqrt(k1*alphas1*betas1)
spreadb=1/sqrt(k1*alphab1*betab1)




xbar=seq(length=200,from=0,to=9)


numsim = 10000

pdmcs = rgamma(numsim,shape = alphas1,scale = betas1)
sigmadmc= 1/(pdmcs)^0.5
#mean(sigmadmc*((k2)^-0.5))
thetadmc= rnorm(numsim,mean=mus1,sigmadmc/sqrt(10))
plot(density(thetadmc),ylim=c(0,3),xlim=c(3,8),)

pdmcb = rgamma(numsim,shape = alphab1,scale = betab1)
sigmadmcb= 1/(pdmcb)^0.5
#mean(sigmadmc*((k2)^-0.5))
thetadmcb= rnorm(numsim,mean=mub1,sd=sigmadmcb/sqrt(10))
lines(density(thetadmcb),ylim=c(0,2),xlim=c(0,10),col='red')
#lines(xbar2,test2,col='red')
lines(xbar,pred.known.vars,col ='green')
lines(xbar,pred.known.varb,col= 'blue')

legend("topright",                                # 
       lty = 1,                                   # 
       lwd = 1,
       col = c("blue", "red",'black','green'),           # col
       legend = c("density function bottom", "direct monte cralo bottom",'direct monte cralo surface',"density function surface") #
)

quantile(thetadmc,c(0.05, 0.95))
quantile(thetadmcb,c(0.05, 0.95))

quantile(pdmcs,c(0.05,0.95))
quantile(pdmcb,c(0.05,0.95))

plot(1:numSim,(thetadmcb-thetadmc),main="",ylab="bottom and mu difference",xlab="Iteration")
#histogram(xG,xlab="Gibbs Simulated Body Length (mm)",ylab="Frequency")
acf((thetadmcb-thetadmc), main="")
effectiveSize(mub-mus)

theta005=mub1+qt(0.05,2*alphab1)/sqrt(k1*alphab1*betab1)  # 0.025 quantile for mean
theta095=mub1+qt(0.95,2*alphab1)/sqrt(k1*alphab1*betab1)  # 0.975 quantile for mean
rho005=qgamma(0.05,alphab1,scale=betab1)             # 0.025 quantile for precision
rho095=qgamma(0.95,alphab1,scale=betab1)             # 0.975 quantile for precision

thetas005=mus1+qt(0.05,2*alphas1)/sqrt(k1*alphas1*betas1)  # 0.025 quantile for mean
thetas095=mus1+qt(0.95,2*alphas1)/sqrt(k1*alphas1*betas1)  # 0.025 quantile for mean
rhos005=qgamma(0.005,alphas1,scale=betas1)             # 0.025 quantile for precision
rhos095=qgamma(0.95,alphas1,scale=betas1)             # 0.975 quantile for precision

#x=sort(thetadmc)
#e_cdf <- 1:length(x) / length(x)
#plot(x, e_cdf, type = "s")
#abline(h = 0.975, lty = 3)
#abline(h = 0.025, lty = 3)
#x[which(e_cdf >= 0.975)[1]]
#x[which(e_cdf >= 0.025)[1]]


#------
# 3


mu0 = 0
k0=0
alpha0= -0.5
beta0=Inf

surfacedata=c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48)
bottomdata=c(5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)
mus1=(k0*mu0+sum(surfacedata))/(k0+10)
mub1=(k0*mu0+sum(bottomdata))/(k0+10)
k1=k0+length(surfacedata)
alphas1=alpha0+length(surfacedata)/2
alphab1=alpha0+length(bottomdata)/2
betas1=( 1/beta0 + 0.5*sum((surfacedata-mean(surfacedata))^2) + 
           0.5*k0*10/(k0+10)*(mean(surfacedata)-mu0)^2 )^-1
betab1=( 1/beta0 + 0.5*sum((bottomdata-mean(bottomdata))^2) + 
            0.5*k0*5/(k0+10)*(mean(bottomdata)-mu0)^2 )^-1

#theta
psdmc= rgamma(10000,shape = alphas1,scale = betas1)
sigmasdmc= 1/(psdmc)^0.5
thetasdmc= rnorm(10000,mean=mus1,sd=sigmasdmc/sqrt(10))
plot(density(thetasdmc),ylim=c(0,2),xlim=c(0,10))


pbdmc= rgamma(10000,shape = alphab1,scale = betab1)
sigmabdmc= 1/(pbdmc)^0.5
thetabdmc= rnorm(10000,mean=mub1,sd=sigmabdmc/sqrt(10))
lines(density(thetabdmc),ylim=c(0,2),xlim=c(0,10),col='red')



diff=thetabdmc- thetasdmc

probRthetasdmcLower = sum(diff>0)/length(diff) #Proportion Plant 2 worse
probRthetasdmcLower
probRthetasdmcLower =sd(diff)

plot(density(diff),ylim=2)

#sd
diffsd=sigmasdmc-sigmabdmc
probsigmasdmcLower = sum(diffsd>0)/length(diffsd) #Proportion Plant 2 worse
probsigmasdmcLower
probRthetasdmcLower =sd(diffsd)
plot(density(diffsd))
lines(c(0,0),c(0,1.4),col='red')

#4

surfacedata=c(3.74,4.61,4,4.67,4.87,5.12,4.52,5.29,5.74,5.48)
bottomdata=c(5.44,6.88,5.37,5.44,5.03,6.48,3.89,5.85,6.85,7.16)
qqnorm(surfacedata)
qqline(surfacedata,col = 'red')

qqnorm(bottomdata)
qqline(bottomdata,col = 'red')

plot()
library(nortest)
shapiro.test(bottomdata)
#spreads=1/sqrt((k1*10/(k1+10))*alphas1*betas1)
#spreadb=1/sqrt((k1*10/(k1+10))*alphab1*betab1)




