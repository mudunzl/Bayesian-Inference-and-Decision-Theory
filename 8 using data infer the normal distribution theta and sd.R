setwd("D:/學校相關/SYST664/7")
reaction <- read.table ("reaction.txt", header=F) 
#1
bar <- do.call(cbind, lapply(reaction, as.numeric))
bar=log(bar)
apply(bar,1,shapiro.test)
par(mfrow=c(3,2))
qqnorm(bar[6,])
qqline(bar[6,])
qqnorm(bar[7,])
qqline(bar[7,])
qqnorm(bar[8,])
qqline(bar[8,])
qqnorm(bar[9,])
qqline(bar[9,])
qqnorm(bar[10,])
qqline(bar[10,])
qqnorm(bar[11,])
qqline(bar[11,])
#2
meanRTime=rowMeans(bar)
sdRtime = apply(bar,1,sd)
rhoVRtime = sdRtime^-2

MeanrhoRtime=mean(rhoVRtime)   #alpha*beta estimate
VarrhoRtime=sd(rhoVRtime)^2     #alpha*beta^2 estimate

#prior beta
betaRtime = VarrhoRtime/MeanrhoRtime
#prior alpha
alphaRtime = MeanrhoRtime/betaRtime
#prior mu
muRtime=sum(bar)/(length(bar[1,])*length(bar[,1]))
#prior k 
MeanlogRtime= rowMeans(bar)
RhoMeanlogRtime= (sd(MeanlogRtime)^-2)/MeanrhoRtime

postalpha=0
postbeta=0
postmu=0
postk=0
postspread=0
x=1
for(x in 1:length(bar[,1])){
  postmu[x]= (RhoMeanlogRtime*muRtime+(length(bar[x,])*meanRTime[x]))/(RhoMeanlogRtime+length(bar[x,]))
  postk[x]=RhoMeanlogRtime+length(bar[x,])
  postalpha[x]=alphaRtime+0.5*length(bar[x,])
  postbeta[x]=((betaRtime^-1)+(0.5*sum((bar[x,]-meanRTime[x])^2))+(RhoMeanlogRtime*length(bar[x,])*(meanRTime[x]-muRtime)^2)/(2*(RhoMeanlogRtime+length(bar[x,]))))^-1
  postspread[x]=1/(postk[x]*postalpha[x]*postbeta[x])^0.5
  
}
postalpha
postbeta
postk
postmu
postspread
#3

theta025=0
theta975=0
rho025=0
rho975=0
xbar=seq(length=101,from=0.01,to=0.99)
for(x in 1:length(reaction[,1])){
theta025[x]=postmu[x]+qt(0.025,2*postalpha[x])/sqrt(postk[x]*postalpha[x]*postbeta[x])  # 0.025 quantile for mean
theta975[x]=postmu[x]+qt(0.975,2*postalpha[x])/sqrt(postk[x]*postalpha[x]*postbeta[x])
rho025[x]=qgamma(0.025,postalpha[x],scale=postbeta[x])            
rho975[x]=qgamma(0.975,postalpha[x],scale=postbeta[x])  
}
theta025
theta975
rho025
rho975
xbar=seq(length=101,from=5.2,to=6.5)
xbar2=seq(length=101,from=10,to=90)
thetadis=array(0,c(11,101))
rhodis=array(0,c(11,101))

for(x in 1:length(reaction[,1])){
  thetadis[x,]<-dt((xbar-postmu[x])/postspread[x],df=postalpha[x]*2)/postspread[x]
  rhodis[x,]=dgamma(xbar2,shape = postalpha[x],scale = postbeta[x])
}
plot(xbar2,rhodis[1,],type='l',ylim=c(0,0.1),xlab = 'rho')
for(x in 2:11){
  lines(xbar2,rhodis[x,])
}
plot(xbar,thetadis[1,],type = 'l',xlab='theta')
for(x in 2:11){
  lines(xbar,thetadis[x,])
}
min(theta025)
max(theta975)
min(rho025^-0.5)
max(rho975^-0.5)
