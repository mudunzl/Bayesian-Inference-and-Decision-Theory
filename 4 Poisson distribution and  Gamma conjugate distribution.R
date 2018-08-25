
# SYST 664 / CSI 674 Spring 2016
# Assignment 4 Problem 1-a
# Analysis of automobile arrivals

NumberofOccu=c(3,5,7,3,3)
alpha=1
beta=10000
#newalpha=alpha+sum(NumberofOccu)
newalpha=alpha+40
newbeta=beta/(1+(sum(NumberofOccu)*beta))
posteriorRamda=dgamma(NumberofOccu,newalpha,1/newbeta)
xaxis=0:(length(NumberofOccu)-1)



barplot(posteriorRamda,main=expression("Posterior Distribution for"~Lambda),
        col=c("mediumpurple"), border=c("purple4"),
        xlab=expression(lambda), ylab="Probability",
        names=xaxis)



#1-b
#mean
meanv=newalpha*newbeta
meanv
#standard diviation
sd=(newalpha*newbeta^2)^0.5
sd
#mediam
mediam=qgamma(0.5,newalpha,1/newbeta)
mediam
#mode
modev=(newalpha-1)*newbeta
modev

#1-c
lefttail=qgamma(0.025,newalpha,1/newbeta)
righttail=qgamma(0.975,newalpha,1/newbeta)

#3

#Alpha = 3.668045
#Beta = 1.12254
NumberofOccu=c(3,5,7,3,3)
alpha=3.668045
beta=1.12254
#newalpha=alpha+sum(NumberofOccu)
newalpha=alpha+40
newbeta=beta/(1+(sum(NumberofOccu)*beta))
posteriorRamda=dgamma(NumberofOccu,newalpha,1/newbeta)
#posteriorRamda=dgamma(1:10,newalpha,1/newbeta)

xaxis=0:(length(NumberofOccu)-1)



barplot(posteriorRamda,main=expression("Posterior Distribution for"~Lambda),
        col=c("mediumpurple"), border=c("purple4"),
        xlab=expression(lambda), ylab="Probability",
        names=xaxis)

#mean
meanv=newalpha*newbeta
meanv
#standard diviation
sd=(newalpha*newbeta^2)^0.5
sd
#mediam
mediam=qgamma(0.5,newalpha,1/newbeta)
mediam
#mode
modev=(newalpha-1)*newbeta
modev

#3-c
lefttail=qgamma(0.025,newalpha,1/newbeta)
righttail=qgamma(0.975,newalpha,1/newbeta)
#3-D
#4

datax=seq(from=1,to=21,by=1)
posteriorRamda= dgamma(datax,newalpha,1/newbeta)
plot(posteriorRamda,type = "l",col="red",lwd="5",xlab = "Number of Cars", ylab = "probability")
pri=dgamma(datax,3.668045,1/1.12254)
#plot(dgamma(datax,3.668045,1/1.12254),type = "l")
lines(pri,col="green",lwd="5")
poisrate=40/sum(NumberofOccu)
liklyhood=dpois(datax,poisrate)

normalizelikly=liklyhood/sum(liklyhood)
lines(normalizelikly,lwd="5")

legend("topright",                                # 
       lty = 1,                                   # 
       lwd = 5,
       col = c("green", "red", "black"),           # col
       legend = c("Prior", "posterior", "Normalize Liklyhood") #
)




#----
