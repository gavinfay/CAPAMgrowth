###########################
#' Plot growth increment figure for CAPAM presentation
#' @date 31st OCtober 2014
#' @author Gavin Fay
############################

M <- 0.2
maxage <- 20
numbers <- rep(NA,(maxage+1))
ages <- 0:maxage

numbers <- exp(-1*M*ages)
numbers[length(numbers)] <- 
  numbers[length(numbers)]/(1-exp(-1*M))
plot(numbers~ages)

linf <- 100
vbk <- 0.15
cvlenage <- 0.2
tzero <- 0

mulenage <- linf*(1-exp(-1*vbk*(ages-tzero)))
sdlenage <- cvlenage*mulenage
lolenage <- mulenage*qlnorm(0.025,meanlog=-0.5*cvlenage^2,sdlog=cvlenage)
hilenage <- mulenage*qlnorm(0.975,meanlog=-0.5*cvlenage^2,sdlog=cvlenage)
par(mfrow=c(1,1))
plot(mulenage~ages,type='l',lwd=3,ylim=c(0,linf*1.5))
polygon(c(ages,rev(ages)),c(lolenage,rev(hilenage)),col=gray(0.8),border=NA)
lines(mulenage~ages,lwd=3)

num.recaps <- 50
release.age <- 5
recap.ages <- sample((release.age+1):maxage,num.recaps,replace=TRUE,
                     prob=numbers[(release.age+2):(maxage+1)])
release.ages <- rep(release.age,length(recap.ages))
release.lengths <- 
  mulenage[which(ages==release.age)]*rlnorm(num.recaps,meanlog=-0.5*cvlenage^2,
                                            sdlog=cvlenage)
recap.lengths <- 
  mulenage[recap.ages+1]*rlnorm(num.recaps,meanlog=-0.5*cvlenage^2,
                                            sdlog=cvlenage)

#recap.lengths <- 
#  release.lengths+(release.lengths-linf)*(exp(-1*vbk*(recap.ages-release.ages
#    ))-1)*rlnorm(num.recaps,meanlog=-0.5*cvlenage^2,sdlog=cvlenage)

for (i in 1:num.recaps)
{
  pilinf <- plnorm(release.lengths[i],
  meanlog=log(mulenage[release.ages[i]])+0.5*cvlenage^2,sdlog=cvlenage)
  ilinf <- linf*qlnorm(pilinf,meanlog=-0.5*cvlenage^2,sdlog=cvlenage)
  recap.lengths[i] <- ilinf*(1-exp(-1*vbk*(recap.ages[i]-tzero)))
}

#par(mfrow=c(2,2))
#hist(release.lengths)
#hist(recap.lengths)
par(mfrow=c(1,1))
plot(mulenage~ages,type='l',lwd=3,ylim=c(0,linf*1.5))
polygon(c(ages,rev(ages)),c(lolenage,rev(hilenage)),col=gray(0.8),border=NA)
lines(mulenage~ages,lwd=3)
for (i in 1:num.recaps)
 if (release.lengths[i]>50)
  lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],recap.lengths[i]),
        lwd=0.5,col=gray(0.4))
for (i in 1:num.recaps)
    lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],recap.lengths[i]),
          lwd=0.5,col=gray(0.4))

#for (i in 1:num.recaps)
#  if (release.lengths[i]<50)
#    lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],
#                                      recap.lengths[i]),lwd=0.5,col=gray(0.1))


len50 <- 50
len95 <- 65

selex.release <- 1./(1+exp(-1*log(19)*(release.lengths-len50)/(len95-len50)))
selex.recap <- 1./(1+exp(-1*log(19)*(recap.lengths-len50)/(len95-len50)))

is.release.sel <- rbinom(num.recaps,size=1,prob=selex.release)
is.recap.sel <- rbinom(num.recaps,size=1,prob=selex.recap)
is.recap.sel[is.release.sel==0] <- 0

#points and lines for all tag releases/recaptures
par(mfrow=c(1,1),oma=c(2,2,0,0),mar=c(2,2,2,2))
plot(mulenage~ages,type='l',lwd=3,ylim=c(0,linf*1.5),xlab="Age",ylab="Length")
polygon(c(ages,rev(ages)),c(lolenage,rev(hilenage)),col=gray(0.8),border=NA)
lines(mulenage~ages,lwd=3)
for (i in 1:num.recaps)
{
  #if (release.lengths[i]>50)
  #points(release.ages[i],release.lengths[i],col="orange",pch=1)  
  lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],recap.lengths[i]),
          lwd=1.5,col=gray(0.4))
 points(release.ages[i],release.lengths[i],col=gray(0.4),pch=16,lwd=2.5,cex=0.25)
 points(recap.ages[i],recap.lengths[i],col=gray(0.4),pch=16,lwd=2.5,cex=0.25)  
  }

#point color
tempcol<-col2rgb("orange")/255
col.use <- rgb(tempcol[1,1],tempcol[2,1],tempcol[3,1],alpha=0.7)

#plot only those that are selected
par(mfrow=c(1,1),oma=c(2,2,0,0),mar=c(2,2,2,2))
plot(mulenage~ages,type='l',lwd=3,ylim=c(0,linf*1.5),xlab="Age",ylab="Length")
polygon(c(ages,rev(ages)),c(lolenage,rev(hilenage)),col=gray(0.8),border=NA)
lines(mulenage~ages,lwd=3)
for (i in 1:num.recaps)
{
  #points(release.ages[i],release.lengths[i],col=gray(0.4),pch=1)
  #points(recap.ages[i],recap.lengths[i],col=gray(0.4),pch=1)    
  if (is.recap.sel[i]==0)
    {lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],recap.lengths[i]),
        lwd=1.5,col=gray(0.4))    
    points(release.ages[i],release.lengths[i],col=gray(0.4),pch=16,lwd=2.5,cex=0.25)
    points(recap.ages[i],recap.lengths[i],col=gray(0.4),pch=16,lwd=2.5,cex=0.25)  
  }
  if (is.recap.sel[i]==1)
  {
#  points(release.ages[i],release.lengths[i],col=col.use,pch=1)  
  lines(c(release.ages[i],recap.ages[i]),c(release.lengths[i],recap.lengths[i]),
        lwd=1.5,col=col.use)
  points(release.ages[i],release.lengths[i],col=col.use,pch=1,lwd=2.5)
  points(recap.ages[i],recap.lengths[i],col=col.use,pch=1,lwd=2.5)  
  }
}

selex <- c(seq(0.1,0.9,0.1),0.99)
selex <- c(seq(0.25,0.75,0.25),0.95)

plot.lens <- len50-1*log((1/selex)-1)*(len95-len50)/log(19)

for (i in 1:length(selex))
 {
  text(0,plot.lens[i],selex[i],cex=0.7)
  lines(c(0.5,1.25),rep(plot.lens[i],2),lwd=0.5)
 }
plot.lens <- len50-1*log((1/0.99)-1)*(len95-len50)/log(19)
text(0,plot.lens,"Sel",cex=0.7)


