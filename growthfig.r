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

num.recaps <- 100
release.age <- 5
recap.ages <- sample((release.age+1):maxage,num.recaps,replace=TRUE)
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


