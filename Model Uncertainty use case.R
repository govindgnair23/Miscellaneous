betaABfromMeanSD = function( mean , sd ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( sd <= 0 ) stop("sd must be > 0")
  kappa = mean*(1-mean)/sd^2 - 1
  if ( kappa <= 0 ) stop("invalid combination of mean and sd")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

betaABfromModeKappa = function( mode , kappa ) {
  if ( mode <=0 | mode >= 1) stop("must have 0 < mode < 1")
  if ( kappa <=2 ) stop("kappa must be > 2 for mode parameterization")
  a = mode * ( kappa - 2 ) + 1
  b = ( 1.0 - mode ) * ( kappa - 2 ) + 1
  return( list( a=a , b=b ) )
}

par(mfrow=c(2,1))
d1 <- betaABfromModeKappa(0.58,100)
d2 <- betaABfromModeKappa(0.58,10000)

b1 <- rbeta(10000,d1$a,d1$b)
b2 <- rbeta(10000,d2$a,d2$b)
par(mfrow=c(1,2))
plot(density(b1),xlim=c(0.4,0.8),main="",xlab="")
abline(v = 0.58,col='red')
plot(density(b2),xlim= c(0.4,0.8),main="",xlab="")
abline(v = 0.58,col='red')

##Probability mass greater than 0.6
sum(b1>= 0.6)/length(b1)
##Probability mass greater than 0.6
sum(b2>= 0.6)/length(b2)


########Use ggplot##########
##Use Case 1
df <- data.frame(Event = as.factor(rep(c(1,2),each=10000)),N = c(b1,b2))
library(ggplot2)

ggplot(data=df,aes(x = N, fill = Event)) + geom_density(alpha = 0.3) +geom_vline(xintercept = 0.58,col='red')

##Use Case 2
d1 <- betaABfromModeKappa(0.6,100)
d2 <- betaABfromModeKappa(0.58,10000)

b1 <- rbeta(10000,d1$a,d1$b)
b2 <- rbeta(10000,d2$a,d2$b)
df <- data.frame(Event = as.factor(rep(c(1,2),each=10000)),N = c(b1,b2))

ggplot(data=df,aes(x = N, fill = Event)) + geom_density(alpha = 0.3) +geom_vline(xintercept = c(0.58,0.6),
                                                                                 col=c('blue','red'))

