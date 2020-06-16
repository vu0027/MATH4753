#' Lab 5
#'
#' Summary lab 5
#' @param iter
#' @param n
#' @param p
#'
#' @return function result
#' @export
#'
#' @examples
#'
mybin=function(iter=100,n=10, p=0.7){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
# 100 iteration
mybin(iter=100,n=10, p=0.7)
# 200 iteration
mybin(iter=200,n=10, p=0.7)
# 500 iteration
mybin(iter=500,n=10, p=0.7)
# 1000 iteration
mybin(iter=1000,n=10, p=0.7)
# 10000 iteration
mybin(iter=10000,n=10, p=0.7)


dbinom(1:10,10,0.7)

#Task 3
# vector for 12 is "1" and 8 is "0"
marbles=c(rep("1",12), rep("0",8))
#replace=FALSE
sample(marbles,size=5, replace=FALSE)
#replace=TRUE
sample(marbles,size=5, replace=TRUE)



myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}

myhyper(iter=100,N=20,r=12,n=5)
myhyper(iter=200,N=20,r=12,n=5)
myhyper(iter=500,N=20,r=12,n=5)
myhyper(iter=1000,N=20,r=12,n=5)
myhyper(iter=10000,N=20,r=12,n=5)


myhyper(iter=10000,N=20,r=12,n=5)

dhyper(x=0:19, m=12, n=8, k=5)


# Task 4
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}

mysample(n=1000, iter=30,time=1)


mysample(n=1000, iter=1,time=1)


# Task 5
## 8 choose 4
choose(8,4)


## P(Y>4), Y~Pois($\lambda$=2)
1-ppois(4,2)

## P(Y=10),Y~NegBin(p=0.4,r=3)

#P(Y=10), Y~NegBin(p=0.4,r=3).   (Book theory)
#  This means that in R we need n=3 (size), x=y-r=10-3=7
dnbinom(7,3,0.4)  # Nu Failures, Nu of successes, prob success
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
mynbin(10,3,0.4)


pbinom(8,15,0.4)


