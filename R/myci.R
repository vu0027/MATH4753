#' 95% confidence interval generating function
#' Confident interval
#'
#' a function that will create a 95% confidence interval for mean x from a single sample x
#' @param x
#'
#' @return
#' confidence interval for the given vector
#' @export
#'
#' @examples
#' x = rnorm(30, mean = 10, sd = 12)
#' myci(x)
#
myci=function(x){
  size=length(x)
  t=qt(1-0.05/2,size-1)
  ci=c()
  left=mean(x)-t*sd(x)/sqrt(size)
  right=mean(x)+t*sd(x)/sqrt(size)
  sprintf("[%f, %f]", left,right)
}
