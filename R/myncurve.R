#' This function will display the curve, shaded area between the curve and x axis from -∞ to x=a, and calculate the area (probability, P(X<=a)) which is released to the command-line in a lis
#' @param mu    the mean
#' @param sigma the standard variation
#' @param a     x value
#' @return      shaded area between the curve and x axis from -∞ to x=a
#' @examples
#' myncurve(mu=10,sigma=5,a=6)
#' @export
myncurve = function(mu, sigma, a){
  # graph itself
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu - 3*sigma,a,length=1000)
  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  # Fill in the polygon with the given vertices
  polygon(c(mu - 3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  # Put in the text with the appropriate area
  # Area
  prob=1-pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  # Click to paste the text onto the graph
  text(0,0.04, paste("Area = ",prob, sep=""))

}

