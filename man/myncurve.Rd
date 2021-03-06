% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{This function will display the curve, shaded area between the curve and x axis from -∞ to x=a, and calculate the area (probability, P(X<=a)) which is released to the command-line in a lis}
\usage{
myncurve(mu, sigma, a)
}
\arguments{
\item{mu}{the mean}

\item{sigma}{the standard variation}

\item{a}{x value}
}
\value{
shaded area between the curve and x axis from -∞ to x=a
}
\description{
This function will display the curve, shaded area between the curve and x axis from -∞ to x=a, and calculate the area (probability, P(X<=a)) which is released to the command-line in a lis
}
\examples{
myncurve(mu=10,sigma=5,a=6)
}
