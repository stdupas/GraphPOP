% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphPOP.R
\name{conQuadratic}
\alias{conQuadratic}
\title{conQuadratic reaction norm}
\usage{
conQuadratic(X, p)
}
\arguments{
\item{X}{Numeric. Present value for the parameter.}

\item{p}{Numeric. Lower and higher limit for the reaction norm. \code{p} must have two values.}
}
\value{
Numeric. Value between 0 and 1.
}
\description{
Quadratic reaction norm between the \code{p} parameter values and 0 outside.
}
