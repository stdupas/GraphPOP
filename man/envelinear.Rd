% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphPOP.R
\name{envelinear}
\alias{envelinear}
\title{Envelope linear reaction norm}
\usage{
envelinear(X, p)
}
\arguments{
\item{X}{Numeric. Present value for the parameter.}

\item{p}{Numeric. Lower and higher limit for the reaction norm. \code{p} must have two values.}
}
\value{
Numeric. Value between 0 and 1.
}
\description{
An envelope linear reaction norm. Its value is 0 when \code{X} its above or below the range marked by \code{p}. The result value is 0 when \code{X = p[1]} and 1 when \code{X = p[2]}, and the resulting diagonal straight line between \verb{(p[1],0)} and \verb{(p[2],1)}.
}
