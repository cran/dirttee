% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.modreg.R
\name{plot.modreg}
\alias{plot.modreg}
\title{Plot regression terms for modreg objects}
\usage{
\method{plot}{modreg}(x, ...)
}
\arguments{
\item{x}{The object to plot, must be of class modreg.}

\item{...}{Additional arguments to pass to \code{\link[mgcv]{plot.gam}}.}
}
\value{
The functions main purpose is its side effect of generating plots. It also silently returns a list of the data used to produce the plots, which can be used to generate customized plots.
}
\description{
Plots smooth components of a fitted modreg object.
}
\details{
This function is a wrapper for \code{\link[mgcv]{plot.gam}}. It displays term plots of smoothed variables. Optionally produces term plots for parametric model components as well. Standard errors will not be displayed but can be estimated by \code{boot_modreg}.
}
\examples{

data(colcancer)
# mode regression with P-splines. Convergence criteria are changed to speed up the function
reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer[1:70, ], 
control = modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, tol = 10^-3))
plot(reg)


}
