\name{dirttee-package}
\alias{dirttee-package}
\alias{dirttee}
\docType{package}
\title{
DIstributional Regression for Times To EvEnt
}
\description{
This package includes regession methods for right-censored response variables.
It allows for the estimation of distributional regression methods with semiparametric predictors,
including, for example, nonlinear, spatial or random effects.
The distribution of the response can be estimated with expectiles, quantiles and mode regression.
Censored observations can be included with accelerated failure time models or inverse probability of censoring weights.
}
\details{
\tabular{ll}{
Package: \tab dirttee\cr
Type: \tab Package\cr
Version: \tab 1.0-0\cr
Date: \tab 2022-08-15\cr
License: \tab GPL (>= 2) \cr
}
}

\author{
Alexander Seipp, Fabian Otto-Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de/eub} \cr

Maintainer: Alexander Seipp <alexander.seipp@uni-oldenburg.de>

Special thanks for their help go to Lisa Eilers and Florian Berger!

Partially funded by the German Research Foundation (DFG) grant SO1313/1-1, project 'Distributional Regression for Time-to-Event Data'.
}
\references{
Seipp A, Uslar V, Weyhe D, Timmer A, Otto-Sobotka F (2021)
\emph{ Weighted expectile regression for right-censored data }
Statistics in Medicine, 40(25), 5501-5520


Seipp A, Uslar V, Weyhe D, Timmer A, Otto-Sobotka F (2022)
\emph{ Flexible semiparametric mode regression for time-to-event data }
(under review)
}

\keyword{ package }
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }
\keyword{ survival }

\seealso{
\code{\link[expectreg:expectreg-package]{expectreg}}, \code{\link[gamlss:gamlss-package]{gamlss}}, \code{\link[flexsurv:flexsurv-package]{flexsurv}}
}
\examples{

\donttest{
data(colcancer)
c100 <- colcancer[1:100,]

#mode regression
reg <- modreg(Surv(logfollowup, death) ~ sex + LNE, data = c100)

#expectile regression
fit_exp <- expectreg.aft(Surv(logfollowup, death) ~ LNE, data = c100,smooth="f")
fit_expipc <- expectreg.ipc(Surv(logfollowup, death) ~ sex + LNE, data = c100)

#quantile regression
qu1 <- qureg.aft(Surv(logfollowup, death) ~ sex + LNE, data=c100, smooth="fixed")
}



}
