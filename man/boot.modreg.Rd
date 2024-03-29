% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot.modreg.R
\name{boot.modreg}
\alias{boot.modreg}
\title{Estimate confidence intervals and standard errors for the mode regression fit}
\usage{
boot.modreg(
  reg,
  nboot,
  level = 0.95,
  newdata = NULL,
  bw = c("variable", "fixed"),
  quiet = FALSE,
  terms = NULL,
  seed = NULL
)
}
\arguments{
\item{reg}{an object of class modreg (output of the modreg function)}

\item{nboot}{number of bootstrap replications}

\item{level}{confidence level}

\item{newdata}{Should be a data frame containing all the variables needed for predictions. If supplied, confidence intervals are calculated for the corresponding predictions.}

\item{bw}{Either "\code{variable}" or "\code{fix}", determining if the bandwidth of the 
original fit should be used for the bootstrap fits (\code{fix}) or if the bandwith 
should be recalculated (\code{variable}).}

\item{quiet}{if TRUE, printing of the status is suppressed}

\item{terms}{character scalar. If supplied, uses this term for confidence intervals of the prediction}

\item{seed}{the seed to use}
}
\value{
a list with the following elements
\item{confpredict}{data frame, the confidence intervals for the predictions.}
\item{confparams}{data frame, the confidence intervals and standard errors for the parametric regression coefficients.}
\item{level}{confidence level}
\item{na}{scalar, stating the number of NA bootstrap repetitions.}
\item{seed}{scalar, the used seed.}
}
\description{
Performs bootstrap on the modreg object.
}
\details{
A nonparametric residual bootstrap is performed to calculate standard errors of parameters and confidence intervals. More details can be found in Seipp et al. (2022).
\code{newdata} can be supplied to get confidence intervals for specific predictions. \code{terms} can be specified to calculate confidence interval for the contribution of one covariate (useful for P-splines).
\code{variable} bandwidth is the default, which has higher coverage than \code{fix}, but is computationally much more demanding. A \code{seed} can be supplied to guarantee a reproducible result.
}
\examples{

\donttest{
data(colcancer)
colcancer80 <- colcancer[1:80, ]

# linear mode regression
regL <- modreg(Surv(logfollowup, death) ~ sex + age, data = colcancer80)

# bootstrap with a fixed bandwidth and 3 iterations, chosen to speed up the function. 
# Should in practice be much more than 3 iterations.
btL <- boot.modreg(regL, 3, bw = "fixed", level = 0.9, seed = 100)

# coefficients, SE and confidence intervals
cbind(coef(regL), btL$confparams)


## confidence inverval for smooth effect / predictions

reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer80, 
              control = modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, tol = 10^-3))
ndat <- data.frame(sex = rep(colcancer80$sex[1], 200), age = seq(50, 90, length = 200))

# iterations should in practice be much more than 2!
bt <- boot.modreg(reg, 2, bw = "fixed", newdata = ndat, terms = "s(age)", seed = 100)

pr <- predict(reg, newdata = ndat, type = "terms", terms = "s(age)")[, 1]

plot(ndat$age, pr, ylim = c(-0.75, 1.5), type = "l", xlab = "age", ylab = "s(age)")
lines(ndat$age, bt$confpredict$lower, lty = 2)
lines(ndat$age, bt$confpredict$upper, lty = 2)
}



}
\references{
Seipp, A., Uslar, V., Weyhe, D., Timmer, A., & Otto-Sobotka, F. (2022). Flexible Semiparametric Mode Regression for Time-to-Event Data. Manuscript submitted for publication.
}
