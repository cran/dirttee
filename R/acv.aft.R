acv.aft <-
function(penalty,yy,B,quantile,DD,nb,constmat,delta)
# asymmetric cross validation
# computes the acv score for the smoothing of the regression
# score has to be minimized dependant on parameter "penalty"
# therefore a grid search can be applied to this function
# parameters:
# penalty - smoothing parameter lambda
# yy - vector of responses
# B - basis for the approximation
# p - quantile
# DD - penalization matrix
{
  aa <- asyregpen.aft(yy, B, quantile, abs(penalty), DD, nb, constmat,likeli.asynorm)

  #H = solve(t(B)%*%(aa$weight*B) + penalty*t(DD)%*%DD)
  ##H = B%*%H%*%t(B)
  ##H = diag(H)*aa$weight
  #H = apply(sqrt(aa$weight)*B,1,function(x){t(x)%*%H%*%x})

  #H = diag(diag(aa$diag.hat.ma))

  score = -(delta * log(dasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)) + (1-delta) * log(1-pasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)))/(1-aa$diag.hat.ma)^2

  mean(score[which(is.finite(score))],na.rm=TRUE)
}
