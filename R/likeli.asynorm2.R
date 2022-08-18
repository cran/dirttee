likeli.asynorm2 <-
function(par,tau,ycens,delta,X,K)
  { delta * log(dasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau)) + (1-delta) * log(1-pasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau)) + rep(t(par[-1])%*%K%*%par[-1],length(ycens))/length(ycens)}
