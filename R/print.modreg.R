print.modreg <-
function(x, ...){
  cat("\nFormula:\n", deparse(x$called$formula), "\n", sep = "")
  cat("\nCoefficients:\n")
  print(summary(x$reg)$p.coeff, ...)
  
  cat("\nBandwidth type:", x$called$bw)
  if(!is.null(x$lambda))cat('\nLambda: ', x$lambda)
}
