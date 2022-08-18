bicfun.aft <-
function(penalty,yy,delta,B,quantile,DD,nb,constmat,likfun)
{
  aa <- asyregpen.aft(yy,delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

  score = -2*likfun(c(aa$sigma,aa$a),quantile,yy,delta,B,aa$K) + log(length(yy))*(1+sum(aa$diag.hat.ma))

  score
}
