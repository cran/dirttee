aicfun.aft <-
function(penalty,yy,delta,B,quantile,DD,nb,constmat,likfun)
{
  aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

#print(dim(B))
#print(length(aa$a1))

  score = -2*likfun(c(aa$sigma,aa$a),quantile,yy,delta,B,aa$K) + 2*(1+sum(aa$diag.hat.ma))

  score
}
