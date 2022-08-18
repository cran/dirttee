predict.modreg <-
function(object, ...){
  args <- list(...)
  args$object <- object$reg
  do.call(mgcv::predict.gam, args)
}
