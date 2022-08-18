plot.modreg <-
function(x, ...){
  args    <- list(...)
  args$x  <- x$reg
  args$se <- FALSE
  do.call(mgcv::plot.gam, args)
}
