modreg.control <-
function(
  StartInterval = sqrt(3),
  nStart = 11,
  nInterim = NULL,
  maxit = 100,
  itInterim = 10,
  tol = 10^-4,
  tol_bw_plugin = 10^-3,
  maxit_bw_plugin = 10,
  maxit_penalty_plugin = 10,
  tol_penalty_plugin = 10^-3,
  tol_regopt = tol * 100,
  tol_opt = 10^-3,
  maxit_opt = 200,
  tol_opt2 = 10^-3,
  maxit_opt2 = 200
){
  obj <- list(  StartInterval = StartInterval,
                nStart = nStart,
                nInterim = nInterim,
                maxit = maxit,
                itInterim = itInterim,
                tol = tol,
                tol_bw_plugin = tol_bw_plugin,
                maxit_bw_plugin = maxit_bw_plugin,
                maxit_penalty_plugin = maxit_penalty_plugin,
                tol_penalty_plugin = tol_penalty_plugin,
                tol_regopt = tol_regopt,
                tol_opt = tol_opt,
                maxit_opt = maxit_opt,
                tol_opt2 = tol_opt2,
                maxit_opt2 = maxit_opt2)
  class(obj) <- "modreg_control"
  return(obj)
}
