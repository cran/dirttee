summary.modreg <-
function(object, ...){
    
    out <- list(
      formula = deparse(object$called$formula),
      coefficients = summary(object$reg)$p.coeff,
      bw = object$bw,
      aic = object$aic,
      edf = object$edf,
      pseudologlik = object$pseudologlik,
      converged = object$converged,
      bw_method = object$called$bw,
      hp_status = object$hp_opt$status,
      hp_iterations = object$hp_opt$iterations
    )
  
    out[["iterations"]] <- object$iterations
    
    if(object$called$bw == "Plugin"){
      out$iterations_plugin <- object$iterations_plugin
      if(!is.null(object$cova)) out$SE <- sqrt(diag(object$cova))
    }
   
    class(out) <- "summary.modreg"
    return(out)

  }
