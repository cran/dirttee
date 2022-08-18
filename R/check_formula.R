check_formula <-
function(formula, expect){
  vars <- rhs.vars(formula)
  for(v in seq_along(vars)){
    str <-strsplit(vars[[v]], "")[[1]]
    if(any(str=="(")){
      if(!expect){
        if(which(str=="(") != 2 | all(!str[1] %in%  c("s","I")))stop('Please use the s function from the splines package to implement splines.')
        cargs <- call_args(str2lang(vars[[v]]))
        if(str[1]=="s" & (!any(names(cargs) == "bs") | cargs$bs != "ps"))stop("Please use s function from the splines package with the argument: bs = \"ps\".")
      }else{
        if(which(str=="(") != 3 | all(!str[1:2] == c("r","b")))stop('Please use the rb function of the expectreg package to implement splines.')
      }
    }
  }
}
