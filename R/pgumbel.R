pgumbel <-
function(q, location = 0, scale = 1){
  if(any(scale <= 0)) stop("standard deviation must be strictly > 0.")
  1-exp(-exp((q - location)/scale))
}
