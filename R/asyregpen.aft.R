asyregpen.aft <-
function(y,delta, B, p, lambda, DD,nb, constmat,likfun=likeli.asynorm)
###### Asymmetric regression with difference penalty
# parameters:
# y - response variable
# B - B-spline basis
# p - asymmetry parameter
# lambda - smoothing parameter
# DD - difference matrix 
#
# needed: ncol(B) = ncol(DD)
{
  w1 <- 0 * y + 0.5
  n <- ncol(B)

  lambda = c(rep(0,times=n - sum(nb)),rep(lambda,times=nb))

  P <- sqrt(lambda) * DD
  augm <- rep(0, nrow(P))
  conpen <- rep(0,nrow(constmat))
  
  model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)

  #mod2 = maxLik(likeli.asynorm,start=c(sd(y),model$coefficients),method="NR",tau=p,ycens=y,delta=delta,X=B,K=t(P)%*%P,control=list(tol=1e-6,reltol=1e-6))
  #a1 = mod2$estimate
  

  
  K=t(P)%*%P
  

  
  a1 = optim(c(sd(y),model$coefficients),likfun,tau=p,ycens=y,delta=delta,X=B,K=K)$par
  
  z1 = B%*%a1[-1]
  
# #   diffcon = -1
  # it.con = 1
  
  # while(any(diffcon < -1e-5) && it.con < 20)
  # {
    # it = 1
    # dw1 = 1
    
    # while(dw1 != 0 && it < 50) 
    # {
        # model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)
        # a1 <- model$coefficients
       
        # z1 <- B %*%a1
        # w01 <- w1
        # #w1 <- as.vector(ifelse(y > z1, p, 1 - p))
        # w1[] = p
        # w1[!(y > z1)] = 1-p
        # dw1 <- sum(w1 != w01,na.rm=TRUE)     
        # it = it + 1
    # }
    # diffcon =  constmat %*% a1
    
    # if(any(diffcon < 0))
    # {
      # wc = which(diffcon < 0)
      # conpen[wc] = conpen[wc] + 100000
    # }
    # it.con = it.con + 1
  # }
  # diag.hat.ma1 <- hat(model$qr)[1:length(y)]  

  # if(it == 50)
     # warning("IWLS weights did not converge after 50 iterations.")
     
  list(a=a1[-1], fitted = z1,diag.hat.ma=hat(model$qr)[1:length(y)],sigma=a1[1],delta=delta,K=K)  
}
