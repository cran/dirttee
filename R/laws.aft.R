laws.aft <-
function(B,DD,yy,delta,pp,lambda,smooth,nb,center,constmat,types)
###### expectile regression according to eilers, schnabel
# parameters:
# formula - vector of responses ~ f(vector of independent,type="which base to use") + ...
# smooth - if smoothing with schall's algorithm, asymmetric cross validation or no smoothing shall be done
# lambda - smoothing penalty, important if no smoothing is done
{
  nterms = length(nb)
  m = length(yy)
  np = length(pp)

  
#  myapply <- lapply
#  if (.Platform$OS.type == "unix" && require("parallel")) 
#  {
#      if (!parallel:::isChild()) 
#      {
#          myapply <- mclapply
#      }
#  }
  
  if(length(lambda) < nterms)
      lala = rep(lambda[1],nterms)
    else
      lala = lambda


  dummy.reg <- function(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center,types)
  {
    message("Expectile: ",pp,"\n")

    # if(smooth == "gcv")
    # {
      # #acv.min = nlm(acv,p=lala,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,iterlim=100)
      # #acv.min = optim(par=lala,acv,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,method="L-BFGS-B",lower=0,upper=10000)
      # acv.min = nlminb(start=lala,objective=acv.aft,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,lower=0,upper=10000)

      # #aa <- asyregpen.aft(yy, B, pp, abs(acv.min$estimate), DD, nb, constmat)
      # aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat)
      # vector.a.ma.schall <- aa$a  
      # #lala <- abs(acv.min$estimate)
      # lala <- abs(acv.min$par)
     # # print(c(acv.min$par,acv.min$objective))
      # diag.hat = aa$diag.hat.ma
    # }
     if(smooth == "aic")
    {
    
      acv.min = nlminb(start=lala,objective=aicfun.aft,yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,likfun=likeli.asynorm,lower=0.1,upper=20000,control=list(iter.max=60, rel.tol=1e-4))


      #acv.min = multimin.init(x=lala,f=function(x) aicfun.aft(abs(x),yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat),method="nm")

      #for(i in 1:50)
      #{
      #  acv.min = multimin.iterate(acv.min)
      #  print(acv.min$x)
      #}

      #acv.min = RcppDE::DEoptim(aicfun, rep(0,length(lala)), rep(10000,length(lala)), control = DEoptim.control(trace=F,strategy=2), yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat)

      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat,likeli.asynorm)
      #aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$x), DD, nb, constmat)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$par)#abs(acv.min$x)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "bic")
    {
      acv.min = nlminb(start=lala,objective=bicfun.aft,yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,likfun=likeli.asynorm,lower=0.1,upper=20000,control=list(iter.max=60, abs.tol=1e-10,rel.tol=1e-5))

      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat,likeli.asynorm)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$par)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "gridbic")
    {
    	lala = bicgrid.aft(yy,delta,B,pp,DD,nb,constmat,types,likeli.asynorm)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else if(smooth == "gridaic")
    {
    	lala = aicgrid.aft(yy,delta,B,pp,DD,nb,constmat,types,likeli.asynorm)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else if(smooth == "lcurve")
    {
    	lala = lcurve.aft(yy,delta,B,pp,DD,nb,constmat,types)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else
    {
      aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
      vector.a.ma.schall <- aa$a  
      diag.hat = aa$diag.hat.ma 
    }
    
    list(vector.a.ma.schall,lala,diag.hat,aa$sigma)
  }

  coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = 1)
  
  #if (.Platform$OS.type == "unix")
  #coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = max(1,min(detectCores()-1,2)))
  #else if (.Platform$OS.type == "windows")
  #coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = 1)
  
  
  lala <- matrix(lambda, nrow=nterms, ncol=np)
  vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)
  diag.hat = matrix(NA,nrow=m,ncol=np)
  sigma = rep(NA,np)


  for(i in 1:np)
  {
  	#print(i)
    vector.a.ma.schall[,i] = coef.vector[[i]][[1]]
    lala[,i] = coef.vector[[i]][[2]]
    diag.hat[,i] = coef.vector[[i]][[3]]
    sigma[i] = coef.vector[[i]][[4]]
  }

  return(list(vector.a.ma.schall,lala,diag.hat,sigma))
}
