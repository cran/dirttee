aicgrid.aft <-
function(yy,delta,B,quantile,DD,nb,constmat,types,likfun)
{
	las1 = seq(-1, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    score = rep(0, nrow(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))

    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = unlist(lambdas[i,])
#print(penalty)
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

        score[i] =  -2*likfun(c(abs(aa$sigma),aa$a),quantile,yy,delta,B,aa$K) + 2*(1+sum(aa$diag.hat.ma))
    }
    
    penalty[glatterms] = lambdas[which.min(score),]

    penalty   
}
