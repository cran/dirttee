schall_ipc <-
function(yy,B,pp,DD,nb,lala,constmat,center,types, KMweights, hat1) #!
{
    glatterms = which(types != "parametric")
    
    nterms = length(nb)
    m = length(yy)
    np = length(pp)
    
    dc = 1
    dw = 1
    w <- rep(1,times=m)
    it = 1
    while((dc >= 0.01 || dw != 0) && it < 100)
    {
        aa <- asyregpen_ipc(yy, B, pp, lala, DD, nb, constmat, KMweights = KMweights, hat1 = hat1) #!
        vector.a.ma.schall <- aa$a 
        diag.hat = aa$diag.hat.ma
        
        w0 <- w
        l0 <- lala
        
        for(i in glatterms)
        {
            partbasis = (sum(nb[0:(i-1)])+1):(sum(nb[0:i]))
            if(center)
            {
                partB = B[,-1,drop=FALSE][,partbasis,drop=FALSE]
                partDD = DD[,-1,drop=FALSE][-1,,drop=FALSE][,partbasis,drop=FALSE]
                partaa = aa$a[-1][partbasis]
            }
            else
            {
                partB = B[,partbasis,drop=FALSE]
                partDD = DD[,partbasis,drop=FALSE]
                partaa = aa$a[partbasis]
            }
            
            #partB = partB[rowSums(abs(partB)) > 0,]
            
            if(any(partDD != 0))
            {
                v <- partDD %*% partaa
                z <- aa$fitted
                #z <- partB %*% partaa

                #if(nterms > 1)
                #z = B[,-partbasis,drop=F] %*% aa$a[-partbasis]
                
                w <- aa$weight
                
                H = solve(t(partB)%*%(w*partB) + lala[i]*t(partDD)%*%partDD)
                ##H = B%*%H%*%t(B)
                #H = diag(H)*aa$weight
                H = apply(sqrt(w)*partB,1,function(x){t(x)%*%H%*%x})
                
                #sig2 <- sum(w * (yy - z) ^ 2,na.rm=TRUE) / (m - sum(aa$diag.hat.ma,na.rm=TRUE))
                sig2 <- sum(w*(yy - z) ^ 2,na.rm=TRUE) / (m - sum(aa$diag.hat.ma,na.rm=TRUE))
                tau2 <- sum(v ^ 2,na.rm=TRUE) / sum(H,na.rm=TRUE) + 1e-06
                
                lala[i] = max(sig2 / tau2,1e-10,na.rm=TRUE)
            }
        }
        
        dc <- max(abs(log10(l0 + 1e-6)-log10(lala + 1e-6)))
        dw <- sum(w != w0,na.rm=TRUE)
        it = it + 1
    }
    if(it == 100)
        warning("Schall algorithm did not converge. Stopping after 100 iterations.")
    
    aic <- log(sum(aa$weight * (yy - B %*% aa$a)^2/length(yy))) * length(yy) + 2 * (sum(aa$diag.hat.ma)) #!
    
    list(aa$a,lala,aa$diag.hat.ma,aa$fitted, aic = aic, hat = aa$hat)
}
