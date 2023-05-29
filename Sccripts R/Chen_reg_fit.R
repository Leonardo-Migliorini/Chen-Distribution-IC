
##=========================================================================================
chen_reg.fit<- function (y, X, tau=0.5,link="log", diag=1)
{
  
  #==================== Escolha da Função de ligação==================#
  if(link=="log"){
    ginv_lig<-function(c){exp(c)}
    g_lig<-function(c){log(c)}}
  if(link=="sqrt")
  {
    ginv_lig<-function(c){c^2}
    g_lig<-function(c){sqrt(c)}
  }
  #=============================##====================================#
  #Funções uteis do VaRES
  dchen=function (x, b=1, lambda=1, log=FALSE)
  {
    pdf=x
    pdf[log==FALSE]=b*lambda*x**(b-1)*exp(x**b)*exp(lambda-lambda*exp(x**b))
    pdf[log==TRUE]=log(b*lambda)+(b-1)*log(x)+x**b+lambda-lambda*exp(x**b)
    return(pdf)
  }
  
  pchen=function (x, b=1, lambda=1, log.p=FALSE, lower.tail=TRUE)
  {
    cdf=x
    cdf[log.p==FALSE&lower.tail==TRUE]=1-exp(lambda-lambda*exp(x**b))
    cdf[log.p==FALSE&lower.tail==FALSE]=exp(lambda-lambda*exp(x**b))
    cdf[log.p==TRUE&lower.tail==TRUE]=log(1-exp(lambda-lambda*exp(x**b)))
    cdf[log.p==TRUE&lower.tail==FALSE]=lambda-lambda*exp(x**b)
    return(cdf)
  }
  
  
  y <- na.omit(y)
  
  if ( min(y) < 0)
    stop("OUT OF RANGE!")
  dados=cbind(y, X)
  
  ##===== Chute Inicial ===============================================================  
  ynew <- g_lig(y)
  ajuste <- lm(ynew~X[,-1])
  mqo <- c(ajuste$coef)
  res=summary(ajuste)
  lambdac=0.6
  
  par <- round(c(as.numeric(lambdac), as.numeric(mqo)), 2)
  
  
  
  ##=====Função logverossimilança=====================================================
  lvero=function(theta, dados)
  {
    n=length(dados[,1])
    lambda=theta[1]
    y=dados[,1]
    X=dados[,-1]
    beta <- theta[2:length(theta)]
    
    eta <- X%*%as.matrix(beta) 
    md <- ginv_lig(eta)
    
    
    lv=suppressWarnings(log(log(1- tau)/(1- exp(md^lambda)))+ log(lambda)+(lambda-1)*log(y)+
                          (log(1- tau)/(1- exp(md^lambda)))*(1- exp(y^lambda))+(y^lambda))
    return(sum(lv))
  
  }
  
  
  lveroh0=function(theta, dados)
  {
    n=length(dados[,1])
    lambda=theta[1]
    y=dados[,1]

    md = theta[2]
    
    
    lv=suppressWarnings(log(log(1- tau)/(1- exp(md^lambda)))+ log(lambda)+(lambda-1)*log(y)+
                          (log(1- tau)/(1- exp(md^lambda)))*(1- exp(y^lambda))+(y^lambda))
    return(sum(lv))
  }
      ##=== função escore (vetor das derivadas da logverossimilhança)===========================
  escore <- function(theta, dados)
  {
    lambda <- theta[1]
    beta <- theta[2:length(theta)]
    y=dados[,1]
    X=dados[,-1]
    
    eta=as.vector(X%*%as.matrix(beta))
    md <- ginv_lig(eta)
    mB = as.vector(-(lambda*md^(lambda-1)*exp(md^lambda)*(exp(md^lambda)+
                                                            log(1- tau)*exp(y^lambda)-log(1- tau)-1))/((exp(md^lambda)-1)^2))
    mL=as.vector(((-log(1- tau)*y^lambda*log(y)*exp(y^lambda)+(md^lambda)*log(md)*exp(md^lambda))/(1-exp(md^lambda)))
                 +((log(1- tau)*(md^lambda)*log(md)*exp(md^lambda)*(1-exp(y^lambda)))/((1-exp(md^lambda))^2))+
                   1/lambda+y^lambda*log(y)+log(y))
    
    
    mT <- diag(exp(eta))
    
    Ulambda <- sum(mL)
    Ubeta <- t(X)%*% mT %*% mB
    
    rval <- c(Ulambda,Ubeta)
    return(rval)
  }
  tempo <- Sys.time()
  opt=optim(par, lvero, dados=dados, gr=escore, method = "Nelder-Mead", hessian = T,
            control = list(fnscale = -1, maxit = 1000, reltol = 1e-10))
  
  
  if (opt$conv != 0)
    warning("FUNCTION DID NOT CONVERGE!")
  
  z <- c()
  z$conv <- opt$conv
  coef <- (opt$par)[1:(1+ncol(X))]
  names(coef) <- c("lambda",c(paste("beta",1:ncol( as.matrix(X) ),sep="")))
  z$coeff <- coef
  
  lambda <-coef[1]
  beta <- coef[2:length(coef)]
  
  z$lambda <- lambda
  
  etahat <- X%*%as.matrix(beta)
  muhat <- ginv_lig(etahat)
  
  z$fitted <- muhat
  z$etahat <- etahat
  z$serie <- y
  z$X <- X
  z$chen <- names(coef)
  z$tau= tau
  z$link=link
  
  ##=======================================================================================
  # resíduo quantílico
  
  ETA <- (log(1- tau))/(1-(exp(muhat^lambda)))
  z$residual = qnorm(pchen(y, b=lambda, lambda=ETA, log.p=FALSE, lower.tail=TRUE))
  residc <- z$residual
  
  par0 <- round(c(as.numeric(lambdac), mean(muhat)), 2)
  
  opth0=optim(par0, lveroh0, dados=dados, method = "Nelder-Mead", hessian = T,
              control = list(fnscale = -1, maxit = 1000, reltol = 1e-10))
  
  ############################################
  
  vcov <- chol2inv(chol(-opt$hessian))
  z$vcov <- vcov
  
  stderror <- sqrt(diag(vcov))
  z$stderror <- stderror
  
  z$zstat <- abs(z$coef/stderror)
  z$pvalues <- 2*(1 - pnorm(z$zstat) )
  
  z$loglik <- opt$value
  z$counts <- as.numeric(opt$counts[1])
  z$aic <- -2*z$loglik+2*(1+length(beta))
  z$bic <- -2*z$loglik+log(n)*(1+length(beta))
  z$rsq <- 1 - exp(-2/n*(z$loglik - opth0$value))
  
  model_presentation <- cbind(round(z$coef,4),round(z$stderror,4),round(z$zstat,4),round(z$pvalues,4))
  colnames(model_presentation)<-c("Estimate","Std. Error","z value","Pr(>|z|)")
  
  z$model <- model_presentation
  if( diag==1){
    print(model_presentation)
    print(" ",quote=F)
    print(c("Log-likelihood:",round(z$loglik,4)),quote=F)
    print(c("Number of iterations in BFGS optim:",z$counts),quote=F)
    print(c("AIC:",round(z$aic,4)," BIC:",round(z$bic,4)),quote=F)
    print(c("R²:",round(z$rsq,4)), quote=F)
    print("Residuals:",quote=F)
    print(summary(as.vector(residc)))}
  
  
  
  return(z)
}




