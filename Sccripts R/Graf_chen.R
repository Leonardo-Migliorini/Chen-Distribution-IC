##========================================================================================
## GRAPHICS
diag.br.fit <- function(model=fit.model,sim=100,conf=.95, pch=1,  pdf=0) {
  main = "Half-Normal Plot of Residuals"
  ylab= "Residuals (absolute values)"
  xlab= "Normal quantiles"
  

  alfa <-(1-conf)/2
  lambda=model$lambda
  X=model$X
  yfitted=model$fitted
  eta=model$etahat
  y <-model$serie
  n <-length(y)
  tau<-model$tau
  
  res <- model$resid
  e <- matrix(0,n,sim)
  e1 <- numeric(n)
  e2 <- numeric(n)
  
  i<-1
  while(i<=sim) {
    
    md <- exp(eta)
    ynew <- rchen(n,  md, lambda, tau)
    fit <- try(chen_reg.fit(ynew,X, diag=0),silent=T)
    if(class(fit) != "try-error"){
      if(fit$conv==0){
        ti <- fit$resid
        eo <- sort(abs(fit$resid))
        e[,i] <- eo
        i<- i+1
      }
    }
  }
  
  for(i in 1:n) {
    eo <- sort(e[i,])
    e1[i] <- quantile(eo,alfa)
    e2[i] <- quantile(eo,1-alfa)
  }
  
  par(mfrow=c(2,2))
  par(family="Times") 
  par(mar=c(2.7, 2.5, 2, 1)) 
  par(mgp=c(1.5, 0.8, 0))
  
  t<-seq(-5,n+6,by=1)
  j<-seq(n)
  
  t<-seq(-5,n+6,by=1)
  
  par(mfrow=c(1,1))
  par(mar=c(2.8, 2.7, 1.2, 1)) # margens c(baixo,esq,cima,direia)
  par(mgp=c(1.7, 0.45, 0))
  plot(j, res,main="",xlab="Index",ylab="Residuals", pch = "+",
       ylim=c(min( c(-4,res) ),max(c(4,res) ) )) 
  lines(t,rep(-3,n+12),lty=2,col=1)
  lines(t,rep(3,n+12),lty=2,col=1)
  lines(t,rep(-2,n+12),lty=3,col=1)
  lines(t,rep(2,n+12),lty=3,col=1)
  
  
  densidade<-density(res)
  plot(densidade,ylab="Density", xlab="Range", main=" ", ylim=c(0, 0.5))
  lines(densidade$x,dnorm(densidade$x),lty=2)
  legend("topleft",c("Estimated density","Standard normal"),#pch=vpch,
         pt.bg="white", lty=c(1,2), bty="n")
  
  
  plot(y,  yfitted, xlab="Observed",ylab="Fitted", pch = pch) 
  lines(c(0,1),c(0,1),lty=2)
  
  med <- apply(e,1,median)
  qq <- qnorm((n+1:n+.5)/(2*n+1.125))
  plot(qq, sort(abs(res)), ylim = range(abs(res), e1, e2), 
       pch=pch, main="", xlab="Normal quantiles", ylab="Residuals (absolute values)")
  lines(qq,e1,lty=1)
  lines(qq,e2,lty=1)
  lines(qq,med,lty=2) 
  
  plot(yfitted, res,main="",xlab="Fitted Values",ylab="Residuals", pch = "+" ,ylim=c(min( c(-4,res) ),max(c(4,res) ) )) 
  lines(t,rep(-3,n+12),lty=2,col=1)
  lines(t,rep(3,n+12),lty=2,col=1)
  lines(t,rep(-2,n+12),lty=3,col=1)
  lines(t,rep(2,n+12),lty=3,col=1)
  
  if(pdf==1)
  {
    mar_b<-2.5
    mar_e<-2.5
    mar_c<-0.5
    mar_d<-0.5
    dist_text<-1.5
    dist_tick<-0.5
    
    
    pdf(file = "resid_v_ind.pdf",width = 4, height = 4,family = "Times")
    par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
    par(mgp=c(dist_text, dist_tick, 0))
    plot(j, res,main="",xlab="Index",ylab="Residuals", pch = "+" ,ylim=c(min( c(-4,res) ),max(c(4,res) ) )) 
    lines(t,rep(-3,n+12),lty=2,col=1)
    lines(t,rep(3,n+12),lty=2,col=1)
    lines(t,rep(-2,n+12),lty=3,col=1)
    lines(t,rep(2,n+12),lty=3,col=1)
    dev.off()
    
    pdf(file = "density.pdf",width = 4, height = 4,family = "Times")
    par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
    par(mgp=c(dist_text, dist_tick, 0))
    densidade<-density(res)
    plot(densidade,ylab="Density",main=" ", ylim=c(0, 0.5), xlab="Range")
    lines(densidade$x,dnorm(densidade$x),lty=2)
    legend("topleft",c("Estimated density","Standard normal"),#pch=vpch,
           pt.bg="white", lty=c(1,2), bty="n")
    dev.off()
    
    pdf(file = "obs_v_fitted.pdf",width = 4, height = 4,family = "Times")
    par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
    par(mgp=c(dist_text, dist_tick, 0))
    plot(y,  yfitted, xlab="Observed",ylab="Fitted", pch = pch) 
    lines(c(0,1),c(0,1),lty=2)
    dev.off()
    
    pdf(file = "envelope.pdf",width = 4, height = 4,family = "Times")
    par(mar=c(mar_b, mar_e, mar_c, mar_d))
    par(mgp=c(dist_text, dist_tick, 0))
    med <- apply(e,1,median)
    qq <- qnorm((n+1:n+.5)/(2*n+1.125))
    plot(qq, sort(abs(res)), ylim = range(abs(res), e1, e2), 
         pch=pch, xlab=xlab, ylab=ylab, main="")
    lines(qq,e1,lty=1)
    lines(qq,e2,lty=1)
    lines(qq,med,lty=2) 
    dev.off()  
    
    pdf(file = "resid_v_fitted_v.pdf",width = 4, height = 4,family = "Times")
    par(mar=c(mar_b, mar_e, mar_c, mar_d)) 
    par(mgp=c(dist_text, dist_tick, 0))
    plot(yfitted, res,main="",xlab="Fitted Values",ylab="Residuals", pch = "+" ,ylim=c(min( c(-4,res) ),max(c(4,res) ) )) 
    lines(t,rep(-3,n+12),lty=2,col=1)
    lines(t,rep(3,n+12),lty=2,col=1)
    lines(t,rep(-2,n+12),lty=3,col=1)
    lines(t,rep(2,n+12),lty=3,col=1)
    dev.off()
  }
  
}

