source("Chen_reg_fit.R")
source("Filtragem dos dados.R")


M4n <- M4 |> 
  dplyr::select(
    -c(1)
  )

#Função para retirar as linhas da matrix que possuem NA ------------------------

which.NA<-function(x)
{
  x<-data.frame(x)
  lines<-GLDEX::which.na(x[,1])
  tmp<-length(lines)
  if(tmp>0){
    y<-x[-lines,]
  }
  else{y<-x}
  return(y)
}

#Selection of Covariates--------------------------------------------------------

auto_rq_chen<-function(y,V,tau,ncov)
{
  k<-t(combn(length(V[1,]),ncov))
  nmod<-length(k[,1])
  models<-matrix(nrow=nmod,ncol=ncov+6)
  names_cov<-c("Cov1","Cov2","Cov3","Cov4","Cov5","Cov6","Cov7","Cov8","Cov9","Cov10")
  colnames(models)<-c(names_cov[1:ncov],"AIC","BIC","R2","Log_Like","Box_Test","Shapiro")
  i<-1
  for(j in 1:nmod){
    b<-as.numeric(k[j,])
    mod<-try(chen_reg.fit(y = y,X=as.matrix(V[,b]), tau = tau, link="log", diag = 1),T)
    if(class(mod)=="list"){
      if(sum(mod$model[,4]<0.1,na.rm=TRUE)==ncov){
        p_value_BT<-Box.test(mod$resid,lag=10)$p.value
        if(p_value_BT>0.4){
          p_value<-shapiro.test(mod$resid)$p.value
          if(p_value>=0.4){
            models[i,]<-c(b,mod$aic,mod$bic,mod$r2,mod$loglik,p_value_BT,p_value)
            i<-i+1  
          }
        }
      }
    }
  }
  return(which.NA(models))
}

auto_rq_chen(y = M4$y, V = M4n, tau = 0.5, ncov = 6)

