source("Chen_reg_fit.R")
source("Filtragem dos dados.R")

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

auto_rg_chen<-function(y,V,tau,ncov)
{
  k<-t(combn(length(V[1,]),ncov)) 
  nmod<-length(k[,1])                                                           
  models<-matrix(nrow = nmod, ncol = ncov+5)  
  names_cov<-c("Cov1","Cov2","Cov3","Cov4","Cov5","Cov6","Cov7","Cov8","Cov9","Cov10")
  colnames(models)<-c(names_cov[1:ncov],"AIC","BIC","R2","Box_Test","Shapiro")
  pb <- progress::progress_bar$new(            
    format = "[:bar] :elapsedfull | Faltam: :eta",
    total = nmod,    # 100
    width = 60)
  i<- 1
  for(j in 1:nmod){
    pb$tick() 
    b<-as.numeric(k[j,])
    mod<-try(chen_reg.fit(y = y, X = cbind(rep(1,length(V[,1])),as.matrix(V[,b])),
                          tau = tau, link ="log", diag = 0),T) 
    if(class(mod)=="list"){
      if(sum(mod$model[,4]<0.1,na.rm=TRUE)==ncov){
        p_value_BT<-Box.test(mod$resid,lag=10)$p.value
        if(p_value_BT>0.5){
          p_value<-shapiro.test(mod$resid)$p.value
          if(p_value>=0.0005){
            models[i,]<-c(b, mod$aic, mod$bic, mod$rsq, p_value_BT, p_value)
            i<-i+1  
          }
        }
      }
    }
  }
  return(which.NA(models))
}

DA <- name(formatar = 0, limpar = 0, salvar = 0)

Vars <- DA |> 
  dplyr::select(
    -c(1)
  )


Modelos <- auto_rg_chen(DA$y, Vars, 0.1, 5) 


