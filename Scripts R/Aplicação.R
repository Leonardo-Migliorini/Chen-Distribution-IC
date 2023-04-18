# Importando Funções e Banco de dados

source("graf_chen.R")
source("Chen_reg_fit.R")
# source("Filtragem dos dados.R")


# ln=500

# lambda=1
# beta=c(2.5,1,-2)
# X=cbind(rep(1,n), runif(n), runif(n))


# simu.chen <- function(n, lambda, tau, X, beta) {
  
#  eta = X%*%as.matrix(beta)
#  md <- exp(eta)
#  y <- rchen(n, md, lambda, tau)
  
#  return(y) 
# }

##===Função da distribuição Chen==================================================

rchen<-function(n,md,lambda, tau){
  u=runif(n)
  y= (log(1-(log(1-u)/(log(1- tau)/(1-exp(md^lambda)))))) ^(1/lambda)
  return(y)
 }

n = nrow(pf)
X = cbind(rep(1, n), pf$x1, pf$x2, pf$x3)
y <- pf$y

A<-chen_reg.fit(y , X ,tau = 0.5, link = "log")
diag.br.fit(A, pdf = 0)


#A<-chen_reg.fit(y,X,  resid=1, link="sqrt")
#diag.br.fit(A,pdf=1)