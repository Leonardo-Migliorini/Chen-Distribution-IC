# Funçãp de Log Verossimilhança Chen

lvc <- function(y, theta)
{
  mu=theta[1]
  lambda=theta[2]
  t=0.5
  lt=log(log(1-t)/(1-exp(mu^lambda)))+(lambda-1)*log(y)+log(lambda)+
    (log(1-t)*(1-exp(y^lambda))/(1-exp(mu^lambda)))+y^lambda
  result=sum(lt)
  return(result)
}


# Função Densidade de Probabilidade Chen

dpcr <- function(y,lambda, mu, t = 0.5)
{
  ifelse(y>0, 
         log(1-t)/(1-exp(mu^lambda))*lambda*y^(lambda-1) * 
           exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda))+y^lambda), NA)
}


# Função de Distribuição Acumulada

fdac <- function(y, lambda, mu)
{
  t=0.5
  pr=1-exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda)))
  return(pr)
}


