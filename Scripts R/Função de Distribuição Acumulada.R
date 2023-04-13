fdac <- function(y, lambda, mu)
{
  t=0.5
  pr=1-exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda)))
  return(pr)
}

y=(1:20)
lmabda=0.37
mu=2.96

fdac(y, lambda, mu)
