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
maxiB= optim(theta, lvc, y=dados, 
             control = list(fnscale=-1))

y=seq(1,3, by=0.01)
theta=c(mu=2, lambda=0.7)
lvc(y, theta)
