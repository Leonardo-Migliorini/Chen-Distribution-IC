dpcr <- function(y,lambda, mu, t)
{
 ifelse(y>0, 
        log(1-t)/(1-exp(mu^lambda))*lambda*y^(lambda-1) * 
    exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda))+y^lambda), NA)
}

y=seq(from=0, to=20, by=0.1)
lambda=1.1
mu=8
t=0.5

dpcr(y, lambda, mu, t)
plot(dpcr(y, lambda, mu, t))
