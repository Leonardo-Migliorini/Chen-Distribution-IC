# Importação dos dados

planilha <- readr::read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
  )

# Filtragem dos dados

pf <- dplyr::filter(
  planilha, year == 2018
) |> 
  dplyr::select(
    co2_per_capita, ghg_per_capita, ghg_excluding_lucf_per_capita,	
    methane_per_capita,	energy_per_gdp, energy_per_capita, ghg_excluding_lucf_per_capita
  ) |> 
  dplyr::filter_all(
    dplyr::all_vars(. > 0)
  ) |> 
  dplyr::rename(
    y = co2_per_capita, x1 = ghg_per_capita, x2 = ghg_excluding_lucf_per_capita,
    x3 = methane_per_capita, x4 = energy_per_gdp, x5 = energy_per_capita
  ) 

pf <- pf[!(row.names(pf) %in% c("159")),]

# função de log verossimilhança 

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

# função de densidade de probabilidade reparametrizada 

dpcr <- function(y,lambda, mu)
  
{
  t=0.5
  ifelse(y>0, 
         log(1-t)/(1-exp(mu^lambda))*lambda*y^(lambda-1) * 
           exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda))+y^lambda), NA)
}

# função de distribuição acumulada

fdac <- function(y, lambda, mu)
  
{
  t=0.5
  pr=1-exp(log(1-t)/(1-exp(mu^lambda))*(1-exp(y^lambda)))
  return(pr)
}

y=pf$Co2_pc

theta=c(mu=median(pf$y), lambda=0.7)
maxiB = optim(theta, lvc, y=y, 
             control = list(fnscale=-1))

x=seq(0.001,max(y), by=0.1)
hist(y, freq = F, main="", xlab="Valores de x",
     ylab="Densidade", ylim=c(0,0.2))
curve(dpcr(x, lambda=maxiB$par[2], mu=maxiB$par[1]),col=2, lty=1,lwd=1,add=T)

