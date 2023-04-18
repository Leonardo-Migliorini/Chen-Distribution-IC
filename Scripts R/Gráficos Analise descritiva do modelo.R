source("Filtragem dos dados.R")
source("Funções Distribuição Chen.R")

# Histograma da Variável Resposta

y=pf$y

theta=c(mu=median(pf$y), lambda=0.4)
maxiB = optim(theta, lvc, y=y, 
             control = list(fnscale=-1))

x=seq(0.001,max(y), by=0.1)
hist(y, col = "white", freq = F, main="", xlab="Valores de x",
     ylab="Densidade", ylim=c(0,0.2))
curve(dpcr(x, lambda=maxiB$par[2], mu=maxiB$par[1]),col= "black", lty=1,lwd=1,add=T)


# Corrplot

x <- cor(pf)
corrplot::corrplot(
  x, type="upper", method = "number"
)
