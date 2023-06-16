source("Filtragem dos dados.R")
source("Funções Distribuição Chen.R")

# Histograma da Variável Resposta


theta=c(mu=median(DA$y), lambda=0.4)
maxiB = optim(theta, lvc, y=DA$y, 
             control = list(fnscale=-1))

x=seq(0.001,max(DA$y), by=0.1)
hist(DA$y, col = "white", freq = F, main="", xlab="Valores de x",
     ylab="Densidade", ylim=c(0,0.2))
curve(dpcr(x, lambda=maxiB$par[2], mu=maxiB$par[1]),col= "black", lty=1,lwd=1,add=T)


# Corrplot

x <- cor(Dados_Aplicação)
corrplot::corrplot(
  x, type="upper", method = "number"
)

DA <- name(1,0,0)
