# Importando Funções e Banco de dados

source("Chen_env_alisson.R")
source("Graf_chen.R")
source("Chen_reg_fit.R")
source("Filtragem dos dados.R")

##===Função da distribuição Chen================================================

rchen<-function(n,md,lambda, tau){
  u=runif(n)
  y= (log(1-(log(1-u)/(log(1- tau)/(1-exp(md^lambda)))))) ^(1/lambda)
  return(y)
}

# Modelo 1

################################################################################

# n = nrow(M1)
# X1 = cbind(rep(1, n), M1$x1, M1$x2, M1$x3)
# y1 <- M1$y
# 
# A1<-chen_reg.fit(y = y1 , X = X1 ,tau = 0.5, link = "log")
# diag.br.fit(A1, pdf = 1)
# 
# chen_envlp(A1, b = 100, v = A1$residual)

#A<-chen_reg.fit(y,X,  resid=1, link="sqrt")
#diag.br.fit(A,pdf=1)

################################################################################

# Modelo 2

################################################################################
# 
# n = nrow(M2)
# X2 = cbind(rep(1, n), M2$x1, M2$x2)
# y2 <- M2$y
# 
# A2<-chen_reg.fit(y = y2 , X = X2 ,tau = 0.5, link = "log")
# diag.br.fit(A2, pdf = 0)
# chen_envlp(A2, b = 100, v = A2$residual)
# 
# 
# dados <- as.data.frame(cbind(y2, A2$residual))
# 
# g_indexres=ggplot2::ggplot (dados, ggplot2::aes(x= zoo::index(y2),
#                             y=V2))+
#   ggplot2::geom_point(size=1.5) + 
#   ggplot2::geom_hline(yintercept=3, colour="red2", 
#              size=0.5,  linetype="dashed") +
#   ggplot2::geom_hline(yintercept=0, colour="black", 
#              size=0.7, linetype="dashed") +  
#   ggplot2::geom_hline(yintercept=-3, colour="red2", 
#              size=0.5,  linetype="dashed")+
#   ggplot2::labs(x = "Índices das observações", y = "Resíduos")
# 
# plotly::ggplotly(g_indexres)

#A<-chen_reg.fit(y,X,  resid=1, link="sqrt")
#diag.br.fit(A,pdf=1)

################################################################################

# Modelo 3

################################################################################

# n = nrow(M3)
# X3 = cbind(rep(1, n), M3$x1, M3$x2, M3$x5)
# y3 <- M3$y
# 
# A3<-chen_reg.fit(y = y3 , X = X3 ,tau = 0.5, link = "log")
# diag.br.fit(A3, pdf = 0)
# chen_envlp(A3, b = 100, v = A3$residual)




# dados <- as.data.frame(cbind(y3, A3$residual))

# g_indexres=ggplot2::ggplot (dados, ggplot2::aes(x= zoo::index(y3),
#                                                 y=V3))+
#   ggplot2::geom_point(size=1.5) + 
#   ggplot2::geom_hline(yintercept=3, colour="red2", 
#                       size=0.5,  linetype="dashed") +
#   ggplot2::geom_hline(yintercept=0, colour="black", 
#                       size=0.7, linetype="dashed") +  
#   ggplot2::geom_hline(yintercept=-3, colour="red2", 
#                       size=0.5,  linetype="dashed")+
#   ggplot2::labs(x = "Índices das observações", y = "Resíduos")
# 
# plotly::ggplotly(g_indexres)


################################################################################

# Modelo 4

################################################################################


n = nrow(M4)
X4 = cbind(rep(1, n), M4$x5, M4$x3)
y4 <- M4$y

A4<-chen_reg.fit(y = y4 , X = X4 ,tau = 0.5, link = "log")
diag.br.fit(A4, pdf = 0)
chen_envlp(A4, b = 100, v = A4$residual)


shapiro.test(A4$residual) # Checa se os dados seguem a distribuição normal padrão
Box.test(A4$residual) # Checar aleatoriedade dos dados

# dados <- as.data.frame(cbind(y3, A3$residual))

# g_indexres=ggplot2::ggplot (dados, ggplot2::aes(x= zoo::index(y3),
#                                                 y=V3))+
#   ggplot2::geom_point(size=1.5) + 
#   ggplot2::geom_hline(yintercept=3, colour="red2", 
#                       size=0.5,  linetype="dashed") +
#   ggplot2::geom_hline(yintercept=0, colour="black", 
#                       size=0.7, linetype="dashed") +  
#   ggplot2::geom_hline(yintercept=-3, colour="red2", 
#                       size=0.5,  linetype="dashed")+
#   ggplot2::labs(x = "Índices das observações", y = "Resíduos")
# 
# plotly::ggplotly(g_indexres)
