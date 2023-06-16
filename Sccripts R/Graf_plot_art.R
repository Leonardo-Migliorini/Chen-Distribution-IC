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

################################################################################

# Geração do Modelo

################################################################################

DA <- name(formatar = 1, limpar = 0, salvar = 1)

X = cbind(rep(1, nrow(DA)), DA$x2, DA$x3, DA$x4, DA$x7, DA$x8)
y <- DA$y

A4 <- chen_reg.fit(y = y , X = X ,tau = 0.25, link = "log", diag = 1)

# Testes para avaliar modelo ###################################################

# shapiro.test(A4$residual)
# Box.test(A4$residual)

# Exportação dos parâmetros do modelo ##########################################

# sink("Parâmetros Modelo.txt")
# print(A4)
# sink()

# Plotagem de gráficos para avaliação do Modelo ################################


diag.br.fit(A4, pdf = 1)
chen_envlp(A4, b = 100, v = A4$residual)


# Gráfico com intereção nos pontos #############################################

 
# dados <- as.data.frame(cbind(y, A4$residual))
# 
#  g_indexres=ggplot2::ggplot (dados, ggplot2::aes(x= zoo::index(y),
#                                                  y=V2))+
#    ggplot2::geom_point(size=1.5) +
#    ggplot2::geom_hline(yintercept=3, colour="red2",
#                        size=0.5,  linetype="dashed") +
#    ggplot2::geom_hline(yintercept=0, colour="black",
#                        size=0.7, linetype="dashed") +
#    ggplot2::geom_hline(yintercept=-3, colour="red2",
#                        size=0.5,  linetype="dashed")+
#    ggplot2::labs(x = "Índices das observações", y = "Resíduos")
# 
#  plotly::ggplotly(g_indexres)
