##########################
######## Pacotes #########
##########################


if (!require("pacman")) install.packages("pacman")
pacman::p_load("GLMsData","gamlss")


##########################
######### Função #########
##########################


source("plot.alt.R", encoding = "UTF-8")


##########################
######## Modelos #########
##########################

data(hcrabs)

fit1=gamlss(Sat~Wt+Col,family=PO(mu.link = "log"),data=hcrabs)

fit5=gamlss(Sat~Wt + Col,sigma.formula = ~ Col,
            nu.formula = ~ 1,tau.formula = ~ Col,
            family=ZISICHEL(tau.link = "logit"),data=hcrabs)

##########################
######## Exemplos ########
##########################

# A funcao plot.alt() apresenta 2 argumentos:

# fit - um modelo GAMLSS

# mfrow - um vetor de dois numeros que define o 
# numero de linhas e colunas do painel grafico


plot.alt(fit1, mfrow = c(2,2))

plot.alt(fit5, mfrow = c(2,2))

# Para se obter os graficos individuais, usar mfrow = c(1,1)

plot.alt(fit5, mfrow = c(1,1))






