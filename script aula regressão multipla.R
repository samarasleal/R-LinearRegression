
################################################################################
###                               Lendo os dados                                                                                                                     #
################################################################################

#Os dados são de  casas vendidas recentemente . Com as variáveis 
#price: preço de venda (em $), sqft :o tamanho da casa (em pés quadrados),Bedroons: o número de quartos e baths: número de banheiros, 


 Housing = read.table("housing.txt",header=TRUE)

attach(Housing)
head(Housing)
#detach('Housing')

###É possível (mas não necessário) para a construção de um novo quadro de dados constituído exclusivamente pelas variáveis price, sqrf, bedrooms e baths
### usando  os comandos:

myvars=c('sqft','price','bedrooms', 'baths')

Housing2 = Housing[myvars]
 head(Housing2)
 #

################################################################################
#                                      Diagrama de Dispersão 					                #			                  
################################################################################


####  queremos investigar como as variáveis estão relacionados entre si.
### Podemos fazer isso graficamente através da construção de gráficos de dispersão de todas as combinações de
##### pares de variáveis no quadro de dados. 


plot(Housing2)

#### ou 

pairs(cbind(price,sqft,bedrooms,baths))



###############################################################################
####                                                 Ajuste do Modelo Linear                                                                              #
###############################################################################

summary(lm(price ~ sqft))
summary(lm(price ~ bedrooms))
###Combinando variáveis.

##O que acontece quando tentamos prever preço (log (price),) usando tanto sqrf e bedrooms?
summary(lm(log(price) ~ sqft + bedrooms))


ajuste=lm(log(price) ~ sqft + bedrooms)
 ajuste
#### Com a função summary, diversas medidas descritivas úteis para a análise do ajuste podem ser obtidas:
summary(ajuste)


##############################################################################
###                                            Tabela da Análise de Variância                                                                     ##
##############################################################################

anova(ajuste)

##############################################################################
## Suponha que serão as variáveis bedrooms, baths, sqft  em nosso modelo
###e estamos interessados em testar se o número de quartos e banheiros
## são significativas depois de levar em consideração sqft  em consideração.

reduzido = lm(log(price) ~ sqft) # modelo reduzido
 full = lm(log(price) ~ sqft +  bedrooms + baths) #  Modelo com todas as variáveis 
anova(reduzido, full) 

###############################################################################
###                                   Esboçando  a reta ajustada no diagrama de dispersão                                        ##
###############################################################################

windows()
install.packages('scatterplot3d')
require(scatterplot3d)
scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regressão", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab='sqft')

par(las=2) 

scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regressão", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab='sqft')
####################################################################################################
#  Scatterplot 3D colorido e  com linhas verticais 

windows()

scatterplot3d(bedrooms, log(price), sqft, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot",xlab="bedrooms",ylab="log(price)",zlab=sqft)


# e com o plano produzido pela  Regressão 


#  ajuste=lm(log(price) ~ sqft + bedrooms + baths)

s3d =scatterplot3d(bedrooms, log(price), sqft, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
 
s3d$plane3d(ajuste)
#################################################################################################
windows() 

scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regressão", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab=sqft)


s3d=scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regressão", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab=sqft)

s3d$plane3d(ajuste)

##################################################################################


windows()
par(mfrow=c(3,1))
#termplot(lm(y~x+z))
termplot(ajuste)
####################################################################################

install.packages('rgl')
library(rgl)
windows()
plot3d(bedrooms, log(price), sqft, col="red", size=3)
#####################################################################################
install.package('Rcmdr')
library(Rcmdr)
scatter3d(bedrooms, log(price), sqft)




##############################################################################
##            Intervalos de confiança para \beta_0  , \beta_1 e \beta_2                                                                            #
##############################################################################

confint(ajuste)

#############################################################################
#  Diagnóstico do modelo                                                                                                                                 #
#############################################################################

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(ajuste)


###### Detectando Outliers###########################################################

 # Bonferonni p-value para as observações mais extremas
install.pckages('car')
library(car)

outlierTest(ajuste)
qqPlot(ajuste, main="QQ Plot")  

# gráfico leverage ( pontos de alavanca)
leveragePlots(ajuste) 

###################################################################################
# 
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(Housing)-length(ajuste$coefficients)-2)) 
plot(ajuste, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(ajuste,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )






##################################################################################
##                Análise dos Resíduos                                                                                                                               ##
###            ---------------------------                                                                                                                          ##                                                                                    
##                                                                                   ## 
#################################################################################

# Normalidade dos Resíduos
# qq plot para os resíduos estudentizados
 
qqPlot(ajuste, main="QQ Plot")

# distribução dos resíduos estudentizados
library(MASS)
sresid <- studres(ajuste) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# Avaliando  homoscedasticidade
# non-constant error variance test
ncvTest(ajuste)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(ajuste)


#####     Multi-colinearidade

vif(ajuste) # variance inflation factors 
sqrt(vif(ajuste)) > 2 


# Avaliando não- linearity
# componente + residual plot 
crPlots(ajuste)# Ceres plots 
ceresPlots(ajuste)

### Avaliação de Independência dos erros

# Teste de Autocorrelação dos  Erros
##hipótese nula correlação=0
durbinWatsonTest(ajuste)


##################################################################
# Global test of model assumptions
install.packages('gvlma')
library(gvlma)
gvmodel <- gvlma(ajuste) 
summary(gvmodel)



######## ou ###################################################

#######################################################################################
##  Teste de normalidade dos Resíduos                                                                                                                            ###
#######################################################################################


shapiro.test(residuals(ajuste))

##### Testes opcionais 

install.packages("nortest")
library("nortest")

### Anderson- Darling
install.packages('rgr')
require('rgr')
ad.test(residuals(ajuste)) 




############################################################################################
##                                             Intervalos de Confiança para Resposta Média e Individual                                               #####   
###                                                                                                                                                                 ###
############################################################################################
newdata = data.frame(sqft=3380,bedrooms=5)


predict(ajuste,newdata,interval="prediction")

##################################################################################################
# Seleção de Variáveis 													#	
#																#	
#################################################################################################

# Selecionar um conjunto de variáveis preditoras em um conjunto maior(e.g., stepwise selection)
# Podemos fazer seleção de variáveis stepwise  (forward, backward,ou ambas) usando 
# a função stepAIC( ) do pacote MASS.


 
# Stepwise Regression
install.packages('MASS')
library(MASS)
ajuste1=lm(log(price) ~ sqft + bedrooms+baths)
step <- stepAIC(ajuste1, direction="both")
step$anova # display results


