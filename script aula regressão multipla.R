
################################################################################
###                               Lendo os dados                                                                                                                     #
################################################################################

#Os dados s�o de  casas vendidas recentemente . Com as vari�veis 
#price: pre�o de venda (em $), sqft :o tamanho da casa (em p�s quadrados),Bedroons: o n�mero de quartos e baths: n�mero de banheiros, 


 Housing = read.table("housing.txt",header=TRUE)

attach(Housing)
head(Housing)
#detach('Housing')

###� poss�vel (mas n�o necess�rio) para a constru��o de um novo quadro de dados constitu�do exclusivamente pelas vari�veis price, sqrf, bedrooms e baths
### usando  os comandos:

myvars=c('sqft','price','bedrooms', 'baths')

Housing2 = Housing[myvars]
 head(Housing2)
 #

################################################################################
#                                      Diagrama de Dispers�o 					                #			                  
################################################################################


####  queremos investigar como as vari�veis est�o relacionados entre si.
### Podemos fazer isso graficamente atrav�s da constru��o de gr�ficos de dispers�o de todas as combina��es de
##### pares de vari�veis no quadro de dados. 


plot(Housing2)

#### ou 

pairs(cbind(price,sqft,bedrooms,baths))



###############################################################################
####                                                 Ajuste do Modelo Linear                                                                              #
###############################################################################

summary(lm(price ~ sqft))
summary(lm(price ~ bedrooms))
###Combinando vari�veis.

##O que acontece quando tentamos prever pre�o (log (price),) usando tanto sqrf e bedrooms?
summary(lm(log(price) ~ sqft + bedrooms))


ajuste=lm(log(price) ~ sqft + bedrooms)
 ajuste
#### Com a fun��o summary, diversas medidas descritivas �teis para a an�lise do ajuste podem ser obtidas:
summary(ajuste)


##############################################################################
###                                            Tabela da An�lise de Vari�ncia                                                                     ##
##############################################################################

anova(ajuste)

##############################################################################
## Suponha que ser�o as vari�veis bedrooms, baths, sqft  em nosso modelo
###e estamos interessados em testar se o n�mero de quartos e banheiros
## s�o significativas depois de levar em considera��o sqft  em considera��o.

reduzido = lm(log(price) ~ sqft) # modelo reduzido
 full = lm(log(price) ~ sqft +  bedrooms + baths) #  Modelo com todas as vari�veis 
anova(reduzido, full) 

###############################################################################
###                                   Esbo�ando  a reta ajustada no diagrama de dispers�o                                        ##
###############################################################################

windows()
install.packages('scatterplot3d')
require(scatterplot3d)
scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regress�o", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab='sqft')

par(las=2) 

scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regress�o", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab='sqft')
####################################################################################################
#  Scatterplot 3D colorido e  com linhas verticais 

windows()

scatterplot3d(bedrooms, log(price), sqft, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot",xlab="bedrooms",ylab="log(price)",zlab=sqft)


# e com o plano produzido pela  Regress�o 


#  ajuste=lm(log(price) ~ sqft + bedrooms + baths)

s3d =scatterplot3d(bedrooms, log(price), sqft, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
 
s3d$plane3d(ajuste)
#################################################################################################
windows() 

scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regress�o", pch = 19,
xlab="bedrooms",ylab="log(price)",zlab=sqft)


s3d=scatterplot3d(bedrooms, log(price), sqft, col.axis = "black", 
main = "Exemplo aula de Regress�o", pch = 19,
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
##            Intervalos de confian�a para \beta_0  , \beta_1 e \beta_2                                                                            #
##############################################################################

confint(ajuste)

#############################################################################
#  Diagn�stico do modelo                                                                                                                                 #
#############################################################################

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(ajuste)


###### Detectando Outliers###########################################################

 # Bonferonni p-value para as observa��es mais extremas
install.pckages('car')
library(car)

outlierTest(ajuste)
qqPlot(ajuste, main="QQ Plot")  

# gr�fico leverage ( pontos de alavanca)
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
##                An�lise dos Res�duos                                                                                                                               ##
###            ---------------------------                                                                                                                          ##                                                                                    
##                                                                                   ## 
#################################################################################

# Normalidade dos Res�duos
# qq plot para os res�duos estudentizados
 
qqPlot(ajuste, main="QQ Plot")

# distribu��o dos res�duos estudentizados
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


# Avaliando n�o- linearity
# componente + residual plot 
crPlots(ajuste)# Ceres plots 
ceresPlots(ajuste)

### Avalia��o de Independ�ncia dos erros

# Teste de Autocorrela��o dos  Erros
##hip�tese nula correla��o=0
durbinWatsonTest(ajuste)


##################################################################
# Global test of model assumptions
install.packages('gvlma')
library(gvlma)
gvmodel <- gvlma(ajuste) 
summary(gvmodel)



######## ou ###################################################

#######################################################################################
##  Teste de normalidade dos Res�duos                                                                                                                            ###
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
##                                             Intervalos de Confian�a para Resposta M�dia e Individual                                               #####   
###                                                                                                                                                                 ###
############################################################################################
newdata = data.frame(sqft=3380,bedrooms=5)


predict(ajuste,newdata,interval="prediction")

##################################################################################################
# Sele��o de Vari�veis 													#	
#																#	
#################################################################################################

# Selecionar um conjunto de vari�veis preditoras em um conjunto maior(e.g., stepwise selection)
# Podemos fazer sele��o de vari�veis stepwise  (forward, backward,ou ambas) usando 
# a fun��o stepAIC( ) do pacote MASS.


 
# Stepwise Regression
install.packages('MASS')
library(MASS)
ajuste1=lm(log(price) ~ sqft + bedrooms+baths)
step <- stepAIC(ajuste1, direction="both")
step$anova # display results


