
################################################################################
###                               Lendo os dados                                                                                                                     #
################################################################################

gerentes = read.table("gerentes.txt",header=T)
##  header=T indica que a primeira linha do arquivo cont�m os r�tulos da planilha
 attach(gerentes)
## attach anexa o objeto gerentes no caminho de procura do software.
 gerentes
head(gerentes)
## Para deixar de anexar o objeto use 
## detach(gerentes)

################################################################################
##  Estat�stica Descritiva                                                                                                                                         #                                  
################################################################################

summary(Salario)
 var(Salario)
 sd(Salario)

summary(Experiencia)
 var(Experiencia)
 sd(Experiencia)

################################################################################
#                               Diagrama de Dispers�o 					                #			                  
################################################################################

plot(Experiencia,Salario,pch=19,col='blue')

#Mudar a legenda = xalb

#Ligar os pontos = type = l 

## Voc� pode mudar o tipo de ponto a ser plotado, basta usar "pch" como par�metro
## da fun��o
# pch=19: solid circle,
# pch=20: bullet (smaller solid circle, 2/3 the size of 19),
# pch=21: filled circle,
# pch=22: filled square,
# pch=23: filled diamond,
# pch=24: filled triangle point-up,


################################################################################
#                                      Coeficiente de Correla��o Linear de Pearson			                #			                  
################################################################################

 cor(Experiencia,Salario)

################################################################################
###               Teste de Hip�teses para  o Coeficiente de Correla��o                                                                  #
###              (supondo que as suposi��es do teste sejam satisfeitas):                                                               #
################################################################################

cor.test(Experiencia,Salario,conf.level = 0.95)
# P valor abaixo do n�vel de signific�ncia = rejeita hip�tese nula, ou seja h� correla��o
# P valor acima do n�vel de signific�ncia = n�o rejeita hip�tese nula

###############################################################################
####                                                 Ajuste do Modelo Linear                                                                              #
###############################################################################

# Retorna B0 e B1
ajuste=lm(Salario ~ Experiencia)
 ajuste
#### Com a fun��o summary, diversas medidas descritivas �teis para a an�lise do ajuste podem ser obtidas:
summary(ajuste)


##############################################################################
###                                            Tabela da An�lise de Vari�ncia                                                                     ##
##############################################################################

anova(ajuste)


###############################################################################
###                                   Esbo�ando  a reta ajustada no diagrama de dispers�o                                        ##
###############################################################################

windows()
 plot(Experiencia,Salario)
 abline(lm(Salario ~ Experiencia))

 plot(Experiencia,Salario)
 abline(lm(Salario ~ Experiencia),col='red',lty=2)

##############################################################################
##            Intervalos de confian�a para \beta_0  e \beta_1                                                                            #
##############################################################################

confint(ajuste)


##################################################################################
##                An�lise dos Res�duos                                                                                                                               ##
###            ---------------------------                                                                                                                          ##                                                                                    
## Para avaliar as suposi��es de que os erros possuem vari�ncia constante (homocedasticidade) e s�o n�o correlacionados ##
## entre si, construa os gr�ficos de �Res�duos versus Valores Ajustados da Vari�vel Resposta� e             ##
###�Res�duos versus   Valores da Vari�vel Explicativa"                                                                                      ## 
#################################################################################

 windows()
# homocedasticidade (igualdade de vari�ncia)
 plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Res�duos")
 abline(h=0)
# identidade (aleatoriedade dos res�duos)
 windows()
 plot(Experiencia,residuals(ajuste),xlab="Experi�ncia",ylab="Res�duos")
 abline(h=0)

######## ou ###################################################
par(mfrow=c(2,2))
plot(ajuste)

####Para exibir os Valores Ajustados e os Res�duos do ajuste, digite os comandos:

 ajuste$residuals
 ajuste$fitted.values

#################################################################################
##    Parece existir uma viola��o da Homocedasticidade   (vide gr�fico anterior )                                           ##
##      pode-se dividir o conjunto de dados em duas partes, utilizando a mediana por exemplo, e          ##
##        realizar um teste para comparar as vari�ncias de cada subconjunto:                                                ##
##################################################################################


median(Experiencia)

var.test(residuals(ajuste)[gerentes$Experiencia>13],residuals(ajuste)[gerentes$Experiencia<13])

########################################################################################
#  Outra maneira de avaliar a heterocedasticidade dos erros � realizar algum teste de homocedasticidade.       ##
#  Na biblioteca lmtest do R, a fun��o bptest realiza o teste de Breusch-Pagan. Ressalta-se, entretanto ,          ##
#     que tal teste n�o � muito poderoso e pode levar � erros                                                                                            ##
########################################################################################
# teste de Breusch-Pagan para heterocedasticidade (H0=homocedasticidade)
# carregar biblioteca lmtest
install.packages('lmtest')
library (lmtest)
bptest(ajuste)




########################################################################################
####                                                Avaliando a normalidade dos res�duos 				              ##
###              Para avaliar a suposi��o de normalidade dos erros, deve-se construir o gr�fico da 	              ##
####                                    �Probabilidade Normal   dos Res�duos�:				              ##
########################################################################################

windows()
qqnorm(residuals(ajuste), ylab="Res�duos",xlab="Quantis te�ricos",main="")
 qqline(residuals(ajuste))

#######################################################################################
##  Teste de normalidade dos Res�duos                                                                                                                            ###
#######################################################################################

shapiro.test(residuals(ajuste))

##### Testes opcionais 

install.packages("nortest")
library("nortest")

### Anderson- Darling
ad.test(residuals(ajuste)) 

####       cvm.test()
###       Realiza o teste de Cramer-von Mises para normalidade. 


###    lillie.test()
##       Realiza o teste de Lilliefors (Kolmogorov-Smirnov) para normalidade. 



###  pearson.test()
###  Realiza o teste Qui-quadrado de Pearson para normalidade. 




############################################################################################
##                                             Intervalos de Confian�a para Resposta M�dia e Individual                                               #####   
###    Dado um novo conjunto de preditoras, X = X0, a fim de fazer infer�ncia sobre os valores preditos                  ### 
#### das respostas m�dia e individual de Y , utilize a fun��o predict(). Essa fun��o requer que o segundo                ###
###    argumento seja um data frame com as covari�veis nomeadas do mesmo modo que o banco                               ###
###      de dados original:                                                                                                                                                                  ###
############################################################################################

x0 = data.frame(Experiencia=3)
### resposta m�dia
predict(ajuste,x0,interval="confidence")
####  resposta individual
 predict(ajuste,x0,interval="prediction")

############################################################################################
###     se X0 � um vetor, por exemplo, X0= (0; 1; ...; 34; 35), pode-se contruir gr�ficos com as estimativas                 ####
###    pontuais e intervalares das respostas m�dias e individuais, por meio dos respectivos comandos :                   ####
############################################################################################
 windows()
 par(mfrow=c(2,1))
 x0 = data.frame(Experiencia=seq(0,35,1))
 p1 = predict(ajuste,x0,interval="confidence",se=T)
 matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Experi�ncia",ylab="Sal�rio")
p2 = predict(ajuste,x0,interval="prediction",se=T)
 matplot(x0,p2$fit,lty=c(1,2,2),type="l",xlab="Experi�ncia",ylab="Sal�rio")
###############################################################################################