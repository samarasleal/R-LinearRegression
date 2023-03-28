
################################################################################
###                               Lendo os dados                                                                                                                     #
################################################################################

gerentes = read.table("gerentes.txt",header=T)
##  header=T indica que a primeira linha do arquivo contém os rótulos da planilha
 attach(gerentes)
## attach anexa o objeto gerentes no caminho de procura do software.
 gerentes
head(gerentes)
## Para deixar de anexar o objeto use 
## detach(gerentes)

################################################################################
##  Estatística Descritiva                                                                                                                                         #                                  
################################################################################

summary(Salario)
 var(Salario)
 sd(Salario)

summary(Experiencia)
 var(Experiencia)
 sd(Experiencia)

################################################################################
#                               Diagrama de Dispersão 					                #			                  
################################################################################

plot(Experiencia,Salario,pch=19,col='blue')

#Mudar a legenda = xalb

#Ligar os pontos = type = l 

## Você pode mudar o tipo de ponto a ser plotado, basta usar "pch" como parâmetro
## da função
# pch=19: solid circle,
# pch=20: bullet (smaller solid circle, 2/3 the size of 19),
# pch=21: filled circle,
# pch=22: filled square,
# pch=23: filled diamond,
# pch=24: filled triangle point-up,


################################################################################
#                                      Coeficiente de Correlação Linear de Pearson			                #			                  
################################################################################

 cor(Experiencia,Salario)

################################################################################
###               Teste de Hipóteses para  o Coeficiente de Correlação                                                                  #
###              (supondo que as suposições do teste sejam satisfeitas):                                                               #
################################################################################

cor.test(Experiencia,Salario,conf.level = 0.95)
# P valor abaixo do nível de significância = rejeita hipótese nula, ou seja há correlação
# P valor acima do nível de significância = não rejeita hipótese nula

###############################################################################
####                                                 Ajuste do Modelo Linear                                                                              #
###############################################################################

# Retorna B0 e B1
ajuste=lm(Salario ~ Experiencia)
 ajuste
#### Com a função summary, diversas medidas descritivas úteis para a análise do ajuste podem ser obtidas:
summary(ajuste)


##############################################################################
###                                            Tabela da Análise de Variância                                                                     ##
##############################################################################

anova(ajuste)


###############################################################################
###                                   Esboçando  a reta ajustada no diagrama de dispersão                                        ##
###############################################################################

windows()
 plot(Experiencia,Salario)
 abline(lm(Salario ~ Experiencia))

 plot(Experiencia,Salario)
 abline(lm(Salario ~ Experiencia),col='red',lty=2)

##############################################################################
##            Intervalos de confiança para \beta_0  e \beta_1                                                                            #
##############################################################################

confint(ajuste)


##################################################################################
##                Análise dos Resíduos                                                                                                                               ##
###            ---------------------------                                                                                                                          ##                                                                                    
## Para avaliar as suposições de que os erros possuem variância constante (homocedasticidade) e são não correlacionados ##
## entre si, construa os gráficos de “Resíduos versus Valores Ajustados da Variável Resposta” e             ##
###“Resíduos versus   Valores da Variável Explicativa"                                                                                      ## 
#################################################################################

 windows()
# homocedasticidade (igualdade de variância)
 plot(fitted(ajuste),residuals(ajuste),xlab="Valores Ajustados",ylab="Resíduos")
 abline(h=0)
# identidade (aleatoriedade dos resíduos)
 windows()
 plot(Experiencia,residuals(ajuste),xlab="Experiência",ylab="Resíduos")
 abline(h=0)

######## ou ###################################################
par(mfrow=c(2,2))
plot(ajuste)

####Para exibir os Valores Ajustados e os Resíduos do ajuste, digite os comandos:

 ajuste$residuals
 ajuste$fitted.values

#################################################################################
##    Parece existir uma violação da Homocedasticidade   (vide gráfico anterior )                                           ##
##      pode-se dividir o conjunto de dados em duas partes, utilizando a mediana por exemplo, e          ##
##        realizar um teste para comparar as variâncias de cada subconjunto:                                                ##
##################################################################################


median(Experiencia)

var.test(residuals(ajuste)[gerentes$Experiencia>13],residuals(ajuste)[gerentes$Experiencia<13])

########################################################################################
#  Outra maneira de avaliar a heterocedasticidade dos erros é realizar algum teste de homocedasticidade.       ##
#  Na biblioteca lmtest do R, a função bptest realiza o teste de Breusch-Pagan. Ressalta-se, entretanto ,          ##
#     que tal teste não é muito poderoso e pode levar à erros                                                                                            ##
########################################################################################
# teste de Breusch-Pagan para heterocedasticidade (H0=homocedasticidade)
# carregar biblioteca lmtest
install.packages('lmtest')
library (lmtest)
bptest(ajuste)




########################################################################################
####                                                Avaliando a normalidade dos resíduos 				              ##
###              Para avaliar a suposição de normalidade dos erros, deve-se construir o gráfico da 	              ##
####                                    “Probabilidade Normal   dos Resíduos”:				              ##
########################################################################################

windows()
qqnorm(residuals(ajuste), ylab="Resíduos",xlab="Quantis teóricos",main="")
 qqline(residuals(ajuste))

#######################################################################################
##  Teste de normalidade dos Resíduos                                                                                                                            ###
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
##                                             Intervalos de Confiança para Resposta Média e Individual                                               #####   
###    Dado um novo conjunto de preditoras, X = X0, a fim de fazer inferência sobre os valores preditos                  ### 
#### das respostas média e individual de Y , utilize a função predict(). Essa função requer que o segundo                ###
###    argumento seja um data frame com as covariáveis nomeadas do mesmo modo que o banco                               ###
###      de dados original:                                                                                                                                                                  ###
############################################################################################

x0 = data.frame(Experiencia=3)
### resposta média
predict(ajuste,x0,interval="confidence")
####  resposta individual
 predict(ajuste,x0,interval="prediction")

############################################################################################
###     se X0 é um vetor, por exemplo, X0= (0; 1; ...; 34; 35), pode-se contruir gráficos com as estimativas                 ####
###    pontuais e intervalares das respostas médias e individuais, por meio dos respectivos comandos :                   ####
############################################################################################
 windows()
 par(mfrow=c(2,1))
 x0 = data.frame(Experiencia=seq(0,35,1))
 p1 = predict(ajuste,x0,interval="confidence",se=T)
 matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Experiência",ylab="Salário")
p2 = predict(ajuste,x0,interval="prediction",se=T)
 matplot(x0,p2$fit,lty=c(1,2,2),type="l",xlab="Experiência",ylab="Salário")
###############################################################################################