
###########################################################################
#
# Introdução ao R
# 
# Análise de dados
#
###########################################################################

# limpando a memória R:
rm(list=ls(all=TRUE))

##################################################################################################
# parte 1
##################################################################################################


# lendo o banco de dados:
dados <- read.table("dados_diabetes_todo.csv", sep=";", header=TRUE)

# verificando a classe e o tipo do objeto dados:
class(dados)
typeof(dados)

#  verificando o nome das variáveis pertencentes ao objeto dados:
names(dados)


# Vamos dar uma olhada agora nos tipos de variáveis que nós temos no nosso objeto "dados"

classe <- sapply(dados, class)
tipo <- sapply(dados, typeof)
info.dados <- cbind(classe,tipo)
info.dados

# observação: estudaremos a função sapply() mais tarde...


##################################################################################################
# parte 2
##################################################################################################

# Fazendo alguns testes com diferentes tipos/classes de variáveis:


# aplicando as funções summary() e plot() a um objeto da classe "factor" (tipo "integer")
class(dados$sexo)
typeof(dados$sexo)
dados$sexo
summary(dados$sexo)
plot(dados$sexo)


# agora vamos aplicar as funções summary() e plot() a um objeto da classe "numeric" e do tipo "double" 
class(dados$peso)
typeof(dados$peso)
summary(dados$peso)
plot(dados$peso)


# agora vamos aplicar as funções summary() e plot() a um objeto do tipo "character" 
sexo.char <- as.character(dados$sexo) # converte a variável sexo para a classe/tipo "character". 
class(sexo.char)
typeof(sexo.char)
summary(sexo.char)
plot(sexo.char)


################################################################################################################
################################################################################################################
################################################################################################################

# ANÁLISE EXPLORATÓRIA DE VARIÁVEIS QUALITATIVAS

##################################################################################################
# parte 3
##################################################################################################

dados$estadocivil
dados$dieta

# verificando o classe e o tipo das variáveis escolaridade e dieta:

class(dados$estadocivil)
typeof(dados$estadocivil)

class(dados$dieta)
typeof(dados$dieta)

# aplicando a função table() as variáveis estadocivil e dieta:

table(dados$estadocivil)
table(dados$dieta)

# É possível salvar as tabelas em objetos, como por exemplo

tab <- table(dados$estadocivil)
str(tab)
class(tab)
typeof(tab)


##################################################################################################
# parte 4
##################################################################################################

# agora vamos aplicar a função plot() às variáveis estadocivil e dieta:

plot(dados$estadocivil) 
plot(dados$dieta)

# ops! o  primeiro gráfico sumiu!!!

plot(dados$estadocivil) 
win.graph()  # abrindo uma nova janela gráfica
plot(dados$dieta)

# fazendo um gráfico de barras para a variável dieta:

# solução (a)
dieta <- as.factor(dados$dieta)
plot(dieta)

# solução (b)
tab.dieta <- table(dados$dieta)
barplot(tab.dieta, main="solução (b)")

##################################################################################################
# parte 5
##################################################################################################

# Fazendo gráficos de barras com a função barplot()

barplot(dados$escolaridade)

barplot(table(dados$escolaridade))
win.graph()
barplot(table(dados$escolaridade), horiz=TRUE)


##################################################################################################
# parte 6
##################################################################################################

# construindo tabelas de contingência para as variáveis sexo e estadocivil:

tab_sexo.estadocivil <- table(dados$sexo, dados$estadocivil)
tab_sexo.estadocivil
rownames(tab_sexo.estadocivil)
colnames(tab_sexo.estadocivil)

tab_estadocivil.sexo <- table(dados$estadocivil,dados$sexo)
tab_estadocivil.sexo
rownames(tab_estadocivil.sexo)
colnames(tab_estadocivil.sexo)

##################################################################################################
# parte 7
##################################################################################################

# fazendo alguns gráficos de barras: 

barplot(tab_sexo.estadocivil,legend=rownames(tab_sexo.estadocivil))
win.graph()
barplot(tab_sexo.estadocivil, beside=TRUE,legend=rownames(tab_sexo.estadocivil))

# refazendo os gráficos com barras horizontais:

barplot(tab_sexo.estadocivil,legend=rownames(tab_sexo.estadocivil), horiz=TRUE)
win.graph()
barplot(tab_sexo.estadocivil, beside=TRUE,legend=rownames(tab_sexo.estadocivil), horiz=TRUE)

##################################################################################################
# parte 8
##################################################################################################


tab_estadocivil.sexo <- table(dados$estadocivil, dados$sexo)
barplot(tab_estadocivil.sexo, legend=colnames(tab_sexo.estadocivil))
win.graph()
barplot(tab_estadocivil.sexo, legend=colnames(tab_sexo.estadocivil), beside=TRUE)


##################################################################################################
# parte 9
##################################################################################################

# construindo gráfico de setores (pizza):

class(dados$escolaridade)
typeof(dados$escolaridade)

tab.escolaridade <- table(dados$escolaridade)
pie(tab.escolaridade, main="gráfico de setores (pizza)")


##################################################################################################
# parte 10
##################################################################################################

tab <- table(dados$escolaridade)
tab

# verificando a classe e o tipo do objeto "tab"

class(tab)
typeof(tab)

# obtendo uma tabela com as frequências relativas:
tab.freq.rel <- prop.table(tab)
tab.freq.rel

# alterando o  número de casas decimais da tabela:
round(tab.freq.rel,2) 

##################################################################################################
# parte 11
##################################################################################################

# construindo tabelas de contingência no R:
tab <- table(dados$sexo, dados$escolaridade)
tab

# encontrando as distribuições marginais de uma tabela  de contingência com duas variáveis:  

tab
margin.table(tab,1)    # soma as caselas por colunas
margin.table(tab,2)    # soma as caselas  por linhas
margin.table(tab)      # soma todas as caselas

##################################################################################################
# parte 12
##################################################################################################

# construindo uma tabela de frequências com quatro entradas:
ftab <- ftable(dados$sexo,dados$trabalho, dados$escolaridade, dados$estadocivil,
       dnn = c("sexo", "trabalho", "escolaridade", "estadocivil"))
ftab

#  construindo uma tabela de frequências relativas com quatro entradas:
ftab.freq.rel <- prop.table(ftab)
round(ftab.freq.rel,2)



##################################################################################################
# parte 13
##################################################################################################

# para usuários de latex:

#install.packages("xtable") # instala o pacote "xtable"
tab <- table(dados$grupo, dados$escolaridade)
tab

# carregando o pacote xtable:
require(xtable)
xtable(tab)



################################################################################################################
################################################################################################################
################################################################################################################

# ANÁLISE EXPLORATÓRIA DE VARIÁVEIS QUANTITATIVAS

##################################################################################################
# parte 14
##################################################################################################

# Construindo tabelas de distribuições de frequências:

# aplicando a função table() a uma variável quantitativa discreta:
table(dados$atividade.fisica)


# aplicando a função table() a uma variável quantitativa contínua:
table(dados$peso)


# variáveis contínuas -> precisamos agrupar os dados em classes de frequências:

range(dados$peso)
k=nclass.Sturges(dados$peso)  
min=min(dados$peso) - 0.3
max=max(dados$peso) + 0.3
h=(max-min)/k
tab <- table(cut(dados$peso, seq(min, max, by=h), right=FALSE)) 
tab


# usando a função barplot para fazer o histograma a partir da tabela obtida:
barplot(tab, space=0)


##################################################################################################
# parte 15
##################################################################################################


# visualizando a distribuição de frequências de uma variável quantitativa discreta

tab <- table(dados$atividade.fisica)
barplot(tab, xlab="número de atividades por semana", ylab="frequência")
barplot(prop.table(tab), xlab="número de atividades por semana", ylab="freq. relativa")


##################################################################################################
# parte 14
##################################################################################################


# obtendo algumas medidas de tendência central:

# média artmética:
mean(dados$ghb1)
# mediana:
median(dados$ghb1)
# moda:
mode(dados$ghb1)

######################################################################
#
# Exercício para casa: obter a moda de uma variável quantitativa
#
######################################################################


##################################################################################################
# parte 14
##################################################################################################

# obtendo algumas medidas de variabilidade:

# variância:
var(dados$ghb1)

# desvio padrão:
sd(dados$ghb1)

# range:
range(dados$ghb1)
 
#coeficiente de variação:
cv(dados$ghb1)

##################################################################################################
# parte 15
##################################################################################################


# escrevendo uma função que calcula o coeficiente de variação:

my.cv <- function(x)
{
	sd(x)/mean(x)
}
my.cv(dados$ghb1)


##################################################################################################
# parte 16
##################################################################################################

# Calculando percentis:


alpha <- 0.1
quantile(dados$ghb1, alpha)

# passando um conjunto de valores de alpha:

alpha <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
quantile(dados$ghb1, alpha)


##################################################################################################
# parte 17
##################################################################################################


# a função summary fornece um sumário com algumas características de uma variável quantitativa:
summary(dados$ghb1)


# usando a função sapply() para obter a classe e o tipo das variáveis do nosso banco de  dados

classe <- sapply(dados,class)
tipo <- sapply(dados, typeof)

classe
tipo

# mostrando na tela:
cbind(classe, tipo)

##################################################################################################
# parte 18
##################################################################################################

# Agora vamos usar a variável tipo, vamos selecionar as variáveis contínuas do nosso banco de dados

nomes.discretas <- c("idade","filhos", "atividade.fisica")
nomes.continuas <- subset(names(dados), tipo=="double")

discretas <- dados[,nomes.discretas]
continuas <- dados[,nomes.continuas]

str(discretas)
str(continuas)

##################################################################################################
# parte 19
##################################################################################################

# usando a as funções sapply() e summary() para obter sumarios das nossas variáveis:

sumario.d <- sapply(discretas,summary)
sumario.c <- sapply(continuas,summary)
sumario.d
sumario.c

# conferindo os objetos obtidos...

class(sumario.c)
typeof(sumario.c)

class(sumario.d)
typeof(sumario.d)


# transpondo as tabelas:

t(sumario.c)
t(sumario.d)

##################################################################################################
# parte 20
##################################################################################################

# Agora vamos  construir uma função para fornecer algumas medidas de variabilidade

summary.var <- function(x)
{	
	var <- var(x)
	dp <- sd(x)
	cv <- my.cv(x)
	out <- c(var, dp, cv)
	names(out) <-  c("var", "dp", "cv")
	out
}

#  calculando as medidas de variabilidade para a variável peso:

sumario.var <- summary.var(dados$peso)
sumario.var

# obtendo as medidas de variabilidade para todas as variáveis  contínuas do banco de dados:
sapply(continuas, summary.var)


##############################################################
# Exercício: construir a sua versão da função summary()
##############################################################



##################################################################################################
# parte 21
##################################################################################################

# criando uma lista:

summary.var2 <- function(x)
{	
	out <- list(var=var(x), dp=sd(x), cv=my.cv(x), range=range(x))
}

sumario.var2 <- summary.var2(dados$idade)
sumario.var2


class(sumario.var)
typeof(sumario.var)
str(sumario.var)
mode(sumario.var)

# criando uma lista, podemos guardar objetos de diferentes tamanhos...

##################################################################################################
# parte 22
##################################################################################################


# obtendo o diagrama de ramos e folhas para a variável ghb2:
dados$ghb2
stem(dados$ghb2)
stem(dados$ghb2, scale=2)


##################################################################################################
# parte 23
##################################################################################################

# utilizando o comando source para carregar um conjunto de funções 

source("myfunctions.R")

# a função get.info fornece a classe e o tipo de um objeto:
get.info(dados$atividade.fisica)


tab.atividade <- table(dados$atividade.fisica)
barplot(tab.atividade, xlab="n. de dias de ativividade física", ylab="frequência")

##################################################################################################
# parte 24
##################################################################################################

# obtendo o histograma e o boxplot para a variável peso:

hist(dados$peso)
win.graph()
boxplot(dados$peso)

h <- hist(dados$peso)
bp <- boxplot(dados$peso)
h
bp

##################################################################################################
# parte 25
##################################################################################################

# Adicionando o comando freq=FALSE para a área  do histograma somar 1:

hist(dados$peso)
win.graph()
hist(dados$peso, freq=FALSE)

# chamando a função showhist (do arquivo myfunctions.R) para fazer os dois histogramas na mesma janela: 
show.hists(dados$peso)


##################################################################################################
# parte 26
##################################################################################################

# usando o histograma e o boxplot para comparar duas distribuições:

circ.abdom <- split(dados$circ.abdom, dados$sexo)
class(circ.abdom)
typeof(circ.abdom)
str(circ.abdom)

hist(circ.abdom$M)
win.graph()
hist(circ.abdom$F)

# a comparação fica bem mais fácil se usarmos o boxplot:

boxplot(circ.abdom)

##################################################################################################
# parte 27
##################################################################################################

# construindo vários boxplots simultaneamente:

plot(idade~escolaridade, data=dados)
win.graph()
plot(idade~estadocivil, data=dados)

par(mfrow=c(2,1))
plot(idade~escolaridade, data=dados)
plot(idade~estadocivil, data=dados)


##################################################################################################
# parte 28
##################################################################################################

# construindo vários boxplots no  mesmo gráfico

idade.sexo <- split(dados$idade,dados$sexo)
idade.estadocivil <- split(dados$idade,dados$estadocivil)
idade.escolaridade <- split(dados$idade,dados$escolaridade)

# verificando o que temos em mãos...

class(idade.sexo)
typeof(idade.sexo)

# alternativamente, usando a função criada (na nossa biblioteca de funções "myfuncions.R")
get.info(idade.sexo) 


boxplot(idade.sexo)
win.graph()
boxplot(idade.estadocivil)


# contando o número de caixas:

length(idade.sexo)
length(idade.escolaridade)


#boxplot(idade.sexo, at = 1:2, xlim = c(0, 8), ylim = range(idade.sexo, idade.escolaridade), xaxt = "n", col=c(2,3), ylab="peso", xlab="variável")
boxplot(idade.sexo, at = 1:2, xlim = c(0, 8), ylim = c(10, max(dados$idade)), xaxt = "n", col=c(2,3), ylab="peso", xlab="variável")
boxplot(idade.escolaridade, at = 4:7 , xaxt = "n", add = TRUE, col=4:8)
#axis(1, 1:7)
axis(1, at = c(1.5, 5.5), labels=c("sexo", "escolaridade"))
legend("bottomleft",col=2:3, names(idade.sexo), lty=1, lwd=1, bty = "n")
legend("bottomright",col=4:8, names(idade.escolaridade), lty=1, lwd=1, bty = "n")


##################################################################################################
# parte 29
##################################################################################################


# alternativamente: 

win.graph()
#boxplot(idade.sexo, at = 1:2, xlim = c(0, 8), ylim = range(idade.sexo, idade.escolaridade), xaxt = "n", col=c(2,3), ylab="peso", xlab="variável")
boxplot(idade.sexo, at = 1:2, xlim = c(0, 8), ylim = c(10, max(dados$idade)), xaxt = "n", col=c(2,3), ylab="peso", xlab="variável")
boxplot(idade.escolaridade, at = 4:7 , xaxt = "n", add = TRUE, col=4:8)
#axis(1, 1:7)
axis(1, at = c(1.5, 5.5), labels=c("sexo", "escolaridade"))
legend(0.6, 30, col=2:3, names(idade.sexo), lty=1, lwd=2, bty = "n")
legend(4.3, 30, col=4:8, names(idade.escolaridade), lty=1, lwd=2, bty = "n")







######################################################################################
# Exercício: refazer os gráficos referentes às alternativas 1 e 2 em duas janelas 
######################################################################################


save.image("aula03.RData")


###########################################################################
# Brincando com Gráficos
#
###########################################################################

# limpando a memória R:
rm(list=ls(all=TRUE))

# carregando a biblioteca "graphics"
require(graphics)


# carregando nossa biblioteca de funções
source("myfunctions.R")


##################################################################################################
# algumas demonstrações de tipos de gráficos que podem ser feitos com o R:

demo(graphics)
demo(persp)
demo(Hershey)
demo(plotmath)


##################################################################################################
# parte 1
##################################################################################################


# abrindo uma nova janela gráfica
win.graph()

# fechando uma janela gráfica
dev.off()


# explorando o argumento type da função plot():

n <-10
x <- seq(-10,10, length=n)
y <- seq(3,9, length=n)

plot(x,y, type="n", main="type='n'")
win.graph()
plot(x,y, main="type='p'")
win.graph()
plot(x,y, type="l", main="type='l'")
win.graph()
plot(x,y, type="b", main="type='b'")
win.graph()
plot(x,y, type="o", main="type='o'")
win.graph()
plot(x,y, type="s", main="type='s'")



##################################################################################################
# parte 2
##################################################################################################


# salvando as figuras em formato ps:

# crie uma pasta chamada graficos dentro do diretório de trabalho do R.

# isso pode ser feito via linha de comando da seguinte forma:
getwd()
dir.create("D:/Fabio/Extensao/2013/cursoR/graficos") 
postscript("graficos//type_n.ps")
plot(x,y, type="n", main="type='n'")
dev.off()

postscript("graficos//type_p.eps")
plot(x,y, type="p", main="type='p'")
dev.off()

pdf("graficos//type_l.pdf")
plot(x,y, type="l", main="type='l'")
dev.off()

png("graficos//type_b.png")
plot(x,y, type="b", main="type='b'")
dev.off()

jpeg("graficos//type_o.jpeg")
plot(x,y, type="o", main="type='o'")
dev.off()

bmp("graficos//type_s.bmp")
plot(x,y, type="s", main="type='s'")
dev.off()


tiff("graficos//pch.tiff")
plot(x,y)
dev.off()



##################################################################################################
# parte 3
##################################################################################################

# ilustrando as funcionalidades da função par():

n <- 50
x <- seq(-1,1,length=n)

op <- par(mfrow=c(2,2), lwd=2, cex.axis=1.5, cex.lab=1.5)
plot(x, x, type="l")
plot(x, x^2, type="l")
plot(x, 1/x, type="l")
plot(x, x^3, type="l")


##################################################################################################
# parte 4
##################################################################################################

# um outro exemplo, agora usando o comando mfcol:

win.graph()
plot(x, type="l", main="(a)")
plot(x, x^2, type="l", main="(b)")
plot(x, 1/x, type="l", main="(c)")
plot(x, x^3, type="l", main="(d)")
plot(x,1-x^2, type="l", main="(e)")
plot(x, 1-1/x, type="l", main="(f)")


##################################################################################################
# parte 5
##################################################################################################

# os argumentos passados a função par() são aplicados à apenas uma janela gráfica.
# se abrirmos simultaneamente várias janelas, precisamos redefinir os parâmetros gráficos
# passados a função par() para cada janela aberta!

win.graph()
op <- par(mfcol=c(2,3), lwd=2, cex.axis=1.5, cex.lab=1.5)
plot(x, type="l", main="(a)")
plot(x, x^2, type="l", main="(b)")
plot(x, 1/x, type="l", main="(c)")
plot(x, x^3, type="l", main="(d)")
plot(x,1-x^2, type="l", main="(e)")
plot(x, 1-1/x, type="l", main="(f)")


##################################################################################################
# parte 6
##################################################################################################

# se quisermos apagar da memória os argumentos passados á função par sem mudarmos de janela
# gráfica, basta digitarmos:

par(op)

#testando:
plot(x, type="l")

##################################################################################################
# parte 7
##################################################################################################

# ilustrando o comando par(new=TRUE)

n <- 20
x <- seq(-2,2,length=n)
y1 <- x
y2 <- x^2
y3 <- x^3

plot(x,y1, xlab="x", ylab="f(x)", type="l", lwd=2)
par(new=TRUE)
plot(x,y2, xlab="x", ylab="f(x)", type="l", lwd=2, lty=2)
par(new=TRUE)
plot(x,y3, xlab="x", ylab="f(x)", type="l", lwd=2, lty=3)


##################################################################################################
# parte 8
##################################################################################################

# acertando os eixos:

win.graph()
plot(x,y1, xlab="x", ylab="f(x)", type="l", lwd=2, xlim=range(x), ylim=range(y1,y2,y3))
par(new=TRUE)
plot(x,y2, xlab="x", ylab="f(x)", type="l", lwd=2, lty=2, ylim=range(y1,y2,y3))
par(new=TRUE)
plot(x,y3, xlab="x", ylab="f(x)", type="l", lwd=2, lty=3, ylim=range(y1,y2,y3))


##################################################################################################
# parte 9
##################################################################################################

# aplicando a função lines

n <- 50
x <- seq(-2*pi,2*pi, length=n)
y <- sin(x)

plot(x,y, xlab="x", ylab="seno(x)", main="Exemplo: lines()")
lines(x,y)
lines(x, rep(0,n), lty=2)

# comparando com o gráfico obtido através do argumento type="o" da função plot:
win.graph()
plot(x,y, xlab="x", ylab="seno(x)", main="type='o'", type="o")
#points(x,y, col="red", pch=16)

##################################################################################################
# parte 10
##################################################################################################

# aplicando simultaneamente as funções lines e abline:

win.graph()
plot(x,y, xlab="x", ylab="seno(x)", main="Exemplo: lines() e abline()")
lines(x,y)
x.axis <- seq(-6,6,by=2)
y.axis <- seq(-1,1,by=0.25)
abline(v=x.axis, h=y.axis, lty=2, col="gray")


##################################################################################################
# parte 11
##################################################################################################

# um exemplo com cores:

n <- 51
x <- seq(-1,1, length=n)
y <- x^2

plot(x,y, xlab="x", ylab=expression(f(x)==x^2), main="exemplo em cores")
lines(x[x<=0],y[x<=0], col="red", lwd=2)
lines(x[x>=0],y[x>=0], col="blue", lwd=2)
abline(v=seq(-1,1,by=0.5), lty=2, col="yellow")
abline(h=seq(0,1,by=0.25), lty=2, col="green")

# refinando as linhas horizontais/verticais

win.graph()
plot(x,y, xlab="x", ylab=expression(f(x)==x^2), main="exemplo em cores")
lines(x[x<=0],y[x<=0], col="red", lwd=2)
lines(x[x>=0],y[x>=0], col="blue", lwd=2)
abline(v=seq(-1,1,by=0.1), lty=2, col="yellow")
abline(h=seq(0,1,by=0.1), lty=2, col="green")

##################################################################################################
# parte 12
##################################################################################################

# removendo os eixos:

plot(x,y,axes=FALSE, type="o")

#adicionando os eixos:

win.graph()
plot(x,y,axes=FALSE, type="o")
axis(1,)
axis(2,)
axis(3,)
axis(4,)


##################################################################################################
# parte 13
##################################################################################################

# adicionando eixos editados:
range(x)
range(y)

x.axis <- seq(-1,1,by=0.25)
y.axis <- seq(-1,1,by=0.1)
x.axis
y.axis

plot(x,y,axes=FALSE, type="l", main="gráfico com eixos editados")
axis(1,x.axis)
axis(2,y.axis)

##################################################################################################
# parte 14
##################################################################################################

# um exemplo com cores...

plot(x,y,axes=FALSE, col="blue")
axis(1,x.axis)
axis(2,y.axis)
axis(3,x.axis)
axis(4,y.axis)
abline(v=round(x,1), col="gray", lty=2)
abline(h=round(y,1), col="gray", lty=2)
lines(x,y, col="red")



##################################################################################################
# parte 15
##################################################################################################

# adicionando expressões matemáticas

?plotmath

n <- 20
x <- seq(-1,1,length=n)
y1 <- x
y2 <- x^2
y3 <- x^3

op <- par(mfrow=c(2,2))
plot(x, x, main=expression(f(x)==x), xlab="x", ylab="f(x)", type="l")
plot(x, x^2, main=expression(f(x)==x^2), xlab="x", ylab="f(x)", type="l")
plot(x, 1/x, main=expression(f(x)==frac(1,x)), xlab="x", ylab="f(x)", type="l")
plot(x, x^3, main=expression(f(x)==x^3), xlab="x", ylab="f(x)", type="l")

 

##################################################################################################
# parte 16
##################################################################################################


# legendas com expressões matemáticas:

#op <- par(lwd=2, cex.axis=1.5, cex.lab=1.5)
plot(x,y1, xlim=range(x), ylim=range(y1,y2,y3), xlab="x", ylab="f(x)", type="l", main="legendas com expressões matemáticas")
par(new=TRUE)
plot(x,y2, xlim=range(x), ylim=range(y1,y2,y3), xlab="x", ylab="f(x)", type="l", col="red")
par(new=TRUE)
plot(x,y3, xlim=range(x), ylim=range(y1,y2,y3), xlab="x", ylab="f(x)", type="l", col="blue")
legend("bottomright", col=c("black", "red", "blue"), c(expression(f(x)==x), expression(f(x)==x^2),  expression(f(x)==x^3)), lty=1, lwd=1.25, bty="n", cex=1.25)
par(op)


##################################################################################################
# parte 17
##################################################################################################

# estudando a função segments()

# fixando a semente do gerador de números aleatórios.
set.seed(13042013)

# gerando um conjunto de pontos:
x <- 1:10
y <- sample(x) # toma uma permutação de x
cbind(x,y)

# ilustrando o uso da função segments():
par(lwd=2, cex.lab=1.25, cex.axis=1.25)
plot(x,y, axes=FALSE)
axis(1,1:10)
axis(2,1:10)
axis(3,1:10)
axis(4,1:10)
abline(h=y, v=x, col="gray", lty=3)
segments(x[8], y[8], x[3], y[3])
segments(x[2], y[2], x[4], y[4])
segments(x[1], y[1], x[5], y[5])
segments(x[10], y[10], x[6], y[6])



##################################################################################################
# parte 18
##################################################################################################

# ilustrando o uso da função arrows():

win.graph()
par(lwd=2, cex.lab=1.25, cex.axis=1.25)
plot(x,y, axes=FALSE)
axis(1,1:10)
axis(2,1:10)
axis(3,1:10)
axis(4,1:10)
abline(h=y, v=x, col="gray", lty=3)
arrows(x[8], y[8], x[3], y[3])
arrows(x[2], y[2], x[4], y[4])
arrows(x[1], y[1], x[5], y[5])
arrows(x[10], y[10], x[6], y[6])


##################################################################################################
# parte 19
##################################################################################################

# mais um exemplo...

par(lwd=2, cex.lab=1.52, cex.axis=1.25)
plot(x,y, axes=FALSE)
axis(1,1:10)
axis(2,1:10)
axis(3,1:10)
axis(4,1:10)
abline(h=y, v=x, col="gray", lty=3)
arrows(x[1:9], y[1:9], x[2:10], y[2:10])
arrows(x[10], y[10], x[1], y[1], col="red")



##################################################################################################
# parte 20
##################################################################################################

# passeando aleatoriamente pelo quadrado 16x16:

set.seed(13042013)
n <-16
seq <- 1:n
x <- sample(seq)
y <- sample(seq)
plot(x,y, axes=FALSE)
axis(1,1:n)
axis(2,1:n)
axis(3,1:n)
axis(4,1:n)
abline(h=1:n, v=1:n, col="gray", lty=3)
arrows(x[1:n-1],y[1:n-1], x[2:n], y[2:n])
par(new=TRUE)
plot(x[1],y[1], col="green", lwd=2, xlim=range(x),  ylim=range(y), xlab="", ylab="")
par(new=TRUE)
plot(x[n],y[n], col="red", lwd=2, xlim=range(x),  ylim=range(y), xlab="", ylab="")



##################################################################################################
# parte 21
##################################################################################################


# lendo o banco de dados dos pacientes diabéticos:
dados <- read.table("dados_diabetes_todo.csv", sep=";", header=TRUE)
info <- sapply(dados,get.info)
info


##################################################################################################
# parte 22
##################################################################################################


# diagrama de dispersão das variáveis ghb1 e ghb2:
plot(dados$ghb1,dados$ghb2, main="diagrama de dispersão", xlab="ghb1", ylab="ghb2")

# diagrama de dispersão de ghb1 versus ghb2 de arcordo com a escolaridade:

win.graph()
data <- split(dados[c("ghb1", "ghb2")], dados$escolaridade)
par(mfrow=c(2,2), cex.axis=1.25, cex.lab=1.25)
plot(data$Analfabeto$ghb1, data$Analfabeto$ghb2, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2", main="Analfabeto")
plot(data$Fundamental$ghb1, data$Fundamental$ghb2, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2", main="Fundamental")
plot(data$Medio$ghb1, data$Medio$ghb2, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2", main="Medio")
plot(data$Superior$ghb1, data$Superior$ghb2, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2", main="Superior")


##################################################################################################
# parte 23
##################################################################################################

# utilizando o comando par(new=TRUE) para plotar ghb1 versus ghb2 por grupo num  mesmo gráfico:

# preparando os dados (maneira alternativa):

ghb1A <- subset(dados$ghb1, dados$grupo=="A")
ghb2A <- subset(dados$ghb2, dados$grupo=="A")

ghb1B <- subset(dados$ghb1, dados$grupo=="B")
ghb2B <- subset(dados$ghb2, dados$grupo=="B")

ghb1C <- subset(dados$ghb1, dados$grupo=="C")
ghb2C <- subset(dados$ghb2, dados$grupo=="C")


plot(ghb1A, ghb2A, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2")
par(new=TRUE)
plot(ghb1B, ghb2B, xlim=range(dados$ghb1), ylim=range(dados$ghb2), pch=2, xlab="gbh1", ylab="ghb2")
par(new=TRUE)
plot(ghb1C, ghb2C, xlim=range(dados$ghb1), ylim=range(dados$ghb2), pch=3, xlab="gbh1", ylab="ghb2")
legend("bottomright", pch=1:3, c("A", "B", "C"), bty="n")


# usando cores...

dev.new()
plot(ghb1A, ghb2A, xlim=range(dados$ghb1), ylim=range(dados$ghb2), xlab="gbh1", ylab="ghb2", pch=16)
par(new=TRUE)
plot(ghb1B, ghb2B, xlim=range(dados$ghb1), ylim=range(dados$ghb2), col="red", xlab="gbh1", ylab="ghb2", pch=16)
par(new=TRUE)
plot(ghb1C, ghb2C, xlim=range(dados$ghb1), ylim=range(dados$ghb2), col="blue", xlab="gbh1", ylab="ghb2", pch=16)
legend("bottomright", col=c("black", "red", "blue"), c("A", "B", "C"), lwd=2, lty=1, bty="n")


##################################################################################################
# parte 24
##################################################################################################


# Lendo os bancos de dados:

# Dados de poluentes na cidade de São Paulo, 1º Jan. a 30 Abr. 1991. 
# Variáveis:
# CO: monóxido de carbono(ppm); O3 : ozônio(ppb); temp: temperatura(ºC); umid: umidade relativa do ar ao meio-dia (%). 
# Fonte: Livro Estatística Básica, Wilton Bussab & Pedro Morettin, Editora Saraiva, 7ª Edição.								

poluicao <- read.table("poluicao.txt", header=TRUE)
info <- sapply(poluicao,get.info)
info


##################################################################################################
# parte 25
##################################################################################################

plot(poluicao$CO)

# editando o gráfico:
win.graph()
alpha <- c(0.01, 0.25, 0.5, 0.75, 0.99)
q <- round(quantile(poluicao$CO, alpha),1)
aux <- sort(c(range(poluicao$CO),q))
plot(poluicao$CO, type="o", xlab="tempo (dias)", ylab="CO (ppm)", main="Cidade: São Paulo", 
sub="1º Jan. a 30 Abr. 1991", axes=FALSE)
axis(1, seq(0,120, by=30))
axis(2,aux)
#abline(h=mean(poluicao$C), col="gray", lty=2)
abline(h=q, col="gray", lty=2)

# Exercício: colorir o gráfico...


##################################################################################################
# parte 26
##################################################################################################

# dividindo a janela gráfica em 4 partes:
par(mfrow=c(2,2))
plot(poluicao$CO, type="o", xlab="dia", ylab="monóxido de carbono", main="(a)")
plot(poluicao$O3, type="o", xlab="dia", ylab="ozônio", main="(b)")
plot(poluicao$temp, type="o", xlab="dia", ylab="temperatura", main="(c)")
plot(poluicao$umid, type="o", xlab="dia", ylab="umidade relativa do ar", main="(d)")


##################################################################################################
# parte 27
##################################################################################################

# editando os gráficos...

par(mfrow=c(2,2))
plot(poluicao$CO, type="o", xlab="dia", ylab="monóxido de carbono", main="(a)", axes=FALSE, pch=16)
abline(h=mean(poluicao$CO), col="red")
axis(1,)
axis(2,)
plot(poluicao$O3, type="o", xlab="dia", ylab="ozônio", main="(b)", axes=FALSE, pch=16)
abline(h=mean(poluicao$O3), col="red")
axis(1,)
axis(2,)
plot(poluicao$temp, type="o", xlab="dia", ylab="temperatura", main="(c)", axes=FALSE, pch=16)
abline(h=mean(poluicao$temp), col="red")
axis(1,)
axis(2,)
plot(poluicao$umid, type="o", xlab="dia", ylab="umidade relativa do ar", main="(d)", axes=FALSE, pch=16)
abline(h=mean(poluicao$umid), col="red")
axis(1,)
axis(2,)


##################################################################################################
# parte 28
##################################################################################################

# distribuição normal

?dnorm


# gerando uma amostra de tamanho n=100 de X~N(75,7)
# fixando a semente (para obtermos sempre a mesma amostra):
set.seed(13042013)

n=500
mu <- 75
sigma <- 7
x <- rnorm(n, mu, sigma)
summary(x)
summary.var(x)

win.graph()
hist(x, prob=TRUE)
lines(x, dnorm(x,mean(x), sd(x)))


##################################################################################################
# parte 29
##################################################################################################


# precisamos ordenar os dados primeiro...
win.graph()
hist(x, prob=TRUE)
x <- sort(x)
lines(x, dnorm(x,  mean(x), sd(x)))


##################################################################################################
# parte 30
##################################################################################################

# ainda resta acertarmos o eixo y:
win.graph()
y <- dnorm(x,mean(x), sd(x))
hist(x, prob=TRUE, ylim=range(y))
lines(x, dnorm(x, mean(x), sd(x)))


##################################################################################################
# parte 31
##################################################################################################

# fazendo algumas contas:

quantile(x, 0.05)
qnorm(0.05,mu,sigma)

quantile(x, 0.90, lower.tail=FALSE)
qnorm(0.1,mu,sigma, lower.tail=FALSE)

# P(X>90):
pnorm(90,mu,sigma, lower.tail=FALSE)

# P(X<70):
pnorm(70,mu,sigma)



##################################################################################################
# parte 32
##################################################################################################

# Brincando com a distribuição normal padrão - Z ~ N(0,1):

# calculando P(Z < 1.96):
round(pnorm(1.96),3)

# calculando P(Z > 1.96):
round(pnorm(1.96, lower.tail=FALSE),3)

# calculando percentis:

# z : P(Z<z) = 0.05
round(qnorm(0.05),3)
# z : P(Z>z) = 0.05
round(qnorm(0.05, lower.tail=FALSE),3)
round(qnorm(0.95),3)


##################################################################################################
# parte 33
##################################################################################################

# Distribuição binomial:

?dbinom

n <- 3
prob <- 0.4
x <- 0:n

p <- dbinom(x,n,prob)
P <- pbinom(x,n,prob)
cbind(x,p,P)

##################################################################################################
# parte 34
##################################################################################################

# calculando algumas probabilidades:

# P(X=3)
dbinom(3,n,prob)

# P(X=1)
dbinom(1,n,prob)


# P(X <= 1) = P(X=0) + P(X=1)
pbinom(1, n, prob)

#conferindo
sum(p[1:2])


# P(X>1) = P(X >= 2) = P(X=2) + P(X=3) = 1 - [P(X=0) + P(X=1)]
pbinom(1,n,prob, lower.tail=FALSE)
1-pbinom(1, n, prob)

# P(X >= 1) = P(X>0) = P(X=1) + P(X=2) + P(X=3) = 1 - P(X=0)
pbinom(0,n,prob, lower.tail=FALSE)
1-pbinom(0, n, prob)



##################################################################################################
# parte 35
##################################################################################################

# explorando um pouco mais...

prob <- 0.5
n  <- 10
x <- 0:n

p <- dbinom(x, n,  prob)
P <- pbinom(x, n,  prob)
tab1 <- round(cbind(x,p,P),3)
tab1

q <- seq(0.1,.9,by=0.1)
quantis <- qbinom(q, n, prob)

tab2 <- cbind(q,quantis)
tab2

##################################################################################################
# parte 36
##################################################################################################

# fixando a semente:
set.seed(13042013)

# número de valores gerados
N <- 1000

# probabilidades de sucesso
prob1 <- 0.1
prob2 <- 0.5
prob3 <- 0.9

# número de ensaios:
n  <- 100

# gerando as amostras:
x1 <- rbinom(N,n,prob1)
x2 <- rbinom(N,n,prob2)
x3 <- rbinom(N,n,prob3)
x <-cbind(x1,x2,x3)

# transformando o objeto x num data.frame:
x <- as.data.frame(x)

sapply(x,summary)

par(mfrow=c(1,3))
hist(x1, prob=TRUE, xlim=range(x), ylim=c(0,0.15), main="prob=0.1")
hist(x2, prob=TRUE, xlim=range(x), ylim=c(0,0.15), main="prob=0.5")
hist(x3, prob=TRUE, xlim=range(x), ylim=c(0,0.16), main="prob=0.9")


##################################################################################################
# parte 37
##################################################################################################

# fixando a semente do gerador de números aleatórios:
set.seed(13042013)

# Permutações:

x <- 1:7
y <- sample(x)
cbind(x,y)


# amostragem com reposição:

amostra1 <- sample(x, 3, replace = TRUE)
amostra1

amostra2 <- sample(x, 3, replace = TRUE)
amostra2

amostra3 <- sample(x, 3, replace = TRUE)
amostra3

# amostragem sem reposição:

amostra4 <- sample(x, 3, replace = FALSE)
amostra4






