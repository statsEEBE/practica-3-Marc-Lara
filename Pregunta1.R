
#Pràctica 3

x<- c(0,1)                              #Definir variable aleatoria, o té +=2 teles o no
f<-c(0.68,0.32)                         #Funció massa Bernulli (o si o no)
plot(f~x, ylim=c(0,1), col='red')


n<- 43
sum(sample(x,n,f,replace=TRUE))         #Enquesta amb n assajos dels valors de x amb f probabiliat dels successos de x


Y<- function(i) {sum(sample(x,n,f,replace=TRUE))}
Y(i)

encuestas <- sapply(1:400000,Y)           #Iteracions , faig 40 vegades les 43 enquestes
encuestas

fr<- table (encuestas)/400000                  #taula de freq relatives

barplot(table (encuestas)/400000)

fr['13']

dbinom(13, 43, 0.32)                        #prob de tenir 13 sís en una enquesta de 43 valors, on p(sí)= 0.32

y<- 0:43
plot(y,dbinom(y, 43, 0.32), type='h', col='red')      # graficar prob d'obtenir yi sís en 43 preg


pbinom(16, 44, 0.32)                         # Pbinom és la freq acomulada. de [0,16]    pbinom no= dbinom

qbinom(0.5, 44, 0.32)                        # media de la binomial p(xi> media) = p(xi< media)
qbinom(0.25, 44, 0.32)                       # valor quartil, num que deixa el 25% de aera (probabilitat) a l'esquerra



