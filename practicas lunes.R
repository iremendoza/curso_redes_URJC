
####CURSO DE REDES DE LA UNIVERSIDAD REY JUAN CARLOS####
###LUNES 27 DE MAYO DEL 2019###

###PRACTICA 1###

library(bipartite)
library(tidyverse)

df = read.table("List_interactions.txt", header=T, sep="\t")

#se selecciona la localidad de Nevero#

df.nev = df %>% filter(Habitat.type == "Pasture", Study.site =="nevero") %>% droplevels ()

web = matrix(table(df.nev$Plant, df.nev$Insect), nrow = length(levels(df.nev$Plant)), ncol = length(levels(df.nev$Insect)))

rownames(web)=levels(df.nev$Plant)
colnames(web)=levels(df.nev$Insect)

edit(web)

#Número de visitas de la matriz de adyacencia
rowSums(web)
colSums(web)
V = sum(web)

#Número total de interacciones únicas

(I = sum(web!= 0))

#Densidad de interacciones para plantas

I.plants = I/nrow(web)

#Densidad de interacciones para animales

I.animals = I/ncol(web)

#conectancia = porcentaje de interacciones que se observaron del total posible

(C=I/length(web))
(C.percentage=C*100)

(rowvisits=rowSums(web))
(colvisits=colSums(web))

binary.web = web

binary.web[which(web!=0)]=1

sum(binary.web)

(rowinteractions=rowSums(binary.web))
(colinteractions=colSums(binary.web))


par(mfrow=c(2,2),mar=c(6,3,3,1))
barplot(rowinteractions,las=2,main="Plant interactions")
barplot(colinteractions,las=2,main="Animal interactions")
barplot(rowvisits,las=2,main="Plant visits")
barplot(colvisits,las=2, main="Animal visits")


par(mfrow=c(2,2),mar=c(6,3,5,1))
barplot(rowinteractions[order(rowinteractions,decreasing=T)],las=2,main="Plant interactions")
barplot(colinteractions[order(colinteractions,decreasing=T)],las=2,main="Animal interactions")
barplot(rowvisits[order(rowvisits,decreasing=T)],las=2,main="Plant visits")
barplot(colvisits[order(colvisits,decreasing=T)],las=2,main="Animal visits")

###FOOD WEBS###

library(foodweb)
data(moss)
write.csv(moss, file = "moss.csv", sep = ",", col.names = "F")
