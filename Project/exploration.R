#Projet Rec. Sys Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2,lsa,tidytext)
rm(list=ls())
setwd('C:/Users/mikap/OneDrive/Documents/GitHub/RecSys/Project')
#setwd('C:/Users/claudedb/Documents/GitHub/RecSys/Project')
# Data Load
text.data<-data.table(read.table("data/out.matrix",sep=" ",skip=2))
colnames(text.data)<-c('courses.id','terms.id','n')
m.text <- sparseMatrix(text.data$terms.id,text.data$courses.id,x=text.data$n)

courses.data<-read.table('data/out.docs',sep="/")
colnames(courses.data)<-c('university','code')

terms.data<-read.table('data/out.terms')

colnames(m.text) <- as.vector(courses.data[,2])
rownames(m.text) <- as.vector(terms.data[,1])

# Nous garderons seulement les cours a la polyy
# Pour l'instant nous n'avons pas de facon de tous les analyses
id.poly <- which(courses.data$university=="Poly")
id.hec <- which(courses.data$universtity=="HEC")
m.poly<-m.text[,id.poly]

#
m.poly<-m.poly[rowSums(m.poly)>0,]

#On voit que le termes qui apparait le plus souvent est 
#de et il est présent 11332 fois dans les 1168 descriptions

which.max(rowSums(m.poly))
max(rowSums(m.poly))
#En moyenne, notre terme max se retrouve 9.7 fois par description
max(rowSums(m.poly))/dim(m.poly)[2]

#% de réduction de la matrice 
dim(m.poly)[1]/dim(m.text)[1]
dim(m.poly)[2]/dim(m.text)[2]


#Moyenne de mots dans les description
mean(colSums(m.poly))
sd(colSums(m.poly))
