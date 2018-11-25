#Projet Rec. Sys Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2,lsa,tidytext)
rm(list=ls())
order.partial=function(vec)
  {
  idx<-which(vec<=sort(vec,partial=11)[11])
  idx[order(vec[idx])][2:11]
}

cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));k= sqrt(rowSums(t(v^2))); p=(t(v) %*% m)/k; a=t(p)/n
  return(a)
}

max.nindex <- function(m, n=10) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
max.nindex.corr <- function(m, n=6) {
  i <- order(m, decreasing=TRUE)
  return(i[2:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}

setwd('C:/Users/mikap/OneDrive/Documents/GitHub/RecSys/Project')
#setwd('C:/Users/claudedb/Documents/GitHub/RecSys/Project')
# Data Load
text.data<-data.table(read.table("data/out.matrix",sep=" ",skip=2))
colnames(text.data)<-c('courses.id','terms.id','n')
m.text <- sparseMatrix(text.data$terms.id,text.data$courses.id,x=text.data$n)

courses.data<-read.table('data/out.docs',sep="/")
colnames(courses.data)<-c('university','code')
courses.data.poly <-courses.data[courses.data$university=='Poly',]

terms.data<-read.table('data/out.terms')

colnames(m.text) <- as.vector(courses.data[,2])
rownames(m.text) <- as.vector(terms.data[,1])

# Nous garderons seulement les cours a la polyy
# Pour l'instant nous n'avons pas de facon de tous les analyses
id.hec <- which(courses.data$universtity== "HEC")
id.poly <- which(courses.data$university== "Poly")
id.udm <- which(courses.data$university== "UdM")
id.uqam <- which(courses.data$university == "UQAM")
#m.poly<-m.text[,c(id.poly,id.hec,id.udm)]
m.poly <- m.text[,id.poly]
m.poly <- m.poly[,colSums(m.poly) > 20]
m.poly<-m.poly[rowSums(m.poly) > 0,]

#À décommenter si on veut faire le focus sur le cours 
#IND4707
m.poly=cbind(m.poly[,636:737],m.poly[,1:635],m.poly[,738:1159])

# Similarite terme-terme --------------------------------------------------

#On calcul la similarite entre les cours avec les termes directement

#Compte des termes pour poly
term.count <- text.data[courses.id %in% id.poly,.(total=sum(n),count=.N),by = terms.id ]
term.count$term <- terms.data[term.count$terms.id,]

# On peut utiliser la distance euclidienne pour compute les 20 neirest neighbours

#sums_2<-colSums(m.poly^2)
#dist_eucl<-sweep(sweep(-2*t(m.poly) %*% m.poly,1,sums_2,"+"),2,sums_2,"+")
#neighbors<-t(apply(dist_eucl,1,order.partial))

# Similarités avec la correlation 
correlation <- cor(as.matrix(m.poly),method="spearman")
#correlation=cosinus.vm(as.matrix(m.poly),as.matrix(m.poly))
neighbors <-t(apply(correlation,1,max.nindex.corr))
correlation.termes=cor(t(as.matrix(m.poly)),method="spearman")
correlation.termes[is.na(correlation.termes)]=-1
neighbors.termes <-t(apply(correlation.termes,1,max.nindex.corr))

# Matrix Transformation ---------------------------------------------------
n.courses <- ncol(m.poly)
log.m <- log(m.poly)
log.m[is.infinite(log.m)] <- 0
tf.idf <- (1 + log.m) * log (n.courses/(rowSums(m.poly > 0)+1))
#tf.idf[m.poly==0]=0
#tf.idf <- m.poly * log(n.courses/(rowSums(m.poly > 0)+1))
pij <- m.poly / rowSums(m.poly)
log2.pij <- log2(pij)
log2.pij[is.infinite(log2.pij)] <- 0
global.entropy <- 1 + rowSums((pij * log2.pij * pij)/log2(n.courses))
log.entropy <- log2(1 + m.poly) * global.entropy


# TF-IDF ------------------------------------------------------------------

correlation.tfidf <-cor(as.matrix(tf.idf),method="spearman")
neighbors.tfidf <- t(apply(correlation.tfidf,1,max.nindex.corr))

# log entropy -------------------------------------------------------------

correlation.ent <- cor(as.matrix(log.entropy),method="spearman")
neighbors.ent<-t(apply(correlation.ent,1,max.nindex.corr))

# LSA ---------------------------------------------------------------------

lsa.startTime <- Sys.time()
lsaSpace.tfidf <- lsa(tf.idf,dims=50)
X.lsa.tfidf <- as.textmatrix(lsaSpace.tfidf)
lsaSpace.entropy <- lsa(log.entropy, dims=50)
X.lsa.ent <- as.textmatrix(lsaSpace.entropy)
X.lsa <- as.textmatrix(lsa(m.poly,dims=50))
lsa.endTime <- Sys.time()
lsa.elapsedTime <- lsa.endTime-lsa.startTime
lsa.elapsedTime

correlation.lsa <- cor(as.matrix(X.lsa),method="spearman")
neighbors.lsa <-t(apply(correlation.lsa,1,max.nindex.corr))
correlation.termes.lsa=cor(t(as.matrix(X.lsa)),method="spearman")
correlation.termes.lsa[is.na(correlation.termes.lsa)]=-1
neighbors.termes.lsa <-t(apply(correlation.termes.lsa,1,max.nindex.corr))

#X.lsa.tfidf <- lsaSpace.tfidf$tk %*% diag(lsaSpace.tfidf$sk) %*% t(lsaSpace.tfidf$dk)
correlation.lsa.tfidf <- cor(as.matrix(X.lsa.tfidf),method="spearman")
neighbors.lsa.tfidf<-t(apply(correlation.lsa.tfidf,1,max.nindex.corr))


# X.lsa.ent2 <- lsaSpace.entropy$tk %*% diag(lsaSpace.entropy$sk) %*% t(lsaSpace.entropy$dk)
correlation.lsa.ent <- cor(as.matrix(X.lsa.ent),method="spearman")
neighbors.lsa.ent <- t(apply(correlation.lsa.ent,1,max.nindex.corr))

# Evaluation des methodes -------------------------------------------------
 
#Choisissons 10 cours a evaluer
liste.cours <- c("MTH1006","MEC1210","MEC2115","AR320","INF2010","IND4704")
index.cours=c(907,739,747,36,632,548)
for (cours in liste.cours)
{
  #cours1 <- "ELE1403"
  id.cours1 <- which(colnames(m.poly)==cours)
  
  comparaison <- cbind(colnames(m.poly)[neighbors[id.cours1,]],
                       colnames(m.poly)[neighbors.tfidf[id.cours1,]],
                       colnames(m.poly)[neighbors.ent[id.cours1,]],
                       colnames(m.poly)[neighbors.lsa[id.cours1,]],
                       colnames(m.poly)[neighbors.lsa.tfidf[id.cours1,]],
                       colnames(m.poly)[neighbors.lsa.ent[id.cours1,]])
  
  colnames(comparaison) <-c('terme.terme','tf.idf','log.entropy','lsa','lsa.tfidf','lsa.entropy')
  
  write.csv(comparaison, paste("CSVs/", cours,'.csv',sep = ''))
  
}



#Vérification de la symétrie de la matrice de cosinus
#verif.sym=sum(cosinus.tfidf-t(cosinus.tfidf))
#Donne zéro: oui! 

#Vérification des 100 termes extrêmes pour TF-IDF
tf.idf.tot=rowSums(tf.idf)
mots.top100=tf.idf.tot[max.nindex(tf.idf.tot,100)]
mots.least100=tf.idf.tot[min.nindex(tf.idf.tot,100)]
name.top100=names(mots.top100)
name.least100=names(mots.least100)
tableau.top=data.frame(mots=name.top100)
tableau.least=data.frame(mots=name.least100)


#Infos nices
#On sort les tf-idf maximaux
tf.idf.reduit=tf.idf[,index.cours]
tf.idf.max=t(apply(t(tf.idf.reduit),1,max.nindex))
mat.tf.idf=matrix(NA,nrow=nrow(tf.idf.max),ncol=ncol(tf.idf.max))



for (i in 1:dim(tf.idf.max)[1]){
  mat.tf.idf[i,]=rownames(m.poly)[tf.idf.max[i,]]
}
rownames(mat.tf.idf)=rownames(tf.idf.max)


mat.termes=matrix(NA,nrow=nrow(neighbors.termes),ncol=ncol(neighbors.termes))
mat.termes.lsa=matrix(NA,nrow=nrow(neighbors.termes.lsa),ncol=ncol(neighbors.termes.lsa))
for (i in 1:dim(neighbors.termes)[1]){
  mat.termes[i,]=rownames(m.poly)[neighbors.termes[i,]]
  mat.termes.lsa[i,]=rownames(m.poly)[neighbors.termes.lsa[i,]]
}
rownames(mat.termes)=rownames(neighbors.termes)
rownames(mat.termes.lsa)=rownames(neighbors.termes.lsa)

#On prend 2 mots qui décrit chaque cours pris dans notre évaluation
mots=c(1393,2328,2033,237,
       2160,2684,1210,4570,4032,4218,610,
       3902,4510)
mat.termes.reduit=mat.termes[mots,]
mat.termes.lsa.reduit=mat.termes.lsa[mots,]
#Stats de base 


#On voit que le termes qui apparait le plus souvent est 
#de et il est présent 11308 fois dans les 1159 descriptions

which.max(rowSums(m.poly))
max(rowSums(m.poly))
#En moyenne, notre terme max se retrouve 9.8 fois par description
max(rowSums(m.poly))/dim(m.poly)[2]

#% de réduction de la matrice 
dim(m.poly)[1]/dim(m.text)[1]
dim(m.poly)[2]/dim(m.text)[2]


#Moyenne de mots dans les description
mean(colSums(m.poly))
sd(colSums(m.poly))




