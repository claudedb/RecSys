#TP 2 Code
#Par Claude Demers-Bélanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et vérifier que les packages nécessaires sont bien installés
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)

m = read.table("http://www.groupes.polymtl.ca/log6308/Public/citeseer.rtable")
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}
cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) 
  return(n)
  }

# Question 1 --------------------------------------------------------------
doc.id.q1<-'422908'
#Algorithme page rank
d<-0.85
CDi<-rowSums(m)+1
n<-dim(m)[1]
PageRank<-rep(1,n)
erreur<-1
itt<-1

while(erreur>=0.00001)
{
  r<-(1-d)+(d*(t(as.matrix(m))%*%(PageRank/CDi)))
  erreur<-sqrt(sum((r-PageRank)^2));
  PageRank<-r
  itt<-itt+1
}

itt
references.PageRank<-m[doc.id.q1,]*PageRank
recommendations.references<-references.PageRank[order(-references.PageRank)][1:10]
colnames(recommendations.references)

#Ajoutons l'espace S' (reference des references)

S.prime<-colSums(m[which(m[doc.id.q1,]==1),])+m[doc.id.q1,]

S.prime[S.prime>=1]<-1
RR.PageRank<-S.prime*PageRank

recommendations.RR<-RR.PageRank[order(-RR.PageRank)][1:10]
colnames(recommendations.RR)




# Question 2 --------------------------------------------------------------
#Vecteur qui correspond a l'article recherché
doc.id.q2<-paste("X",doc.id.q1,sep="")
vecteur.q2<-m[,doc.id.q2]

cosinus.q2<-cosinus.vm(vecteur.q2,as.matrix(m))

recommendations.ii.cos<-cosinus.q2[order(-cosinus.q2)][1:10]
rownames(data.frame(recommendations.ii.cos))




# Question 3 --------------------------------------------------------------


# Outputs -----------------------------------------------------------------


