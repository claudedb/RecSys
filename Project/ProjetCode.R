#Projet Rec. Sys Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2,lsa,tidytext)

order.partial=function(vec)
  {
  idx<-which(vec<=sort(vec,partial=11)[11])
  idx[order(vec[idx])][2:11]
}

cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));k= sqrt(rowSums(t(v^2))); p=(t(v) %*% m)/k; a=t(p)/n
  return(a)
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
m.poly<-m.poly[rowSums(m.poly) > 0,]

# Similarite terme-terme --------------------------------------------------

#On calcul la similarite entre les cours avec les termes directement

#Compte des termes pour poly
term.count <- text.data[courses.id %in% id.poly,.(total=sum(n),count=.N),by = terms.id ]
term.count$term <- terms.data[term.count$terms.id,]

# On peut utiliser la distance euclidienne pour compute les 20 neirest neighbours

sums_2<-colSums(m.poly^2)
dist_eucl<-sweep(sweep(-2*t(m.poly) %*% m.poly,1,sums_2,"+"),2,sums_2,"+")
neighbors<-t(apply(dist_eucl,1,order.partial))

#Similarités avec le cosinus 
cosinus.tt <- cosinus.vm(m.poly,m.poly)
neighbors.tt <-t(apply(cosinus.tt,1,order.partial))


# Matrix Transformation ---------------------------------------------------
n.courses <- ncol(m.poly)
log.m <- log(m.poly)
log.m[is.infinite(log.m)] <- 0
tf.idf <- (1+log.m) * log (n.courses/(rowSums(m.poly > 0)+1))

pij <- m.poly / rowSums(m.poly)
log2.pij <- log2(pij)
log2.pij[is.infinite(log2.pij)] <- 0
global.entropy <- 1 + rowSums((pij * log2.pij * pij)/log2(n.courses))
log.entropy <- log2(1 + m.poly) * global.entropy


# TF-IDF ------------------------------------------------------------------

sums2.TFIDF<-colSums(tf.idf^2)
dist.eucl.tfidf<-sweep(sweep(-2*t(tf.idf) %*% tf.idf,1,sums2.TFIDF,"+"),2,sums2.TFIDF,"+")
neighbors.tfidf<-t(apply(dist.eucl.tfidf,1,order.partial))

# log entropy -------------------------------------------------------------

sums2.ent<-colSums(log.entropy^2)
dist.eucl.ent<-sweep(sweep(-2*t(log.entropy) %*% log.entropy,1,sums2.ent,"+"),2,sums2.ent,"+")
neighbors.ent<-t(apply(dist.eucl.ent,1,order.partial))


# LSA ---------------------------------------------------------------------

lsa.startTime <- Sys.time()
lsaSpace.tfidf <- lsa(tf.idf,dims=50)
#lsaSpace.tfidf <- as.textmatrix(lsaSpace.tfidf)
#lsaSpace.entropy <- lsa(log.entropy, dims=50)
#lsaSpace.entropy <- as.textmatrix(lsaSpace.entropy)
lsa.endTime <- Sys.time()
lsa.elapsedTime <- lsa.endTime-lsa.startTime
lsa.elapsedTime

X.lsa <- lsaSpace.tfidf$tk %*% diag(lsaSpace.tfidf$sk) %*% t(lsaSpace.tfidf$dk)
sums2.lsa<-colSums(X.lsa^2)
dist.eucl.lsa<-sweep(sweep(-2*t(X.lsa) %*% X.lsa,1,sums2.lsa,"+"),2,sums2.lsa,"+")
neighbors.lsa<-t(apply(dist.eucl.lsa,1,order.partial))

# Evaluation des methodes -------------------------------------------------

#Vérification de la symétrie de la matrice de cosinus
verif.sym=sum(cosinus.tt-t(cosinus.tt))
#Donne zéro: oui! 

#Vérification des 10 termes ayant le plus petit TF-IDF
