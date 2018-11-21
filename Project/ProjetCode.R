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

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}

#setwd('D:/Polytechnique/A2018/SystRec/TP1')
setwd('C:/Users/claudedb/Documents/GitHub/RecSys/Project')
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

#sums_2<-colSums(m.poly^2)
#dist_eucl<-sweep(sweep(-2*t(m.poly) %*% m.poly,1,sums_2,"+"),2,sums_2,"+")
#neighbors<-t(apply(dist_eucl,1,order.partial))

# Similarités avec le cosinus 
cosinus <- cosinus.vm(m.poly,m.poly)
neighbors <-t(apply(cosinus,1,order.partial))


# Matrix Transformation ---------------------------------------------------
n.courses <- ncol(m.poly)
log.m <- log(m.poly)
log.m[is.infinite(log.m)] <- 0
tf.idf <- (1 + log.m) * log (n.courses/(rowSums(m.poly > 0)+1))
#tf.idf <- m.poly * log(n.courses/(rowSums(m.poly > 0)+1))
pij <- m.poly / rowSums(m.poly)
log2.pij <- log2(pij)
log2.pij[is.infinite(log2.pij)] <- 0
global.entropy <- 1 + rowSums((pij * log2.pij * pij)/log2(n.courses))
log.entropy <- log2(1 + m.poly) * global.entropy


# TF-IDF ------------------------------------------------------------------

cosinus.tfidf <-cosinus.vm(tf.idf,tf.idf)
neighbors.tfidf <- t(apply(cosinus.tfidf,1,order.partial))

# log entropy -------------------------------------------------------------

cosinus.ent <- cosinus.vm(log.entropy,log.entropy)
neighbors.ent<-t(apply(cosinus.ent,1,order.partial))

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

cosinus.lsa <- cosinus.vm(X.lsa,X.lsa)
neighbors.lsa <-t(apply(cosinus.lsa,1,order.partial))

# X.lsa.tfidf <- lsaSpace.tfidf$tk %*% diag(lsaSpace.tfidf$sk) %*% t(lsaSpace.tfidf$dk)
cosinus.lsa.tfidf <- cosinus.vm(X.lsa.tfidf,X.lsa.tfidf)
neighbors.lsa.tfidf<-t(apply(cosinus.lsa.tfidf,1,order.partial))

# X.lsa.ent2 <- lsaSpace.entropy$tk %*% diag(lsaSpace.entropy$sk) %*% t(lsaSpace.entropy$dk)
cosinus.lsa.ent <- cosinus.vm(X.lsa.ent,X.lsa.ent)
neighbors.lsa.ent <- t(apply(cosinus.lsa.ent,1,order.partial))

# Evaluation des methodes -------------------------------------------------
 
#Choisissons 10 cours a evaluer
id.cours1 <- which(courses.data.poly$code=="AER2100")

comparaison <- cbind(courses.data.poly[neighbors[id.cours1,],],
                     courses.data.poly[neighbors.tfidf[id.cours1,],],
                     courses.data.poly[neighbors.ent[id.cours1,],],
                     courses.data.poly[neighbors.lsa[id.cours1,],],
                     courses.data.poly[neighbors.lsa.tfidf[id.cours1,],],
                     courses.data.poly[neighbors.lsa.ent[id.cours1,],])[,c(2,4,6,8,10,12)]

colnames(comparaison) <-c('terme.terme','tf.idf','log.entropy','lsa','lsa.tfidf','lsa.entropy')


#Vérification de la symétrie de la matrice de cosinus
verif.sym=sum(cosinus.tt-t(cosinus.tt))
#Donne zéro: oui! 

#Vérification des 100 termes extrêmes pour TF-IDF
tf.idf.tot=rowSums(tf.idf)
mots.top100=tf.idf.tot[max.nindex(tf.idf.tot,100)]
mots.least100=tf.idf.tot[min.nindex(tf.idf.tot,100)]
name.top100=names(mots.top100)
name.least100=names(mots.least100)
tableau.top=data.frame(mots=name.top100)
tableau.least=data.frame(mots=name.least100)