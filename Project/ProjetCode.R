#Projet Rec. Sys Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2,lsa,tidytext)

#setwd('D:/Polytechnique/A2018/SystRec/TP1')
setwd('C:/Users/claudedb/Documents/GitHub/RecSys/Project')
# Data Load
text.data<-data.table(read.table("data/out.matrix",sep=" ",skip=2))
colnames(text.data)<-c('courses.id','terms.id','n')
m.text <- sparseMatrix(text.data$terms,text.data$courses,x=text.data$n)

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
m.poly<-m.poly[rowSums(m.poly)>0,]

# Similarite terme-terme --------------------------------------------------

#On calcul la similarite entre les cours avec les termes directement
#Compte des termes pour poly
term.count <- text.data[ courses.id %in% id.poly,.(total=sum(n)),by = terms.id ]
term.count$terms <- terms.data[term.count$terms.id, ]

# Matrix Transformation ---------------------------------------------------
n.courses <- ncol(m.poly)
tf.idf <- (1 + log(m.poly)) * log (n.courses/(rowSums(m.poly > 0)+1))

pij <- m.poly / rowSums(m.poly)
global.entropy <- 1 + rowSums((pij * log2(pij) * pij)/log2(n.courses))
log.entropy <- log2(1 + m.poly) * global.entropy


# LSA ---------------------------------------------------------------------

lsa.startTime <- Sys.time()
lsaSpace.tfidf <- lsa(tf.idf,dims=dimcalc_share())
lsaSpace.tfidf <- as.textmatrix(lsaSpace.tfidf)
lsaSpace.entropy <- lsa(log.entropy, dims=dimcalc_share())
lsaSpace.entropy <- as.textmatrix(lsaSpace.entropy)
lsa.endTime <- Sys.time()
lsa.elapsedTime <- lsa.endTime-lsa.startTime
lsa.elapsedTimes


# Evaluation des methodes -------------------------------------------------



