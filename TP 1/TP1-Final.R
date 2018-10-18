#TP 1 Code
#Par Claude Demers-Bélanger (1534217) et Mikael Perreault (1741869)
#Modifié le 3 octobre 2018

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et vérifier que les packages nécessaires sont bien installés
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)

#setwd('D:/Polytechnique/A2018/SystRec/TP1')
setwd('C:/Users/claudedb/Documents/Polymtl/LOG6308/TP1')
# Data Load
u.data<-read.csv('Data/u.data.csv',sep='|')
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')


u.item <- read.csv(file='Data/u.item.csv', sep='|', header=T)
u.user <- read.csv(file='Data/u.user.csv', sep='|', header=T)
u.data.user<-merge(u.data,u.user, by.x='user.id',by.y='id')

#Fonction Utile fourni par le site du cours
## Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premiÃ¨res 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}
order.partial=function(vec){
  idx<-which(vec<=sort(vec,partial=21)[21])
  idx[order(vec[idx])][2:21]
}


# Question 1 --------------------------------------------------------------
#Question 1) Quelle est la moyenne des votes par profession ("job") et par âge?

# On crée un data frame en groupant les ratings selon la moyenne par job
q1.job<-u.data.user %>% group_by(job) %>% summarise(avg=mean(rating, na.rm=TRUE))

# Nous pouvons faire la même chose pour les âges
q1.age<-u.data.user %>% group_by(age) %>% summarise(avg=mean(rating, na.rm=TRUE))

# Cependant, puisque qu'il manque certaines valeurs d'âge, il est préférable d'utiliser des groupes
# La ligne suivante groupe les âges par tranche de 10 ans. De 0 à 9 étant groupe 0 et ainsi de suite.
# Des groupes d'âge différent comme 18-25 aurait aussi pu être sélectionnés.
# On ajoute aussi la variable nb.ratings pour calculer le nombre de films dont nous avons l'évaluation par tranche d'âge
#Les groupes seront 18 et moins, 18-25, 25-35, 35-50 et 50+

q1.age2<-u.data.user%>% mutate(age.group=findInterval(age,c(19,26,36,51)))%>% group_by(age.group)%>% summarise(avg.rating=mean(rating,na.rm=TRUE),nb.rating=n())

#On crée le graphique suivant qui montre avec le diagramme à barres la moyenne par groupe d'âge
#et la ligne représente le nombre de vote associé sur l'axe secondaire

g<-ggplot(q1.age2)+geom_bar(aes(age.group,avg.rating),stat='identity',fill='blue')+geom_text(aes(age.group,avg.rating,label=round(avg.rating,3)), position=position_dodge(width=0.9), vjust=-0.25)+geom_line(aes(age.group,nb.rating/10000),color='red',size=2)+scale_y_continuous(sec.axis=sec_axis(trans=~.*10000,name="Nombre de notes"))+
  labs(title="Moyenne des notes par groupe d'age",y="Moyenne des notes",x="Goupe d'age")



# Question 2 --------------------------------------------------------------
#Quels sont les 10 films les plus similaires à "Star Trek V: The Final Frontier (1989)" selon respectivement 
#la mesure du cosinus et de la corrélation avec la matrice de votes. 

# Trouvons d'abord le ID du film désiré
movie.q2<-'Star Trek V: The Final Frontier (1989)'
q2.id<-filter(u.item,movie.title==movie.q2)$movie.id

#On extrait le vecteur du film désiré de la matrice M
vecteur.q2<-m[,q2.id]

#Méthode du Cosinus

#On identifie d'abord avec une matrice logique les positions des votes communs avec le film actuel (Star Trek)
positions.votes.communs=(m[,q2.id]*m)>0

#votes.communs est un vecteur de longueur 1682 où chaque instance représente le nombre de votes communs avec le film actuel 
votes.communs<-(colSums((m[,q2.id]*m)>0,na.rm=T))

k=vecteur.q2%*%m

#m.clean est la matrice m où les valeurs manquantes sont représentées par des NA contrairement aux zéros de la matrice m
#On ne veut pas garder les 0, car ce ne sont pas tous les calculs qui sont cohérents (exemple : le dot product est cohérent
#mais la sommation des vecteurs ne l'est pas)
m.clean=m
m.clean[!positions.votes.communs]=NA

#vecteur.q2.clean est une matrice où chaque colonne i est le vecteur des votes de Star Trek communs aux films i.
#On peut donc calculer le cosinus en prenant seulement les votes communs et en omettant les valeurs manquantes
vecteur.q2.clean=vecteur.q2
vecteur.q2.clean=matrix(vecteur.q2.clean,nrow=dim(m)[1],ncol=dim(m)[2])
vecteur.q2.clean=as(vecteur.q2.clean, "dgCMatrix")
vecteur.q2.clean[!positions.votes.communs]=NA
n=sqrt(colSums(m.clean^2,na.rm=T))
d=sqrt(colSums(vecteur.q2.clean^2,na.rm=T))

cosinus.q2=k/(n*d)
cosinus.q2=as.vector(cosinus.q2)
cosinus.q2[is.nan(cosinus.q2)]=0
#Paramètre à modifier parce que si il n'est pas là, tous les films avec les plus hauts cosinus sont ceux 
#qui n'ont pas beaucoup de votes communs et que les votes sont très similaires 
cosinus.q2.VC=cosinus.q2
cosinus.q2.VC[votes.communs<=15]=NA  



#On utilise les 11 items les plus haut pour inclure le movie.q2 lui même, on le retire ensuite pour la réponse finale
top10.index.cos<-max.nindex(cosinus.q2.VC,11)
top10.title.cos<-u.item[top10.index.cos[top10.index.cos!=q2.id],]$movie.title

#Méthode corrélation

#La méthodologie est la même qu'avec le cosinus. Comme la fonction cor() ne permet pas d'omettre les NA, nous avons
#décidé de la coder nous-même. En gardant les valeurs manquantes comme des valeurs nulles 
#(comme il est suggéré sur le sites du cours) et en utilisant cor(), on obtient des corrélations similaires mais fausses

cor.mat=t(t(m.clean)-colMeans(m.clean,na.rm=T))
cor.vecteur.q2.mat=matrix(m.clean[,q2.id],nrow=dim(m)[1],ncol=dim(m)[2])
cor.vecteur.q2.sparse=as(cor.vecteur.q2.mat, "dgCMatrix")
cor.vecteur.q2.sparse[!positions.votes.communs]=NA
cor.vecteur.q2=t(t(cor.vecteur.q2.sparse)-colMeans(cor.vecteur.q2.sparse,na.rm=T))
cor.num=colSums(cor.vecteur.q2*cor.mat,na.rm=T)
cor.deno1=colSums(cor.vecteur.q2^2,na.rm=T)
cor.deno2=colSums(cor.mat^2,na.rm=T)
cor.q2=cor.num/sqrt(cor.deno1*cor.deno2)
cor.q2=as.vector(cor.q2)
cor.q2[is.nan(cor.q2)]=-1
cor.q2[votes.communs<=15]=NA  

top10.index.cor<-max.nindex(cor.q2,11)
top10.title.cor<-u.item[top10.index.cor[top10.index.cor!=q2.id],]$movie.title


#Test pour la corrélation entre l'index 449 et 450 et validation
test1=m[which(positions.votes.communs[,449]),450]
test2=m[which(positions.votes.communs[,449]),449]
cor.test=cor(test1,test2)   #Fonction R 
cor.validation=cor.q2[449]

#On remarque que si on avait utilisé la fonction corrélation simplement avec les vecteurs initiaux où les NA sont 
#représentées par des valeurs nulles, les corrélations obtenues ne sont pas les bonnes. Avec la validation, on voit
#que notre méthode donne de bons résultats. 


# Question 3 --------------------------------------------------------------


#On trouve le vecteur qui correspond au film actuel 
vecteur.q3<-vecteur.q2
vecteur.q3[!positions.votes.communs[,q2.id]]=NA

#On calcule la distance en prenant bien soin de normaliser par le nombre de votes communs pour pouvoir les comparer 
distance.ii<-sqrt(colSums((m.clean-vecteur.q3)^2,na.rm=T))/votes.communs

#On vérifie si cette valeur est égale à distance.ii[449] pour valider
test.distance=sqrt(sum((test1-test2)^2)) 



#On décide d'éliminer les distances avec les films qui possèdent moins de 5 votes communs avec le film actuel
#On apporte cette correction dès le calcul de la distance parce qu'il n'est pas rare de voir de très petites distances
#avec les films possédant peu de votes communs. Cependant, cette petite distance n'est pas significative. 
distance.ii[votes.communs<5]=NA 

#On sélectionne les 20 items les plus proches et 
distance.top20.ii<-min.nindex(distance.ii,21)
distance.top20.ii<-distance.top20.ii[distance.top20.ii!=q2.id]


#On calcule le cosinus
cosinus.q3=cosinus.q2[distance.top20.ii]
#On apporte une pondération fonction du nombre de votes communs avec Star trek 
cosinus.q3.VC=(pmax(votes.communs[distance.top20.ii],5)/5)*cosinus.q3
facteur.k<-1/sum(abs(cosinus.q3.VC),na.rm=T)


#La moyenne des votes pour le film actuel 
avg.startrek<-sum(m[,q2.id])/sum(m[,q2.id]>0)


#m.estim représente la matrice m qu'on a filtré avec les 20 plus proches voisins, elle est donc de dimension 943x20. 
m.estim=m
m.estim[m.estim==0]=NA
m.estim=m.estim[,distance.top20.ii]

#Calcul de l'Estimation du vote basé sur les plus proches voisins
ecart=t(t(m.estim)-colMeans(m.estim,na.rm=T))
ecart.mat=as.matrix(ecart)
terme=t(t(ecart.mat)*cosinus.q3.VC)

#On prédit même pour ceux ayant déjà un vote à Star Trek, la différence avec les vraies valeurs sera trouvée à la 
#question 4
estimation=avg.startrek+facteur.k*rowSums(terme,na.rm=T)

#On donne une valeur manquante aux users qui n'ont pas de votes communs avec les 20 plus proches voisins du film actuel
estimation[which(rowSums(m.estim,na.rm=T)==0)]=NA


#On effectue un test "manuel" pour le user 1 pour s'assurer que les ratings sont bien calculés 
test.u1=m.estim[1,]
test.ecart=test.u1-colMeans(m.estim,na.rm=T)
test.terme=test.ecart*cosinus.q3.VC
test.estimation=avg.startrek+facteur.k*sum(test.terme,na.rm=T)
#On s'assure que la valeur de test.estimation est égale à estimation[1]


# Question 4 --------------------------------------------------------------
#Calcul du RMSE 
donnee=m[which(m[,q2.id]>0),q2.id]
predit=estimation[which(m[,q2.id]>0)]
erreur.quadratique=sqrt(sum((donnee-predit)^2)/length(donnee))


# Question 5 --------------------------------------------------------------

#On trouve d'abord les indices des films Star Trek et Star Wars 

u944<-as.vector(matrix(0,nrow=ncol(m)))
indices.star.trek<-grep('trek',as.character(u.item$movie.title),ignore.case=T)
indices.star.wars<-c(50,172,181)
u944[indices.star.trek]<-5
u944[indices.star.wars]<-1

#Pour réutiliser le code de la question 3, on rajoute le nouvel user à la matrice m initial, on transpose cette dernière
#Et on applique une méthode identique. Nous sommes conscient que ce n'est pas la façon optimale de faire, mais c'est celle
#qui était la plus simple dans le cadre de cet énoncé. Nous ne recommenterons pas cette section, car elle est analogue 
#à la question 3. 
m.ajout=rbind(m,u944)
m.uu<-t(m.ajout)
q5.id=944
vecteur.q5<-m.uu[,q5.id]

#Méthode du Cosinus
positions.votes.communs.uu=(m.uu[,q5.id]*m.uu)>0
votes.communs.uu<-(colSums((m.uu[,q5.id]*m.uu)>0,na.rm=T))
k.uu=vecteur.q5%*%m.uu
m.clean.uu=m.uu
m.clean.uu[!positions.votes.communs.uu]=NA
vecteur.q5.clean=vecteur.q5
vecteur.q5.clean=matrix(vecteur.q5.clean,nrow=dim(m.uu)[1],ncol=dim(m.uu)[2])
vecteur.q5.clean=as(vecteur.q5.clean, "dgCMatrix")
vecteur.q5.clean[!positions.votes.communs.uu]=NA
n.uu=sqrt(colSums(m.clean.uu^2,na.rm=T))
d.uu=sqrt(colSums(vecteur.q5.clean^2,na.rm=T))

cosinus.q5=k.uu/(n.uu*d.uu)
cosinus.q5=as.vector(cosinus.q5)
cosinus.q5[is.nan(cosinus.q5)]=0

cosinus.q5.VC=cosinus.q5
cosinus.q5.VC[votes.communs.uu<=30]=NA


#Estimation
vecteur.q5<-m.uu[,q5.id]
vecteur.q5[!positions.votes.communs.uu[,q5.id]]=NA
distance.ii.uu<-sqrt(colSums((m.clean.uu-vecteur.q5)^2,na.rm=T))/votes.communs.uu

distance.ii.uu[votes.communs.uu<3]=NA


distance.top20.ii.uu<-min.nindex(distance.ii.uu,21)
distance.top20.ii.uu<-distance.top20.ii.uu[distance.top20.ii.uu!=q5.id]


cosinus.q5.estim=cosinus.q5[distance.top20.ii.uu]
cosinus.q5.VC=(pmax(votes.communs.uu[distance.top20.ii.uu],5)/5)*cosinus.q5.estim
facteur.k.uu<-1/sum(abs(cosinus.q5.VC),na.rm=T)

avg.u944<-sum(m[,q5.id])/sum(m[,q5.id]>0)


m.estim.uu=m.uu
m.estim.uu[m.estim.uu==0]=NA
m.estim.uu=m.estim.uu[,distance.top20.ii.uu]
ecart.uu=t(t(m.estim.uu)-colMeans(m.estim.uu,na.rm=T))
ecart.mat.uu=as.matrix(ecart.uu)
terme.uu=t(t(ecart.mat.uu)*cosinus.q5.VC)

estimation.uu=avg.u944+facteur.k.uu*rowSums(terme.uu,na.rm=T)
estimation.uu[which(rowSums(m.estim.uu,na.rm=T)==0)]=NA
#On ne veut pas recommander les films de Star Trek ni de Star Wars
estimation.uu[c(indices.star.trek,indices.star.wars)]=NA

recommendations.uu<-u.item[max.nindex(estimation.uu,10),]$movie.title




test.i1.uu=m.estim.uu[1,]
test.ecart.uu=test.i1.uu-colMeans(m.estim.uu,na.rm=T)
test.terme.uu=test.ecart.uu*cosinus.q5.VC
test.estimation.uu=avg.u944+facteur.k.uu*sum(test.terme.uu,na.rm=T)
#Doit être égal à estimation.uu[1]

#Question 6 --------------------------------------------------------------

#Je suis un nouvel utilisateur. Vous connaissez ma profession, mon sexe et mon Age. 
#Developpez un algorithme bayesien pour recommander 10 films sur la base de ces trois categories.

#Pour reduire la cardinalité de la variable Age, nous allons creer des groupes d'Age
#Les groupes seront 18 et moins, 18-25, 25-35, 35-50 et 50+
u.data.user.q6<-u.data.user %>% mutate(age.group=findInterval(age,c(19,26,36,51)))
u.data.user.q6<-data.table(u.data.user.q6) #On utilise les data tables pour faciliter la manipulation des donnees

u.data.user.q6[,.(n=uniqueN(user.id)),by=age.group] # Montre le nombre d'usager par groupe d'Age

#Pour chacune des combinaisons Profession, sexe et groupe d'Age, nous devons trouver la probabilite qu'ils apprecient un film
#On juge qu'un usager a apprecie un film quand sa cote est > 3
#Ajoutons une variable 'aime' et aime pas. On pourra facilement faire la somme des variables pour les etapes subsequentes

u.data.user.q6$aime<-u.data.user.q6$rating>3
u.data.user.q6$aimepas<-u.data.user.q6$rating<=3

#On trouve le ratio PH en divisant la probabilite qu'un usager aime le film par celle de que l'usager n'aime pas le film

PH<-u.data.user.q6[,.(n=(sum(aime,na.rm=T)+1)/(sum(aimepas,na.rm=T)+2)),by=item.id]

#On ordonne par le item.id pour s'assurer que les donnÃ©es soient ordonnees de la meme facon pour tous les data table
PH<-PH[order(item.id)]

# Pour chacun des items, on veut le nombre de personne qui ont aime vs pas aime
Count.Aime<-u.data.user.q6[,.(aime.total=sum(aime,na.rm=T),aime.pas.total=sum(aimepas,na.rm=T)),by=item.id]

# On veut pour une job donnee et un film donne avoir le nombre de personnes qui ont aime vs pas aime
PE1<-u.data.user.q6[,.(aime=sum(aime,na.rm=T),aime.pas=sum(aimepas,na.rm=T)),by=.(item.id,job)]
# On ajoute le nombre de personne qui ont aime et pas aime pour chacun des films sans tenir compte du job
PE1<-merge(PE1,Count.Aime, by='item.id')
# On effectue le ratio des probabilites avec la correction de laplace (+1 numerateur +2 denominateur)
PE1$Ratio<-((PE1$aime+1)/(PE1$aime.total+2))/((PE1$aime.pas+1)/(PE1$aime.pas.total+2))

#On utilise dcast pour cree un data table qui x=item.id y=job
PE1<-dcast(PE1,item.id~job,sum,value.var='Ratio')

# En utilisant dcast, on cree des zeros lorsqu'il y a aucune donnee pour une combinaison d'item.id et job/gender/age group
# On remplace donc par 1 car selon la correction de laplace (1/2)/(1/2)= 1
PE1[PE1==0]<-1

# On effectue les memes operations qu'en PE1 mais avec le genre
PE2<-u.data.user.q6[,.(aime=sum(aime,na.rm=T),aime.pas=sum(aimepas,na.rm=T)),by=.(item.id,gender)]
PE2<-merge(PE2,Count.Aime, by='item.id')
PE2$Ratio<-((PE2$aime+1)/(PE2$aime.total+2))/((PE2$aime.pas+1)/(PE2$aime.pas.total+2))
PE2<-dcast(PE2,item.id~gender,sum,value.var='Ratio')
PE2[PE2==0]<-1

# On effectue les memes operations qu'en PE1 mais avec le groupe d'Age
PE3<-u.data.user.q6[,.(aime=sum(aime,na.rm=T),aime.pas=sum(aimepas,na.rm=T)),by=.(item.id,age.group)]
PE3<-merge(PE3,Count.Aime, by='item.id')
PE3$Ratio<-((PE3$aime+1)/(PE3$aime.total+2))/((PE3$aime.pas+1)/(PE3$aime.pas.total+2))
PE3<-dcast(PE3,item.id~age.group,sum,value.var='Ratio')
PE3[PE3==0]<-1


# On utilise les variables suivantes pour trouver des exemples de recommendations
job<-'engineer'
age<-23
gender<-'F'
age.group<-toString(findInterval(age,c(19,26,36,51)))

#On trouve la probabilite que la personne represente par les variables ci-haut aime chacun des films
PTotal<-PH$n*PE1[[job]]*PE3[[age.group]]*PE2[[gender]]
ProbTotal<-PTotal/(PTotal+1)

#On choisit les 10 plus grandes probabilites ce qui constitueront nos recommendations.
Recommendations.bayes<-u.item[max.nindex(ProbTotal,10),]$movie.title




# Question 7 --------------------------------------------------------------
#Cross Validation

#La cross-validation sera effectuée pour les questions 3 et 4. Nous avons jugé qu'il était impertinent de le faire 
#pour la question 5, puisque l'on possède que 11 valeurs de votes 

#On crée une fonction cross.validation qui prend en input la matrice m non modifiée, la proportion des valeurs tests 
#(par défaut = 30%) et le nombre de "fold" qui devra être fait (par défaut = 10)

cross.validation <- function(m,prop.test=0.3,nb.cv=10){
  
  
  movie.cv<-'Star Trek V: The Final Frontier (1989)'
  cv.id<-filter(u.item,movie.title==movie.cv)$movie.id
  estimation.cv.mat=matrix(0,nrow=nb.cv, ncol=dim(m)[1])
  erreur.quadratique.cv.vect=vector(mode="numeric",length=nb.cv)
  for (i in 1:nb.cv){
    m.cv=m
    
    #Dans la cross-validation, le seul élément qui change c'est qu'on affecte aléatoirement 30% des valeurs connues 
    #de votes du film actuel à la valeur nulle (=> une valeur manquante dans la représentation initiale)
    #On entraîne ensuite le modèle avec les valeurs restantes (training set) et on viendra calculer le RMSE 
    #par rapport aux valeurs enlevées au préalable (testing set)
    
    vect.alea=sample(which(m.cv[,cv.id]>0),floor(prop.test*sum(m.cv[,cv.id]>0)))
    
    m.cv[vect.alea,cv.id]=0
    
    
    
    #À partir d'ici c'est la méthode analogue à la question 3
    #On extrait le vecteur du film désiré de la matrice M
    vecteur.cv<-m.cv[,cv.id]
    
    #Méthode du Cosinus
    positions.votes.communs.cv=(m.cv[,cv.id]*m.cv)>0
    votes.communs.cv<-(colSums((m.cv[,cv.id]*m.cv)>0,na.rm=T))
    k.cv=vecteur.cv%*%m.cv
    m.clean.cv=m.cv
    m.clean.cv[!positions.votes.communs.cv]=NA
    vecteur.cv.clean=vecteur.cv
    vecteur.cv.clean=matrix(vecteur.cv.clean,nrow=dim(m.cv)[1],ncol=dim(m.cv)[2])
    vecteur.cv.clean=as(vecteur.cv.clean, "dgCMatrix")
    vecteur.cv.clean[!positions.votes.communs.cv]=NA
    n.cv=sqrt(colSums(m.clean.cv^2,na.rm=T))
    d.cv=sqrt(colSums(vecteur.cv.clean^2,na.rm=T))
    
    cosinus.cv=k.cv/(n.cv*d.cv)
    cosinus.cv=as.vector(cosinus.cv)
    cosinus.cv[is.nan(cosinus.cv)]=0
    
    
    
    
    vecteur.q7<-vecteur.cv
    vecteur.q7[!positions.votes.communs.cv[,cv.id]]=NA
    distance.ii.cv<-sqrt(colSums((m.clean.cv-vecteur.q7)^2,na.rm=T))/votes.communs.cv
    test1.cv=m[which(positions.votes.communs.cv[,449]),450]
    test2.cv=m[which(positions.votes.communs.cv[,449]),449]
    test.distance.cv=sqrt(sum((test1.cv-test2.cv)^2))  #On vérifie si cette valeur est égale à distance.ii[449] pour valider
    
    
    distance.ii.cv[votes.communs.cv<5]=NA 
    
    
    distance.top20.ii.cv<-min.nindex(distance.ii.cv,21)
    distance.top20.ii.cv<-distance.top20.ii.cv[distance.top20.ii.cv!=cv.id]
    
  
    cosinus.q7=cosinus.cv[distance.top20.ii.cv]
    cosinus.q7.VC=(pmax(votes.communs.cv[distance.top20.ii.cv],5)/5)*cosinus.q7
    facteur.k.cv<-1/sum(abs(cosinus.q7.VC),na.rm=T)
  
    avg.startrek.cv<-sum(m.cv[,cv.id])/sum(m.cv[,cv.id]>0)
    
    
    m.estim.cv=m.cv
    m.estim.cv[m.estim.cv==0]=NA
    m.estim.cv=m.estim.cv[,distance.top20.ii.cv]
    ecart.cv=t(t(m.estim.cv)-colMeans(m.estim.cv,na.rm=T))
    ecart.mat.cv=as.matrix(ecart.cv)
    terme.cv=t(t(ecart.mat.cv)*cosinus.q7.VC)
    
    estimation.cv=avg.startrek.cv+facteur.k.cv*rowSums(terme.cv,na.rm=T)
    estimation.cv[which(rowSums(m.estim.cv,na.rm=T)==0)]=NA
    
    #On stocke le vecteur d'estimation pour chaque fold dans une matrice, que l'on déroule ensuite dans un vecteur 
    #afin de faciliter son retour de la fonction
    estimation.cv.mat[i,]=estimation.cv
    estimation.cv.vect=as.vector(estimation.cv.mat)
  
    
    #On calcul le RMSE sur le training set. Il est bien important que les valeurs du training set ne soient pas comprises
    #dans les valeurs d'entraînement, ce qui a été fait au début de la boucle
    donnee.cv=m[vect.alea,cv.id]
    predit.cv=estimation.cv[vect.alea]
    erreur.quadratique.cv=sqrt(sum((donnee.cv-predit.cv)^2)/length(donnee.cv))
    
    #On stocke aussi le RMSE dans un vecteur
    erreur.quadratique.cv.vect[i]=erreur.quadratique.cv
  }
  return(list(estimation.cv.vect,erreur.quadratique.cv.vect))
  
}
nb.cv=10
prop.test=0.3
resultat=cross.validation(m,prop.test,nb.cv)

#resultats des estimations pour chaque fold (les colonnes = user, les lignes = fold)
resultat.estimation=matrix(resultat[[1]],nrow=nb.cv,ncol=dim(m)[1])

#resultats des RMSE pour chaque fold 
resultat.erreur=resultat[[2]]

resultat.erreur.avg=mean(resultat.erreur)

#DISCUSSION SUR LES RÉSULTATS 

#On constate que la moyenne des RMSE calculés en cross-validation (stagne à 1.02 pour 50-100 fold) 
#est supérieure au RMSE calculé initialement en Q4 (0.95) avec toutes les valeurs. 
#Ceci est un résultat attendu, puisqu'on émet l'hypothèse que les données dégagent une tendance et que cette tendance
#est renforcée plus on possède de données. Il est donc normal que le RMSE moyen de la cross-validation soit plus haut 
#que le RMSE calculé avec toutes les données à notre disposition. 

#De plus, on remarque dans la matrice resultat.estimation que certains user ont parfois une valeur calculée pour leur vote et 
#et ont parfois une valeur manquante selon le fold. Ceci s'explique par le fait que, dépendamment des valeurs de votes retirées 
#dans le testing set, les 20 plus proches voisins seront probablement différents. Alors, il est plausible que certains 
#users n'ont pas de votes communs avec les 20 plus proches voisins issus d'un training set donné, mais qu'ils en ont 
#pour un training set différent.



# Outputs -----------------------------------------------------------------

print("Question 1)")
print("Voici la moyenne des notes selon la profession (job)")
print(q1.job,n=40)
print("Voici la moyenne des notes selon les groupes d'age")
print("(0 = 18 et moins, 1= 18-25, 2= 25-35, 3= 35-50 et 4= 50+")
print(q1.age2)
print(g)

print("Question 2)")
print('Voici les 10 titres les plus similaires selon la corrélation et le chiffre de leur corrélation')
print('Votes Communs >=15')
resultat.cor.q2<-cbind(data.frame(top10.title.cor),cor.q2[top10.index.cor[2:11]])
colnames(resultat.cor.q2)<-c('Title','Correlation')
print(resultat.cor.q2)
print('Voici les 10 titres les plus similaires selon le cosinus et le chiffre de leur cosinus')
print('Votes Communs >=15')
resultat.cos.q2<-cbind(data.frame(top10.title.cos),cosinus.q2[top10.index.cos[2:11]])
colnames(resultat.cos.q2)<-c('Title','Cosinus')
print(resultat.cos.q2)

print("Question 3)")
print("Pour la méthode item-item, voici le titre des films qui ont été utilisé ainsi que leur Cosinus et leur distance")
print('Votes Communs >=5')
resultat.q3<-cbind(data.frame(u.item[distance.top20.ii,2]),distance.ii[distance.top20.ii],cosinus.q3) #u.item[distance.top20.ii,]$movie.title,
colnames(resultat.q3)<-c('Title','Distance','Cosinus') #'Title',
print(resultat.q3)
print("voici un exemple du vecteur d'estimation des cotes pour star trek")
print(estimation[1:20])

print("Question 4)")
print(paste0('RMSE = ',round(erreur.quadratique,4)))

print("Question 5)")
print("Pour la méthode user.user, voici le ID des users qui ont été utilisé ainsi que leur Cosinus et leur distance")
print('Votes Communs >=5')
resultat.q5<-cbind(data.frame(u.user[distance.top20.ii.uu,]$id),distance.ii.uu[distance.top20.ii.uu],cosinus.q5[distance.top20.ii.uu])
colnames(resultat.q5)<-c('User ID','Distance','Cosinus')
print(resultat.q5)
print('Voici les recommendations de la méthode user-user')
print(recommendations.uu)


print("Question 6)")
print("Voici les recommendations pour la méthode de Bayes pour une ingénieure de 23 ans")
print(Recommendations.bayes)

print("Question 7)")
print("Voici les resultats d'estimation pour les 10 essais de Cross-Validation des 5 premiers utilisateurs")
print(resultat.estimation[1:10,1:5])
print('Voici les RMSE pour les 10 essais de Cross-Validation')
print(resultat.erreur)
print("Voir le code pour la discussion sur les résultats")
