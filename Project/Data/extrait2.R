### Les commentaires sont précédés de "###"

### Il faut charger en premier le paquetage Matrix

> library(Matrix)
Le chargement a nécessité le package : lattice
m <- readMM('out.matrix')
> t <- scan('out.terms','character')
> Read 21080 items
> d <- scan('out.docs','character')
Read 16335 items
> colnames(m) <- t
> rownames(m) <- d
### INF6304 a 50 termes :
> sum(m['Poly/INF6304',])
[1] 50
> which(m['Poly/INF6304',]>0)
           null         dialogu          enjeux        recherch         sensibl 
              1              97             315             544            1143 
         prefer            plan           model           filtr          machin 
           1513            3310            3314            3543            3775 
      titrecour     intelligent          system        connaiss       semantiqu 
           4087            4278            4519            4724            5085 
       interact     utilisateur       adaptatif              de             aid 
           5087            5335            5533            6000            6199 
       document           valid       structure           limit           agent 
           7530            7865            9166           10398           12106 
descriptioncour          assist      cooperatif            semi         conseil 
          12360           12826           13568           14245           14383 
   collaboratif        intellig             but          tuteur  caracteristiqu 
          14797           14942           15718           16669           17281 
         inform        interfac         context         interet          humain 
          17467           18349           19270           20064           20980 
### Produit scalaire du vecteur INF6304 avec les autres cours qui
### indique le nombre de termes communs (en fait, la somme du produit
### de leur fréquence)
> r <- m %*% m['Poly/INF6304',] 
Note: Method with signature "Matrix#numeric" chosen for function "%*%",
 target signature "dgTMatrix#numeric".
 "TsparseMatrix#ANY" would also be valid
Note: Method with signature "Matrix#matrix" chosen for function "%*%",
 target signature "dgTMatrix#matrix".
 "TsparseMatrix#ANY" would also be valid
> length(r)
[1] 16335
> head(r)
6 x 1 sparse Matrix of class "dgCMatrix"
                
HEC/1-404-96  78
HEC/1-407-00  55
HEC/1-409-05 113
HEC/1-611-09   3
HEC/1-611-96  59
HEC/1-612-96  43
### Ordonne le résultat pour ne garder que les 100 valeurs les plus
### grandes du produit scalaire
> head(r[order(r[,1],decreasing=T),],100)
 HEC/6-005-06  HEC/6-011-06 HEC/80-463-05 HEC/80-430-90  HEC/6-408-84 
          508           434           420           392           388 
HEC/80-470-07  HEC/3-006-84  UQAM/JUR7620  HEC/4-900-04  HEC/3-730-04 
          384           329           314           312           311 
 UQAM/MBA8V1B HEC/30-515-09  HEC/6-015-08  HEC/6-040-02 HEC/53-038-07 
          300           297           294           293           291 
 UQAM/MBA8V1A  HEC/4-499-06  UQAM/PTG2235  UQAM/PTG3125  HEC/6-013-06 
          288           287           279           279           277 
HEC/36-131-08 HEC/53-064-07  HEC/4-440-02  UQAM/ORH2202  UQAM/ARA3410 
          272           270           269           266           265 
 UQAM/KIN7220  UQAM/PTG3235  HEC/2-420-06  UQAM/ITA3270  UQAM/AVM4900 
          265           265           262           260           258 
 HEC/6-012-06  UQAM/EUT5013  UQAM/DSR2010  UQAM/MBA8M92  UQAM/ALL4351 
          256           256           253           247           246 
 UQAM/DIC9000  UQAM/EST5011  UQAM/ASM2600  HEC/4-401-03 HEC/51-402-02 
          244           244           242           241           241 
HEC/80-452-90  UQAM/ECO3310  UQAM/PTG3230  UQAM/ARA2425  UQAM/ARA2410 
          238           235           235           231           229 
 HEC/2-730-09  UQAM/CHN1612  UQAM/ITA2275  UQAM/ORH5006  UQAM/RUS2375 
          227           225           225           225           225 
 UQAM/VIT1632  HEC/3-017-05  UQAM/AVM5965  HEC/4-507-07  UQAM/EST4116 
          225           223           222           220           220 
 UQAM/EST4126  UQAM/ITA2270  UQAM/MKG5329 HEC/30-420-09 HEC/53-065-07 
          220           220           220           219           219 
HEC/53-301-02  UQAM/JUR4536  UQAM/ALL2340  UQAM/ARA2420  UQAM/ASS7049 
          219           217           215           215           215 
 UQAM/MOD1620  UQAM/MOD6571  UQAM/ALL3340  UQAM/JUR7880  UQAM/MAT1026 
          215           215           214           213           212 
 HEC/2-905-06  HEC/6-710-09 HEC/80-010-76  UQAM/BIA1601  HEC/3-010-04 
          211           211           211           211           210 
HEC/53-063-07  UQAM/DID3280 HEC/30-410-09  HEC/4-131-03  UQAM/ECO1035 
          210           210           209           209           209 
 UQAM/ITA3275  UQAM/MGP7098  UQAM/MOD4420  UQAM/MOD4511  UQAM/MOD4620 
          209           209           209           209           209 
 UQAM/MOD5555 HEC/80-439-95  UQAM/DAN4301  UQAM/EUT1031  UQAM/EUT4021 
          209           208           207           207           207 
 UQAM/EST5002  UQAM/MET5333  HEC/4-506-06  HEC/6-780-09  UQAM/DAM4441 
          206           206           205           205           205 
 UQAM/DAM4451  UQAM/EST4108  UQAM/RUS1370  HEC/6-014-07  UQAM/AVM3900 
          205           205           205           204           204 
### Vérification pour le produit scalaire du plus grand
> m['HEC/6-005-06',] %*% m['Poly/INF6304',]
     [,1]
[1,]  508
> d
*** output flushed ***
### Extraction des termes communs
> t[m['HEC/6-005-06',] & m['Poly/INF6304',]]
 [1] "null"            "recherch"        "plan"            "titrecour"      
 [5] "system"          "connaiss"        "de"              "descriptioncour"
 [9] "inform"          "context"         "humain"         
> t[m['HEC/6-011-06',] & m['Poly/INF6304',]]
[1] "null"            "enjeux"          "recherch"        "titrecour"      
[5] "connaiss"        "de"              "descriptioncour" "context"        

### Effectue l'extration des termes à nouveau mais cette fois entre
### INF6304 et les 100 cours avec le plus grand produit scalaire pour
### en extraire les termes communs.  Il est intéressant de voir que
### les terms communs avec DIC9000 sont nombreux et plus pertinents.
### Pourtant, le cours ne sort qu'en 41ème position, ce qui indique
### que le simple produit scalaire n'est pas très efficace.  On voit
### aussi que plusieurs termes non pertinents reviennent, comme
### "descriptioncour", "titrecours", et "null" qui est toujours présent

> sapply(names(head(r[order(r[,1],decreasing=T),],100)), function(i) t[m[i,] & m['Poly/INF6304',]]
+ )
$`HEC/6-005-06`
 [1] "null"            "recherch"        "plan"            "titrecour"      
 [5] "system"          "connaiss"        "de"              "descriptioncour"
 [9] "inform"          "context"         "humain"         

$`HEC/6-011-06`
[1] "null"            "enjeux"          "recherch"        "titrecour"      
[5] "connaiss"        "de"              "descriptioncour" "context"        

$`HEC/80-463-05`
 [1] "null"            "recherch"        "model"           "titrecour"      
 [5] "connaiss"        "de"              "valid"           "limit"          
 [9] "descriptioncour" "context"         "humain"         

$`HEC/80-430-90`
 [1] "null"            "enjeux"          "titrecour"       "connaiss"       
 [5] "de"              "descriptioncour" "but"             "context"        
 [9] "interet"         "humain"         

$`HEC/6-408-84`
 [1] "null"            "recherch"        "model"           "titrecour"      
 [5] "system"          "connaiss"        "de"              "descriptioncour"
 [9] "conseil"         "inform"         

$`HEC/80-470-07`
 [1] "null"            "recherch"        "model"           "titrecour"      
 [5] "system"          "connaiss"        "de"              "document"       
 [9] "descriptioncour" "context"         "humain"         

$`HEC/3-006-84`
[1] "null"            "enjeux"          "titrecour"       "de"             
[5] "descriptioncour" "inform"          "context"         "humain"         

$`UQAM/JUR7620`
[1] "null"            "plan"            "titrecour"       "system"         
[5] "de"              "descriptioncour" "humain"         

$`HEC/4-900-04`
 [1] "null"            "enjeux"          "model"           "titrecour"      
 [5] "utilisateur"     "de"              "aid"             "descriptioncour"
 [9] "inform"          "context"        

$`HEC/3-730-04`
 [1] "null"            "titrecour"       "connaiss"        "de"             
 [5] "aid"             "descriptioncour" "collaboratif"    "but"            
 [9] "inform"          "humain"         

$`UQAM/MBA8V1B`
[1] "null"            "titrecour"       "de"              "aid"            
[5] "descriptioncour" "context"        

$`HEC/30-515-09`
[1] "null"            "titrecour"       "system"          "de"             
[5] "valid"           "descriptioncour" "inform"          "interfac"       
[9] "interet"        

$`HEC/6-015-08`
[1] "null"            "enjeux"          "sensibl"         "prefer"         
[5] "titrecour"       "de"              "descriptioncour" "context"        
[9] "humain"         

$`HEC/6-040-02`
[1] "null"            "enjeux"          "recherch"        "titrecour"      
[5] "de"              "structure"       "descriptioncour" "caracteristiqu" 
[9] "interet"        

$`HEC/53-038-07`
[1] "null"            "sensibl"         "machin"          "titrecour"      
[5] "de"              "limit"           "descriptioncour"

$`UQAM/MBA8V1A`
[1] "null"            "enjeux"          "titrecour"       "de"             
[5] "limit"           "descriptioncour" "inform"          "context"        

$`HEC/4-499-06`
 [1] "null"            "enjeux"          "recherch"        "plan"           
 [5] "titrecour"       "interact"        "de"              "descriptioncour"
 [9] "caracteristiqu"  "context"         "interet"        

$`UQAM/PTG2235`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "interet"        

$`UQAM/PTG3125`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "interet"        

$`HEC/6-013-06`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "de"              "structure"       "descriptioncour" "context"        

$`HEC/36-131-08`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "document"        "descriptioncour" "assist"          "but"            
[9] "context"        

$`HEC/53-064-07`
 [1] "null"            "model"           "titrecour"       "system"         
 [5] "connaiss"        "de"              "structure"       "descriptioncour"
 [9] "assist"          "but"            

$`HEC/4-440-02`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "caracteristiqu"  "inform"         

$`UQAM/ORH2202`
 [1] "null"            "enjeux"          "model"           "titrecour"      
 [5] "connaiss"        "de"              "descriptioncour" "caracteristiqu" 
 [9] "context"         "humain"         

$`UQAM/ARA3410`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "document"        "descriptioncour" "context"         "interet"        

$`UQAM/KIN7220`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "valid"           "limit"           "descriptioncour" "context"        

$`UQAM/PTG3235`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "document"        "descriptioncour" "context"         "interet"        

$`HEC/2-420-06`
 [1] "null"            "enjeux"          "recherch"        "titrecour"      
 [5] "system"          "de"              "descriptioncour" "conseil"        
 [9] "context"         "interet"        

$`UQAM/ITA3270`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "document"        "descriptioncour" "context"         "interet"        

$`UQAM/AVM4900`
[1] "null"            "recherch"        "titrecour"       "connaiss"       
[5] "interact"        "de"              "descriptioncour" "cooperatif"     
[9] "inform"         

$`HEC/6-012-06`
[1] "null"            "titrecour"       "de"              "descriptioncour"
[5] "context"        

$`UQAM/EUT5013`
[1] "null"            "enjeux"          "plan"            "titrecour"      
[5] "de"              "descriptioncour" "caracteristiqu"  "context"        

$`UQAM/DSR2010`
[1] "null"            "enjeux"          "titrecour"       "de"             
[5] "agent"           "descriptioncour" "caracteristiqu"  "context"        

$`UQAM/MBA8M92`
[1] "null"            "plan"            "titrecour"       "system"         
[5] "de"              "descriptioncour"

$`UQAM/ALL4351`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "document"        "descriptioncour" "assist"          "context"        
[9] "interet"        

$`UQAM/DIC9000`
 [1] "null"            "recherch"        "plan"            "titrecour"      
 [5] "connaiss"        "semantiqu"       "de"              "descriptioncour"
 [9] "intellig"        "inform"          "humain"         

$`UQAM/EST5011`
[1] "null"            "model"           "titrecour"       "interact"       
[5] "de"              "descriptioncour" "cooperatif"      "caracteristiqu" 
[9] "context"        

$`UQAM/ASM2600`
[1] "null"            "model"           "titrecour"       "connaiss"       
[5] "de"              "descriptioncour"

$`HEC/4-401-03`
[1] "null"            "enjeux"          "titrecour"       "de"             
[5] "descriptioncour" "humain"         

$`HEC/51-402-02`
[1] "null"            "sensibl"         "titrecour"       "connaiss"       
[5] "de"              "descriptioncour" "semi"           

$`HEC/80-452-90`
[1] "null"            "recherch"        "titrecour"       "connaiss"       
[5] "de"              "document"        "limit"           "descriptioncour"

$`UQAM/ECO3310`
[1] "null"            "enjeux"          "titrecour"       "interact"       
[5] "de"              "descriptioncour" "caracteristiqu"  "inform"         

$`UQAM/PTG3230`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "context"        
[9] "interet"        

$`UQAM/ARA2425`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "de"              "document"        "descriptioncour" "inform"         
[9] "interet"        

$`UQAM/ARA2410`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`HEC/2-730-09`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "structure"       "descriptioncour" "inform"         

$`UQAM/CHN1612`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`UQAM/ITA2275`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`UQAM/ORH5006`
 [1] "null"            "recherch"        "plan"            "titrecour"      
 [5] "connaiss"        "de"              "limit"           "descriptioncour"
 [9] "conseil"         "humain"         

$`UQAM/RUS2375`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`UQAM/VIT1632`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`HEC/3-017-05`
[1] "null"            "recherch"        "titrecour"       "connaiss"       
[5] "de"              "document"        "descriptioncour" "context"        

$`UQAM/AVM5965`
[1] "null"            "model"           "titrecour"       "interact"       
[5] "de"              "descriptioncour" "caracteristiqu" 

$`HEC/4-507-07`
[1] "null"            "titrecour"       "system"          "de"             
[5] "aid"             "descriptioncour" "inform"          "context"        

$`UQAM/EST4116`
[1] "null"            "titrecour"       "de"              "descriptioncour"
[5] "caracteristiqu" 

$`UQAM/EST4126`
[1] "null"            "titrecour"       "de"              "descriptioncour"
[5] "caracteristiqu" 

$`UQAM/ITA2270`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "context"        
[9] "interet"        

$`UQAM/MKG5329`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "context"        

$`HEC/30-420-09`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "agent"           "descriptioncour" "but"             "context"        

$`HEC/53-065-07`
[1] "null"            "plan"            "titrecour"       "de"             
[5] "descriptioncour" "but"             "context"        

$`HEC/53-301-02`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "inform"          "context"         "interet"        

$`UQAM/JUR4536`
[1] "null"            "titrecour"       "system"          "de"             
[5] "descriptioncour" "caracteristiqu"  "context"        

$`UQAM/ALL2340`
[1] "null"            "titrecour"       "de"              "document"       
[5] "descriptioncour" "interet"        

$`UQAM/ARA2420`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "context"        
[9] "interet"        

$`UQAM/ASS7049`
[1] "null"            "plan"            "titrecour"       "de"             
[5] "limit"           "descriptioncour"

$`UQAM/MOD1620`
[1] "null"            "enjeux"          "recherch"        "plan"           
[5] "titrecour"       "de"              "descriptioncour" "context"        

$`UQAM/MOD6571`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "de"              "limit"           "descriptioncour" "interet"        
[9] "humain"         

$`UQAM/ALL3340`
[1] "null"            "recherch"        "titrecour"       "de"             
[5] "document"        "descriptioncour" "context"        

$`UQAM/JUR7880`
[1] "null"            "enjeux"          "titrecour"       "de"             
[5] "limit"           "descriptioncour" "inform"          "context"        
[9] "interet"        

$`UQAM/MAT1026`
[1] "null"            "dialogu"         "titrecour"       "connaiss"       
[5] "de"              "descriptioncour" "caracteristiqu"  "context"        

$`HEC/2-905-06`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "context"        

$`HEC/6-710-09`
 [1] "null"            "recherch"        "plan"            "titrecour"      
 [5] "system"          "de"              "descriptioncour" "assist"         
 [9] "conseil"         "inform"          "context"        

$`HEC/80-010-76`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "humain"         

$`UQAM/BIA1601`
[1] "null"            "plan"            "titrecour"       "system"         
[5] "connaiss"        "de"              "descriptioncour"

$`HEC/3-010-04`
[1] "null"            "enjeux"          "titrecour"       "connaiss"       
[5] "de"              "document"        "descriptioncour" "caracteristiqu" 
[9] "context"        

$`HEC/53-063-07`
[1] "null"            "titrecour"       "de"              "descriptioncour"
[5] "assist"          "but"            

$`UQAM/DID3280`
[1] "null"            "recherch"        "titrecour"       "connaiss"       
[5] "de"              "document"        "limit"           "descriptioncour"
[9] "context"        

$`HEC/30-410-09`
[1] "null"            "titrecour"       "de"              "descriptioncour"
[5] "context"        

$`HEC/4-131-03`
[1] "null"            "titrecour"       "system"          "utilisateur"    
[5] "de"              "descriptioncour" "inform"          "context"        

$`UQAM/ECO1035`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "inform"          "interet"         "humain"         

$`UQAM/ITA3275`
[1] "null"            "recherch"        "titrecour"       "de"             
[5] "document"        "descriptioncour" "context"        

$`UQAM/MGP7098`
 [1] "null"            "model"           "titrecour"       "system"         
 [5] "connaiss"        "de"              "valid"           "descriptioncour"
 [9] "interfac"        "context"        

$`UQAM/MOD4420`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "connaiss"        "de"              "structure"       "descriptioncour"

$`UQAM/MOD4511`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "connaiss"        "de"              "structure"       "descriptioncour"

$`UQAM/MOD4620`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "connaiss"        "de"              "structure"       "descriptioncour"

$`UQAM/MOD5555`
[1] "null"            "recherch"        "plan"            "titrecour"      
[5] "connaiss"        "de"              "structure"       "descriptioncour"

$`HEC/80-439-95`
[1] "null"            "titrecour"       "system"          "de"             
[5] "descriptioncour" "but"             "context"        

$`UQAM/DAN4301`
[1] "null"            "enjeux"          "plan"            "titrecour"      
[5] "de"              "descriptioncour" "caracteristiqu"  "inform"         

$`UQAM/EUT1031`
[1] "null"            "model"           "titrecour"       "system"         
[5] "de"              "agent"           "descriptioncour"

$`UQAM/EUT4021`
[1] "null"            "enjeux"          "plan"            "titrecour"      
[5] "system"          "de"              "descriptioncour"

$`UQAM/EST5002`
[1] "null"            "titrecour"       "interact"        "de"             
[5] "descriptioncour" "caracteristiqu"  "context"        

$`UQAM/MET5333`
[1] "null"            "plan"            "titrecour"       "system"         
[5] "de"              "descriptioncour" "humain"         

$`HEC/4-506-06`
[1] "null"            "titrecour"       "system"          "de"             
[5] "descriptioncour" "interet"        

$`HEC/6-780-09`
 [1] "null"            "plan"            "titrecour"       "system"         
 [5] "de"              "aid"             "valid"           "structure"      
 [9] "descriptioncour" "inform"          "interet"        

$`UQAM/DAM4441`
[1] "null"            "plan"            "titrecour"       "de"             
[5] "descriptioncour" "assist"         

$`UQAM/DAM4451`
[1] "null"            "plan"            "titrecour"       "de"             
[5] "descriptioncour" "assist"         

$`UQAM/EST4108`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "descriptioncour" "caracteristiqu" 

$`UQAM/RUS1370`
[1] "null"            "titrecour"       "connaiss"        "de"             
[5] "aid"             "document"        "descriptioncour" "context"        
[9] "interet"        

$`HEC/6-014-07`
[1] "null"            "recherch"        "titrecour"       "connaiss"       
[5] "de"              "descriptioncour" "context"        

$`UQAM/AVM3900`
[1] "null"            "recherch"        "model"           "titrecour"      
[5] "interact"        "de"              "descriptioncour" "cooperatif"     
[9] "caracteristiqu" 

> 
