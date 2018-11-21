mydir <- "C:/Users/claudedb/Desktop/essai"

essai <- textmatrix( mydir, stemming=TRUE, language="french", minWordLength=4, maxWordLength=FALSE, minDocFreq=1,  maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE,  stopwords=NULL, vocabulary=NULL, phrases=NULL,  removeXML=FALSE, removeNumbers=FALSE)
essai.not <- textmatrix( mydir, stemming=FALSE, language="french", minWordLength=4, maxWordLength=FALSE, minDocFreq=1,  maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE,  stopwords=NULL, vocabulary=NULL, phrases=NULL,  removeXML=FALSE, removeNumbers=FALSE)
