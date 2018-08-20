#Für die Analyse werden in diesem Abschnitt diverse DFMs erstellt.

#Installation der Packages und Zeitzonen 
if(!require("quanteda")) install.packages("quanteda")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
Sys.setenv(TZ="Europe/Zurich") 

#Stopword Liste Quelle:#  
#https://github.com/igorbrigadir/stopwords/blob/master/en/snowball_original.txt  
#Diese Liste wurde ergänzt. Die in Quanteda integrierte snowball Liste enthielt das Wort "will", welches für die Analyse relevant ist.

#Import
load("data/datenbank_mag_april.rda")

#Quanteda Korpus erstellen
daten.df$id <- rownames(daten.df)
korpus <- corpus(daten.df, docid_field = "id", text_field = "text")

#Stopword Liste
stopwords.list <- readLines("stopword/stopwords.txt", encoding = "UTF-8")

#Dokument-Feature-Matrix (DFM)
#Dabei wird mittels der angepassten Snowball Liste Stopwörter entfernt.  
#Zudem werden Satzzeichen sowie Zahlen entfernt.Die Mindestlänge von Features wird bei 2 definiert. Diese DFM dient auch als Grundlage für die weiteren DFMs mit Gewichtung und Mindesttermfrequenz.
dfm <- dfm(korpus, remove = stopwords.list, remove_punct = T, remove_numbers=T)
dfm <- dfm_select(dfm, min_nchar = 2)

#Dokument-Feature-Matrix (DFM) - stemmed
dfm.stemmed <- dfm(korpus, remove = stopwords.list, remove_punct = T, remove_numbers=T, stem=T)
dfm.stemmed <- dfm_remove(dfm.stemmed, "u.")
dfm.stemmed <- dfm_select(dfm.stemmed, min_nchar = 2)

#Dokument-Feature-Matrix (DFM) mit tf-idf Gewichtung - stemmed
dfm.tfidf <- dfm_tfidf(dfm.stemmed, scheme_tf = "count", scheme_df = "inverse", base = 10)

#Topfeatures extrahieren und als CSV Abspeichern
#Unterstützte auch die Erstellung der eigenen Stopword-Liste.

#DFM - stemmed
top400features <- topfeatures(dfm.stemmed, n = 400)
top400features <- as.data.frame(top400features)

#DFM(tf-idf) - stemmed
top400dfm.tfidf <- topfeatures(dfm.tfidf, n = 400)
top400dfm.tfidf <- as.data.frame(top400dfm.tfidf)

#Extrahieren der 10 Features mit der grössten Summe über alle Dokumente
top10dfm.tfidf <- topfeatures(dfm.tfidf, n = 10)
top10dfm.tfidf <- as.data.frame(top10dfm.tfidf)
top10dfm.tfidf.list <- rownames(top10dfm.tfidf) # Liste mit den 10 Features erstellt

dfm.tfidf.10 <- dfm_keep(dfm.tfidf, top10dfm.tfidf.list, valuetype = "glob")
dfm.tfidf.10 <- as.data.frame(dfm.tfidf.10)
dfm.tfidf.10$document <- NULL

#stemmed DFM (TF-IDF) mit Mindesttermfrequenz = 5# 
dfm.red <- dfm_trim(dfm, min_termfreq = 5)
dfm.tfidf.red <- dfm_tfidf(dfm.red, scheme_tf = "count", scheme_df = "inverse", base = 10) %>% round(digits = 0)
dfm.tfidf.stem.red <- dfm_wordstem(dfm.tfidf.red)
dfm.tfidf.stem.red <- dfm_remove(dfm.tfidf.stem.red, "u.")

#Speicherung der verschiedenen DFMs#  
#Für die Analyse können die Dateien so einfacher und übersichtlicher abgerufen werden.  
save(dfm, file = "data/dfm.rda")
save(dfm.stemmed, file = "data/dfm.stemmed.rda")
save(dfm.tfidf, file = "data/dfm.tfidf.rda")
save(top400features, file = "data/top400features.rda")
save(top400dfm.tfidf, file = "data/top400dfm.tfidf.rda")
save(dfm.tfidf.stem.red, file = "data/dfm.tfidf.stem.red.rda")
