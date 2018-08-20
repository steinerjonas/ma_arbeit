# Teilweise wurde Code von C. Puschmannn übernommen.

#######################################################################################
#################### Installation und Laden der notwendigen Bibliotheken
#######################################################################################

# Packages
if(!require("lubridate")) install.packages("lubridate")
if(!require("quanteda")) install.packages("quanteda")
if(!require("dplyr")) install.packages("dplyr")

# Zeitzonen
Sys.setenv(TZ="Europe/Zurich")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#Stopword Liste Quelle:
# https://github.com/igorbrigadir/stopwords/blob/master/en/snowball_original.txt
# Diese wurde ergänzt. Die in Quanteda integrierte snowball Liste enthielt das Wort will, welches für die Analyse relevant ist.

#Datenbank laden
load("data/datenbank_mag_april.rda")
load("data/dfm.rda")
load("data/dfm.stemmed.rda")
load("data/dfm.tfidf.rda")
load("data/top400features.rda")
load("data/top400dfm.tfidf.rda")
load("data/dfm.tfidf.stem.red.rda")

#######################################################################################
############# Korpus erstellen
#######################################################################################
daten.df$id <- rownames(daten.df)
korpus <- corpus(daten.df, docid_field = "id", text_field = "text")

#######################################################################################
############# Anzahl Tokens im Korpus
#######################################################################################
sum(ntoken(korpus))

#######################################################################################
############# Anzahl Dokumente im Korpus
#######################################################################################
ndoc(korpus)

#######################################################################################
############# Verteilung der Artikel über Zeit
#######################################################################################
#Column erstellen, die nur den Monat zeigt
daten.df$month.yrs <- as_date(daten.df$date)
day(daten.df$month.yrs) <- 1

#Publikationsdaten aller Artikel exportieren
write.table(daten.df[,1:2], file = "exports/daten.df.pub.csv", sep = "\t", row.names = F)

#######################################################################################
############# Verteilung der Sources
#######################################################################################

# Sources auszählen
source.df <- count(daten.df, source)
colnames(source.df)[2] <- "freq"
source.df <- source.df[with(source.df, order(-freq)),]
source.df <- source.df[1:20,]

#Source export
write.table(source.df, file = "exports/source.freq20.csv", sep = "\t", row.names = F)

#######################################################################################
############# DFM (stemmed) - Häufigste Terme (ungewichtet)
#######################################################################################

#DFM Stemmed (reduziert) 20 häufgiste Terme
dfmstem20freq <- textstat_frequency(dfm.stemmed, n=20)

#Export der 20 Begriffe die im Korpus am häufigsten vorkommen.
write.table(dfmstem20freq, file = "exports/dfmstem20freq.csv", sep = "\t", row.names = F)

#Export der 400 Begriffe die im Korpus am häufigsten vorkommen.
write.csv(top400features, "exports/topfeatures/top400features_unweighted.csv", row.names=T)

#######################################################################################
############# DFM (stemmed) - Häufigste Terme (tf-idf)
#######################################################################################
top400dfm.tfidf$term <- rownames(top400dfm.tfidf)

#Export der 20 Begriffe, welche über alle Dokumente die grösste Summe der tf-idf Scores haben.
write.csv(top400dfm.tfidf[1:20,], "exports/topfeatures/top20dfm_tf-idf.csv", row.names=T)

#Export der 400 Begriffe, welche über alle Dokumente die grösste Summe der tf-idf Scores haben.
write.csv(top400dfm.tfidf, "exports/topfeatures/top400dfm_tf-idf.csv", row.names=T)

#######################################################################################
############# Reduzierte DTM mit Mindesttokenfrequenz 100 und Dokumentfrequenz 3
#######################################################################################

# Erstellung einer reduzierten Dokument-Term-Matrix mit Mindesttokenfrequenz = 100 und Dokumentenfrequenz = 3
dfm.trim <- dfm_trim(dfm, min_termfreq = 100, min_docfreq = 3)
topfeatures(dfm.trim)

# Begriffe mit hoher Keyness (1-44 Monate)
head(textstat_keyness(dfm.trim, 1), 8)
head(textstat_keyness(dfm.trim, 2), 8)
head(textstat_keyness(dfm.trim, 3), 8)

#######################################################################################
############# Bigram - Analyse mit quanteda
#######################################################################################
# Bigram mit Stopword Entfernung
stopwords.list <- readLines("stopword/stopwords.txt", encoding = "UTF-8")
# https://github.com/quanteda/quanteda/issues/1018
bigram.tokens <- tokens(korpus) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(pattern = stopwords.list, padding  = TRUE) %>%
  tokens_ngrams(n = 2)

#DFM erstellen mit Stemming
bigram.dfm <- dfm(bigram.tokens, tolower = T, stem = T)

#Häufigkeit
bigram.häufigkeiten <- textstat_frequency(bigram.dfm)
bigram.häufigkeiten <- bigram.häufigkeiten[with(bigram.häufigkeiten, order(-frequency)),]
bigram.häufigkeiten <- bigram.häufigkeiten[1:50,]

#TF-IDF
bigram.tfidf <- dfm_tfidf(bigram.dfm, scheme_tf = "count", scheme_df = "inverse", base = 10)
top20bigram.tfidf <- topfeatures(bigram.tfidf, n = 20)
top20bigram.tfidf <- as.data.frame(top20bigram.tfidf)

#Bigram Export
write.table(bigram.häufigkeiten, "exports/bigram.häufigkeiten.csv", row.names = F, sep = "\t")
write.table(top20bigram.tfidf, "exports/top20bigram.tfidf.csv", row.names = F, sep = "\t")


#######################################################################################
############# Trigram - Analyse mit quanteda
#######################################################################################
# Bigram mit Stopword Entfernung
stopwords.list <- readLines("stopword/stopwords.txt", encoding = "UTF-8")
# https://github.com/quanteda/quanteda/issues/1018
trigram.tokens <- tokens(korpus) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(pattern = stopwords.list, padding  = TRUE) %>%
  tokens_ngrams(n = 3)

trigram.dfm <- dfm(trigram.tokens)

#Häufigkeit
trigram.häufigkeiten <- textstat_frequency(trigram.dfm)
trigram.häufigkeiten <- trigram.häufigkeiten[with(trigram.häufigkeiten, order(-frequency)),]
trigram.häufigkeiten <- trigram.häufigkeiten[1:50,]

#TF-IDF
trigram.tfidf <- dfm_tfidf(trigram.dfm, scheme_tf = "count", scheme_df = "inverse", base = 10)
top20trigram.tfidf <- topfeatures(trigram.tfidf, n = 20)
top20trigram.tfidf <- as.data.frame(top20trigram.tfidf)

#Bigram Export
write.table(trigram.häufigkeiten, "exports/trigram.häufigkeiten.csv", row.names = F, sep = "\t")
write.table(top20trigram.tfidf, "exports/top20trigram.tfidf.csv", row.names = F, sep = "\t")
