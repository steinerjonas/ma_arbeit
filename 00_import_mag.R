#Import der Magazin Artikel geladen am 03.05.2018.  
#Lexis Nexis suche: atleast3(blockchain) and type(magazine)  
#Zudem wurden Websites, Newswires ausgeschlosssen  
#Gewählt wurden English Language News.    

###Installation der Packages und Zeitzonen
if(!require("stringr")) install.packages("stringr")
if(!require("lubridate")) install.packages("lubridate")
if(!require("dplyr")) install.packages("dplyr")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("cld2")) install.packages("cld2")

Sys.setlocale("LC_TIME", "en_US.UTF-8")
Sys.setenv(TZ="Europe/Zurich") 

#Import
#Artikel importieren#
#Daten werden aus dem LexisNexis Export gelesen
dateien <- dir("data_sources/", pattern = ".TXT")

#Set einlesen#
daten <- character(0)
for (i in seq_along(dateien))
  daten <- c(daten, scan(file = paste("data_sources/", dateien[i], sep = ""), what = "char", sep ="\n",  encoding = "latin1", blank.lines.skip = F, multi.line = TRUE))
daten <- str_trim(daten)


#Löschen von nicht benötigten Zeilen#
line.cleaning.list <- str_split(readLines("cleaning/line_cleaning.txt"), pattern = "\n")
line.cleaning <- paste(unlist(line.cleaning.list), collapse = "|")
daten <- gsub(line.cleaning, "", daten, perl = T)

#Löschen der Values#  
rm(line.cleaning, line.cleaning.list, i, dateien)

#Artikel extrahieren#  
#Code von Cornelius Puschmann - angepasst auf die eigenen Daten.  
#Dabei wird nach einem Muster gesucht in den Daten und somit die Artikel extrahiert.
muster <- "([0-9]{1,3} of [0-9]{1,3} DOCUMENTS)" #Muster wird definiert
artikel <- which(str_detect(daten, muster)); ac <- length(artikel) #Artikel werden definiert

#Parsen der Daten
daten.df <- data.frame(text = character(ac), row.names = 1:ac, stringsAsFactors = F)
for (i in seq_along(artikel))
{
  if (!exists("parse_start")|i == 1) parse_start <- 1
  else parse_start <- artikel[i-1] + 2
  parse_stop <- artikel[i]
  artikel_text <- paste(daten[parse_start:parse_stop], collapse = " ")
  daten.df$text[i] <- str_replace(artikel_text, muster, "")
}
daten.df$text <- str_trim(daten.df$text) #Text wird getrimmt

#Aufräumen
rm(ac, artikel, artikel_text, daten, i, muster, parse_start, parse_stop)


#Zur besseren Identifikation werden IDs vergeben für jeden Artikel.
daten.df$id <- rownames(daten.df)

#Bereinigung

#Erkennnung der Sprache
#Löschen von nicht-englischen Artikeln.  
#Im LexisNexis Export waren trotz der Begrenzung auf englische Artikel französische, sowie italienische Artikel  zu finden. 
#Mithilfe cld2 (Google's Compact Lanaguage Detector 2)
daten.df$language <- detect_language(daten.df$text)
daten.df <- daten.df[daten.df$language == "en",]
daten.df$language <- NULL
daten.df <- daten.df[complete.cases(daten.df), ] #NA Zeile löschen


#Löschen der SECTION und LENGHT 

#Wird einzeln ausgeführt, da LENGHT über mehrere Zeilen lang sein kann  
daten.df$text <- str_replace(daten.df$text, "SECTION:.+?(?=LENGTH:)","")
daten.df$text <- str_replace(daten.df$text, "LENGTH:.*?(words)","")

#Löschen von nicht benötigten Wörtern, Störungen#  
#Die importierte Liste wurde von Hand mithilfe von Auszählungen erstellt.  

#Import des Textfiles
words.cleaning.list <- str_split(readLines("cleaning/words_cleaning.txt"), pattern = "\n")
words.cleaning <- paste(unlist(words.cleaning.list), collapse = "|")
#Löschen der Störungen
daten.df$text <- gsub(words.cleaning, "", daten.df$text, perl = T)

#Löschen der Values
rm(words.cleaning, words.cleaning.list)

#Überprüfen ob die Artikel ungewöhnlich kurz geworden sind#  

#Ungewöhnlich kurze Artikel?
daten.df$char.text <- nchar(daten.df$text)
daten.df$char.text <- NULL

#Typus extrahieren und bereinigen#  

#Extrahieren des PUBLICATION-TYPE
daten.df$publicationtype <- str_extract(daten.df$text, '(?<=PUBLICATION-TYPE:\\s)\\w+')
sum(is.na(daten.df$publicationtype))

#Extrahieren des TYPE
daten.df$type <- str_extract(daten.df$text, '(?<=TYPE:\\s)\\w+')
sum(is.na(daten.df$type))

#Rechtschreibfehler in der Quelle korrigieren, um Source extrahieren zu ermöglichen
daten.df$text <- str_replace(daten.df$text, "Janaury", "January")

#Löschen des Publicationstype, da nur magazine im Korpus sind
daten.df$publicationtype <- NULL
daten.df$type <- NULL


#Source extrahieren und bereinigen#  
#Extrahieren des Newspapers / Source
pattern.source <- ".+?(?=(January|February|March|April|May|June|July|August|September|October|November|December))"
daten.df$source <- str_extract(daten.df$text, pattern.source)

#Entfernen der Source aus $text
daten.df$text <- str_replace(daten.df$text, pattern.source,"")
daten.df$source <- str_trim(daten.df$source)

#Ungewöhnlich lange Source Daten?
daten.df$char.source <- nchar(daten.df$source)
daten.df$char.source <- NULL

#Löschen der Values
rm(pattern.source)


#Load-Date und Publication-Date extrahieren und bereinigen#  

#Load-Date extrahieren
daten.df$loaddate <- str_extract(daten.df$text, "LOAD-DATE: (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2}, (2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018)")
daten.df$loaddate <- str_replace(daten.df$loaddate, "LOAD-DATE:","")

#Load-Date in Dateformat bringen
daten.df$loaddate <- str_trim(daten.df$loaddate)
daten.df$loaddate <- as.Date(daten.df$loaddate,format='%B %d, %Y')

#Unvollständige Werte?
sum(is.na(daten.df$loaddate))

#Publication-Date extrahieren

#Extrahieren des Datums
pattern.date <- ".+?(2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018)"
daten.df$date <- str_extract(daten.df$text, pattern.date)
#Entfernen des Datum aus $text
daten.df$text <- str_replace(daten.df$text, pattern.date,"")

#Unvollständige Werte? 
sum(is.na(daten.df$date))

#Datum erstellen aus dem String daten.df$date
pattern.month <- "(January|February|March|April|May|June|July|August|September|October|November|December|Aug)"
daten.df$date.month <- str_extract(daten.df$date, pattern.month)
daten.df$date <- str_replace(daten.df$date, pattern.month, "")

pattern.year <- "[0-9]{4}"
daten.df$date.year <- str_extract(daten.df$date, pattern.year)
daten.df$date <- str_replace(daten.df$date, pattern.year, "")

pattern.day <- "[0-9]{1,2}"
daten.df$date.day <- str_extract(daten.df$date, pattern.day)

#Kontrolle über fehlende Werte
sum(is.na(daten.df$source))
sum(is.na(daten.df$date.day)) #Day sind 367 Fälle NA - Grund dafür sind die Magazine
sum(is.na(daten.df$date.month))
sum(is.na(daten.df$date.year))
sum(is.na(daten.df$date))

#Artikel, die nur den Monat angegeben haben werden mit dem Wert 1. versehen.
#Darauf achten, dass in Zukunft mindestens monatlich analysiert wird.  
daten.df$date.day[is.na(daten.df$date.day)] <- 1

#Datum neu zusammenfügen
daten.df$date <- paste(daten.df$date.month, daten.df$date.day, daten.df$date.year, sep=" ")
daten.df$date <- as.Date(daten.df$date,format='%B %d %Y')

#Löschen der nicht mehr benötigten Werte
daten.df$date.publish <- NULL
daten.df$date.day <- NULL
daten.df$date.month <- NULL
daten.df$date.year <- NULL

rm(pattern.date, pattern.year, pattern.month, pattern.day)

#Bereinigen des Textfeldes
#Entfernen des Wochentags
pattern.weekday <- "(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)+"
daten.df$text <- str_replace(daten.df$text, pattern.weekday, "")

#Entfernen der Uhrzeit
pattern.time <- "[0-9]{1,2}:[0-9]{2} (PM|AM) (PST|MST|MSK|GMT|EST)"
daten.df$text <- str_replace(daten.df$text, pattern.time, "")

#Löschen aller Informationen hinter dem String "LOAD-DATE:", "CONTACT:", "PUBLICATIONTYPE:" "TYPE:" oder "URL:"
pattern.loaddate <- "LOAD-DATE.+"
daten.df$text <- str_replace(daten.df$text, pattern.loaddate, "")

pattern.pubtype <- "PUBLICATION-TYPE:.+"
daten.df$text <- str_replace(daten.df$text, pattern.pubtype, "")

pattern.type <- "TYPE:.+"
daten.df$text <- str_replace(daten.df$text, pattern.type, "")

pattern.url <- "URL:.+"
daten.df$text <- str_replace(daten.df$text, pattern.url, "")

#Löschen falls ein zweites Datum im Text verwendet wurde
pattern.date.unused <- "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2}, [0-9]{4}"
daten.df$text <- str_replace(daten.df$text, pattern.date.unused, "")
daten.df$text <- str_trim(daten.df$text)
rm(pattern.type, pattern.date.unused, pattern.weekday, pattern.url, pattern.loaddate, pattern.pubtype, pattern.time)

#Extrahieren des Titels#  
#Da der Titel teilweise auch mit ; abgetrennt wird, wird das Semikolon nun durch das Keyword TITELSTOP ersetzt.  
#Nach dem Seminkolon folgt teilweise auch der Leadtext, welcher hier auch zum Fliesstext gezählt wird.  


#Semikon zu Beginn des Textfeldes löschen und sonstige Korrekturen
pattern.semistart <- "^;"
daten.df$text <- str_replace(daten.df$text, pattern.semistart, "")

pattern.unused.date <- "^- (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}"
daten.df$text <- str_replace(daten.df$text, pattern.unused.date, "")

pattern.wrong <- "^(Financial Mail Edition|Cover Story:|Comment:|Comment - |Shaping tomorrow - Comment - )"
daten.df$text <- str_replace(daten.df$text, pattern.wrong, "")

daten.df$text <- str_trim(daten.df$text)

#Start des Titels extrahieren
pattern.semi <- ";"
daten.df$text <- str_replace(daten.df$text, pattern.semi, "TITLESTOP")

pattern.spaces <- "[:space:]{3,10}"
daten.df$text <- str_replace(daten.df$text, pattern.spaces, "TITLESTOP")

#Titel extrahieren
pattern.title.detection <- ".+?TITLESTOP"
daten.df$title <- str_extract(daten.df$text, pattern.title.detection)

#Entfernen des Titels aus Text
daten.df$text <- str_replace(daten.df$text, pattern.title.detection,"")

#TITLE STOP entfernen - kann auch mitten im Text vorkommen, da ; ersetzt wurde um Titel und leadtext zu trennen
pattern.titlestop <- "TITLESTOP"
daten.df$title <- str_replace(daten.df$title, pattern.titlestop, " ")
daten.df$text <- str_replace(daten.df$text, pattern.titlestop, " ")

daten.df$title <- str_trim(daten.df$title)
daten.df$text <- str_trim(daten.df$text)

#Dies wird benutzt um die fehlerhaften Titel zu erkennen
daten.df$char.title <- nchar(daten.df$title)

rm(pattern.spaces, pattern.title.detection, pattern.semi, pattern.semistart, pattern.titlestop, pattern.unused.date, pattern.wrong)


#Korrektur der fehlenden Werte#  
#Subsetting von NA Werten im Titel. Nach dem Extrahieren der Source können diese eventuell manuell hinzugefügt werden.  

sub.df.title <- daten.df[is.na(daten.df$title),]

#Korrektur der fehlenden Titel
#ID 14
#Titel war unten im Text
daten.df[daten.df$id == 14,6] <- "Manufacturing is coming back to the US and Europe. It is going to happen"
daten.df[daten.df$id == 14,1] <- str_replace(daten.df[daten.df$id == 14,1], "Manufacturing is coming back to the US and Europe. It is going to happen","")

#ID 52
daten.df[daten.df$id == 52,6] <- "Robotics drive industry to the west"
daten.df[daten.df$id == 52,1] <- str_replace(daten.df[daten.df$id == 52,1], "Robotics drive industry to the west","")
daten.df[daten.df$id == 52,1] <- str_replace(daten.df[daten.df$id == 52,1], "Jonathan Lopez San Antonio","")

#ID 725
daten.df[daten.df$id == 725,1] <- paste(daten.df[daten.df$id == 725,6], daten.df[daten.df$id == 725,1], sep= " ")
daten.df[daten.df$id == 725,6] <- "Vanguard employing blockchain in index data distribution"
daten.df[daten.df$id == 725,1] <- str_replace(daten.df[daten.df$id == 725,1], "Vanguard employing blockchain in index data distribution","")
daten.df[daten.df$id == 725,1] <- str_replace(daten.df[daten.df$id == 725,1], "INDUSTRY HIGHLIGHTS","")

#ID 726
daten.df[daten.df$id == 726,6] <- "The Digital Republic"
daten.df[daten.df$id == 726,1] <- str_replace(daten.df[daten.df$id == 726,1], "The Digital Republic","")

#ID 2181 Source: http://ec2-54-72-50-240.eu-west-1.compute.amazonaws.com/Transactions-Technology/Technology/Goldman-Sachs-turbo-charges-access-to-data?ct=true
daten.df[daten.df$id == 2181,6] <-"Goldman Sachs turbo charges access to data"

#ID 2265 Source: http://ec2-54-72-50-240.eu-west-1.compute.amazonaws.com/Transactions-Technology/Technology/One-small-step-towards-innovation-at-Standard-Chartered
daten.df[daten.df$id == 2265,6] <- "One small step towards innovation at Standard Chartered"

#ID 772
daten.df[daten.df$id == 772,6] <- "Fellow geologists were critical to a fault"
daten.df[daten.df$id == 772,1] <- str_replace(daten.df[daten.df$id == 772,1], "Fellow geologists were critical to a fault","")



#Source Analyse#  
#Überprüfung ob alles in Ordnung ist mit den Quellen.  

#Häufigste Sources aufzeigen
freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}
freqfunc(daten.df$source,200)

#Excel Datei für mehr Übersicht
export <- daten.df[,"source"]
write.csv2(export, file= "exports/sourcelist.csv")

#Entfernen von Values
rm(export, sub.df.title, words.cleaning.list, pattern.date.unused, pattern.loaddate, pattern.pubdtype, pattern.semikolon, pattern.spaces, pattern.time, pattern.title.detection, pattern.titlestop, pattern.pubtype, pattern.subhead, pattern.type, pattern.weekday, words.cleaning, '%nin%')

#Str_Trim
daten.df$title <- str_trim(daten.df$title)
daten.df$text <- str_trim(daten.df$text)
daten.df$source <- str_trim(daten.df$source)

#Dies wird benutzt um die fehlerhaften Texte zu erkennen
daten.df$ntext <- nchar(daten.df$text)

#Entfernen der Auszählungen
daten.df$char.title <- NULL
daten.df$ntext <- NULL



#Löschen von weiteren Störungen mithilfe einer Liste#  
#Diverse Wörter, Zeichen, URLs, Bilder etc. werden durch eine Liste, sowie einzelne Befehle entfernt.

#Import des TXT.FILES
words.cleaning.list <- str_split(readLines("cleaning/cleaning_text2.txt"), pattern = "\n")
words.cleaning <- paste(unlist(words.cleaning.list), collapse = "|")

#Sichern des Textes um anschliessende Differenz zu berechnen
daten.df$ntext <- nchar(daten.df$text)

#Löschen der Wörter
daten.df$text <- gsub(words.cleaning, "", daten.df$text, perl = T)

#Berechnen, ob es grosse unterschiede gab zin der Verarbeitung.
daten.df$ntext2 <- nchar(daten.df$text)
daten.df$ntext3 <- daten.df$ntext - daten.df$ntext2

daten.df$ntext <- NULL
daten.df$ntext2 <- NULL
daten.df$ntext3 <- NULL
daten.df$loaddate <- NULL

#Anordnen der Spalten
daten.df <- daten.df[,c(2,3,4,5,1)]

#ID löschen für CorText
daten.df$id <- NULL

#Löschen von Duplikaten
daten.df <- daten.df[!duplicated(daten.df[c(1,2,3)]),] #171 Duplikate wurden gelöscht

#Löschen von '
daten.df$text <- str_replace_all(daten.df$text, "'", "")
daten.df$title <- str_replace_all(daten.df$title, "'", "")

#Einzelne Artikel besitzen bereits in der Quelldatei Begriffe wie "don t". Diese werden entfernt.
daten.df$text <- str_replace_all(daten.df$text, "\\s[b-z]{1}\\s", " ")

#Monate berechnen
daten.df$month <- months(daten.df$date)
daten.df$year <- format(daten.df$date,format="%y")
daten.df$month <- paste("01",daten.df$month,daten.df$year, sep=" ")
daten.df$month <- as.Date(daten.df$month,format='%d %B %y')
daten.df$year <- NULL

#Quartale berechnen
daten.df$qtr <- paste(format(daten.df$date, "%y"),
                      sprintf("%02i", (as.POSIXlt(daten.df$date)$mon) %/% 3L + 1L), 
                      sep="/")

#Sichern der Textdaten
#Die Dateien werden gesichert für die weitere Arbeit.  

#Sichern der DB in Rda und csv
save(daten.df, file = "data/datenbank_mag_april.rda")
write.table(daten.df, file= "data/datenbank_mag_april.csv", sep="\t",row.names=FALSE)


