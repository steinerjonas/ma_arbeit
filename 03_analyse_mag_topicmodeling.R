#Packages
if(!require("quanteda")) install.packages("quanteda")
if(!require("topicmodels")) install.packages("topicmodels")
if(!require("TraMineR")) install.packages("TraMineR")
if(!require("reshape2")) install.packages("reshape2")
if(!require("ggplot2")) install.packages("ggplot2")

#Zeitzonen
Sys.setlocale("LC_TIME", "en_US.UTF-8")
Sys.setenv(TZ="Europe/Zurich") 

#Daten laden
load("data/datenbank_mag_april.rda")
load("data/dfm.tfidf.stem.red.rda")


#Anzahl Topics
K <- 34

#Seed für Reproduzierbarkeit
set.seed(9161)

#estimated
topicmodel <- LDA(dfm.tfidf.stem.red, K, method = "Gibbs", control=list(seed=0, iter = 500))
#0.2
topicmodel0.2 <- LDA(dfm.tfidf.stem.red, K, method = "Gibbs", control=list(seed=0, iter = 500, alpha = 0.2))
#0.05
topicmodel0.05 <- LDA(dfm.tfidf.stem.red, K, method = "Gibbs", control=list(seed=0, iter = 500, alpha = 0.05))
#50/K
topicmodel1.4 <- LDA(dfm.tfidf.stem.red, K, method = "Gibbs", control=list(seed=0, iter = 500, alpha = 1.428571))

exampleIds <- c(150,203,502,1030,1992,2100)
N <- length(exampleIds)

tmResult <- posterior(topicmodel)
tmResult0.2 <- posterior(topicmodel0.2)
tmResult0.05 <- posterior(topicmodel0.05)
tmResult1.4 <- posterior(topicmodel1.4)

#estimated
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicmodel, 5), 2, paste, collapse = " ")

#0.2
theta0.2 <- tmResult0.2$topics
beta0.2 <- tmResult0.2$terms
topicNames0.2 <- apply(terms(topicmodel0.2, 5), 2, paste, collapse = " ")

#0.05
theta0.05 <- tmResult0.05$topics
beta0.05 <- tmResult0.05$terms
topicNames0.05 <- apply(terms(topicmodel0.05, 5), 2, paste, collapse = " ")

#50/K
theta1.4 <- tmResult1.4$topics
beta1.4 <- tmResult1.4$terms
topicNames1.4 <- apply(terms(topicmodel1.4, 5), 2, paste, collapse = " ")

#estimated
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

#0.2
topicProportionExamples0.2 <- theta0.2[exampleIds,]
colnames(topicProportionExamples0.2) <- topicNames0.2
vizDataFrame0.2 <- melt(cbind(data.frame(topicProportionExamples0.2), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

#0.05
topicProportionExamples0.05 <- theta0.05[exampleIds,]
colnames(topicProportionExamples0.05) <- topicNames0.05
vizDataFrame0.05 <- melt(cbind(data.frame(topicProportionExamples0.05), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

#50/K
topicProportionExamples1.4 <- theta1.4[exampleIds,]
colnames(topicProportionExamples1.4) <- topicNames1.4
vizDataFrame1.4 <- melt(cbind(data.frame(topicProportionExamples1.4), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

#Topics erstellen 
K <- 34
topicmodel <- LDA(dfm.tfidf.stem.red, K, method = "Gibbs", control=list(seed=0, iter = 500, alpha = 0.05))

#Resultate betrachten
topicmodelterms <- terms(topicmodel, 20)
topicmodelterms <-as.data.frame(topicmodelterms)

#Vorbereitung für die Analyse
tmResult <- posterior(topicmodel)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicmodel, 20), 2, paste, collapse = ", ")

#Welchen Anteil haben die Topics im gesamten Korpus?
topicProportions <- colSums(theta) / 2299
names(topicProportions) <- topicNames
sort(topicProportions, decreasing = TRUE)
topicProportions <- as.data.frame(topicProportions)
topicProportions$topics <- rownames(topicProportions)
rownames(topicProportions) <- 1:nrow(topicProportions)

#Mittelwerte der Topic Proportionen pro Quartal und Monat berechnen
topic_proportion_per_quarter <- aggregate(theta, by = list(quarter = daten.df$qtr), mean)
topic_proportion_per_month <- aggregate(theta, by = list(month = daten.df$month), mean)

#Wörter der Topics den jeweiligen Daten zuordnen
colnames(topic_proportion_per_quarter)[2:(K+1)] <- topicNames
colnames(topic_proportion_per_month)[2:(K+1)] <- topicNames

#Reshape data frame
vizDataFrame.quarter <- melt(topic_proportion_per_quarter, id.vars = "quarter")
vizDataFrame.month <- melt(topic_proportion_per_month, id.vars = "month")

#Speichern der Daten
save(topicmodel,file = "topicmodels/topicmodel.rda")
save(tmResult,file = "topicmodels/tmresult.rda")
save(theta,file = "topicmodels/theta.rda")
save(beta,file = "topicmodels/beta.rda")
save(topicProportions,file = "topicmodels/topicproportions.rda")
save(vizDataFrame.month,file = "topicmodels/vizDataFrame-month.rda")
save(vizDataFrame.quarter,file = "topicmodels/vizDataFrame-quarter.rda")
save(vizDataFrame0.2, file = "topicmodels/vizDataFrame0.2.rda")
save(vizDataFrame0.05, file = "topicmodels/vizDataFrame0.05.rda")
save(vizDataFrame1.4, file = "topicmodels/vizDataFrame1.4.rda")
save(vizDataFrame, file = "topicmodels/vizDataFrame.rda")

#CSV Dateien exportieren
write.table(topicmodelterms, "exports/topicmodeling/topicmodelterms.csv", sep = "\t", row.names = F)
write.table(topicProportions, "exports/topicmodeling/topicProportions.csv", sep = "\t", row.names = F)
write.table(vizDataFrame.month, "exports/topicmodeling/vizdataframe_final_month.csv", sep = "\t", row.names = F)
write.table(vizDataFrame.quarter, "exports/topicmodeling/vizdataframe_final_quarter.csv", sep = "\t", row.names = F)

#Alpha Tests
write.table(vizDataFrame, "exports/topicmodeling/testsalpha/vizdataframe.csv", sep = "\t", row.names = F)
write.table(vizDataFrame0.05, "exports/topicmodeling/testsalpha/vizdataframe0.05.csv", sep = "\t", row.names = F)
write.table(vizDataFrame0.2, "exports/topicmodeling/testsalpha/vizdataframe0.2.csv", sep = "\t", row.names = F)
write.table(vizDataFrame1.4, "exports/topicmodeling/testsalpha/vizdataframe1.4.csv", sep = "\t", row.names = F)

