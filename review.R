install.packages("dplyr")
install.packages("readr")
install.packages("timeDate")
install.packages("sqldf")
install.packages("textcat")
install.packages("NLP")
install.packages("openNLP")
install.packages("quanteda")
install.packages("qdap")
install.packages("devtools")
install.packages("quantmod")
install.packages("timeDate")
install.packages("textcat")
install.packages(c("RTextTools","topicmodels"))
install.packages("lda")
install.packages("tm")

library(dplyr)
library(readr)
library(timeDate)
library(sqldf)
library(textcat)
library(NLP)
library(openNLP)
library(quanteda)
library(qdap)
library(LDAvis)
library(devtools)
library(timeDate)
library(quantmod)
library(RTextTools)
library(topicmodels)
library(textcat)
library(slam)
library("servr")
library(lda)
library(tm)
library(reshape)
library(ScottKnott)
library(RTextTools)
library(LDAvisData)


setwd("C:/OPIM5671/")
reviews=read_csv("yelp_review.csv")
View(reviews)
user=read_csv("yelp_user.csv")
View(user)
business<-read.csv("RestaurantBusiness.csv")
View(business)
maindata=inner_join(reviews, user, by = c("user_id" = "user_id"))
View(maindata)
maindata <- maindata[,-(17),drop=FALSE]
maindata <- maindata[,-(11:14),drop=FALSE]
maindata <- maindata[,-(9),drop=FALSE]
maindata <- maindata[,-(4),drop=FALSE]
View(maindata)
class(maindata$business_id)
class(business$business_id)
business$business_id<-as.character(business$business_id)
maindata=inner_join(maindata, business, by = c("business_id" = "business_id"))
View(maindata)
#8,11-14,16-17,19,22
maindata <- maindata[,-(22),drop=FALSE]
maindata <- maindata[,-(19),drop=FALSE]
maindata <- maindata[,-(16:17),drop=FALSE]
maindata <- maindata[,-(11:14),drop=FALSE]
maindata <- maindata[,-(8),drop=FALSE]
maindata <- maindata[,-(12),drop=FALSE]
View(maindata)
class(maindata$date)
maindata$date<-as.Date(maindata$date, "%m/%d/%Y")
class(maindata$date)
maindata <- maindata[maindata$date > as.Date("2010-11-1"),]
View(maindata)
maindata<- filter(maindata,maindata$review_count.x>=25)
maindata<- filter(maindata,maindata$review_count.y>=10)
maindata <- maindata[,-(9:10),drop=FALSE]
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
maindata$IsWeekend <- c('1', '0')[(weekdays(maindata$date) %in% weekdays1)+1L]
View(maindata)
h1<-holiday(2010,"USThanksgivingDay")
h2<-holiday(2010,"USChristmasDay")
h3<-holiday(2011,"USThanksgivingDay")
h4<-holiday(2011,"USChristmasDay")
h5<-holiday(2012,"USThanksgivingDay")
h6<-holiday(2012,"USChristmasDay")
h1<-as.Date(h1)
h2<-as.Date(h2)
h3<-as.Date(h3)
h4<-as.Date(h4)
h5<-as.Date(h5)
h6<-as.Date(h6)
maindata$HolidaySeason <- ifelse((maindata$date >= h1 & maindata$date < h1+7) | (maindata$date >= h2 & maindata$date < h2+11) | (maindata$date >= h3 & maindata$date < h3+7) | (maindata$date >= h4 & maindata$date < h4+11) | (maindata$date >= h5 & maindata$date < h5+7) | (maindata$date >= h6 & maindata$date < h6+11) , 1, 0)
View(maindata)
colnames(maindata)[which(names(maindata) == "funny.x")] <- "funny"
colnames(maindata)[which(names(maindata) == "useful.x")] <- "useful"
colnames(maindata)[which(names(maindata) == "cool.x")] <- "cool"
colnames(maindata)[which(names(maindata) == "stars.x")] <- "stars"
View(maindata)
hist(maindata$stars, breaks = 10, xlab="Stars", ylab="No of customers",border="blue",col="green",las=1)
write_csv(maindata,"Merged.csv")
maindata$ScaledRating <- (5*maindata$stars/maindata$average_stars)
View(maindata)

maindata$language<- textcat(maindata$text)
table(maindata$language)
maindata <- filter(maindata, language == 'english')
View(maindata)

maindata$IsAsian <- grepl("Indian",maindata$text)| grepl("chineese",maindata$text) | grepl("sushi",maindata$text)== "TRUE"
maindata$IsBreakfast <- grepl("Breakfast",maindata$text) | grepl("brunch",maindata$text) == "TRUE"
maindata$IsMexican <- grepl("Mexican",maindata$text) == "TRUE"
maindata$IsHappyHour <- grepl("Happy hour",maindata$text) | grepl("Fast Food",maindata$text)  == "TRUE"
maindata$IsPizzaBurger <- grepl("pizza",maindata$text) | grepl("Burger",maindata$text)  == "TRUE"
View(maindata)

comments<-maindata
text_reviews <- as.character(maindata$text)
Corpus <- corpus(text_reviews)
summary(Corpus, n = 10)
MyDataFrame <- dfm(Corpus,ignoredFeatures = stopwords("english"), stem = TRUE)

#######################################################################################
# Text pre-cleaning
text_reviews <- gsub("[0-9]", "", text_reviews) 
tdocs <-tm_map(docs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern," ", x))});
text_reviews <- tm_map(text_reviews, toSpace, "-");
text_reviews <- tm_map(text_reviews, toSpace, "’");
text_reviews <- tm_map(text_reviews, toSpace, "‘");
text_reviews <- tm_map(text_reviews, toSpace, "•");
text_reviews <- tm_map(text_reviews, toSpace, "”");
text_reviews <- tm_map(text_reviews, toSpace, "“");
text_reviews <- tm_map(text_reviews, removePunctuation)
#Strip digits
text_reviews <- tm_map(text_reviews, removeNumbers)
#remove stopwords
text_reviews <- tm_map(text_reviews, removeWords, stopwords("english"))
#remove whitespace
text_reviews <- tm_map(text_reviews, stripWhitespace);
text_reviews <- tm_map(text_reviews,stemDocument);
dtm <- DocumentTermMatrix(text_reviews);
rownames(dtm) <- filenames;
freq <- colSums(as.matrix(dtm));
length(freq);
ord <- order(freq,decreasing=TRUE);
write.csv(freq[ord],"word_freq.csv");
Tokens.list <- strsplit(text_reviews, "[[:space:]]+")
#######################################################################################
#CorpusObj <- VectorSource(maindata$text)
#CorpusObj.tdm <- TermDocumentMatrix(CorpusObj, control = list(minWordLength = 3))
#mTDM <- as.matrix(CorpusObj.tdm)
#v <- sort(rowSums(mTDM),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)
#Corpus <- tm_map(Corpus, toSpace,"-")
#getTransformations()
#toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
#Corpus <- tm_map(Corpus, removePunctuation)
#dtm <- DocumentTermMatrix(reviews)
#######################################################################################
Terms <- table(unlist(Tokens.list))
Terms <- sort(Terms, decreasing = TRUE)
del <- names(Terms) %in% stop_words
documents <- lapply(Tokens.list, get.terms)
##########################################################################################

D <- length(documents)  
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ]))  
N <- doc.length  
term.frequency <- as.integer(Terms)  

# Tuning model parameters:
K <- 8  # Number of topics
G <- 50 # Number of iterations
Alpha <- 0.05
Beta <- 0.05
set.seed(123)
model <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,num.iterations = G, Alpha = Alpha,Beta = Beta, initial = NULL, burnin = 0,compute.log.likelihood = TRUE)

########################################################################################################
Theta <- t(apply(model$document_sums + Alpha, 2, function(x) x/sum(x)))
Phi <- t(apply(t(model$topics) + Beta, 2, function(x) x/sum(x)))

docs = Corpus(VectorSource(text_reviews))
tdm= DocumentTermMatrix(docs)
CorpusObj.tdm<-tdm
mTDM <- as.matrix(CorpusObj.tdm)
v <- sort(rowSums(mTDM),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
freq <- colSums(as.matrix(tdm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),bounds = list(global = c(3,27))))
dtmr
freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)
#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#inspect least frequently occurring terms
freqr[tail(ordr)]  
findFreqTerms(dtmr,lowfreq=70)
findAssocs(dtmr,"cheap",0.6)
findAssocs(dtmr,"priced",0.6)
findAssocs(dtmr,"dead",0.6)
wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
library(wordcloud)
wordcloud(names(freqr),freqr, min.freq=70)
freq <- colSums(as.matrix(dtmr))
length(freq)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq.csv")
library(topicmodels)
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
ldaOut <-LDA(dtmr,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))



term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)
summary(col_sums(tdm))

#Getting best number of topics using log likelihood
mymodel <- lapply(seq(2, 10, by = 1), function(d){LDA(tdm, d)})
mymodel.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
mymodel.logLik.df <- data.frame(topics=c(2:100), LL=as.numeric(as.matrix(mymodel.logLik)))
mymodel.logLik.df[which.max(best.model.logLik.df$LL),]
topics = LDA(tdm, 8, method = "Gibbs", control = NULL, model = NULL)
termstopics=as.data.frame(terms(topics,20))
write.csv(termstopics,"TopicTerms.csv")

ldaOut <-LDA(dtm,k = 8, method="Gibbs", control=list(nstart=5, seed = list(23,50,10,5,15,45,23,78), best= TRUE, burnin = 4000, iter = 2000, thin=500))
ldaOut.topics <- as.matrix(topics(ldaOut));
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"));

ldaOut.terms <- as.matrix(terms(ldaOut,6));
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
##############################################################################################################
View(maindata)
maindata <- maindata[maindata$date < as.Date("2013-1-1"),]
maindata$date<-as.Date(maindata$date, "%m/%d/%Y")
class(maindata$date)
table(format(maindata$date,"%b-%Y"))
TimeSeries<-table(format(maindata$date,"%b-%Y"))
TimeSeries
write.csv(TimeSeries, file = "TS.csv")
data<-read.csv("TS.csv")
data<-ts(data[,2],start = c(2009,3),frequency = 12)
data
plot.ts(data)
plot(data, xlab="Years", ylab = "Reviews")
plot(diff(data),ylab="Differenced Reviews")
plot.ts(diff(data))
plot(log10(data),ylab="Log (Reviews)")
plot(diff(log10(data)),ylab="Differenced Log (Reviews)")
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main="ACF Reviews")
pacf(ts(diff(log10(data))),main="PACF Reviews")

library(forecast)
ARIMAfit <- auto.arima(log10(data),approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
pred <- predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type="l",xlim=c(2008,2011),ylim=c(1,1600),xlab = "Year",ylab = "reviews")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+2*pred$se),col="orange")
lines(10^(pred$pred-2*pred$se),col="orange")
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main="ACF Residual")
pacf(ts(ARIMAfit$residuals),main="PACF Residual")
