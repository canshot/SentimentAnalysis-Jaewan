library(plyr)
library(igraph)
library(wordcloud)
library(tm)
library(ggplot2)
library(stats)

##Load Dataset 
book1 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf1.csv", stringsAsFactors = FALSE)
book2 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf2.csv", stringsAsFactors = FALSE)
book3 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf3.csv", stringsAsFactors = FALSE)
book4 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf4.csv", stringsAsFactors = FALSE)
book5 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf5.csv", stringsAsFactors = FALSE)
book6 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf6.csv", stringsAsFactors = FALSE)

##Select the review on Hardcover 

#Before selecting, identify the number of each format

#first method
nrow(book1[book1$formats == "Hardcover",])
length(book1$formats[book1$formats == "Hardcover"])

nrow(book1[book1$formats == "Kindle Edition",])
length(book1$formats[book1$formats == "Kindle Edition"])

nrow(book1[book1$formats == "Audible Audio Edition",])
length(book1$formats[book1$formats == "Audible Audio Edition"])

#more simple method
library(plyr)
count(book1, formats)
count(book2, formats)
count(book3, formats)
count(book4, formats)
count(book5, formats)
count(book6, formats)

#or

table(book1$formats)
table(book2$formats)
table(book3$formats)
table(book4$formats)
table(book5$formats)
table(book6$formats)


#make histogram for formats
library(ggplot2)
ggplot(data.frame(book1), aes(x=formats)) + geom_bar()
ggplot(data.frame(book2), aes(x=formats)) + geom_bar()
ggplot(data.frame(book3), aes(x=formats)) + geom_bar()
ggplot(data.frame(book4), aes(x=formats)) + geom_bar()
ggplot(data.frame(book5), aes(x=formats)) + geom_bar()
ggplot(data.frame(book6), aes(x=formats)) + geom_bar()

#create the row of Hardcover format 
hard1 = subset(book1, formats == "Hardcover")
hard2 = subset(book2, formats == "Hardcover")
hard3 = subset(book3, formats == "Hardcover")
hard4 = subset(book4, formats == "Hardcover")
hard5 = subset(book5, formats == "Hardcover")
hard6 = subset(book6, formats == "Hardcover")

#create the row of Kindle format 
kindle1 = subset(book1, formats == "Kindle Edition")

#create the row of Audible format 
audible1 = subset(book1, formats == "Audible Audio Edition")

#create the row of Paperback format 
paper1 = subset(book1, formats == "Paperback")

#create the row of Audio CD format 
audio1 = subset(book1, formats == "Audio CD")

##manipulate DATE

# %Y: 4-digit year (1982), %y: 2-digit year (82), %m: 2-digit month (01), %d: 2-digit day of the month (13), 
# %A: weekday (Wednesday)%a: abbreviated weekday (Wed), %B: month (January), %b: abbreviated month (Jan)

hard1$dates = as.Date(hard1$dates, format = "%B %d, %Y")
hard2$dates = as.Date(hard2$dates, format = "%d-%b-%y")
hard3$dates = as.Date(hard3$dates, format = "%d-%b-%y")
hard4$dates = as.Date(hard4$dates, format = "%d-%b-%y")
hard6$dates = as.Date(hard6$dates, format = "%d-%b-%y")

kindle1$dates = as.Date(kindle1$dates, format = "%B %d, %Y")

#order by date
library(dplyr)
hard1=arrange(hard1, dates)
hard2=arrange(hard2, dates)
hard3=arrange(hard3, dates)
hard4=arrange(hard4, dates)
hard6=arrange(hard6, dates)

kindle1=arrange(kindle1, dates)

##character count
library(qdap)

#delete outlier
#hard1.small = after deletion outlier
hard1.small=hard1[-243,]

hard1.cc = character_count(hard1.small$texts)
kindle1.cc=character_count(kindle1$texts)

hard1.cc.length = length(hard1.cc)
hard1.cc.sum = sum(hard1.cc)
hard1.cc.mean = mean(hard1.cc)
hard1.cc.sd = sd(hard1.cc)
book1.cc.df = data.frame(hard1.cc.length,hard1.cc.sum,hard1.cc.mean,hard1.cc.sd)

kindle1.cc.length = length(kindle1.cc)
kindle1.cc.sum = sum(kindle1.cc)
kindle1.cc.mean = mean(kindle1.cc)
kindle1.cc.sd = sd(kindle1.cc)

#create dataframe - book1.cc.df
book1.cc.df = cbind(book1.cc.df, kindle1.cc.length, kindle1.cc.sum, kindle1.cc.mean, kindle1.cc.sd)
View(book1.cc.df)

plot(character_count(hard1.small$texts), ylab="character count", main = "[Book1] Character # of Hardcover")
plot(kindle1.cc, ylab = "character count", main = "[Book1] Character # of Kindle")

qplot(kindle1$dates, kindle1.cc,
      xlab = "Dates",
      ylab="Character Counts", 
      main = "[Book1 - Kindle] Character Counts")

##word count
library(qdap)
sum(wc(hard1.small$texts))
mean(wc(hard1.small$texts))
sd(wc(hard1.small$texts))
wc = wc(hard1.small$texts)

qplot(hard1.small$dates, wc(hard1.small$texts),
      xlab = "Dates",
      ylab="Word Counts", 
      main = "[Book1 - Hardcover] Word Counts")

##Create a list for character and word count
count=list(hard1.char.sum = sum(character_count(hard1.small$texts)),
           hard1.char.mean = mean(character_count(hard1.small$texts)),
           hard1.char.sd = sd(character_count(hard1.small$texts)),
           hard1.word.sum = sum(wc(hard1.small$texts)),
           hard1.word.mean = mean(wc(hard1.small$texts)),
           hard1.word.sd = sd(wc(hard1.small$texts))
)

##Create a rate list
hard1.rate=list(hard1.rate.sum = sum(hard1.small$ratings),
                hard1.rate.mean = mean(hard1.small$ratings),
                hard1.rate.median = median(hard1.small$ratings),
                hard1.rate.sd = sd(hard1.small$ratings)
)

kindle1.rate=list(kindle1.rate.sum = sum(kindle1$ratings),
                  kindle1.rate.mean = mean(kindle1$ratings),
                  kindle1.rate.median = median(kindle1$ratings),
                  kindle1.rate.sd = sd(kindle1$ratings)
)

hard1.kindle1.rate=append(hard1.rate, kindle1.rate)
View(hard1.kindle1.rate)

##Create a plot ratings by dates
qplot(hard1.small$dates, hard1.rate$hard1.rate.mean, 
      xlab = "Dates",
      ylab="Ratings", 
      main = "[Book1 - Hardcover] Ratings")

qplot(kindle1$dates, kindle1.rate$kindle1.rate.mean, 
      xlab = "Dates",
      ylab="median of Ratings", 
      main = "[Book1 - Kindle] Ratings")

##Construct a corpus
hard1.corpus=Corpus(VectorSource(hard1.small$texts))
kindle1.corpus=Corpus(VectorSource(kindle1$texts))

##Data preprocessing (1): To lower case 
kindle1.corpus = tm_map(kindle1.corpus, content_transformer(tolower))

##Data preprocessing (2): Remove punctuation
kindle1.corpus = tm_map(kindle1.corpus, content_transformer(removePunctuation))
kindle1.corpus[[1]][1]

##Data preprocessing (3): Remove numbers
kindle1.corpus = tm_map(kindle1.corpus, content_transformer(removeNumbers))
kindle1.corpus[[1]][1]

##Data preprocessing (4): Remove stopwords with "SMART stopwords list"
mystopwords = stopwords("SMART")
kindle1.corpus = tm_map(kindle1.corpus, removeWords, mystopwords)
kindle1.corpus[[1]][1]

##Data preprocessing (5): Stemming
kindle1.stem = tm_map(kindle1.corpus, stemDocument)
kindle1.corpus[[1]][1]

##Construct Term-Document matrix
kindle1.TDM = TermDocumentMatrix(kindle1.stem, control = list(minWordLength = 1))

##Check the TDM
kindle1.TDM
kindle1.TDM.matrix = as.matrix(kindle1.TDM)

##Find high frequency words
kindle1.word.freq = sort(rowSums(kindle1.TDM.matrix), decreasing = TRUE)
head(kindle1.word.freq, 30)   

##Construct word cloud 
kindle1.word.freq.names = names(kindle1.word.freq)
kindle1.word.cloud.df = data.frame(word <- kindle1.word.freq.names, freq <- kindle1.word.freq)
kindle1.pal = brewer.pal(8, "Dark2")
wordcloud(kindle1.word.cloud.df$word, 
          kindle1.word.cloud.df$freq, 
          min.freq = 3, 
          scale = c(3, 0.1),
          rot.per = 0.1, col=kindle1.pal, random.order = F)

