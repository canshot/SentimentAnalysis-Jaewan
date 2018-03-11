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

do.call(rbind,lapply(paste('X',1:i,sep=''),get))

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

#order by date
library(dplyr)
hard1=arrange(hard1, dates)
hard2=arrange(hard2, dates)
hard3=arrange(hard3, dates)
hard4=arrange(hard4, dates)
hard6=arrange(hard6, dates)

##character count
library(qdap)

#delete outlier
#hard1.small = after deletion outlier
hard1.small=hard1[-243,]
character_count(hard1.small$texts)

sum(character_count(hard1.small$texts))
mean(character_count(hard1.small$texts))
sd(character_count(hard1.small$texts))

plot(character_count(hard1.small$texts), ylab="character count", main = "[Book1] Character # of Hardcover")

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

##Create rate
rate=list(hard1.rate.sum = sum(hard1.small$ratings),
          hard1.rate.mean = mean(hard1.small$ratings),
          hard1.rate.median = median(hard1.small$ratings),
          hard1.rate.sd = sd(hard1.small$ratings)
          )
##Create a plot ratings by dates
qplot(hard1.small$dates, hard1.small$ratings, 
      xlab = "Dates",
      ylab="Ratings", 
      main = "[Book1 - Hardcover] Ratings")


##Construct a corpus
hard1.corpus=Corpus(VectorSource(hard1.small$texts))

##Data preprocessing (1): To lower case 
hard1.corpus = tm_map(hard1.corpus, content_transformer(tolower))

##Data preprocessing (2): Remove punctuation
hard1.corpus = tm_map(hard1.corpus, content_transformer(removePunctuation))
hard1.corpus[[1]][1]

##Data preprocessing (3): Remove numbers
hard1.corpus = tm_map(hard1.corpus, content_transformer(removeNumbers))
hard1.corpus[[1]][1]

##Data preprocessing (4): Remove stopwords with "SMART stopwords list"
mystopwords = stopwords("SMART")
hard1.corpus = tm_map(hard1.corpus, removeWords, mystopwords)
hard1.corpus[[1]][1]

##Data preprocessing (5): Stemming
hard1.stem = tm_map(hard1.corpus, stemDocument)
hard1.corpus[[1]][1]

##Construct Term-Document matrix
hard1.TDM = TermDocumentMatrix(hard1.stem, control = list(minWordLength = 1))

##Check the TDM
hard1.TDM
hard1.TDM.matrix = as.matrix(hard1.TDM)

##Find high frequency words
hard1.word.freq = sort(rowSums(hard1.TDM.matrix), decreasing = TRUE)
head(hard1.word.freq, 30)   

##Construct word cloud 
hard1.word.freq.names = names(hard1.word.freq)
hard1.word.cloud.df = data.frame(word <- hard1.word.freq.names, freq <- hard1.word.freq)
hard1.pal = brewer.pal(8, "Dark2")
wordcloud(hard1.word.cloud.df$word, 
          hard1.word.cloud.df$freq, 
          min.freq = 3, 
          scale = c(3, 0.1),
          rot.per = 0.1, col=hard1.pal, random.order = F)


