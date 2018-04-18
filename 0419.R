library(tidytext)
library(dplyr)
library(stringr)

##Load Dataset 
book1 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf1.csv", stringsAsFactors = FALSE)
book2 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf2.csv", stringsAsFactors = FALSE)
book3 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf3.csv", stringsAsFactors = FALSE)
book4 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf4.csv", stringsAsFactors = FALSE)
book5 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf5.csv", stringsAsFactors = FALSE)
book6 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf6.csv", stringsAsFactors = FALSE)

##character count
library(qdap)
#by total format 
book1.cc=character_count(book1$texts)

#by each format
hard1.large.cc = character_count(hard1$texts)
kindle1.cc=character_count(kindle1$texts)
audible1.cc=character_count(audible1$texts)

hard1.large.cc

#sentiment analysis
review_words1 <- book1 %>%
  select(X, dates, ratings, formats, titles, texts) %>%
  unnest_tokens(word, texts) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

View(review_words1)

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

reviews_sentiment1 <- review_words1 %>%
  inner_join(AFINN, by = "word") %>%
  group_by(X, dates, ratings, formats) %>%
  summarize(sentiment = mean(afinn_score))%>%
  mutate(method = "AFINN")

View(reviews_sentiment1)

#order 'review_sentiment1' by date
# %Y: 4-digit year (1982), %y: 2-digit year (82), %m: 2-digit month (01), %d: 2-digit day of the month (13), 
# %A: weekday (Wednesday), %a: abbreviated weekday (Wed), %B: month (January), %b: abbreviated month (Jan)

reviews_sentiment1$dates = as.Date(reviews_sentiment1$dates, format = "%B %d, %Y")
review_sentiment1.dates=arrange(reviews_sentiment1, dates)

View(review_sentiment1.dates)

#sentiment & ratings by month
library(tidyverse)
library(lubridate)
library(plyr)

review_sentiment1.dates$dates <- floor_date(review_sentiment1.dates$dates, "month")
review_sentiment1.month = ddply(review_sentiment1.dates, "dates", summarise, 
                                sentiment = mean(sentiment), 
                                ratings = mean(ratings))
View(review_sentiment1.month)

#allign setiment & rating with sales
review_sentiment1.month=review_sentiment1.month[-c(1:3),]

#plot of sentiment by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(review_sentiment1.month, aes(dates, sentiment, group = dates)) +
  geom_density() + 
  ylab("Sentiment scores") + 
  xlab("Dates") + 
  ggtitle("Sentiment Score by Month")

#plot of ratings by month 
library(ggplot2)
theme_set(theme_bw())
ggplot(review_sentiment1.month, aes(dates, ratings, group = dates)) +
  geom_density() + 
  ylab("Ratings") + 
  xlab("Dates") + 
  ggtitle("Ratings by Month")

#sales
sales1 <- read.csv("C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/sales1.csv", stringsAsFactors = FALSE)
review_sentiment1.month=review_sentiment1.month[-c(1:3),]
sales1 = sales1[-15,]
theme_set(theme_bw())
ggplot(sales1, aes(date, sales, group = date)) +
  geom_density() + 
  ylab("Sales(number)") + 
  xlab("Dates") + 
  ggtitle("Sales by Month [Title: Lincoln in the Bardo]")

########## Linear Regression ########## 

#The lm of sentiment on ratings
lm.fit1 = lm(review_sentiment1.month$ratings~review_sentiment1.month$sentiment)
lm.fit1
summary(lm.fit1)
names(lm.fit1)
coef(lm.fit1)
confint(lm.fit1)
predict(lm.fit1,review_sentiment1.month$sentiment=0.1052632,interval="confidence")
par(mfrow=c(2,2))
plot(review_sentiment1.month$ratings,review_sentiment1.month$sentiment,
     pch=20, col="red",cex = 2,
     main="Simple Linear Regresion of SENTIMENT1 & RATINGS1",
     ylab = "sentiment", xlab = "rating")
abline(lm.fit1, col="red")

#The lm of sentiment on sales
View(review_sentiment1.month)
lm.fit.sales1 = lm(sales1$sales~review_sentiment1.month$sentiment)
lm.fit.sales1
summary(lm.fit.sales1)
names(lm.fit.sales1)
coef(lm.fit.sales1)
confint(lm.fit.sales1)
plot(review_sentiment1.month$sentiment,sales1$sales,
     pch=20, col = "red", cex = 2,
     main="Simple Linear Regresion of SALES1 & SENTIMENT1",
     ylab = "sales", xlab = "sentiment")
abline(lm.fit.sales1, col="red")

#The lm of ratings on sales
lm.fit.sales1.ratings = lm(sales1$sales~review_sentiment1.month$ratings)
lm.fit.sales1.ratings
summary(lm.fit.sales1.ratings)
names(lm.fit.sales1.ratings)
coef(lm.fit.sales1.ratings)
confint(lm.fit.sales1.ratings)
plot(review_sentiment1.month$ratings,sales1$sales,
     pch=20, col = "red", cex = 2, 
     main="Simple Linear Regresion of SALES1 & RATING1",
     ylab = "Sales", xlab = "Ratings")
abline(lm.fit.sales1.ratings, col="red")

#Multiple lm 
lm.fit.multi1 = lm(sales1$sales~review_sentiment1.month$sentiment+review_sentiment1.month$ratings)
lm.fit.multi1
summary(lm.fit.multi1)




