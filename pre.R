library(plyr)
library(igraph)
library(wordcloud)
library(tm)
library(ggplot2)

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
count(book1, "formats")
count(book2, "formats")
count(book3, "formats")
count(book4, "formats")
count(book5, "formats")
count(book6, "formats")

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


#manipulate and order by DATE

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

#######################task 0309 ends

#character & word count
library(qdap)
character_count(hard1$texts)
sum(character_count(hard1$texts))
mean(character_count(hard1$texts))
sd(character_count(hard1$texts))
plot(character_count(hard1$texts), ylab="character count", main = "[Book1] Character # of Hardcover")
View(hard1)
testdate <- hard1[rev(order(as.Date(hard1$dates))),]

plot(wc(hard1$texts))
sum(wc(hard1$texts))


