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

#create the row of Kindle format 
kindle1 = subset(book1, formats == "Kindle Edition")

#create the row of Audible format 
audible1 = subset(book1, formats == "Audible Audio Edition")

#create the row of Paperback format 
paper1 = subset(book1, formats == "Paperback")

#create the row of Audio CD format 
audio1 = subset(book1, formats == "Audio CD")


