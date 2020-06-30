#ANALYTICS 3 Pratical examination
#Name: DEBORAH MENEZES
#Student_ID: 11012497
#Date: 24th June 2020

#packages to install
install.packages("factoextra")
library("factoextra")
#in order to describe the data including missing values
install.packages("Hmisc")
library(Hmisc)
#in order to describe the data including std deviation etc
install.packages("pastecs")
library(pastecs)
install.packages("factoextra")
library("factoextra")
#for corelation plotting
install.packages("corrplot")
library("corrplot")
#corelationship and plotting
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")


#PROBLEM 1
#1(a) (4 points) Use a statistical method of your choice from Analytic 3 to build a
# predictive model with response Survival. Briefly explain why you choose your method?

#using SVM to predict the model: I choose this methid because SVMs can efficiently 
#perform a non-linear classification using what is called the kernel trick,
#implicitly mapping their inputs into high-dimensional feature spaces.
library(e1071)
library(caret)

d1 <- read.csv("data/dataset_1.csv")
d1$Survival=as.factor(d1$Survival)
s=svm(Survival~., data=d1)
plot(s,d1)
summary(s)

pred <- predict(s, d1)

#confusion table using table
t=table(pred, d1$Survival)

#to find the accuracy with the sum of the diagonal vs the total no of rows
accuracy=sum(diag(t)/nrow(d1))

#hyperparameter tunning the svm to check if the accuracy increases
set.seed(123)
model <- tune(svm, Survival~., data = d1,
              ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

bestmodel=model$best.model
predd=predict(bestmodel,d1)
tab=table(predicted=predd,Actual=d1$Survival)
acc=sum(diag(tab)/nrow(d1))
#compare both the accuracy and see the difference
error=1-acc




#1.(b) (8 points) Choose any other method from Analytic 2 or 3 and build a second
#model which allows you to detect statistical importance of predictor variables.
#Interpret the results, and visualize important variables.

#using logistic regression to predict the code (glm)
# Fit the model
model <- glm( Survival ~., data = d1, family = 'binomial')
# Summarize the model
summary(model)
# Make predictions
probabilities <- predict(model, d1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
# Model accuracy
mean(predicted.classes == d1$Survival)


#1.(c) (4 points) Compare your two models. Are there any differences predicting survival?
#comparing the two models --> Yes, there is a difference in predicting survival
#svm model accuracy =0.8104575 and 
#logistic regression model accuracy is 0.748366
# svm is a better modelfor this dataset

#1.(d) (4 points) Predict survival with your two models for the following new cases:
#create a datafram with the following values
dt=data.frame(age=c(25,30,60),year=c(60,58,67),Freq=c(15,1,47))
predd=predict(bestmodel,dt)


#PROBLEM 2
#Load the text file text.txt line-wise (one line = a character string). Note: The file has
#encoding UTF-8. When you have any problems to load the file, read help.search("read
#text files")


require(textcat)
require(textfeatures)
library(syuzhet)
library(ggplot2)
library(tm)
library(tidyverse)
library(tokenizers)



#2.(a) (2 points) Determine the language of the text.

#to detrmine the language of the text file
DATA_DIR <- system.file("data/text.txt", package = "readtext")
readtext(paste0(DATA_DIR, "/txt/UDHR/*"))

text = readLines('data/text.txt')
textcat(text)
#The text is 95% portuguese and  contains other langauges as well

#count the no of frequency a langauge is used in the text
frequency_df = data.frame(table(t))

#in order to tokenize the text
require(tokenizers)
tokens=unique(get_tokens(text,pattern="\\W"))


#2.(b) (4 points) Generate a table with text features (e. g. punctuation marks etc.).
#Are the any conspicuous features?

#Text Features( to get the textfeature table)
require(textfeatures)

## Loading required package: textfeatures
textfeatures(x, verbose=FALSE)

textfeatures(x, normalize = FALSE, verbose=FALSE)
#No there are no conspicuous features present from the textfeature table


#2.(c) (6 points) Which line has the most letters?
nchar(text)
max(nchar(text))

# to get the text of that particular line that has most no of letters. 
which.max(nchar(text))
text[5]
textcat(text[5])

#no of langauges present in the text.txt file 
table(textcat(text))


#2.(d) (8 points) Conduct a sentiment analysis and present your results in a table and
#a graphic

#sentiment analysis
s <- data.frame(Word=tokens, sentiment=get_sentiment(tokens, method="syuzhet"))
ss=s[!s$sentiment==0,]

#plotting the sentiment
plot(ss$sentiment, axes=F, ylab="Sentiment Score",
     xlab="", type="b", col="red", lwd=2, ylim=c(-1,1))
abline(h=0, col="darkblue")
abline(v=1:nrow(ss), col="darkblue", lty=2)
axis(2)
text(1:nrow(ss), rep(-1, nrow(ss)), lab=ss$Word, srt=35, xpd=TRUE,# pos=1,
     adj=c(1.2,1.3))

#creating a word cloud from text file 
library(wordcloud)

mg <- VCorpus(DirSource("./data", encoding = "UTF-8"),readerControl = list(language = "en") )

#pre-processing steps
mg.clean <- tm_map(mg, content_transformer(tolower))
mg.clean <- tm_map(mg.clean, stripWhitespace)
mg.clean <- tm_map(mg.clean,removePunctuation)
mg.clean <- tm_map(mg.clean,removeNumbers)
mg.clean <- tm_map(mg.clean, removeWords, stopwords("english"))
mg.clean <- tm_map(mg.clean, stemDocument)
dtm <- DocumentTermMatrix(mg.clean)
inspect(dtm)
findFreqTerms(dtm, 20)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)

wordcloud(names(freq),freq,scale = c(5,1), max.words = 50,random.order = F,
          colors = brewer.pal(8, "Dark2"), rot.per = 0.35,use.r.layout = F)

#PROBLEM 3
#3.(a) (4 points) Compute descriptive statistics for all variables.
#Are there any variables with outliers (choose a method of your choice)?
library(factoextra)
#to read the data into Rstudio
d <- read.csv("data/students.csv")
# descriptive statistics basic summary 
summary(d)
str(d)

e=prcomp(d[,c(1:4)],scale = TRUE)
boxplot(e)

#find out the 3rd quartile
summary(e)[["1st Qu."]]
OutVals = boxplot(e)$out
which(e %in% OutVals)
# There are no outliers present here


#3.(b) (6 points) Examine the correlations between numeric variables. Can you reduce
#the number of variables?
# To find the corelationship between the numeric values 
cor(e)

corrplot(c, type="upper", order="hclust", t1.col="black",t1.str=45)

chart.Correlation(e, histogram=TRUE, pch=19)
#yes we can reduce the no of variables 


#Calculation the pca
pca = prcomp(d[,1:4], scale=T)
plot(pca)
fviz_eig(pca, addlabels = TRUE, choice="variance")
#From the scree plot, it's evident that using only two variables, we can use PC1 
#and PC2 to explain 87% of the variance
biplot(pca, xpd=TRUE)
var <- get_pca_var(pca)
fviz_pca_var(pca, col.var = "black")
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

summary(pca)
#From the plot we can see that anxiety has the highest cos2 or the vector length
#which tells the that there is high importance
#Score, A_points and Hours are all closely correlated and can be reduced 



#PROBLEM 4
#4.(a) (6 points) Build a neural network with dichotomous response variable score.
#Use any amount of hidden layers you might find useful. Compute a confusion
#matrix. Because of the small sample size, you do not need to split the data into
#a training and test set.

require(nnet)
#Build a neural network with dichomized response variable score

d <- read.csv("data/students.csv")
fit <- nnet(SCORE ~ ., size=6, data=d, linout=T)
pred <- predict(fit, newdata=d)
y <- d$score


#Simply converting the date fields to integers fixes the problem
## compute residuals
res <- (y)^2

fit.lm <- lm(score ~ ., data=d)
## long output, print only R^2
summary(fit.lm)[c("r.squared", "adj.r.squared")]

rmse <- function(e) {
  sqrt(mean(e^2))
}
rmse(res)

## residuals and RMSE from linear regression
rmse(residuals(fit.lm))

#4.(b) (4 points) Can you suggest an optimal number of hidden layers, e. g. 1 up to 5?

#2 would be optimal number of hidden layers for any neural network model.
#(2 Hidden layers)
