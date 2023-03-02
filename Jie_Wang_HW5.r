################################################
## Homework 5 by Jie Wang 2022/08/12
################################################

## load the libraries
library(tm)
library(stringr)
library(wordcloud)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(network)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(textmineR)
library(igraph)
library(lsa)

NovelsCorpus <- Corpus(DirSource("New_txt"))
ndocs<-length(NovelsCorpus)
NovelsCorpus <- tm_map(NovelsCorpus, content_transformer(tolower))
NovelsCorpus <- tm_map(NovelsCorpus, removePunctuation)
NovelsCorpus <- tm_map(NovelsCorpus, removeWords, stopwords("english"))

MyStopWords <- c("and","like", "very", "can", "I", "also", "lot")
NovelsCorpus <- tm_map(NovelsCorpus, removeWords, MyStopWords)

str(NovelsCorpus)

summary(NovelsCorpus)
meta(NovelsCorpus[[1]])
meta(NovelsCorpus[[1]],5)

## Change the Corpus into a DTM, a DF, and  Matrix
Novels_dtm <- DocumentTermMatrix(NovelsCorpus,
                                 control = list(
                                   stopwords = TRUE, ## remove normal stopwords
                                   wordLengths=c(10, 20), ##  here I tried different parameters:  get rid of words of len; 4~9; 3:15 
                                   removePunctuation = TRUE,
                                   removeNumbers = TRUE,
                                   tolower=TRUE,
                                   remove_separators = TRUE))

str(Novels_dtm)

DTM_mat <- as.matrix(Novels_dtm)
str(DTM_mat)

inspect(Novels_dtm)
Novels_DF <- as.data.frame(DTM_mat)
str(Novels_DF)

Novels_DF[c(1:6),c(1:6)]

# disp data set
disp_df <- Novels_DF[grep('disp', row.names(Novels_DF)), ]
nrow(disp_df)

# labeled data set 
labed_df <- Novels_DF[c(12:77),]
nrow(labed_df)

###################


#######################################
##  Read in and prepare the data.
## WORDCLOUD ##_---------------------------------------
word.freq <- sort(rowSums(t(DTM_mat)), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 2,
          random.order = F, max.words=30)

## Place the LABELS onto the DF---------------------------------
## Recall that the filaments must be converted and added as labels

Temp1<-strsplit(rownames(labed_df),"_")
#Temp1[[5]][1]
MyList<-list()

length(Temp1)

for(i in 1:length(Temp1)){
  MyList[[i]] <- Temp1[[i]][1]
  
}

MyList

## Add MyList to the dataframe
labed_df$LABEL <- unlist(MyList)

NumCol<-ncol(labed_df) ## get the num columns
## Have a look to check for the label - which is at the end

labed_df[1:5, (NumCol-5):NumCol]
labed_df$LABEL

#####################################################

## Make sure that your labels - so LABEL in the text data
## and Decision in the record data are both
## type FACTOR!!
##################################################
str(labed_df$LABEL)
labed_df$LABEL <- as.factor(labed_df$LABEL)
str(labed_df$LABEL)



########################################################
##
##       Decision Trees, etc..
##
#########################################################
## In R - unlike Python - you only need to remove the label
## from the TESTING data but not from the TRAINING data
## There are many ways to create Training and Testing data
##################### CREATE Train/Test for Text data

every3_indexes<-seq(1,nrow(labed_df),3)

NCorpusDF_Test<-labed_df[every3_indexes, ]
NCorpusDF_Train<-labed_df[-every3_indexes, ]

str(NCorpusDF_Train$LABEL)
str(NCorpusDF_Test$LABEL)


## REMOVE the labels from the test data
## -------------------------------
NumCol=ncol(NCorpusDF_Test)
NCorpusDF_TestLabels<-NCorpusDF_Test[NumCol]  ##keeping my label
NCorpusDF_Test<-NCorpusDF_Test[-c(NumCol)] ## removing the label from the test set
NewNumCol<-ncol(NCorpusDF_Test)
NCorpusDF_Test[1:5, (NewNumCol-5):NewNumCol]

##############################################################
##
##            Decision Trees
##
###################################################################
## Text DT
fitText <- rpart(NCorpusDF_Train$LABEL ~ ., data = NCorpusDF_Train,method="class")
summary(fitText)

## or 
### check my train data 
NCorpusDF_Train_D <- NCorpusDF_Train[-c(14:43),]
NCorpusDF_Train_D$LABEL

tree_fit <- rpart::rpart(NCorpusDF_Train_D$LABEL ~ . -alexander - hamilton -madison -james -upon, data = NCorpusDF_Train_D, cp = 0.001)
# tree_fit <- rpart::rpart(NCorpusDF_Train$LABEL ~ . , data = NCorpusDF_Train, cp = 0.001)
tree_fit$cptable
rpart::plotcp(tree_fit)

rpart.plot::rpart.plot(
  tree_fit,
  type = 3,
  extra = 2,
  branch = .75,
  under = TRUE
)

rpart.plot::rpart.rules(tree_fit)



rparty.test <- predict(tree_fit, NCorpusDF_Test)
rparty.test <- rparty.test[, 2]
classifierplots::density_plot(NCorpusDF_Test$LABEL, rparty.test)
##########################
## Predict the Test sets

## Text--------------------------------------
predictedText= predict(tree_fit, NCorpusDF_Test, type="class")
## Confusion Matrix

str(NCorpusDF_TestLabels)
str(predictedText)

# confusion matrix vis
table(unlist(predictedText) ,unlist(NCorpusDF_TestLabels))
ctable <- as.table(matrix(c(15, 4, 2, 1), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
## VIS..................
# fancyRpartPlot(fitText)
#####################################################
## assign the disp papers to the authors

## assign disp to the authors
disp_df[,ncol(disp_df)]
str(disp_df)
########################################
########################################
########################################
disp_df$LABEL <- 'DISP'
str(disp_df$LABEL)
disp_df$LABEL <- as.factor(disp_df$LABEL)

NumCol1=ncol(disp_df)

disp_dfLabels<-disp_df[NumCol1]  ##keeping my label
disp_df<-disp_df[-c(NumCol1)] ## removing the label from the study set
NewNumCol1<-ncol(disp_df)
disp_df[1:5, (NewNumCol1-5):NewNumCol1]

# fitText_assign <- rpart(labed_df$LABEL ~ ., data = disp_df, method="class")
fitText_all <- rpart(labed_df$LABEL ~ .-alexander - hamilton -madison -james -upon, data =labed_df, method="class")
summary(fitText_all)

# fitText_all <- rpart(labed_df$LABEL ~ ., data =labed_df, method="class")

predictedText_assign = predict(fitText_all, disp_df, type="class")
str(predictedText)

print(predictedText_assign)
a <- as.data.frame(predictedText_assign)
table_conf <- table(labed_df$LABEL,predictedText_assign)

tab2 <- table(Predicted =predictedText_assign, Actual = disp_dfLabels )
table(unlist(predictedText_assign) ,unlist(disp_dfLabels))
rpart.plot(tree_fit, extra = 106)

########################################
########################################
########################################
########################################
###--------------End Part 1

#################### Create a new dataset with most frequent 100 words
## word length 4~9
toto <- as.data.frame(colSums(Novels_DF[,c(1:5698)]))
str(toto)
colnames(toto) <- c('WordsCount')
toto$Words <- rownames(toto)
## order by col
tot_order <- toto[order(toto$WordsCount, decreasing = T),]
head(tot_order)
new <- tot_order[c(1:20),]
barplot(new$WordsCount, names.arg = rownames(new), ylab = 'Number of Words', las = 2, cex.names = 1)

newlist <- tot_order$Words[1:100]
newlist

mylist_1 <- append(newlist,'LABEL')

Novels_DF_new <-labed_df[,mylist_1]
str(Novels_DF_new$LABEL)

########################################################
##
##       Decision Trees, etc..
##
#########################################################
## In R - unlike Python - you only need to remove the label
## from the TESTING data but not from the TRAINING data
## There are many ways to create Training and Testing data
##################### CREATE Train/Test for Text data

every3_indexes<-seq(1,nrow(labed_df),3)

NCorpusDF_Test1<-Novels_DF_new[every3_indexes, ]
NCorpusDF_Train1<-Novels_DF_new[-every3_indexes, ]

str(NCorpusDF_Train1$LABEL)
str(NCorpusDF_Test1$LABEL)


## REMOVE the labels from the test data
## -------------------------------
NumCol1=ncol(NCorpusDF_Test1)
NCorpusDF_TestLabels1<-NCorpusDF_Test1[NumCol1]  ##keeping my label
NCorpusDF_Test1<-NCorpusDF_Test1[-c(NumCol1)] ## removing the label from the test set
NewNumCol1<-ncol(NCorpusDF_Test1)
NCorpusDF_Test1[1:5, (NewNumCol1-5):NewNumCol1]

##############################################################
##
##            Decision Trees
##
###################################################################
## Text DT
fitText1 <- rpart(NCorpusDF_Train1$LABEL ~ ., data = NCorpusDF_Train1,method="class")
summary(fitText1)

## or 

tree_fit1 <- rpart::rpart(NCorpusDF_Train1$LABEL ~ ., data = NCorpusDF_Train1)
tree_fit1$cptable
rpart::plotcp(tree_fit1)

rpart.plot::rpart.plot(
  tree_fit1,
  type = 3,
  extra = 2,
  branch = .75,
  under = TRUE
)

rpart.plot::rpart.rules(tree_fit1)



rparty.test <- predict(tree_fit1, NCorpusDF_Test)
rparty.test <- rparty.test[, 2]
classifierplots::density_plot(NCorpusDF_Test$LABEL, rparty.test)
##########################
## Predict the Test sets

## Text--------------------------------------
predictedText= predict(tree_fit1, NCorpusDF_Test1, type="class")
## Confusion Matrix

str(NCorpusDF_TestLabels)
str(predictedText)

# confusion matrix vis
table(unlist(predictedText) ,unlist(NCorpusDF_TestLabels1))
ctable <- as.table(matrix(c(13, 1, 4, 4), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
## VIS..................
# fancyRpartPlot(fitText)
#####################################################
## assign the disp papers to the authors

## assign disp to the authors
disp_df[,ncol(disp_df)]
str(disp_df)
########################################
########################################
########################################
disp_df$LABEL <- 'DISP'
str(disp_df$LABEL)
disp_df$LABEL <- as.factor(disp_df$LABEL)

NumCol1=ncol(disp_df)

disp_dfLabels<-disp_df[NumCol1]  ##keeping my label
disp_df<-disp_df[-c(NumCol1)] ## removing the label from the study set
NewNumCol1<-ncol(disp_df)
disp_df[1:5, (NewNumCol1-5):NewNumCol1]

# fitText_assign <- rpart(labed_df$LABEL ~ ., data = disp_df, method="class")
fitText_all1 <- rpart(Novels_DF_new$LABEL ~ ., data =Novels_DF_new, method="class")
summary(fitText_all)

str(Novels_DF_new)
predictedText_assign1 = predict(fitText_all1, disp_df, type="class")
str(predictedText)

print(predictedText_assign1)
b <- as.data.frame(predictedText_assign1)
#table_conf <- table(labed_df$LABEL,predictedText_assign1)

table(unlist(predictedText_assign1) ,unlist(disp_dfLabels))

#labed_df$alexander
#which(colnames(labed_df) == 'alexander')
