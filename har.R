load.har <- function() {
  
  setwd("~/Documents/Coursera/Machine Learning")
  
  training.data <<- read.csv("training.csv", header=TRUE, sep=",", na.strings=c("NA","#DIV/0!",""))
  testing.data <<- read.csv("testing.csv", header=TRUE, sep=",", na.strings=c("NA","#DIV/0!",""))
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(RColorBrewer)
  library(rattle)
  library(randomForest)
  
  set.seed(12345)
  split.har()
}

split.har <- function() {
  
  ##Define the Error Rate
  
  ##split data into three different section Training, Testing and Validation
  
  #creating partition to pull out training data
  in.Train <- createDataPartition(y=training.data$classe, p=0.6, list=FALSE)
  
  #this is the training data which subsets inTrain from training data
  my.Training <<- training.data[in.Train, ]
  
  in.Testing <- training.data[-in.Train, ]
  
  in.Testing2 <- createDataPartition(y=in.Testing$classe, p=0.5, list=FALSE)
  
  my.Testing <<- in.Testing[in.Testing2, ]
  
  my.Valid <<- in.Testing[-in.Testing2,]
  
  tidy.har()
  
}


tidy.har <- function() {
  
  
  ##Clean passing data
  
  nzv.data <<- nearZeroVar(my.Training, saveMetrics=TRUE)
  nzv.vars2 <<-row.names(nzv.data[nzv.data$nzv==TRUE,])
  nzv.vars <<- names(my.Training) %in% nzv.vars2
  
  my.Training <<- my.Training[!nzv.vars]
  
  ##removing the first column
  my.Training <<- my.Training[c(-1)]

  
  
  training.V3 <<- my.Training
  for(i in 1:length(my.Training)) {
    
    if(sum(is.na(my.Training[,i]))/nrow(my.Training) >=.6) {
      for(j in 1:length(training.V3)) {
        if(length(grep(names(my.Training[i]),names(training.V3)[j])) ==1) {
         training.V3 <<- training.V3[,-j]
          
          
       }
        
      }
      
    }
    
    
  }
  
  
  my.Training <<- training.V3
 
  
  clean.a <<- colnames(my.Training)
  clean.b <<- colnames(my.Training[,-58])
  
 my.Testing <<- my.Testing[clean.a]
  testing <<- testing.data[clean.b]
 
  
  for (i in 1:length(testing)) {
    for(j in 1:length(my.Training)) {
     if(length(grep(names(my.Training[i]), names(testing)[j]))==1) {
       class(testing[j]) <<-class(my.Training[i])
      }
      
    }  
    
 }
  
  testing <<- rbind(my.Training[2, -58], testing)
  testing <- testing[-1,]
  
 
  train.har()
  
}

train.har <- function() {
  
  
  modFitA1 <- rpart(classe ~ ., data=my.Training, method="class")
  
  fancyRpartPlot(modFitA1)
  
  predictionsA1 <<- predict(modFitA1, my.Testing, type="class")
  
  confusionMatrix(predictionsA1, my.Testing$classe)
  
  modFitB1 <<- randomForest(classe ~ ., data=my.Training)
  
  predictionsB1 <<- predict(modFitB1, my.Testing, type="class")
  
 confusionMatrix(predictionsB1, my.Testing$classe)
  
  predictionsB2 <<- predict(modFitB1, testing, type="class")
 
 
}

ans.files = function(x) {
    n = length(x)
    for(i in 1:n){
     filename = paste0("problem_id_2",i,".txt")
      write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
      
      
    }
    
  }
  


