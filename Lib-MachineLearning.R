## Intialize ML Structure
ML.TrainSet<-vector()
ML.TestSet<-vector()
ML.CrossValidateTrain<-vector()
ML.CrossValidateTest<-vector()

LoadDataFromWeb <- function() {
  CachedWebGet("http://d396qusza40orc.cloudfront.net/predmachlearn/", "pml-training.csv", "ML.TrainSet")
  CachedWebGet("http://d396qusza40orc.cloudfront.net/predmachlearn/", "pml-testing.csv", "ML.TestSet")
  
  cleanRawData()
  
  # Partition Validation Data... using 60% of Train as Cross Train and 40% as CrossTest
  inTrain <- createDataPartition(y=ML.TrainSet$classe, p=0.6, list=FALSE)
  assign("ML.CrossValidateTrain", ML.TrainSet[inTrain, ], envir = .GlobalEnv)
  assign("ML.CrossValidateTest", ML.TrainSet[-inTrain, ], envir = .GlobalEnv)
  # ML.CrossValidateTrain <- ML.TrainSet[inTrain, ]; 
  # ML.CrossValidateTest <- ML.TrainSet[-inTrain, ]
  print(paste("ML.Train", nrow(ML.TrainSet), length(ML.TrainSet), sep = " "))
  print(paste("ML.Test", nrow(ML.TestSet), length(ML.TestSet), sep = " "))
  print(paste("ML.CVTrain", nrow(ML.CrossValidateTrain), length(ML.CrossValidateTrain), sep = " "))
  print(paste("ML.CVTest", nrow(ML.CrossValidateTest), length(ML.CrossValidateTest), sep = " "))
}

cleanRawData <- function() {
  print("Original Columns")
  trainData <- get("ML.TrainSet")
  dim(trainData)
  #str(trainData)
  #names(trainData)
  
  print("Removing Columns with greather than 45% NA Values")
  trainDataDT <- data.table(trainData)
  setkey(trainDataDT,V1)
  print("Convert INF and NAN to NA")
  for (j in 1:ncol(trainDataDT)) set(trainDataDT, which(is.infinite(trainDataDT[[j]])), j, NA)
  for (j in 1:ncol(trainDataDT)) set(trainDataDT, which(is.nan(trainDataDT[[j]])), j, NA)
  goodCols <- names(which(colMeans(is.na(trainData)) <= .45 & !(names(trainData) %in% c("V1", "raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp")) ))
  finalData <-trainDataDT[,goodCols, with = FALSE]
  
  #finalData$classe <- factor(finalData$classe) #convert indicator into a factor 
  
  assign("ML.TrainSet",finalData, envir = globalenv())
  print("Final  Columns")
  dim(finalData)
  
  
}




executeML_DecisionTree <- function() {
  ### Decision Tree ###
  registerDoMC(parallel:::detectCores()) #number of cores on the machine
  #Rprof("rpartProfile.Rprof")
    modelDecisionTree <- rpart(classe ~ ., data=ML.CrossValidateTrain , method="class")
    fancyRpartPlot(modelDecisionTree)
    predictDecisionTree <- predict(modelDecisionTree, ML.CrossValidateTest, type = "class")
    print(confusionMatrix(predictDecisionTree, ML.CrossValidateTest$classe))
  #Rprof()
  ##summaryRprof("rpartProfile.Rprof")
  #summaryRprof("rpartProfile.Rprof")$by.self
  #summaryRprof("rpartProfile.Rprof")$by.total
}
executeML_RandomForest <- function() {
  ### Random Forest ###
  print("Verify all data is clean and prepped, NAs are gone, etc.")
  #str(ML.CrossValidateTrain)
  
  print("Understand the Predictor Distribution")
  print(table(ML.CrossValidateTrain$classe)*100/nrow(ML.CrossValidateTrain))
  
  print("Confirm Classification Type of Target as Factor and add Numeric Version")
  ML.CrossValidateTrain$classe <- as.factor(ML.CrossValidateTrain$classe)
  print(class(ML.CrossValidateTrain$classe))
  
  ML.CrossValidateTrain$classeNum <- as.numeric(ML.CrossValidateTrain$classe)
  class(ML.CrossValidateTrain$classeNum)
  
  ML.CrossValidateTest$classe <- factor(ML.CrossValidateTest$classe, levels=levels(ML.CrossValidateTrain$classe))
  class(ML.CrossValidateTest$classe)
  
  ML.CrossValidateTest$classeNum <- as.numeric(ML.CrossValidateTest$classe)
  class(ML.CrossValidateTest$classeNum)
  
  
  print("Build Custom Formula to prevent '.' expansion overhead")  
  abc <- data.frame(ML.CrossValidateTest)
  ML.CrossValidateTest <- abc[ , !(names(abc) %in% c("user_name", "new_window"))]
  print("Dropped Character Values, they are not predictive")  
  print("Check for any Non-Numberic Values in data frame")  
  which(is.na(as.numeric(as.character(ML.CrossValidateTest[[1]]))))
  
  varNames <- names(ML.CrossValidateTrain)
  # Exclude ID or Response variable
  varNames <- varNames[!varNames %in% c("classe", "classeNum", "user_name", "new_window")] #Remove Predictor, and Char Values
  
  # add + sign between exploratory variables
  varNames1 <- paste(varNames, collapse = "+")
  #varNames1 <- paste(head(varNames, 1), collapse = "+")
  
  # Add response variable and convert to a formula object
  RFformula <- as.formula(paste("classe", varNames1, sep = " ~ "))
  print(RFformula)
  
  print("Build Random Forest Model and Evaluate it..")  
  
  modelRandomForest <- randomForest( RFformula, data=ML.CrossValidateTrain)
  
  predictRandomForest1 <- predict(modelRandomForest, ML.CrossValidateTest, type = "class")
  
  table(ML.CrossValidateTest$classe)*100/nrow(ML.CrossValidateTest)
  #str(ML.CrossValidateTest)
  
  levels(predictRandomForest1)
  levels(ML.CrossValidateTrain$classe)
  print(confusionMatrix(predictRandomForest1, ML.CrossValidateTest$classe))
  
  predictRandomForest2 <- predict(modelRandomForest, ML.TestSet, type = "class")
  levels(predictRandomForest2)
  n = length(predictRandomForest2)
  predictRandomForest2[1]
  write.table(predictRandomForest2,file=paste0("RandomForestNumber_Summary.txt"),quote=FALSE,row.names=TRUE,col.names=TRUE)
  for(i in 1:n){
    filename = paste0("RandomForestNumber_",i,".txt")
    write.table(predictRandomForest2[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

unique.count = function(train.data, all.numeric=FALSE) {                                                                                                                                                                                                 
  # first convert each row in the data.frame to a string                                                                                                                                                                              
  train.data.str = apply(train.data, 1, function(x) paste(x, collapse=',', sep = ""))                                                                                                                                                           
  # use table to index and count the strings                                                                                                                                                                                          
  train.data.str.t = table(train.data.str)                                                                                                                                                                                            
  # get the unique data string from the row.names                                                                                                                                                                                     
  train.data.str.uniq = row.names(train.data.str.t)                                                                                                                                                                                   
  weight = as.numeric(train.data.str.t)                                                                                                                                                                                               
  # convert the unique data string to data.frame
  if (all.numeric) {
    train.data.uniq = as.data.frame(t(apply(cbind(train.data.str.uniq), 1, 
                                            function(x) as.numeric(unlist(strsplit(x, split=","))))))                                                                                                    
  } else {
    train.data.uniq = as.data.frame(t(apply(cbind(train.data.str.uniq), 1, 
                                            function(x) unlist(strsplit(x, split=",")))))                                                                                                    
  }
  names(train.data.uniq) = names(train.data)                                                                                                                                                                                          
  list(data=train.data.uniq, weight=weight)                                                                                                                                                                                           
}  

ClearScreen <- function() {
  cat("\014")  
}
asPct <- function(n) {
  paste(round(100*n, 2), "%", sep="")
}
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}

#as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}