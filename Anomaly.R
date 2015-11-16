anomaly<-function(){
  
  ## 1. The following script implements a novelty detection algorithm via a One class
  ##    SVM classification implementation and the past distribution of features. 
  
  ## 2. Line descriptions have been converted to specific topics through latent dirichlet allocation.
  ##    Missing descriptions were allocated a separate topic. The intuition behind topic modelling is
  ##    that detailed textual descriptions would lead to a better model as they introduce more data in the model
  
  ## 3. The final anomalous flags are created using both the SVM flags and the distribution flags
  
  ## 4. As a measure of test case detection confidence, the distance of each datapoint in test case is
  ##    computed from the SVM hyperplan and then appropriately scaled using a sigmoid function for
  ##    better representation
  
  ## 5. The confidence values should be interpreted as: values closer to 0.5 indicate high confidence,
  ##    in other words closser to the SVM hyperplan. Whereas values near 0 or 1 represents low confidence
  
  ## 6  The script takes in the following inputs (which at the moment have been hard-coded):-
  ##        1. Unlcean Dataset
  ##        2. The Kernel function type for one class SVM classification
  ##        3. The median check interval factor (median - mediandev*absolute deviation)
  
  ## 7. The script outputs:-
  ##        1. The test dataset with the 'Final flag' column indicating the anomalous rows
  
  ## Script Inputs
  data=read.csv(file.choose())
  kernel='rbfdot'
  mediandev=2
  
  ## Loading necessary packages
  library(tm)
  library(topicmodels)
  library(RTextTools)
  library(muStat)
  library(plyr)
  library(e1071)
  library(caret)
  library(pracma)
  library(kernlab)
  
  ## Initial cleaning - correcting data types and assigning missing values
  data<-data.frame(lapply(data,as.character),stringsAsFactors = FALSE)
  data[data==""]=NA
  data<-data.frame(lapply(data,as.factor))
  data[,13]=gsub(",","",data[,13])
  data[,13]=as.numeric(as.character(data[,13]))
  data$Period=as.character(data$Period)
  data$Line.Description=as.character(data$Line.Description)
  data$Line.Description[which.na(data$Line.Description)]="No description"
  
  ## Removing Location code - redundant column
  data<-data[,-2]
  
  ## Classifying line descriptions in topics
  descriptions<-as.data.frame(as.character(unique(data[,7])),stringsAsFactors = FALSE)
  index<-which(descriptions[,1]=="No description")
  descriptions<-descriptions[-index,1]
  
  matrix<-create_matrix(descriptions,language="english",
                        removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
  topics<-15 ## setting the number of topics
  lda<-LDA(matrix,topics)
  topics<-as.data.frame(topics(lda))
  descriptions<-cbind(descriptions,topics)
  names(descriptions)<-c("Line.Description","Topic")
  descriptions <- data.frame(lapply(descriptions, as.character), stringsAsFactors=FALSE)
  descriptions$Topic<-as.numeric(descriptions$Topic)
  data<-join(data,descriptions)
  data$Topic[which.na(data$Topic)]=0
  data$Line.Description<-NULL
  names(data)[ncol(data)]<-"Description Category"
  data$Account.Code<-NULL
  data[,ncol(data)]<-as.factor(data[,ncol(data)])
  
  ## Splitting the dataset and manupilations
  data.train<-data[data$Period!="8/1/14"& data$Period!="9/1/14",]
  data.train$Period<-NULL
  data.train<-data.train[-c(which.na(data.train[,9])),]
  data.test<-data[data$Period=="8/1/14"| data$Period=="9/1/14",]
  data.test$Period<-NULL
  data.test<-data.test[-c(which.na(data.test[,9])),]
  
  ## Selecting features manually
  data.train<-cbind(data.train[,c(1,2,3,4,9,10)],as.data.frame(rep("TRUE",nrow(data.train))))
  names(data.train)[7]<-"Flag"
  data.test<-data.test[,c(1,2,3,4,9,10)]
  
  ## Creating a one-class SVM model
  model<-ksvm(Flag~.,data=data.train,kernel=kernel,type="one-svc",nu=0.1)
  train.pred<-predict(model,data.train[,-7])
  accuracy.train<-sum(train.pred)*100/nrow(data.train)    ## Training accuracy
  test.label<-predict(model,data.test)
  anomaly<-(nrow(test.label)-sum(test.label))*100/nrow(test.label)  ## %age of anomalies being flagged of total test data
  
  ## Confidence level of the prediction based on the distance of the test points from the SVM
  ## dividing hyperplane. Followed by conversion to a sigmoid scale.
  test.confidence<-predict(model,data.test,type="decision")
  test.confidence<-sigmoid(test.confidence,a=1,b=0)
  
  ## Determining anomalous rows based on prior distributions
  data.test[,7]<-FALSE
  data.test[,c(8,9)]<-0
  names(data.test)[7:9]<-c("Distribution Flag","Upper","Lower")
  
  for (i in 1:nrow(data.test)){
    median.val<-median(data.train[(data.train[,1]==data.test[i,1] & 
                                     data.train[,2]==data.test[i,2] & 
                                     data.train[,3]==data.test[i,3] & 
                                     data.train[,4]==data.test[i,4] & 
                                     data.train[,6]==data.test[i,6]),5])
    mad.val<-mad(data.train[(data.train[,1]==data.test[i,1] & 
                               data.train[,2]==data.test[i,2] & 
                               data.train[,3]==data.test[i,3] & 
                               data.train[,4]==data.test[i,4] & 
                               data.train[,6]==data.test[i,6]),5])
    upper<-median.val+mediandev*mad.val
    lower<-median.val-mediandev*mad.val
    data.test[i,8]<-upper
    data.test[i,9]<-lower
    if (is.na(data.test[i,5]<=upper & data.test[i,5]>=lower)==FALSE){
      if(data.test[i,5]<=upper & data.test[i,5]>=lower){
        data.test[i,7]=TRUE
      }
    }
    ## Iteration tracking
    Sys.sleep(0.005)
    cat(paste(i,"-",sep=""))
  }

 ## Binding SVM results
 data.test<-cbind(data.test,test.label,test.confidence)
 names(data.test)[10:11]<-c("SVM Flag","Confidence")
 
 ## Creating final anomalous flags based on FALSE flags generated by both the methods
 data.test[,12]<-TRUE
 data.test[(data.test$`SVM Flag`==FALSE & data.test$`Distribution Flag`==FALSE),12]<-FALSE
 names(data.test)[12]<-"Final Flag"
 
 ## Output Test dataset to the console
 data.test
 
 
}