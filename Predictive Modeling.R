EnsurePackage<-function(x)
{ # EnsurePackage(x) - Installs and loads a package
  # if necessary
  x <- as.character(x)
  if (!require(x, character.only=TRUE))
  {
    install.packages(pkgs=x,
                     repos="http://cran.r-project.org")
  }
  require(x, character.only=TRUE)
  
}

#Installs and loads all packages necessary

Prepare<-function(){
  
  EnsurePackage("glmnet")
  EnsurePackage("boot")
  EnsurePackage("MASS")
  EnsurePackage("ISLR")
  EnsurePackage("class")
  EnsurePackage("ElemStatLearn")
  EnsurePackage("pls")
  
}

Prepare()
wd<-getwd();
test<-paste(wd,"test.csv",sep="/");train<-paste(wd,"train.csv",sep="/")
test<-read.csv(test,header = T);train<-read.csv(train,header = T)



CV.logistic<-
  function (data, glmfit, yname, K, seed=321) {
    
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      j.out <- seq_len(n)[(s == i)] #test data
      j.in <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      log.fit=glm(glmfit$call, data=data[j.in,],family = 'binomial')
      #observed test set y
      testy <- datay[j.out]
      #predicted test set y
      log.predy=predict(log.fit, data[j.out,],type='response')
      
      tname=rownames(contrasts(datay))
      class = rep(tname[1], nrow(data[j.out,]))
      class[log.predy > 0.5] = tname[2]
      
      #observed - predicted on test data
      error= mean(testy!=class)
      #error rates 
      CV=c(CV,mean(error))
    }
    
    #Output
    list(call = glmfit$call, K = K, error=mean(CV),
         log_error_rate = paste(100*mean(CV), "%"), seed = seed)  
    
  }

glmfit1=glm(V58~., data=train, family=binomial)
er_log=CV.logistic(data=train,glmfit=glmfit1, yname="V58", K=10, seed=123)
er_log$error



glmfit2$anova;summary(glmfit2,cor=F)$coef



mycv.stepAIC.logistic<-
  function (data, glmfit, K=10, seed=123) {
    #logistic regression
    #this function is to get the Cross-validated mean square error for regression
    #output R2 and MSE
    library(MASS)
    n <- nrow(data)
    set.seed(seed) #K=10
    
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated mean squared error
    
    CV=NULL; O.P=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      glm.fit <- glm(glmfit$call, data = data[train.index,], family=binomial)
      glm.fit1 <- stepAIC(glm.fit, trace = F)
      #observed test set y
      test.y <- glmfit$y[test.index]
      
      #predicted probability for test data
      pred.y <- predict(glm.fit1, newdata=data[test.index,],type="response")
      
      #change prediction probability to class prediction
      tname=names(table(glmfit$y))
      ypred=ifelse(pred.y>.5,tname[2],tname[1])
      
      #
      error=mean(ypred!=test.y) #classification error 
      ovsp <- cbind(pred=ypred,obs=test.y) #pred vs obs vector
      
      
      CV <- c(CV,error) 
      O.P <- rbind(O.P,ovsp)
    }
    
    #Output
    list(call = glmfit$call, K = K, 
         error = mean(CV), ConfusianMatrix=table(O.P[,1],O.P[,2]), 
         seed = seed)  
    
  }
er_log1=mycv.stepAIC.logistic(data=train,glmfit=glmfit1, K=10, seed=123)
er_log1$error



predict1<-predict(glmfit1,newdata=test,type = "response")
predict1<-ifelse(predict1>0.5,"spam","email")
table(predict1,test$V58)
mean(predict1!=test$V58)
predict2<-predict(glmfit2,newdata = test,type = "response")
predict2<-ifelse(predict2>0.5,"spam","email")
table(predict2,test$V58)
mean(predict2!=test$V58)





cv.knn <- function(dataY, dataX, kn=3, K=10, seed=123) { #we are trying to figure out the best k # of k. K is how many folds
  n <- nrow(dataX)
  set.seed(123) 
  library(class)
  
  f <- ceiling(n/K)
  s <- sample(rep(1:K, f), n)
  dataX=scale(dataX)
  CV=NULL;PvsO=NULL
  
  for (i in 1:K){
    test.index <- seq_len(n)[(s==i)] #test data
    train.index <- seq_len(n)[(s != i)]
    train.x <- dataX[train.index,]
    test.x<- dataX[test.index,]
    train.y <- dataY[train.index]
    test.y<- dataY[test.index]
    
    #predicted test set y
    knn.pred=knn(train.x, test.x, train.y, k=kn)
    #observed - predicted on test data
    error=mean(knn.pred!=test.y)
    #error rates
    CV=c(CV,mean(error))
    predvsobs=data.frame(knn.pred,test.y)
    PvsO =rbind(PvsO, predvsobs)
    
  }
  
  #output
  list(k = K, error_rate = mean(CV), confusion=table(PvsO[,1],PvsO[,2]), seed=seed)
}
kerror=NULL
for(i in 1:200){
  kp=cv.knn(dataY = train$V58, dataX = train[,-58], kn=i, K=10, seed = 123)
  kerror[i]=kp$error_rate #vector
}



#corresponds to the lowest min error rate.
K=which(kerror == min(kerror))
#best k for knn
K 
print(min(kerror))



kp2=cv.knn(dataY = test$V58, dataX = test[,-58], kn=1, K=10, seed = 123)
kerror=kp2$error_rate
print(min(kerror))



cv.lda<-
  function (data, model=V58~., yname="V58", K=10, seed=123) {
    #model is lda model
    #yname: name of response variable
    #K: number of partition for cv #K=10
    #seed: random seed number
    
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      lda.fit=lda(model, data=data[train.index,])
      #observed test set y
      lda.y <- data[test.index, yname]
      #predicted test set y
      lda.predy=predict(lda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(lda.y!=lda.predy)
      #error rates 
      CV=c(CV,error)
    }
    
    #Output
    list(call = model, K = K, 
         error = mean(CV), seed = seed)  
    
  }
er_lda=cv.lda(data=train,model=V58~., yname="V58", K=10, seed=123)
er_lda$error


cv.qda<-
  function (data, model=V58~., yname="V58", K=10, seed=123) {
    #model is qda model
    #yname: name of response variable
    #K: number of partition for cv #K=10
    #seed: random seed number
    
    n <- nrow(data)
    set.seed(seed)
    datay=data[,yname] #response variable
    library(MASS)
    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    #generate indices 1:10 and sample n of them  
    # K fold cross-validated error
    
    CV=NULL
    
    for (i in 1:K) { #i=1
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data
      
      #model with training data
      qda.fit=qda(model, data=data[train.index,])
      #observed test set y
      qda.y <- data[test.index, yname]
      #predicted test set y
      qda.predy=predict(qda.fit, data[test.index,])$class
      
      #observed - predicted on test data
      error= mean(qda.y!=qda.predy)
      #error rates 
      CV=c(CV,error)
    }
    
    #Output
    list(call = model, K = K, 
         error = mean(CV), seed = seed)  
    
  }

er_qda=cv.qda(data=train,model=V58~.-V32-V41-V31, yname="V58", K=10, seed=123)
er_qda$error


er_lda1=cv.lda(data=test,model=V58~., yname="V58", K=10, seed=123)
er_lda1$error
er_qda1=cv.qda(data=test,model=V58~.-V32-V41-V31, yname="V58", K=10, seed=123)
er_qda1$error
