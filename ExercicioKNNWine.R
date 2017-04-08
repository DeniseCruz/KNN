setwd("~/Documents/Software Engineering/UFG/mestrado/ARP/Aula 3 - LDA,QDA,KNN/knn")
wine <- read.csv("wine.csv",header=FALSE)
names(wine) <- c("Class", "Alcohol","Malic acid","Ash", "Alcalinity of ash" ,"Magnesium","Total phenols","Flavanoids" ,"Nonflavanoid phenols" ,"Proanthocyanins" ,"Color intensity",
"Hue" ,"OD280/OD315 of diluted wines" ,"Proline" )

#Funcoes
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}


predict <- function(test_data, train_data, k_value){
  pred <- c()
  for(i in c(1:nrow(test_data))){
    distVector = NULL
    classesVector = NULL
    class1 = 0
    class2 = 0
    class3 = 0

    for(j in c(1:nrow(train_data))){
      distVector <- c(distVector, euclideanDist(test_data[i,], train_data[j,]))
      classesVector <- c(classesVector, train_data[j,][[1]])
    }

    neighborsVector <- data.frame(classesVector, distVector)
    neighborsVector <- neighborsVector[order(neighborsVector$distVector),]

    neighborsVector <- neighborsVector[1:k_value,]
    for(k in c(1:k_value)){

      if(neighborsVector$classesVector[k] == 1){
        class1 = class1 + 1
      }
      if(neighborsVector$classesVector[k] == 2){
        class2 = class2 + 1
     }
       if(neighborsVector$classesVector[k]==3){
      class3 = class3 +1 ;
    }
  }
    VectorMaximum <- c(class1,class2,class3)

    maximumValue <- which.max(VectorMaximum)
    pred <-  pred <- c(pred, maximumValue)
    print(maximumValue)
    print("---")
  }

  return(pred)

}

accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,15] == test_data[i,1]){
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100
  return(accu)
}

predictions <- predict(wine,wine,3)

wine[,15] <- predictions
print(accuracy(wine))


