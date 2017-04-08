setwd("~/Documents/Software Engineering/UFG/mestrado/ARP/Aula 3 - LDA,QDA,KNN")
wine <- read.csv("wine.csv",header=TRUE)
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


knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  #empty pred vector
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    class1 = 0              #good & bad variable initialization with 0 value
    class2 = 0
    class3 = 0

    #LOOP-2-looping over train data
    for(j in c(1:nrow(train_data))){

      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))

      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, train_data[j,][[1]])
    }

    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns

    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    #Loop 3: loops over eu and counts classes of wines
    for(k in c(1:nrow(eu))){
      if(eu[1] == 1){
        class1 = class1 + 1
      }
      if(eu[1] == 2){
        class2 = class2 + 1
    }
    if(eu[1]==3){
      class3 = class3 +1 ;
    }
  }

    # Compares the no. of neighbors with class label good or bad
    classesVector <- c(class1,class2,class3)

    maximumValue <- which.max(classesVector)
    pred <-  pred <- c(pred, maximumValue)
  }

  return(pred) #return pred vector

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

predictions <- knn_predict(wine,wine,3)

wine[,15] <- predictions
print(accuracy(wine))


