# ***************** PROYECTO ******************************
# INTEGRANTES:
# Cueva Amarilis
# Hernandez Edwin
# Navas Jefry
# Vizcaino Alisson
# Yunga Joel


# *************** KNN CON DATASET IRIS ********************

library(class)
library(caret)
# Cargar el Dataset Iris
#Generar subconjunto de 100 datos para entrenar
iris.train = iris[ sample( c( 1:150 ), 100 ), 1:5 ]
#Generar subconjunto de 50 datos para prueba
iris.test = iris[ sample( c( 1:150 ), 50 ), 1:5 ]
# Genera modelo y hace prediccion a la vez
iris.pred = knn( train = iris.train[ , 1:4 ], test = iris.test[ , 1:4 ], 
                  cl = iris.train[ ,5 ], k = 2 )
# Matriz de Confusion
conf1 = confusionMatrix(iris.pred,iris.test[,5])
# Validaciones de Rendimiento 
accuracy1 = conf1$overall[1]
precision1 = mean(conf1$byClass[,5])
recall1 = mean(conf1$byClass[,6])
f11 = mean(conf1$byClass[,7])
#Curva Roc
library(pROC)

roc1 <- multiclass.roc(iris.test,
            iris.pred, percent=TRUE,levels = 3,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)

knntable = cbind(accuracy1,precision1,recall1,f11)


# *************** RANDOM FOREST CON EL DATASET IRIS ********************
indice = sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainData = iris[indice==1,]
testData = iris[indice==2,]
library(randomForest)
iris_rf = randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
## Prueba con datos de prueba 
irisPred = predict(iris_rf,newdata=testData)
conf2 = confusionMatrix(irisPred,testData$Species)
accuracy2 = conf2$overall[1]
precision2 = mean(conf2$byClass[,5])
recall2 = mean(conf2$byClass[,6])
f12 = mean(conf2$byClass[,7])

roc2 <- multiclass.roc(testData[,5],
                       testData$Petal.Width, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 90), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE, show.thres=TRUE)

forestTable = cbind(accuracy2,precision2,recall2,f12)

# *************** NAIVE BAYES CON EL DATASET IRIS **********************
## Loading required package: klaR
## Loading required package: MASS
library("MASS")
library("caret")
library("naivebayes")
library(tm)
library("e1071")
library(tidyverse)
x = iris[,-5]
y = iris$Species
classifier = naiveBayes(iris[,1:4], iris[,5]) 
predicted.classes = predict(classifier, iris[,-5])
conf3 = confusionMatrix(predicted.classes, iris[,5])
accuracy3 = conf3$overall[1]
precision3 = mean(conf3$byClass[,5])
recall3 = mean(conf3$byClass[,6])
f13 = mean(conf3$byClass[,7])

roc3 <- multiclass.roc(iris$Species,
                       iris$Petal.Width, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 90), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE, show.thres=TRUE)

bayesTable = cbind(accuracy3,precision3,recall3,f13)

# *************** REGRESION LOGISTICA BINARIA CON EL DATASET IRIS ********************

library(MASS)
modelo = lda(Species~.,data=iris)
prediccion = predict(modelo,iris[-5])
conf4 = confusionMatrix(iris$Species,prediccion$class,dnn=c("Real","Predicciones"))
accuracy4 = conf4$overall[1]
precision4 = mean(conf4$byClass[,5])
recall4 = mean(conf4$byClass[,6])
f14 = mean(conf4$byClass[,7])

regTable = cbind(accuracy4,precision4,recall4,f14)


# *************** REDES NEURONALES CON EL DATASET IRIS **********************

iris.train = cbind( iris.train, iris.train$Species == "setosa" )
iris.train = cbind( iris.train, iris.train$Species == "versicolor" )
iris.train = cbind( iris.train, iris.train$Species == "virginica" )

names(iris.train)[6]="setosa"
names(iris.train)[7]="versicolor"
names(iris.train)[8]="virginica"
library(neuralnet)
iris.nnt = neuralnet( setosa + versicolor + virginica ~ Sepal.Length +
                         Sepal.Width +
                         Petal.Length +
                         Petal.Width, 
                       data = iris.train, hidden = c( 3, 2 ) )
#plot( iris.nnt, col.intercept = "blue" , rep="best")
pred.nn = compute( iris.nnt, iris.test[ 1:4 ] )
# print(pred.nn) # ejecutar para ver los resultados
resultado = 0
for ( i in 1:dim( pred.nn$net.result )[1] ){
  resultado[i] <- which.max( pred.nn$net.result[ i, ] )
}
resultado[ resultado == 1 ] = "setosa"
resultado[ resultado == 2 ] = "versicolor"
resultado[ resultado == 3 ] = "virginica"

conf5 = confusionMatrix(iris$Species,prediccion$class,dnn=c("Real","Predicho"))
accuracy5 = conf5$overall[1]
precision5 = mean(conf5$byClass[,5])
recall5 = mean(conf5$byClass[,6])
f15 = mean(conf5$byClass[,7])

redesTable = cbind(accuracy5,precision5,recall5,f15)
# *************** MAQUINA DE SOPORTE VECTORIAL CON EL DATASET IRIS ********************

library(e1071)
library(caTools)
dataset = iris
# Random Cross Validation
set.seed(2)
split = sample.split(dataset$Species, SplitRatio = .7)
summary(split)
# Training
training_set = subset(dataset, split == TRUE)
# Test
test_set = subset(dataset, split == FALSE)
# Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
classifier1 = svm(formula = Species~., data = training_set, 
                  type = 'C-classification', kernel = 'linear')
# Predicciones del clasificador
test_pred1 = predict(classifier1, type = 'response', 
                     newdata = test_set[-5])
library(caret)
conf6 = confusionMatrix(test_pred1,test_set[,5])
accuracy6 = conf6$overall[1]
precision6 = mean(conf6$byClass[,5])
recall6 = mean(conf6$byClass[,6])
f16 = mean(conf6$byClass[,7])

smvTable = cbind(accuracy6,precision6,recall6,f16)

# ****************** GRAFICAS DE COMPARACION DATASET IRIS **********************

# Accuracy
knnAccurracy = c(accuracy1)
forestAccurracy = c(accuracy2)
bayesAccurracy = c(accuracy3)
regAccurracy = c(accuracy4)
redesAccurracy = c(accuracy5)
svmAccurracy = c(accuracy6)
accuracyTable = cbind(knnAccurracy,forestAccurracy,bayesAccurracy,regAccurracy,redesAccurracy,svmAccurracy)

# Precision
knnPrecision = c(precision1)
forestPrecision = c(precision2)
bayesPrecision = c(precision3)
regPrecision = c(precision4)
redesPrecision = c(precision5)
svmPrecision = c(precision6)
precisionTable = cbind(knnPrecision,forestPrecision,bayesPrecision,regPrecision,redesPrecision,svmPrecision)

# Recall
knnRecall = c(recall1)
forestRecall = c(recall2)
bayesRecall = c(recall3)
regRecall = c(recall4)
redesRecall = c(recall5)
svmRecall = c(recall6)
recallTable = cbind(knnRecall,forestRecall,bayesRecall,regRecall,redesRecall,svmRecall)

# F-Measure
knnF1 = c(f11)
forestF1 = c(f12)
bayesF1 = c(f13)
regF1 = c(f14)
redesF1 = c(f15)
svmF1 = c(f16)
f1Table = cbind(knnF1,forestF1,bayesF1,regF1,redesF1,svmF1)