Chick = ChickWeight  
# *************** KNN CON DATASET ChickWeight ********************

library(class)
library(caret)
#Generar subconjunto de 405 datos para entrenar
chick.train = Chick[ sample( c( 1:578 ), 405 ), 1:4 ]
#Generar subconjunto de  datos para prueba
chick.test = Chick[ sample( c( 1:578 ),173  ), 1:4 ]
# Genera modelo y hace prediccion a la vez
chick.pred = knn( train = chick.train[ , 1:3 ], test = chick.test[ , 1:3 ], 
                 cl = chick.train[ ,4 ], k = 2 )
conf1_chick = confusionMatrix(chick.pred,chick.test[,4])
# Validaciones de Rendimiento 
accuracy1_chick = conf1_chick$overall[1]
precision1_chick = mean(conf1_chick$byClass[,5])
recall1_chick = mean(conf1_chick$byClass[,6])
f11_chick = mean(conf1_chick$byClass[,7])

knnTable_chick = cbind(accuracy1_chick,precision1_chick,recall1_chick,f11_chick)

# *************** RANDOM FOREST CON EL DATASET ChickWeight ********************

indice_chick = sample(2,nrow(Chick),replace=TRUE,prob=c(0.7,0.3))
trainData_chick = Chick[indice_chick==1,]
testData_chick = Chick[indice_chick==2,]
library(randomForest)
Chick_rf = randomForest(Diet~.,data=trainData_chick,ntree=405,proximity=TRUE)
ChickPred = predict(Chick_rf,newdata=testData_chick)
conf2_Chick = confusionMatrix(ChickPred,testData_chick$Diet)
accuracy2_Chick = conf2_Chick$overall[1]
precision2_Chick = mean(conf2_Chick$byClass[,5])
recall2_Chick = mean(conf2_Chick$byClass[,6])
f12_Chick = mean(conf2_Chick$byClass[,7])

forestTable_chick = cbind(accuracy2_Chick,precision2_Chick,recall2_Chick,f12_Chick)

# *************** NAIVE BAYES CON EL DATASET ChickWeight **********************
## Loading required package: klaR
## Loading required package: MASS
library("MASS")
library("caret")
library("naivebayes")
library(tm)
library("e1071")
library(tidyverse)
x_chick = Chick[,-4]
y_chick = Chick$Diet
classifier_Chick = naiveBayes(Chick[,1:3], Chick[,4]) 
predicted.classes_chick = predict(classifier_Chick, Chick[,-4])
conf3_chick = confusionMatrix(predicted.classes_chick, Chick[,4])
accuracy3_chick = conf3_chick$overall[1]
precision3_chick = mean(conf3_chick$byClass[,5])
recall3_chik = mean(conf3_chick$byClass[,6])
f13_chik = mean(conf3_chick$byClass[,7])

bayesTable_chick = cbind(accuracy3_chick,precision3_chick,recall3_chik,f13_chik)

# *************** REGRESION LOGISTICA BINARIA CON EL DATASET ChickWeight ********************

library(MASS)
modelo_chick = lda(Diet~.,data=Chick)
prediccion_chick = predict(modelo_chick,Chick[-4])
m_confusion4_chick = table(Chick$Diet,prediccion_chick$class,dnn=c("Real","Predicciones"))
conf4_chick = confusionMatrix(Chick$Diet,prediccion_chick$class,dnn=c("Real","Predicciones"))
accuracy4_chick = conf4_chick$overall[1]
precision4_chick = mean(conf4_chick$byClass[,5])
recall4_chick = mean(conf4_chick$byClass[,6])
f14_chick = mean(conf4_chick$byClass[,7])

regTable_chick = cbind(accuracy4_chick,precision4_chick,recall4_chick,f14_chick)

# *************** REDES NEURONALES CON EL DATASET ChickWeight **********************
chick.train1 = cbind( iris.train, iris.train$Species == "setosa" )
chick.train = cbind( iris.train, iris.train$Species == "versicolor" )

names(chick.train1)[1]="weight"
names(chick.train1)[2]="Time"
names(chick.train1)[3]="Chick"
names(chick.train1)[6]="Dieta1"
names(chick.train1)[7]="Dieta2"

library(neuralnet)
chick.nnt = neuralnet( Dieta1 + Dieta2  ~ weight +
                        Time +
                        Chick,
                      data = chick.train1, hidden = c( 3, 2 ) )
#plot( chick.nnt, col.intercept = "blue" , rep="best")
pred.nn_chick = compute( chick.nnt, iris.test[ 1:3 ] )
# print(pred.nn) # ejecutar para ver los resultados
resultado_chick = 0
for ( i_chick in 1:dim( pred.nn_chick$net.result )[1] ){
  resultado_chick[i] <- which.max( pred.nn_chick$net.result[ i_chick, ] )
}
resultado_chick[ resultado_chick == 1 ] = "setosa"
resultado_chick[ resultado_chick == 2 ] = "versicolor"

conf5_chick = confusionMatrix(iris$Species,prediccion$class,dnn=c("Real","Predicho"))
accuracy5_chick = conf5_chick$overall[1]
precision5_chick = mean(conf5_chick$byClass[,5])
recall5_chick = mean(conf5_chick$byClass[,6])
f15_chick = mean(conf5_chick$byClass[,7])

redesTable_chick = cbind(accuracy5_chick,precision5_chick,recall5_chick,f15_chick)

# *************** MAQUINA DE SOPORTE VECTORIAL CON EL DATASET ChickWeight ********************

library(e1071)
library(caTools)
dataset_chick = iris
# Random Cross Validation
set.seed(2)
split_chick = sample.split(dataset_chick$Species, SplitRatio = .7)
# Training
training_set_chick = subset(dataset_chick, split_chick == TRUE)
# Test
test_set_chick = subset(dataset_chick, split_chick == FALSE)
# Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
classifier1_chick = svm(formula = Species~., data = training_set_chick, 
                        type = 'C-classification', kernel = 'linear')
# Predicciones del clasificador
test_pred1_chick = predict(classifier1_chick, type = 'response', 
                           newdata = test_set_chick[-5])
cm1 = table(test_set_chick[,5], test_pred1_chick)
library(caret)
conf6_chick = confusionMatrix(test_pred1_chick,test_set_chick[,5])
accuracy6_chick = conf6_chick$overall[1]
precision6_chick = mean(conf6_chick$byClass[,5])
recall6_chick = mean(conf6_chick$byClass[,6])
f16_chick = mean(conf6_chick$byClass[,7])

smvTable_chick = cbind(accuracy6_chick,precision6_chick,recall6_chick,f16_chick)

# ****************** GRAFICAS DE COMPARACION DATASET ChickWeight **********************

# Accuracy
knnAccurracy_chick = c(accuracy1_chick)
forestAccurracy_chick = c(accuracy2_Chick)
bayesAccurracy_chick = c(accuracy3_chick)
regAccurracy_chick = c(accuracy4_chick)
redesAccurracy_chick = c(accuracy5_chick)
svmAccurracy_chick = c(accuracy6_chick)
accuracyTable_chick = cbind(knnAccurracy_chick,forestAccurracy_chick,bayesAccurracy_chick,regAccurracy_chick,redesAccurracy_chick,svmAccurracy_chick)

# Precision
knnPrecision_chick = c(precision1_chick)
forestPrecision_chick = c(precision2_Chick)
bayesPrecision_chick = c(precision3_chick)
regPrecision_chick = c(precision4_chick)
redesPrecision_chick = c(precision5_chick)
svmPrecision_chick = c(precision6_chick)
precisionTable_chick = cbind(knnPrecision_chick,forestPrecision_chick,bayesPrecision_chick,regPrecision_chick,redesPrecision_chick,svmPrecision_chick)

# Recall
knnRecall_chick = c(recall1_chick)
forestRecall_chick= c(recall2_Chick)
bayesRecall_chick = c(recall3_chik)
regRecall_chick = c(recall4_chick)
redesRecall_chick = c(recall5_chick)
svmRecall_chick = c(recall6_chick)
recallTable_chick = cbind(knnRecall_chick,forestRecall_chick,bayesRecall_chick,regRecall_chick,redesRecall_chick,svmRecall_chick)

# F-Measure
knnF1_chick = c(f11_chick)
forestF1_chick = c(f12_Chick)
bayesF1_chick = c(f13_chik)
regF1_chick = c(f14_chick)
redesF1_chick = c(f15_chick)
svmF1_chick = c(f16_chick)
f1Table_chick = cbind(knnF1_chick,forestF1_chick,bayesF1_chick,regF1_chick,redesF1_chick,svmF1_chick)
