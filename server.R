#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
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
    library(gridExtra)
    
    knntable = cbind(accuracy1,precision1,recall1,f11)
    
    # Curva ROC
    roc1_knn <- multiclass.roc(iris.test[,5],
                               iris.test$Sepal.Length, percent=TRUE,
                               # arguments for auc
                               partial.auc=c(100, 60), partial.auc.correct=TRUE,
                               partial.auc.focus="sens",
                               # arguments for ci
                               boot.n=100, ci.alpha=0.9, stratified=FALSE,
                               # arguments for plot
                               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                               print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc2_forest <- multiclass.roc(testData[,5],
                                  testData$Sepal.Length, percent=TRUE,
                                  # arguments for auc
                                  partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                  partial.auc.focus="sens",
                                  # arguments for ci
                                  boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                  # arguments for plot
                                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                  print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc3_bayes <- multiclass.roc(y,
                                 x$Sepal.Length, percent=TRUE,
                                 # arguments for auc
                                 partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                 partial.auc.focus="sens",
                                 # arguments for ci
                                 boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                 # arguments for plot
                                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                 print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc4_reg <- multiclass.roc(iris$Species,
                               iris$Sepal.Width, percent=TRUE,
                               # arguments for auc
                               partial.auc=c(100, 60), partial.auc.correct=TRUE,
                               partial.auc.focus="sens",
                               # arguments for ci
                               boot.n=100, ci.alpha=0.9, stratified=FALSE,
                               # arguments for plot
                               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                               print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc5_redes <- multiclass.roc(iris.test[,5],
                                 iris.test$Petal.Length, percent=TRUE,
                                 # arguments for auc
                                 partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                 partial.auc.focus="sens",
                                 # arguments for ci
                                 boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                 # arguments for plot
                                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                 print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc6_svm <- multiclass.roc(test_set[,5],
                               test_set$Sepal.Length, percent=TRUE,
                               # arguments for auc
                               partial.auc=c(100, 60), partial.auc.correct=TRUE,
                               partial.auc.focus="sens",
                               # arguments for ci
                               boot.n=100, ci.alpha=0.9, stratified=FALSE,
                               # arguments for plot
                               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                               print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc1_knn_chick <- multiclass.roc(chick.test[,4],
                                     chick.test$weight, percent=TRUE,
                                     # arguments for auc
                                     partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                     partial.auc.focus="sens",
                                     # arguments for ci
                                     boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                     # arguments for plot
                                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                     print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc2_fores_chick <- multiclass.roc(testData_chick[,4],
                                       testData_chick$weight, percent=TRUE,
                                       # arguments for auc
                                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                       partial.auc.focus="sens",
                                       # arguments for ci
                                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                       # arguments for plot
                                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                       print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc3_bayes_chick <- multiclass.roc(y_chick,
                                       x_chick$weight, percent=TRUE,
                                       # arguments for auc
                                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                       partial.auc.focus="sens",
                                       # arguments for ci
                                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                       # arguments for plot
                                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                       print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc4_reg_chick <- multiclass.roc(Chick$Diet,
                                     Chick$weight, percent=TRUE,
                                     # arguments for auc
                                     partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                     partial.auc.focus="sens",
                                     # arguments for ci
                                     boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                     # arguments for plot
                                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                     print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc5_redes_chick <- multiclass.roc(Chick$Diet,
                                       Chick$Time, percent=TRUE,
                                       # arguments for auc
                                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                       partial.auc.focus="sens",
                                       # arguments for ci
                                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                       # arguments for plot
                                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                       print.auc=TRUE,show.thres=TRUE)
    
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
    
    roc6_svm_chick <- multiclass.roc(testData_chick[,4],
                                     testData_chick$weight, percent=TRUE,
                                     # arguments for auc
                                     partial.auc=c(100, 60), partial.auc.correct=TRUE,
                                     partial.auc.focus="sens",
                                     # arguments for ci
                                     boot.n=100, ci.alpha=0.9, stratified=FALSE,
                                     # arguments for plot
                                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                     print.auc=TRUE,show.thres=TRUE)
    
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
    
    
    # ****************** GRAFICAS DE COMPARACION DATASET CHICK **********************
    
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
    forestRecall_chick = c(recall2_Chick)
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
    
    
    
    # KNN
    output$knn <- renderPrint({
        print(knntable)
    })
    output$knng <- renderPlot({
        
        barplot(knntable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento KNN",col="yellow",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
        
    })
    output$rocg <- renderPlot({
        
        multiclass.roc(iris.test[,5],
                       iris.test$Sepal.Length, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Random Forest
    output$rf <- renderPrint({
        print(forestTable)
    })
    output$rfg <- renderPlot({
        
        barplot(forestTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="green",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc2g <- renderPlot({
        
        multiclass.roc(testData[,5],
                       testData$Sepal.Length, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Naive Bayes
    output$nai <- renderPrint({
        print(bayesTable)
    })
    output$naig <- renderPlot({
        
        barplot(bayesTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="red",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc3g <- renderPlot({
        
        multiclass.roc(y,
                       x$Sepal.Length, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Regresion Logistica
    output$reg <- renderPrint({
        print(regTable)
    })
    output$regg <- renderPlot({
        
        barplot(regTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="pink",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc4g <- renderPlot({
        
        multiclass.roc(iris$Species,
                       iris$Sepal.Width, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    
    # Redes Neuronales
    
    output$redes <- renderPrint({
        print(redesTable)
    })
    output$redesg <- renderPlot({
        
        barplot(redesTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="pink",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$redesgg <- renderPlot({
        
        plot( iris.nnt, col.intercept = "blue" , rep="best")
    })
    output$roc5g <- renderPlot({
        
        multiclass.roc(iris.test[,5],
                       iris.test$Petal.Length, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Maquina de Soporte Vectorial
    output$smv <- renderPrint({
        print(smvTable)
    })
    output$smvg <- renderPlot({
        
        barplot(smvTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="grey",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
    output$roc6g <- renderPlot({
        
        multiclass.roc(test_set[,5],
                       test_set$Sepal.Length, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Accuracy
    
    output$acc <- renderPrint({
        print(accuracyTable)
    })
    output$accg <- renderPlot({
        barplot(accuracyTable,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col="yellow",border = "blue",ylim =c(0,1))
    })
    
    # Precision
    
    output$pre <- renderPrint({
        print(precisionTable)
    })
    output$preg <- renderPlot({
        barplot(precisionTable,xlab = "Accuracy",ylab = "Puntaje", main = "Precision Iris",col="blue",border = "blue",ylim =c(0,1))
    })
    
    # Recall
    
    output$recc <- renderPrint({
        print(recallTable)
    })
    output$reccg <- renderPlot({
        barplot(recallTable,xlab = "Accuracy",ylab = "Puntaje", main = "Recall Iris",col="green",border = "blue",ylim =c(0,1))
    })
    
    #F-Measure
    
    output$f1 <- renderPrint({
        print(f1Table)
    })
    output$f1g <- renderPlot({
        barplot(f1Table,xlab = "Accuracy",ylab = "Puntaje", main = "F-Measure Iris",col="red",border = "blue",ylim =c(0,1))
    })
    
    ### DATASET ChickWeight
    
    # KNN
    output$knn1 <- renderPrint({
        print(knnTable_chick)
    })
    output$knng1 <- renderPlot({
        
        barplot(knnTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento KNN",col="green",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
        
    })
    output$roc7g <- renderPlot({
        
        multiclass.roc(chick.test[,4],
                       chick.test$weight, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Random Forest
    output$rf1 <- renderPrint({
        print(forestTable_chick)
    })
    output$rfg1 <- renderPlot({
        
        barplot(forestTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="pink",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc8g <- renderPlot({
        
        multiclass.roc(testData_chick[,4],
                       testData_chick$weight, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Naive Bayes
    output$nai1 <- renderPrint({
        print(bayesTable_chick)
    })
    output$naig1 <- renderPlot({
        
        barplot(bayesTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="red",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc9g <- renderPlot({
        
        multiclass.roc(y_chick,
                       x_chick$weight, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Regresion Logistica
    output$reg1 <- renderPrint({
        print(regTable_chick)
    })
    output$regg1 <- renderPlot({
        
        barplot(regTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="blue",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc10g <- renderPlot({
        
        multiclass.roc(Chick$Diet,
                       Chick$weight, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Redes Neuronales
    
    output$redes1 <- renderPrint({
        print(redesTable_chick)
    })
    output$redesg1 <- renderPlot({
        
        barplot(redesTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="purple",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$redesgg1 <- renderPlot({
        
        plot( chick.nnt, col.intercept = "red" , rep="best")
    })
    output$roc11g <- renderPlot({
        
        multiclass.roc(Chick$Diet,
                       Chick$Time, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Maquina de Soporte Vectorial
    output$smv1 <- renderPrint({
        print(smvTable_chick)
    })
    output$smvg1 <- renderPlot({
        
        barplot(smvTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="black",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    output$roc12g <- renderPlot({
        
        multiclass.roc(testData_chick[,4],
                       testData_chick$weight, percent=TRUE,
                       # arguments for auc
                       partial.auc=c(100, 60), partial.auc.correct=TRUE,
                       partial.auc.focus="sens",
                       # arguments for ci
                       boot.n=100, ci.alpha=0.9, stratified=FALSE,
                       # arguments for plot
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE,show.thres=TRUE)
        
    })
    
    # Accuracy
    
    output$acc1 <- renderPrint({
        print(accuracyTable_chick)
    })
    output$accg1 <- renderPlot({
        barplot(accuracyTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy ChickWeight",col="brown",border = "blue",ylim =c(0,1))
    })
    
    # Precision
    
    output$pre1 <- renderPrint({
        print(precisionTable_chick)
    })
    output$preg1 <- renderPlot({
        barplot(precisionTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Precision ChickWeight",col="orange",border = "blue",ylim =c(0,1))
    })
    
    # Recall
    
    output$recc1 <- renderPrint({
        print(recallTable_chick)
    })
    output$reccg1 <- renderPlot({
        barplot(recallTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Recall ChickWeight",col="pink",border = "blue",ylim =c(0,1))
    })
    
    #F-Measure
    
    output$f11 <- renderPrint({
        print(f1Table_chick)
    })
    output$f1g1 <- renderPlot({
        barplot(f1Table_chick,xlab = "Accuracy",ylab = "Puntaje", main = "F-Measure ChickWeight",col="green",border = "blue",ylim =c(0,1))
    })
    
    # Comparacion
    # iris
    # Accuracy
    output$acc2 <- renderPrint({
        print(accuracyTable)
    })
    output$accg2 <- renderPlot({
        barplot(accuracyTable,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col="yellow",border = "blue",ylim =c(0,1))
    })
    #Precision
    output$pre2 <- renderPrint({
        print(precisionTable)
    })
    output$preg2 <- renderPlot({
        barplot(precisionTable,xlab = "Accuracy",ylab = "Puntaje", main = "Precision Iris",col="blue",border = "blue",ylim =c(0,1))
    })
    # Recall
    
    output$recc2 <- renderPrint({
        print(recallTable)
    })
    output$reccg2 <- renderPlot({
        barplot(recallTable,xlab = "Accuracy",ylab = "Puntaje", main = "Recall Iris",col="green",border = "blue",ylim =c(0,1))
    })
    
    #F-Measure
    output$f12 <- renderPrint({
        print(f1Table)
    })
    output$f1g2 <- renderPlot({
        barplot(f1Table,xlab = "Accuracy",ylab = "Puntaje", main = "F-Measure Iris",col="red",border = "blue",ylim =c(0,1))
    })
    
    #chickweight
    #accuracy
    output$acc3 <- renderPrint({
        print(accuracyTable_chick)
    })
    output$accg3 <- renderPlot({
        barplot(accuracyTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy ChickWeight",col="brown",border = "blue",ylim =c(0,1))
    })
    #precision
    output$pre3 <- renderPrint({
        print(precisionTable_chick)
    })
    output$preg3 <- renderPlot({
        barplot(precisionTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Precision Iris",col="blue",border = "blue",ylim =c(0,1))
    })
    # Recall
    
    output$recc3 <- renderPrint({
        print(recallTable_chick)
    })
    output$reccg3 <- renderPlot({
        barplot(recallTable_chick,xlab = "Accuracy",ylab = "Puntaje", main = "Recall ChickWeight",col="pink",border = "blue",ylim =c(0,1))
    })
    
    #F-Measure
    
    output$f13 <- renderPrint({
        print(f1Table_chick)
    })
    output$f1g3 <- renderPlot({
        barplot(f1Table_chick,xlab = "Accuracy",ylab = "Puntaje", main = "F-Measure ChickWeight",col="green",border = "blue",ylim =c(0,1))
    })

})
