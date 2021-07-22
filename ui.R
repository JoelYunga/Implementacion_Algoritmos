#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(tags$style(
        HTML('
        h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15{
                color: black;
                font-weight: bolder;

        }
        h2{
        align-items: center;
        }
         #sidebar {
            background-image: url("");
            color: black
            background: black
        }

        body, label, input, button, select { 
          font-family: "Arial";
          color: black;
          background: black;
          font-weight: bolder;
        }
        
             body {
        background-image: url("https://blog.bismart.com/hubfs/20190903-MachineLearning.jpg");
        
    }
    
    body, label, input, button, select { 
        font-family: "Arial";
        color:black
        font-weight: bolder
        
    } 
    .panel {
        background: black
        background-color: black;
    }
    .tabs h2,h5{
        color: black
        font-weight: bolder
        
    }
    .select {
        background-color: black;
    }         ')
    )),
    tags$h1("PROYECTO FINAL", align="center"),
    titlePanel(h3("Implementacion de Algoritmos", align="center")),
    tags$h3("INTEGRANTES:"),
    tags$li(" CUEVA AMARILIS"),
    tags$li(" HERNANDEZ EDWIN"),
    tags$li(" NAVAS JEFRY"),
    tags$li(" VIZCAINO ALISSON"),
    tags$li(" YUNGA JOEL"),
    br(),
    br(),
    tabsetPanel(type = "tabs", 
                tabPanel(h5("Dataset de Iris"),
                         br(),
                         br(),
                         tabsetPanel(type = "tabs", 
                                     tabPanel(h5("Algoritmo de KNN"),
                                              tags$h2("Evaluacion de Rendimiento con el Algoritmo de KNN"),
                                              verbatimTextOutput("knn"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "knng"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "rocg"),
                                     ),
                                     tabPanel(h5("Algoritmo de Random Forest"),
                                              tags$h2("Evaluacion de Rendimiento con el Algoritmo de Random Forest"),
                                              verbatimTextOutput("rf"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "rfg"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc2g"),
                                     ),                  
                                     tabPanel(h5("Algoritmo de Naive Bayes"),
                                              tags$h2("Evaluacion de Rendimiento con el Naive Bayes"),
                                              verbatimTextOutput("nai"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "naig"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc3g"),
                                     ),
                                     tabPanel(h5("Algoritmo de Regresion Logistica"),
                                              tags$h2("Evaluacion de Rendimiento con Regresion Logistica"),
                                              verbatimTextOutput("reg"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "regg"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc4g"),
                                     ),
                                     tabPanel(h5("Algoritmo de Red Neuronal"),
                                              tags$h2("Evaluacion de Rendimiento con Redes Neuronales"),
                                              verbatimTextOutput("redes"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "redesg"),
                                              tags$h2("Grafico de Redes Neuronales "),
                                              plotOutput(outputId = "redesgg"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc5g"),
                                              
                                     ),
                                     tabPanel(h5("Algoritmo de Maquina de Soporte Vectorial"),
                                              tags$h2("Evaluacion de Rendimiento con Maquina de Soporte Vectorial"),
                                              verbatimTextOutput("smv"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "smvg"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc6g"),
                                     ),
                                     tabPanel(h5("Comparacion de los Algoritmos"),
                                              tags$h2("Comparacion del Accuracy"),
                                              verbatimTextOutput("acc"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Accuracy "),
                                              plotOutput(outputId = "accg"),
                                              tags$h2("Comparacion del Precision "),
                                              verbatimTextOutput("pre"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Precision "),
                                              plotOutput(outputId = "preg"),
                                              tags$h2("Comparacion del Recall "),
                                              verbatimTextOutput("recc"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Recall "),
                                              plotOutput(outputId = "reccg"),
                                              tags$h2("Comparacion del F-Measure "),
                                              verbatimTextOutput("f1"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del F-Measure "),
                                              plotOutput(outputId = "f1g")
                                              
                                     )
                         ),
                         
                ),
                tabPanel(h5("Dataset de ChickWeight"),
                         br(),
                         br(),
                         tabsetPanel(type = "tabs", 
                                     tabPanel(h5("Algoritmo de KNN"),
                                              tags$h2("Evaluacion de Rendimiento con el Algoritmo de KNN"),
                                              verbatimTextOutput("knn1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "knng1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc7g"),
                                     ),
                                     tabPanel(h5("Algoritmo de Random Forest"),
                                              tags$h2("Evaluacion de Rendimiento con el Algoritmo de Random Forest"),
                                              verbatimTextOutput("rf1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "rfg1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc8g"),
                                     ),                  
                                     tabPanel(h5("Algoritmo de Naive Bayes"),
                                              tags$h2("Evaluacion de Rendimiento con el Naive Bayes"),
                                              verbatimTextOutput("nai1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "naig1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc9g"),
                                     ),
                                     tabPanel(h5("Algoritmo de Regresion Logistica"),
                                              tags$h2("Evaluacion de Rendimiento con Regresion Logistica"),
                                              verbatimTextOutput("reg1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "regg1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc10g")
                                     ),
                                     tabPanel(h5("Algoritmo de Red Neuronal"),
                                              tags$h2("Evaluacion de Rendimiento con Redes Neuronales"),
                                              verbatimTextOutput("redes1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "redesg1"),
                                              tags$h2("Grafico de Redes Neuronales "),
                                              plotOutput(outputId = "redesgg1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc11g")
                                              
                                     ),
                                     tabPanel(h5("Algoritmo de Maquina de Soporte Vectorial"),
                                              tags$h2("Evaluacion de Rendimiento con Maquina de Soporte Vectorial"),
                                              verbatimTextOutput("smv1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafico del Rendimiento "),
                                              plotOutput(outputId = "smvg1"),
                                              br(),
                                              br(),
                                              tags$h2("Grafica de la Curva de ROC "),
                                              plotOutput(outputId = "roc12g")
                                     ),
                                     tabPanel(h5("Comparacion de los Algoritmos"),
                                              tags$h2("Comparacion del Accuracy"),
                                              verbatimTextOutput("acc1"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Accuracy "),
                                              plotOutput(outputId = "accg1"),
                                              tags$h2("Comparacion del Precision "),
                                              verbatimTextOutput("pre1"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Precision "),
                                              plotOutput(outputId = "preg1"),
                                              tags$h2("Comparacion del Recall "),
                                              verbatimTextOutput("recc1"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Recall "),
                                              plotOutput(outputId = "reccg1"),
                                              tags$h2("Comparacion del F-Measure "),
                                              verbatimTextOutput("f11"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del F-Measure "),
                                              plotOutput(outputId = "f1g1")
                                              
                                     )
                         ),     
                         
                         
                ),                  
                tabPanel(h5("Comparacion"),
                         br(),
                         br(),
                         tabsetPanel(type = "tabs", 
                                     tabPanel(h5("Accuracy"),
                                              tags$h2("Accuracy Iris"),
                                              verbatimTextOutput("acc2"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Accuracy de Iris "),
                                              plotOutput(outputId = "accg2"),
                                              tags$h2("Accuracy ChickWeight"),
                                              verbatimTextOutput("acc3"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Accuracy de ChickWeight "),
                                              plotOutput(outputId = "accg3"),  
                                     ),
                                     tabPanel(h5("Precision"),
                                              tags$h2("Precision Iris"),
                                              verbatimTextOutput("pre2"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Precision de Iris "),
                                              plotOutput(outputId = "preg2"),
                                              tags$h2("Precision ChickWeight"),
                                              verbatimTextOutput("pre3"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Precision de ChickWeight "),
                                              plotOutput(outputId = "preg3"),       
                                     ),                  
                                     tabPanel(h5("Recall"),
                                              tags$h2("Recall Iris"),
                                              verbatimTextOutput("recc2"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Recall de Iris "),
                                              plotOutput(outputId = "reccg2"),
                                              tags$h2("Recall ChickWeight"),
                                              verbatimTextOutput("recc3"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del Recall de ChickWeight "),
                                              plotOutput(outputId = "reccg3"),            
                                     ),
                                     tabPanel(h5("F-Measure"),
                                              tags$h2("F-Measure Iris"),
                                              verbatimTextOutput("f12"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del F-Measure de Iris "),
                                              plotOutput(outputId = "f1g2"),
                                              tags$h2("F-Measure ChickWeight"),
                                              verbatimTextOutput("f13"),
                                              br(),
                                              tags$h2("Grafico de Comparacion del F-Measure de ChickWeight "),
                                              plotOutput(outputId = "f1g3"),           
                                     ),
                                     tabPanel(h5(),
                                              
                                              
                                              
                                     )
                         ),         
                )),
    br(),
    br(),
    br(),  
))
