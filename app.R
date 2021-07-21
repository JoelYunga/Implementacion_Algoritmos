#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggcorrplot)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
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
                                     ),
                                     tabPanel(h5("Algoritmo de Random Forest"),
                                        tags$h2("Evaluacion de Rendimiento con el Algoritmo de Random Forest"),
                                        verbatimTextOutput("rf"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "rfg"),
                                     ),                  
                                     tabPanel(h5("Algoritmo de Naive Bayes"),
                                        tags$h2("Evaluacion de Rendimiento con el Naive Bayes"),
                                        verbatimTextOutput("nai"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "naig"),       
                                     ),
                                     tabPanel(h5("Algoritmo de Regresion Logistica"),
                                        tags$h2("Evaluacion de Rendimiento con Regresion Logistica"),
                                        verbatimTextOutput("reg"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "regg"),   
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
                                              
                                     ),
                                     tabPanel(h5("Algoritmo de Maquina de Soporte Vectorial"),
                                        tags$h2("Evaluacion de Rendimiento con Maquina de Soporte Vectorial"),
                                        verbatimTextOutput("smv"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "smvg"),         
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
                                     ),
                                     tabPanel(h5("Algoritmo de Random Forest"),
                                        tags$h2("Evaluacion de Rendimiento con el Algoritmo de Random Forest"),
                                        verbatimTextOutput("rf1"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "rfg1"),     
                                     ),                  
                                     tabPanel(h5("Algoritmo de Naive Bayes"),
                                        tags$h2("Evaluacion de Rendimiento con el Naive Bayes"),
                                        verbatimTextOutput("nai1"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "naig1"),          
                                     ),
                                     tabPanel(h5("Algoritmo de Regresion Logistica"),
                                        tags$h2("Evaluacion de Rendimiento con Regresion Logistica"),
                                        verbatimTextOutput("reg1"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "regg1"),      
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
                                              
                                     ),
                                     tabPanel(h5("Algoritmo de Maquina de Soporte Vectorial"),
                                        tags$h2("Evaluacion de Rendimiento con Maquina de Soporte Vectorial"),
                                        verbatimTextOutput("smv1"),
                                        br(),
                                        br(),
                                        tags$h2("Grafico del Rendimiento "),
                                        plotOutput(outputId = "smvg1"),            
                                     ),
                                     tabPanel(h5("Comparacion de los Algoritmos"),
                                          
                                              
                                     )
                         ),     
                         
                         
                ),                  
                tabPanel(h5("Comparacion"),
                        
                )),
    br(),
    br(),
    br(),  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    # KNN
    output$knn <- renderPrint({
       print(knntable)
    })
    output$knng <- renderPlot({
        
        barplot(knntable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento KNN",col="yellow",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
        
    })
    
    # Random Forest
    output$rf <- renderPrint({
        print(forestTable)
    })
    output$rfg <- renderPlot({
        
        barplot(forestTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="green",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
    # Naive Bayes
    output$nai <- renderPrint({
        print(bayesTable)
    })
    output$naig <- renderPlot({
        
        barplot(bayesTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="red",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
    # Regresion Logistica
    output$reg <- renderPrint({
        print(regTable)
    })
    output$regg <- renderPlot({
        
        barplot(regTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="pink",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
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
    
    # Maquina de Soporte Vectorial
    output$smv <- renderPrint({
        print(smvTable)
    })
    output$smvg <- renderPlot({
        
        barplot(smvTable,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="grey",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
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
        barplot(precisionTable,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col="blue",border = "blue",ylim =c(0,1))
    })
    
    # Recall
    
    output$recc <- renderPrint({
        print(recallTable)
    })
    output$reccg <- renderPlot({
        barplot(recallTable,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col="green",border = "blue",ylim =c(0,1))
    })
    
    #F-Measure
    
    output$f1 <- renderPrint({
        print(f1Table)
    })
    output$f1g <- renderPlot({
        barplot(f1Table,xlab = "Accuracy",ylab = "Puntaje", main = "Accuracy Iris",col="red",border = "blue",ylim =c(0,1))
    })
    
    ### DATASET ChickWeight
    
    # KNN
    output$knn1 <- renderPrint({
        print(knnTable_chick)
    })
    output$knng1 <- renderPlot({
        
        barplot(knnTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento KNN",col="green",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
        
    })
    
    # Random Forest
    output$rf1 <- renderPrint({
        print(forestTable_chick)
    })
    output$rfg1 <- renderPlot({
        
        barplot(forestTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="pink",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
    # Naive Bayes
    output$nai1 <- renderPrint({
        print(bayesTable_chick)
    })
    output$naig1 <- renderPlot({
        
        barplot(bayesTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="red",border = "blue",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
    # Regresion Logistica
    output$reg1 <- renderPrint({
        print(regTable_chick)
    })
    output$regg1 <- renderPlot({
        
        barplot(regTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="blue",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
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
    
    # Maquina de Soporte Vectorial
    output$smv1 <- renderPrint({
        print(smvTable_chick)
    })
    output$smvg1 <- renderPlot({
        
        barplot(smvTable_chick,space=0.8,xlab = "Rendimiento",ylab = "Puntaje", main = "Evaluacion de Rendimiento Random Forest",col="black",border = "red",names.arg = c("1 Accuracy","2 Precision", "3 Recall", "4 F-Measure") ,ylim =c(0,1))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)