library(shiny)
library(tidyverse)

CaseStudy2_data <- read_csv("CaseStudy2-data.csv")
dataset=CaseStudy2_data

colors = c('darkgreen','midnightblue','darkgreen')

ui = fluidPage(
  
  # app title
  titlePanel("Case Study 2: Attrition Plots Against Predictor Variables"),
  
  # sidebar layout with input/output definitions
  sidebarLayout(
    
    # side bar panel for graphic parameters
    sidebarPanel(
      
      # selectInput for choosing variables
      selectInput(
        inputId = "data",
        label = "Predictor Variables",
        choices = list(
          'MonthlyIncome',
          'JobLevel',
          'OverTime',
          'Age',
          'HourlyRate',
          'JobInvolvement'
        )
      ),
      selectInput(
        inputId = "groups",
        label = "Display by Groups",
        choices = list(
          'Attrition',
          'Gender',
          'BusinessTravel',
          'MaritalStatus'
        )
      ),
 
      
      ## Attrition Count Plot
      plotOutput(
        outputId = "aplot"
      ),
    ),
    
    # main panel for displaying plot
    mainPanel(
      
      
      # histogram outputm pplot is percentage plot
      plotOutput(
        outputId = "histplot"
      ),
      plotOutput(
        outputId = "pplot"
      )
      
    )
    
  ),
  
)

# server function for creating app

server  = function(input,output){
  
  # renderPlot function is used to map the histogram inputs to main panel outputs
  # this plot is "reactive," i.e. changes when inputs (e.g. bins) are altered
 
  #Histogram Plot
   output$histplot = renderPlot({
    dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
      geom_histogram(stats="identity")+
      xlab(input$data)+
      scale_fill_manual(values=as.vector(colors))+
      ggtitle(paste("Histogram of",
                    input$data,
                    "faceted by",
                    input$groups,
                    sep=" "))
  })
  
  #Percentage Plot
  output$pplot = renderPlot({
    # creating histogram for output
    dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
      geom_bar(position="fill")+
      xlab(input$data)+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_manual(values=as.vector(colors))+
      ggtitle(paste("Histogram",
                    input$data,
                    "in Percentages by",
                    input$groups,
                    sep=" "))
  })
  
  #Attrition Count Plot
  output$aplot=renderPlot({
    dataset %>% ggplot(aes(x=Attrition,fill=Attrition))+ geom_bar()+
      ggtitle("Attrition Count") +
      scale_fill_manual(values=as.vector(colors))+
      xlab("Attrition")+ylab("Count")
  })
  
  #Naive Bayes Model
  naive_data=employeeData
  
  model2 = naive_data
  model2$Attrition = as.factor(model2$Attrition)
  
  trainIndices = sample(1:dim(model2)[1],round(.70 * dim(model2)[1]))
  train = model2[trainIndices,]
  test = model2[-trainIndices,]
  
  classifier1 = naiveBayes(model2[,-c(1,5,9,10,11,14,21,23,24,28,37:45)],model2$Attrition)
  
  pred = predict(classifier1,newdata=test)
  CM = confusionMatrix(table(test$Attrition,pred))
  
  CM
}

shinyApp(ui = ui, server = server)