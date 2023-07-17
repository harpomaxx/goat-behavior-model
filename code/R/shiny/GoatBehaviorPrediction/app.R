# Import necessary libraries
library(shiny)
library(caret)
library(readr)
library(catboost)
library(ggplot2)
library(gridExtra)

source("calculate_shap.R")
source("plot_shap.R")

# Load the pre-trained model
model <- readRDS("goat_behavior_model_caret.rds")

# Define UI for application
ui <- fluidPage(
  
  # App title ----
  titlePanel("Detecting Goat Behaviors"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose TSV File",
                accept = c(
                  "text/tsv",
                  "text/tab-separated-values,text/plain",
                  ".tsv")
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with data, confusion matrix, and download button
      tabsetPanel(
        id = "dataset",
        tabPanel("About", 
                 HTML("
                 <h5> The following model was part of the the research article: 
                 <h4>Developing an Interpretable Machine Learning Model for the Detection of Mimosa Grazing in Goats</h4>
                 
                 <em>In the last years, several machine learning approaches for detecting animal behaviors have been proposed. 
                 However, despite their successful application,  their complexity and lack of explainability have difficulty in their 
                 application to real-world scenarios. The article presents a machine-learning model for differentiating between grazing mimosa and other activities  
                 (resting, walking, and grazing ) in goats using sensor data.  Boruta, an algorithm for selecting the most relevant features, and SHAP, 
                 a technique for interpreting the decision of a machine learning model are two fundamental components of the methodology used for creating the model. 
                 The resulting model, a gradient boost algorithm with 15 selected features proved to be extremely accurate in detecting Grazing activities.  
                 The study demonstrates the fundamental role of model explainability in identifying model weaknesses and errors, thereby creating a path for future 
                 improvements. In addition, the simplicity of the resulting model not only reduces computational complexity and processing time but also enhances 
                 interpretability and facilitates the deployment of real-life scenarios.</em>
                 <p>
                 <p>This application allows users to test the pre-trained machine learning model that predicts goat behavior based on input sensor data. 
                       The input data should be a tab-separated value (.tsv) file containing specific sensor data related to the goat's activity.
                       <p>The application then generates predictions, provides a confusion matrix result, and offers the option to download the predictions.
                       <p>The key features expected in the dataset are: 
                      <table>
  <thead>
    <tr>
      <th>No</th>
      <th>Feature</th>
      <th>Definition</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>Steps</td>
      <td>Number of steps</td>
    </tr>
    <tr>
      <td>2</td>
      <td>HeadDown</td>
      <td>% time with head down</td>
    </tr>
    <tr>
      <td>3</td>
      <td>Standing</td>
      <td>% time Standing</td>
    </tr>
    <tr>
      <td>4</td>
      <td>Active</td>
      <td>% time Active</td>
    </tr>
    <tr>
      <td>5</td>
      <td>MeanXY</td>
      <td>Arithmetic mean between X and Y positions</td>
    </tr>
    <tr>
      <td>6</td>
      <td>Distance</td>
      <td>Distance in meters</td>
    </tr>
    <tr>
      <td>7</td>
      <td>prev_steps1</td>
      <td>Number of steps one step backward</td>
    </tr>
    <tr>
      <td>8</td>
      <td>X_Act</td>
      <td>X position actuator</td>
    </tr>
    <tr>
      <td>9</td>
      <td>prev_Active1</td>
      <td>% time Active one step backward</td>
    </tr>
    <tr>
      <td>10</td>
      <td>prev_Standing1</td>
      <td>% time Standing one step backward</td>
    </tr>
    <tr>
      <td>11</td>
      <td>DFA123</td>
      <td>Accumulative Euclidean distance from actual position to three positions forward</td>
    </tr>
    <tr>
      <td>12</td>
      <td>prev_headdown1</td>
      <td>% time with head down one step backward</td>
    </tr>
    <tr>
      <td>13</td>
      <td>Lying</td>
      <td>% time Lying</td>
    </tr>
    <tr>
      <td>14</td>
      <td>Y_Act</td>
      <td>Y position actuator</td>
    </tr>
    <tr>
      <td>15</td>
      <td>DBA123</td>
      <td>Accumulative Euclidean distance from actual position to three positions backward</td>
    </tr>
  </tbody>
</table> 
                      
                      <p><p> <h5> Experiments, source code and more <a href=https://github.com/harpomaxx/goat-behavior/> here<a/> </h5>")
        ),
        
        tabPanel("Results",
                 tableOutput("contents"),
                 verbatimTextOutput("confusionMatText"),
                 plotOutput("confusionMatPlot"),
                 downloadButton("downloadData", "Download Predictions")),
       
         tabPanel("SHAP Summary",
                 plotOutput("SHAPSummary")),
        
        tabPanel("SHAP Summary per class",
                 plotOutput("SHAPSummaryperclass")),
        
        tabPanel("SHAP Dependency",
                 plotOutput("SHAPDependency"))
                 
        
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # For the predictions dataset
  predictions <- reactive({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    predict(model, dataset)
    
  })
  
  # For the table
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # a file, it will be a data frame with 'name', 'size', 'type', and 'datapath' variables.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    # Read the file from the input$file1 path and return it
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    head(dataset, n = 5)
  })
  
  # Download function for predictions
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(Index = 1:length(predictions()), Prediction = predictions()), file, row.names = FALSE)
    }
  )
  
  # Confusion Matrix
  output$confusionMatText <- renderPrint({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t',progress = FALSE)
    predictions <- predict(model, dataset)
    cm<-caret::confusionMatrix(reference=as.factor(dataset$Activity),predictions,mode="everything")
    cm$overall
  })
  
  output$confusionMatPlot <- renderPlot({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    predictions <- predict(model, dataset)
    cm<-caret::confusionMatrix(reference=as.factor(dataset$Activity),predictions,mode="everything")
    
    # Extract table data from confusion matrix
    confusionMatrixTable <- as.table(cm$table)
    
    # Plot the confusion matrix
    ggplot(as.data.frame(confusionMatrixTable), aes(x=Reference, y=Prediction)) +
      geom_tile(aes(fill = log(Freq)), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })
  
  output$SHAPSummary <- renderPlot({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    predictions <- predict(model, dataset)
    selected_variables <-
      readr::read_delim(
        "selected_features.tsv",
        col_types = cols(),
        delim = '\t'
      )
    new_dataset <-
      dataset %>% select(selected_variables$variable, Anim, Activity)
    new_dataset <- cbind(new_dataset, predictions)
    
    shap_values <- calculate_shap(new_dataset, model, nsim = 30)
    pall<-shap_summary_plot(shap_values %>% as.data.frame())
    pall+xlim(0,0.35)
  })
  
  output$SHAPSummaryperclass <- renderPlot({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    predictions <- predict(model, dataset)
    selected_variables <-
      readr::read_delim(
        "selected_features.tsv",
        col_types = cols(),
        delim = '\t'
      )
    new_dataset <-
      dataset %>% select(selected_variables$variable, Anim, Activity)
    new_dataset <- cbind(new_dataset, predictions)
    
    shap_values <- calculate_shap(new_dataset, model, nsim = 30)
    
    pW<-shap_summary_plot_perclass(shap_values, class= "W",color="#C77CFF")+xlab("Activity W")+xlim(0,0.25)
    pGM<-shap_summary_plot_perclass(shap_values, class= "GM",color="#7CAE00")+xlab("Activity GM")+xlim(0,0.25)
    pG<-shap_summary_plot_perclass(shap_values, class= "G",color="#F8766D")+xlab("Activity G")+xlim(0,0.25)
    pR<-shap_summary_plot_perclass(shap_values, class= "R",color="#00BFC4")+xlab("Activity R")+xlim(0,0.25)
    
    grid.arrange(pW,pR,pG,pGM)
    
    
    
  })
  output$SHAPDependency <- renderPlot({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    dataset <- readr::read_delim(inFile$datapath,delim='\t')
    predictions <- predict(model, dataset)
    selected_variables <-
      readr::read_delim(
        "selected_features.tsv",
        col_types = cols(),
        delim = '\t'
      )
    new_dataset <-
      dataset %>% select(selected_variables$variable, Anim, Activity)
    new_dataset <- cbind(new_dataset, predictions)
    
    shap_values <- calculate_shap(new_dataset, model, nsim = 30) 
    
    li<-list()
    li[[1]]<-dependency_plot("Steps",dataset = new_dataset,shap=shap_values)
    #li[[2]]<-dependency_plot("prev_steps1",dataset = new_dataset,shap=shap_values)
    li[[2]]<-dependency_plot("%HeadDown",dataset = new_dataset,shap=shap_values)
    #li[[4]]<-dependency_plot("prev_headdown1",dataset = new_dataset,shap=shap_values)
    li[[3]]<-dependency_plot("Active",dataset = new_dataset,shap=shap_values)
    #li[[6]]<-dependency_plot("prev_Active1",dataset = new_dataset,shap=shap_values)
    li[[4]]<-dependency_plot("Standing",dataset = new_dataset,shap=shap_values)
    #li[[8]]<-dependency_plot("prev_Standing1",dataset = new_dataset,shap=shap_values)
    #li[[9]]<-dependency_plot("X_Act",dataset = new_dataset, shap=shap_values)
    #li[[10]]<-dependency_plot("Y_Act",dataset = new_dataset, shap=shap_values)
    #li[[11]]<-dependency_plot("DBA123",dataset = new_dataset, shap=shap_values)
    #li[[12]]<-dependency_plot("DFA123",dataset = new_dataset, shap=shap_values)
    do.call(grid.arrange, c(li, ncol = 1)) 
    
    
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
