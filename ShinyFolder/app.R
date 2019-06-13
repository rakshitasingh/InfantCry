#
# A small demo app for the shinyearr function
#
# devtools::install_github("nstrayer/shinysense")

library(shiny)
library(shinysense)
library(tidyverse)
library(forcats)
library(e1071)
library(gridExtra)
load("classifier_linear.rda")
load("classifier_radial.rda")
load("classifier_polynomial.rda")
load("classifier_sigmoid.rda")
Classing <- 0
Classing1<- 0
Classing2 <- 0
Classing3 <- 0
Test <- read.csv("test_mean_and_sd_of_coff.csv")
Train <- read.csv("train_mean_and_sd_of_coff.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Baby Cry Analyzer"),
  p(strong("Click on the button below to record audio from your microphone. After your recording is done a plot will display your data in terms of the fourier transformed signal")),
  p("You can input a label for each recording and then export the results of all your recordings as a tidy csv"),
  hr(),
  fluidRow(
    div(style = "height:100%;",
        column(4, offset = 1,
               shinyearrUI("my_recorder")
        ),
        column(6,
               textInput("label", "recording label"),
               offset = 1
        )
    )
  ),
  column(12,div(style = "height:500px", 
                tabsetPanel(
                  
                  tabPanel("Plot1", plotOutput("frequencyPlot")), 
                  tabPanel("Plot2", plotOutput("summary2")),
                  tabPanel("Plot3", plotOutput("summary3")),
                  tabPanel("Testing Plots",plotOutput("TestPlots")),
                  tabPanel("Training Plots",plotOutput("TrainPlots"))
                  
                )
  )),
  
  
  hr(),
  fluidRow(
   
    column(3, verbatimTextOutput("There2")),
    column(2, verbatimTextOutput("There1")),
    column(3,  verbatimTextOutput("There")),
   column(2, actionButton("update" ,"Predict", icon("refresh"),class = "btn btn-primary")),
   column(2, downloadButton('downloadData', 'download your data')),
    hr()
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #object to hold all your recordings in to plot
  rvs <- reactiveValues(
    recordings = data_frame(value = numeric(), frequency = integer(), num = character(), label = character()),
    counter = 0
  )
  
  recorder <- callModule(shinyearr, "my_recorder")
  
  #initialize plot so it isnt just an empty space.
  output$frequencyPlot <- renderPlot({
    ggplot(rvs$recordings) +
      xlim(0,256) +
      labs(x = "frequency", y = "value") +
      labs(title = "Frequency bins from recording")
  })
  
  observeEvent(recorder(), {
    my_recording <- recorder()
    
    rvs$counter <- rvs$counter + 1
    rvs$recordings <- rbind(
      data_frame(value = my_recording, frequency = 1:256, num = paste("recording", rvs$counter), label = input$label),
      rvs$recordings
    ) %>% mutate(num = fct_inorder(num))
    
    # Generate a plot of the recording we just made
    output$frequencyPlot <- renderPlot({
      
      ggplot(rvs$recordings, aes(x = frequency, y = value)) +
        geom_line() +
        geom_text(aes(label = label), x = 255, y = 100,
                  color = "steelblue", size = 16, hjust = 1) +
        facet_wrap(~num) +
        labs(title = "Frequency bins from recording")
      
    })
    output$summary2 <- renderPlot({
      daily <- group_by(Train, loc)
      MonthsData <- summarise(daily,MEAN=mean(mean.1),DEMAND=mean(sd.1))
      ggplot(aes(x=factor(loc),y=MEAN,group=1),data=MonthsData)+geom_point(shape=23,size=5,col="black",fill="red")+geom_line(col="black",linetype="dashed",size=1.25)+labs(x="Labels",title="Frequency of Multiple classes of Voice")
    })
    output$summary3 <- renderPlot({
      daily <- group_by(Train, loc)
      MonthsData <- summarise(daily,MEAN=mean(mean.1),DEVIATION=mean(sd.1))
      ggplot(aes(x=factor(loc),y=DEVIATION,group=1),data=MonthsData)+geom_point(shape=23,size=5,col="black",fill="red")+geom_line(col="black",linetype="dashed",size=1.25)+labs(x="Labels",title="Standard Deviation of Multiple classes of Voice")
    })
    output$There <- renderPrint({
      Cater <- function(x){
        vecs <- c("bellypain","burping","dontknow","hotandcold","discomfort","hungry","lonely","scared","tired")
        return (vecs[as.integer(x)])
      }
      Test=select(Test,-loc,-filehandles)
      
      hr()
      as.vector(predict(classifier_linear,Test[-25])) -> Xtr
      as.vector(predict(classifier_radial,Test[-25])) -> Xtr1
      as.vector(predict(classifier_polynomial,Test[-25])) -> Xtr2
      as.vector(predict(classifier_sigmoid,Test[-25])) -> Xtr3
      table(as.vector(sapply(Xtr,Cater))) -> Xtr
      table(as.vector(sapply(Xtr1,Cater))) -> Xtr1
      table(as.vector(sapply(Xtr2,Cater))) -> Xtr2
      table(as.vector(sapply(Xtr3,Cater))) -> Xtr3
      
      cat("Predictions on Testing Set\n")
      cat("------------------------------\n")
      cat("Linear Classifier\n")
      print(Xtr)
      cat("------------------------------\n")
      cat("Radial Classifier\n")
      print(Xtr1)
      cat("------------------------------\n")
      cat("Sigmoid Classifier\n")
      print(Xtr2)
      cat("------------------------------\n")
      cat("Polynomial Classifier\n")
      print(Xtr3)
      cat("================================")
      
      #print(as.vector(sapply(Xtr,Cater)))
    })
    
    output$There2 <- renderPrint({
      Cater <- function(x){
        vecs <- c("bellypain","burping","dontknow","hotandcold","discomfort","hungry","lonely","scared","tired")
        return (vecs[as.integer(x)])
      }
      Test=select(Test,-loc,-filehandles)
      
      hr()
      as.vector(predict(classifier_linear,Train[-25])) -> Xtr
      as.vector(predict(classifier_radial,Train[-25])) -> Xtr1
      as.vector(predict(classifier_polynomial,Train[-25])) -> Xtr2
      as.vector(predict(classifier_sigmoid,Test[-25])) -> Xtr3
      table(as.vector(sapply(Xtr,Cater))) -> Xtr
      table(as.vector(sapply(Xtr1,Cater))) -> Xtr1
      table(as.vector(sapply(Xtr2,Cater))) -> Xtr2
      table(as.vector(sapply(Xtr3,Cater))) -> Xtr3
      
      cat("Predictions on Training Set\n")
      cat("------------------------------\n")
      cat("Linear Classifier\n")
      print(Xtr)
      cat("------------------------------\n")
      cat("Radial Classifier\n")
      print(Xtr1)
      cat("------------------------------\n")
      cat("Sigmoid Classifier\n")
      print(Xtr2)
      cat("------------------------------\n")
      cat("Polynomial Classifier\n")
      print(Xtr3)
      cat("================================")
      
      #print(as.vector(sapply(Xtr,Cater)))
    })
    
    output$TrainPlots<- renderPlot({
      Cater <- function(x){
        vecs <- c("bellypain","burping","dontknow","hotandcold","discomfort","hungry","lonely","scared","tired")
        return (vecs[as.integer(x)])
      }
      Test=select(Train,-loc,-filehandles)
      
      
      as.vector(predict(classifier_linear,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Linear") -> p1
      
      as.vector(predict(classifier_radial,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Radial") -> p2
      
      as.vector(predict(classifier_polynomial,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Polynomial") -> p3
      
      as.vector(predict(classifier_sigmoid,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Sigmoid") -> p4
      
      grid.arrange(p1,p2,p3,p4)
      
    })
    
    output$TestPlots<- renderPlot({
      Cater <- function(x){
        vecs <- c("bellypain","burping","dontknow","hotandcold","discomfort","hungry","lonely","scared","tired")
        return (vecs[as.integer(x)])
      }
      Test=select(Test,-loc,-filehandles)
      
      
      as.vector(predict(classifier_linear,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Linear") -> p1
      
      as.vector(predict(classifier_radial,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Radial") -> p2

      as.vector(predict(classifier_polynomial,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Polynomial") -> p3
      
      as.vector(predict(classifier_sigmoid,Test[-25])) -> Xtr
      Predictor <- as.vector(sapply(Xtr,Cater))
      table(Predictor) -> Predictor
      as.data.frame(Predictor) -> Predictor
      Predictor <- data.frame(Predictor)
      ggplot(aes(x=factor(Predictor),y=Freq),data=Predictor)+geom_col(aes(fill=factor(Predictor)))+labs(x="Categories",title="Sigmoid") -> p4
      
      grid.arrange(p1,p2,p3,p4)
      
    })
  })
  
  
  output$downloadData <- downloadHandler(
    filename = "my_recordings.csv",
    content = function(file) {
      write.csv(rvs$recordings, file)
      dat <- data.frame()
      xx <- data.frame()
      #colnames(xx) <- c(
       # "mean 1	","sd 1	","mean 2	","sd 2	","mean 3	","sd 3	","mean 4	","sd 4	","mean 5	","sd 5	","mean 6	","sd 6	","mean 7	","sd 7	","mean 8	","sd 8	","mean 9	","sd 9	","mean 10	","sd 10	","mean 11	","sd 11	","mean 12	","sd 12"
      #) 
      file1 <-rvs$recordings
      
      i=1
      #counter = 1
      for( i in 1: 12)
      {
        j = i
        k11 = i + 12
        me <- mean(na.omit(file1$value[j:k11]))
        
        sdd <- sd(na.omit(file1$value[j:k11]))
        
        print(paste("mean is :", me))
        print(paste("sd is : ",sdd))
        
        # print(paste("counter is :",counter))
        # counter = counter + 1
        
        xx <- rbind(xx,me)
        xx <- rbind(xx,sdd)
        
        
        
      }
      
      xx = t(xx)
      #write.csv(xx, file = "testing_recorded_audio.csv",row.names=FALSE)
      
      xc <- xx
      
      colnames(xc) <- c("mean 1","sd 1","mean 2","sd 2","mean 3","sd 3","mean 4","sd 4","mean 5","sd 5","mean 6","sd 6","mean 7","sd 7","mean 8","sd 8","mean 9","sd 9","mean 10","sd 10","mean 11","sd 11","mean 12","sd 12"
      )
      # write.csv(xc, file = "testing_recorded_audio.csv",row.names=FALSE)
      # xc <- xx
      
      
  
  
  mfcc.test1 <- xc
  
  mfcc.test1 <- mfcc.test1[,1:24]
 # mfcc.test1[,1:24] = (mfcc.test1[,1:24]- min(mfcc.test1[,1:24]))/(max(mfcc.test1[,1:24])-min(mfcc.test1[,1:24]))
  print(mfcc.test1)
  t(mfcc.test1) -> mfcc.test1
  print(as.data.frame(mfcc.test1))
  
 # print(output$There2)
  #print(bn)
  #output$There <- renderPrint(bn)
  
  library(e1071)
  as.data.frame(mfcc.test1) -> mfcc.test1
  colnames(mfcc.test1) <- c("mean.1","sd.1","mean.2","sd.2","mean.3","sd.3","mean.4","sd.4","mean.5","sd.5","mean.6","sd.6","mean.7","sd.7","mean.8","sd.8","mean.9","sd.9","mean.10","sd.10","mean.11","sd.11","mean.12","sd.12")
  y_pred_linear_p = predict(classifier_linear, newdata = mfcc.test1,type="raw")
  print(y_pred_linear_p)
  Classing <- y_pred_linear_p
  Classing1 <- predict(classifier_radial,newdata=mfcc.test1,type="raw")
  Classing2 <- predict(classifier_sigmoid,newdata=mfcc.test1,type="raw")
  Classing3 <- predict(classifier_polynomial,newdata=mfcc.test1,type="raw")
  Cater <- function(x){
    vecs <- c("bellypain","burping","dontknow","hotandcold","discomfort","hungry","lonely","scared","tired")
    return (vecs[as.integer(x)])
  }
  output$There1 <- renderPrint({
    
    cat("Linear SVM Predictions", Cater(Classing),"\n")
    cat("Radial SVM Predictions",Cater(Classing1),"\n")
    cat("Sigmoid SVM Predictions",Cater(Classing2),"\n")
    cat("Polynomial SVM Predictions",Cater(Classing3),"\n")
    
    

    
    })
    }
  )
  
 
}

# Run the application
shinyApp(ui = ui, server = server)