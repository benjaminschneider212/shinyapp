# Define UI for dataset viewer app ----
library(shiny)
ui <- fluidPage(
  headerPanel("Presidential Forecasts"), #giving the title
  # App title ----
  titlePanel("Results of Presidential Forecasts from 1952-2008"), #doing the subtitle
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "method", #create the drop down menu
                  label = "Choose a Method:",
                  choices = c('Actual', 'Campbell', 'Lewis-Beck', 'EWT2C2', 'Fair', 'Hibbs', 'Abramowitz'), #getting the chouces out
                  selected="Actual" #the default choice
      ),
      #end selector
      # Input: Specification of range within an interval ----
      sliderInput("range", h3("Years:"),#creating the basic inputs for the slider
                  min = 1952, max = 2008, #the years of the data 
                  value = c(1952,2008), step=4, sep="" #this makes sure that the input is in fact by the actual election
      )   
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( #getting the main panel inputs in
      plotOutput("presForecast", click = "plotmethod"),
      verbatimTextOutput("info"),
      tableOutput("view")
    )
))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) { #beginning to set up the server
  library(EBMAforecast)
  data(presidentialForecast)
  dataset<-cbind(as.integer(seq(1952, 2008,4)), presidentialForecast) #put the data in
  colnames(dataset)[1]<-"Year" #formating the data for use
  
  # year slider
  sliderValues <- reactive({ #creating the slider for years
    
    data.frame(
      Name = "Year Range",
      Value = as.character(paste(input$Year, collapse = " ")),
      stringsAsFactors = FALSE)
  })
  #plot 
  output$presForecast<-renderPlot({
    plot(dataset$Actual[1:(((input$range[2]-input$range[1])/4)+1)], type="l", axes=F, main="Predicted and Actual Incumbent Vote Share", xlab="Year", 
         ylab="Incumbent Vote Share", col="blue") #plots the actual result automatically
    axis(1,at= seq(from=1,to=(((input$range[2]-input$range[1])/4)+1),by=1), #creates the ability of the range of the graph to slide.
         labels = as.character(seq(input$range[1], input$range[2], 4)))
    legend("topright",
           legend=c("Actual", paste(input$method)), 
           lty=c(1,5), col=c("blue","red"), bty="n") #creates the legend with the specific chosen method
    axis(side=2, las=2)
    if(input$method=="Campbell"){ #creates the plot for one of the predictions in the drop down
      points(dataset$Campbell, type="l", col="red", lty=5)
    }
    if(input$method=="Lewis-Beck"){ #creates the plot for one of the predictions in the drop down
      points(dataset[,3], type="l", col="red", lty=5)
    }
    if(input$method=="EWT2C2"){ #creates the plot for one of the predictions in the drop down
      points(dataset[,4], type="l", col="red", lty=5)
    }
    if(input$method=="Fair"){ #creates the plot for one of the predictions in the drop down
      points(dataset[,5], type="l", col="red", lty=5)
    }
    if(input$method=="Hibbs"){ #creates the plot for one of the predictions in the drop down
      points(dataset[,6], type="l", col="red", lty=5)
    }
    if(input$method=="Abramowitz"){ #creates the plot for one of the predictions in the drop down
      points(dataset[,7], type="l", col="red", lty=5)
    }
  })
  #click output
  output$info <- renderText({
    paste0("x=", input$plotmethod$x, "\ny=", input$plotmethod$y)}) #this allows users to click a point and get the coordinates
  
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)
