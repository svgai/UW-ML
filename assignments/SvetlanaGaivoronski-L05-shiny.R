library("shiny")
library("data.table")
library("gender")
library('caret')

# build the model
wage_url = "https://data.seattle.gov/api/views/2khk-5ukd/rows.csv?accessType=DOWNLOAD"
wage = read.csv(wage_url, stringsAsFactors=F)

wageDT <- as.data.table(wage)

tmp <- predict(dummyVars(" ~ Department", data = wageDT, sep = "."), newdata = wageDT)
wageDT <- data.table(wageDT, tmp)
oldcols <- colnames(tmp)
newnames <- gsub("Department", "Department.", gsub(" ", ".", gsub("&", "", oldcols)))
setnames(wageDT, old = oldcols, new = newnames)

# Commands
genderByName <- gender(unique(wageDT$First.Name))
is.male <- function(name) {
  record <- genderByName[genderByName$name == name,]
  if (dim(record)[1] == 0) { return(0) }
  log.odds <- log(record$proportion_male) - log(record$proportion_female)
  if (log.odds < -5) { return(-5) }
  if (log.odds > 5)  { return(5) }
  return(log.odds)
}
wageDT$IsMale.Prob <- sapply(wageDT$First.Name, is.male)
wageDT$IsMale <- wageDT$IsMale.Prob > 0

trainData <- wageDT

features.names <- setdiff(names(wageDT), c("Department", "Last.Name", "First.Name", "Job.Title"))
trainData <- trainData[,features.names,with=F]
lm.all <- lm(Hourly.Rate~.-Department.Seattle.Public.Utilities, data=trainData)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Wages"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "Department", 
                  label = "Department", 
                  choices = unique(wageDT$Department), selected = "Arts and Culture"),
      
      selectInput(inputId = "Job.Title", 
                  label = "Job Title:", 
                  choices = unique(wageDT$Job.Title), selected = "Accountant"),
      
      selectInput(inputId = "IsMale", 
                  label = "Gender", 
                  choices = list("Male" = 1, "Female" = 0), selected = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("Hourly.Rate"),
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to predict the probability of survival ----
breaks <- seq(2, 180, length.out=90)
server <- function(input, output) {
  
  dt <- reactive({
    dt <- data.table("Department"=input$Department,
                     "Job.Title"=input$Job.Title,
                     "IsMale"=as.logical(as.integer(input$IsMale)),
                     "IsMale.Prob"=as.integer(input$IsMale) * 10 - 5)
  })
  
  qwe <- reactive({
    dt.calculated <- dt()
    deps <- data.table(Department=c(dt.calculated$Department, wageDT$Department))
    x <- data.table(predict(dummyVars(" ~ Department", data = deps, sep = "."), newdata = deps))
    dt.calculated <- data.table(dt.calculated[1,], x[1,])
    
    oldcols <- colnames(x)
    newnames <- gsub("Department", "Department.", gsub(" ", ".", gsub("&", "", oldcols)))
    setnames(dt.calculated, old = oldcols, new = newnames)
    
    dt.calculated$Department <- NULL
    dt.calculated$Job.Title <- NULL
    
    predict(lm.all, dt.calculated)
  })
  
  output$distPlot <- renderPlot({
    hist(wageDT$Hourly.Rate, breaks = breaks, col = "#75AADB", border = "white",
         xlab = "Hourly rate wage", main="", xlim=c(0, 100))
    hist(rep(qwe(),200), breaks = breaks, col = "red", border = "white", main="", xlim=c(0, 100), add=T)
    
  })
  
}

shinyApp(ui, server)

