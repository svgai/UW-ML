# install.packages("shiny")
library("shiny")
library("data.table")
library("glmnet")
library("mice")


# Set it to the directory with the data
setwd("~/Projects/UW/data/Titanic/")
d <- fread("train.csv", stringsAsFactors=F)
d$y = d$Survived

# Basic feature fixing
d[,Embarked:=factor(Embarked)]
# Above is (almost) the same as:
# d$Embarked <- factor(Embarked)
d[,Pclass:=factor(Pclass)]
d[,IsFemale:=as.integer(d$Sex=="female")]
d$Embarked[d$Embarked==""] <- "S"
d[,Embarked:=factor(d$Embarked)]

imp.dt <- d[,c("y", "Age", "Fare","Embarked", "IsFemale", "Pclass"), with=F]
imp.m <- matrix(0, 6,6); imp.m[2,2:6] <- 1
Age.imp <- mice(imp.dt, print=F, seed=123, predictorMatrix=imp.m)
summary(Age.imp)

fit <- glm.mids(y~., data=Age.imp, family = binomial)
#summary(fit)
#summary(pool(fit))

d[,Age.Mice:=complete(Age.imp)$Age]

# Train model
X.col <- c("y", "Pclass", "Age.Mice", "SibSp", "Parch", "Fare", "Embarked", "IsFemale")
X.train <- d[!is.na(y),X.col, with=F]
y.train <- d[!is.na(y),y]

fit <- glm(y~., family=binomial, data=X.train)
summary(fit)

# A bit of cheat for a nice figure
prob.train <- as.vector(predict(fit, X.train, type="response"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Would I surivive Titanic?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput(inputId = "Age.Mice",
                  label = "Age:",
                  min = 0,
                  max = 100,
                  value = 30),
      
      sliderInput(inputId = "Fare",
                  label = "Fare:",
                  min = 0,
                  max = 500,
                  value = 100),

      sliderInput(inputId = "SibSp",
                  label = "# of siblings / spouses aboard:",
                  min = 0,
                  max = 8,
                  value = 1),

      sliderInput(inputId = "Parch",
                  label = "# of parents / children aboard:",
                  min = 0,
                  max = 6,
                  value = 1),
      
      selectInput(inputId = "Pclass", 
                  label = "Passenger Class:", 
                  choices = list("1" = 1, "2" = 2, "3" = 3), selected = 1),
      
      selectInput(inputId = "Embarked", 
                  label = "Embarked:", 
                  choices = list("C" = "C", "Q" = "Q", "S" = "S"), selected = 1),
  
      selectInput(inputId = "IsFemale", 
                  label = "Gender", 
                  choices = list("Male" = 0, "Female" = 1), selected = 0)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      verbatimTextOutput("Probability"),
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to predict the probability of survival ----
server <- function(input, output) {
  
  dt <- reactive({
    dt <- data.table("Pclass"=factor(input$Pclass, levels=levels(X.train$Pclass)),
                     "Age.Mice"=input$Age.Mice,
                     "SibSp"=input$SibSp,
                     "Parch"=input$Parch,
                     "Fare"=input$Fare,
                     "Embarked"=factor(input$Embarked, levels=levels(X.train$Embarked)),
                     "IsFemale"=as.integer(input$IsFemale))
    str(dt)
    dt
  })
  
  prob <- reactive({
    as.vector(predict(fit, dt(), type="response"))
  })
  
  output$Probability = renderText({
    sprintf("You have %.2f%% chance of survival", prob()*100)
  })

  output$distPlot <- renderPlot({
    hist(prob.train, breaks = 0:20/20, col = "#75AADB", border = "white",
         xlab = "Probability of Survival", main="")
    
    hist(rep(prob(),20), breaks = 0:20/20, col = "red", border = "white", main="", add=T)
    
  })
  
}

shinyApp(ui, server)
