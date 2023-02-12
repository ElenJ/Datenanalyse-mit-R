library(shinydashboard)
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(caret)

#helper Functions
retrieve_adult <- function(){
  # Datensatz
  adult.data <- read.table("data/adult.data", sep=",")
  adult.test <- read.table("data/adult.test", sep=",")
  adult <- rbind(adult.data, adult.test)
  colnms <- c("age", 
              "workclass", 
              "fnlwgt",
              "education",
              "education-num",
              "marital-status",
              "occupation",
              "relationship",
              "race",
              "sex",
              "capital-gain",
              "capital-loss",
              "hours-per-week",
              "native-country",
              "class")
  colnms <- make.names(colnms)
  colnames(adult) <- colnms
  adult <- data.frame(lapply(adult, function(x) {
    gsub("^[[:space:]]", "", x)}))
  adult[adult == "?"] <- NA
  adult$class <- gsub("[[:punct:]]$","", adult$class)
  numeric_cols <- c("age", "fnlwgt", "education.num" ,"capital.gain",   "capital.loss", "hours.per.week", "age_log")
  for (n in numeric_cols){
    cn <- which(colnames(adult)==n)
    adult[,cn] <-  as.numeric(adult[,cn])
  }
  return(adult)
}

hot_one_adult <- function(mydf){
  mydf$capital.gain <- ifelse(mydf$capital.gain > 0, 1, 0)
  mydf$capital.loss <- ifelse(mydf$capital.loss > 0, 1, 0)
  numeric_cols <- c("age", "fnlwgt", "education.num" ,"capital.gain",   "capital.loss", "hours.per.week")
  cat_col <- setdiff(colnames(mydf), c("class",numeric_cols ))
  dummy <- dummyVars(" ~ .", data=mydf[,cat_col], fullRank = TRUE)
  newdata <- data.frame(predict(dummy, newdata = mydf)) 
  mydf <- cbind(mydf[,c("class",numeric_cols )], newdata)
  mydf$class <- ifelse(mydf$class ==">50K", "over50K","less50K")
  mydf$age_log <- log(mydf$age)
  nzv <- nearZeroVar(mydf)
  df <- mydf[, -nzv]
  return(df)
}
ui <- dashboardPage(
  dashboardHeader(title = "Adult dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datensatz", tabName = "datensatz", icon = icon("dashboard")),
      menuItem("Vorhersage", tabName = "vorhersage", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "datensatz",
              fluidRow(
                box(plotOutput("histogram", height = 250)),
                
                box(
                  title = h3("Controls")
                  , uiOutput("selectColumn.ui")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "vorhersage",
              fluidRow(
                column(6 
                       , h2("Inputelemente")
                       , sliderInput("age", "Your age", min = 18, max = 100, value = 25)
                       , numericInput("workinghrs", "Working hours", value = 40)  
                       , checkboxInput("sex", strong("Are you male?"), value = TRUE)
                       , checkboxInput("race", strong("Are you white?"), value = TRUE)
                       , checkboxInput("marital", strong("Are you merried?"), value = TRUE)
                       , checkboxInput("children", strong("Do you have children?"), value = TRUE)
                       , radioButtons("workclass", "Workclass",
                                      choices = list("Government" = 1, "Private" = 2,"Self-employed" = 3),selected = 2)
                       , radioButtons("education", "Workclass",
                                      choices = list("Bachelors" = 1, "HS.grad" = 2,"Masters" = 3, "Some.college" = 4),selected = 1)
                       , radioButtons("occupation", "Occupation",
                                      choices = list("Craft.repair" = 1, "Exec.managerial" = 2,"Machine.op.inspct" = 3, "Other.service" = 4, "Prof.specialty" = 5, "Sales" = 6, "Transport.moving" = 7),selected = 6)
                       , checkboxInput("us", strong("Are you US native?"), value = TRUE)),
                column(6
                       , h2("Verdienstvorhersage", style="color:red")
                       , verbatimTextOutput("prediction"))
              )
              
      )
    )
  )
)

server <- function(input, output) {
  adult <- retrieve_adult()
  #Auswahl des Spalte
  output$selectColumn.ui <- renderUI({
    numeric_cols <- c("age", "fnlwgt", "education.num" ,"capital.gain",   "capital.loss", "hours.per.week")
    selectInput("selectColumn", h3("Select parameter"), 
                choices = numeric_cols, selected = numeric_cols[1])
  })
  output$histogram <- renderPlot({
    if (is.null(input$selectColumn)) {
      mycol = which(colnames(adult)=="age")
    } else {
      mycol = which(colnames(adult)==input$selectColumn)
    }
    ggplot(adult, aes(x= adult[,mycol], fill=class)) +
      geom_histogram() +
      ggtitle(paste0("Histogram von ", input$selectColumn)) + 
      theme_classic() 
  })
  
  #Elemente für das Vorhersagetab
  adult_ho <- hot_one_adult(adult)
  preProcValues <- preProcess(adult_ho, method = c("center", "scale"))
  model_knn <- readRDS(paste0(getwd(), "/data/model_knn.rds"))
  
  # Input des users
  test_user_df <- reactive({
    data.frame("fnlwgt"=median(adult_ho$fnlwgt, na.rm=T),
               "education.num"=median(adult_ho$education.num, na.rm=T), 
               "capital.gain"=0,#median(adult_ho$capital.gain, na.rm=T),
               "hours.per.week"=input$workinghrs,                   
               "age"=input$age,      
               "workclassLocal.gov"=if (input$workclass == 1) 1 else 0,
               "workclassPrivate"=if (input$workclass == 2) 1 else 0,
               "workclassSelf.emp.not.inc"=if (input$workclass == 3) 1 else 0,
               "educationBachelors"=if (input$education == 1) 1 else 0,
               "educationHS.grad"=if (input$education == 2) 1 else 0,
               "educationMasters"=if (input$education == 3) 1 else 0,
               "educationSome.college"=if (input$education == 4) 1 else 0,
               "marital.statusMarried.civ.spouse"=if (input$marital) 1 else 0,
               "marital.statusNever.married"=if (input$marital) 0 else 1,
               "occupationCraft.repair"=if (input$occupation == 1) 1 else 0,
               "occupationExec.managerial"=if (input$occupation == 2) 1 else 0,
               "occupationMachine.op.inspct"=if (input$occupation == 3) 1 else 0,
               "occupationOther.service"=if (input$occupation == 4) 1 else 0,
               "occupationProf.specialty"= if (input$occupation == 5) 1 else 0,
               "occupationSales"= if (input$occupation == 6) 1 else 0,
               "occupationTransport.moving"= if (input$occupation == 7) 1 else 0,
               "relationshipNot.in.family"= if (input$marital) 0 else 1, 
               "relationshipOwn.child"= if (input$children) 1 else 0,
               "relationshipUnmarried"= if (input$marital) 1 else 0,                           
               "raceBlack"= if (input$race) 0 else 1, 
               "raceWhite"= if (input$race) 1 else 0,
               "sexMale" = if (input$sex) 1 else 0,
               "native.countryUnited.States" = if (input$us) 1 else 0)
  })
  
  
  #Output des gewählten Verdienstes
  output$prediction <- renderPrint({ 
    test_user <- test_user_df()
    test_user$age_log <- log(test_user$age)
    test <- predict(preProcValues, test_user)
    prediction <- predict(model_knn, newdata = test)
    res <- if(prediction == "less50K" ) "<50K" else ">50K" 
    print(paste0("Ihr Verdienst ist ", res))
  })
}

shinyApp(ui, server)