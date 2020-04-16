## app.R ##
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
data <- read.csv("C:/Users/daheekim/OneDrive - Ralco Nutrition, Inc/ShinyR_Metadata/Metadata.csv")
setwd("C:/Users/daheekim/OneDrive - Ralco Nutrition, Inc/ShinyR_Metadata/")
data$Tank.Tenure <- as.numeric(data$Tank.Tenure)

ui <- navbarPage("trushrimp data center", theme = shinytheme("flatly"),
                                tabPanel("Reef",
                                   sidebarLayout(
                                     sidebarPanel(width=2,
                                        ## conditionalPanel() functions for selected tab
                                       conditionalPanel(condition="input.tabselected==1",
                                                        selectInput("TheFile", "Select Dataset", 
                                                                    choices = c("Metadata.csv", "AH_final_Mean.csv")),
                                                        selectizeInput('Cohorts', 'Select Cohort:', choices = c(levels(data$Cohort))),
                                                        selectizeInput('Tanks', 'Select Tank:', choices = c( levels(data$Tank))),
                                                       # Copy the line below to make a slider range 
                                                        sliderInput(inputId = "Tenures", 
                                                                   label = "Tank Tenure Range", min = min(na.omit(data$Tank.Tenure)), 
                                                                   max =  max(na.omit(data$Tank.Tenure)), value = c(40, 60)),
                                                       "Please make sure to select Cohort, Tank, Total Tenure variables!",
                                                        selectizeInput(inputId = 'Variables', 
                                                                      label = 'Variables:', 
                                                                      choices = c( colnames(data)), multiple = TRUE)
                                       ),
                                       conditionalPanel(condition="input.tabselected==2",
                                                        selectizeInput('Cohorts2', 'Select Cohort:', choices = c(levels(data$Cohort))),
                                                        selectizeInput('Tanks2', 'Select Tank:', choices = c( levels(data$Tank))))
                                        
                                       
                                     ),
                                     
                                     
                                     mainPanel(
                                       h2("REEF"),
                                       tabsetPanel(
                                       tabPanel("Table",value=1, tableOutput("table")),
                                       tabPanel("Statistics",value=1, verbatimTextOutput("table2")),
                                       #tabPanel("Table3", verbatimTextOutput("table3")),
                                       tabPanel("Table2", value=2,DT::dataTableOutput("table3")),id = "tabselected"
                                     )
                                   )
                                  )
                                  ),
                                tabPanel("Hatchery",
                                         h2("Trafficker tab"),
                                         mainPanel(
                                           h2("REEF"),
                                           tabsetPanel(
                                             tabPanel("Table", DT::dataTableOutput("table4"))
                                           )
                                         )
                                 ),
                 
                                 tabPanel("Innovation Center",
                                          h2("Trafficker tab")
                                 ),
                                 tabPanel("Analytic Center",
                                          h2("Trafficker tab")
                                 )
)

server <- function(input, output,session) {
  myCSV <- reactive({
    read.csv(input$TheFile)
  })
  
  # Selectize 2 choice's list(1)
  Tanks.choice <- reactive({
    myCSV() %>% 
      filter(Cohort ==input$Cohorts) %>%
      pull(Tank)
  })
  # Observe(1)
  observe({
    updateSelectizeInput(session, "Tanks", choices = Tanks.choice())
  })
  
  
  # Selectize 2 choice's list(2) 
  Tanks.choice2 <- reactive({
    myCSV() %>% 
      filter(Cohort ==input$Cohorts2) %>%
      pull(Tank)
  })
  # Observe(2)
  observe({
    updateSelectizeInput(session, "Tanks2", choices = Tanks.choice2())
  })
  
  # Table
  tab <- reactive({ 
    myCSV() %>% 
      select(input$Variables) %>%
      filter(Cohort == input$Cohorts) %>% 
      filter(Tank == input$Tanks) %>%
      filter(Tank.Tenure> input$Tenures[1],Tank.Tenure < input$Tenures[2])
    
  })
  output$table <- renderTable({ 
    tab()
  })
  
  
  # Table2
  tab2 <- reactive({ 
    myCSV() %>% 
      select(input$Variables) %>%
      filter(Cohort == input$Cohorts2) %>% 
      filter(Tank == input$Tanks2) %>%
      filter(Tank.Tenure> input$Tenures[1],Tank.Tenure < input$Tenures[2])
    
  })
  
  output$table3 <-   DT::renderDataTable({ 
    DT::datatable(tab2(), extensions = 'Buttons'
                  , options = list( 
                    dom = "Blfrtip"
                    , buttons = 
                      list("copy", list(
                        extend = "collection"
                        , buttons = c("csv", "excel", "pdf")
                        , text = "Download"
                      ) ) # end of buttons customization
                    
                    # customize the length menu
                    , lengthMenu = list( c(10, 20,50, -1) # declare values
                                         , c(10, 20,50, "All") # declare titles
                    ) # end of lengthMenu customization
                    , pageLength = 50
                    
                    
                  ) # end of options
                  
    ) # end of datatables
  })  
  # Staitistics
  output$table2 <- renderPrint({
    tab() %>% 
      summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
  })
  output$table4<-   DT::renderDataTable({ 
    DT::datatable(data, extensions = 'Buttons'
                  , options = list( 
                    dom = "Blfrtip"
                    , buttons = 
                      list("copy", list(
                        extend = "collection"
                        , buttons = c("csv", "excel", "pdf")
                        , text = "Download"
                      ) ) # end of buttons customization
                    
                    # customize the length menu
                    , lengthMenu = list( c(10, 20, -1) # declare values
                                         , c(10, 20, "All") # declare titles
                    ) # end of lengthMenu customization
                    , pageLength = 20
                    
                    
                  ) # end of options
                  
    ) # end of datatables
  })  
}


shinyApp(ui, server)