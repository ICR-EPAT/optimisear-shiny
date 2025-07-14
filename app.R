library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(magrittr)
library(dplyr)
library(ggsurvfit)
#library(bslib)

ui <- dashboardPage(
  title = "shiny",
  dashboardHeader(title = span(tags$img(src = "ICR_logo.png"),
                               "Kaplan-Meier graph test - George and Karrar"),
                  titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("App", tabName = "app", icon = icon("table")),
      menuItem("Description", tabName = "description", icon = icon("book")),
      menuItem("Version History", tabName = "history", icon = icon("history"))
    )
  ),
  dashboardBody(
    tags$style(
      HTML("#alldata {overflow:auto}")
    ),
    # this is here to demonstrate that the page can be styled with standard css/html
    tags$head(tags$style(HTML(
      ".content-wrapper{background-color:AliceBlue !important; padding:10px; }            
      .main-header .logo {
      background-color:DarkSeaGreen !important;
      text-align:left !important;
      color:DimGray !important;
      font-size:28px;}"
    ))),
    tabItems(
      tabItem(tabName = "app",
        fluidRow(
          textOutput("textHeader")
        ),
        fluidRow(
          fluidRow(
            column(3,  checkboxGroupInput("numdose", label = "Dosage",
                                    choices = c("1", "2", "3"), selected = NULL, inline = TRUE)),),
          fluidRow(
            column(1,  plotOutput("graphA", width = "600px"))
            ),
            fluidPage(
            fileInput("upload","Upload data file", accept = c(".csv"))
        ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      tabItem(tabName = "description",
        h4("About this site"),
        p("We used the skeleton of the GenAlpha Shiny app to run a trail for only Graph A (KP graph) using the dummy variable"),
        p("Authors: George (Server) and Karrar (UI)"),
        p("Maintainers: Rachel Alcraft, ICR Scientific Computing"),
      ),
      tabItem(tabName = "history",
              p("v1.0 - Internal ICR release 18/01/2025"))

    )
  )
)
###############################################################################
server <- function(input, output, session) {
  output$textHeader <- renderText(
                                  {"Kaplan-Meier Graph"})
  #import data
  reactive({
    req(input$numdose)
    no_dose<-input$numdose
  })

  summary_df<-reactive({
            req(input$upload)
            ordinal_data <- read.csv(input$upload$datapath,header = TRUE,sep = ",")[,-1]
            ordinal_data <- ordinal_data %>% group_by(id, toxicity) %>% mutate(base_adj = grade-grade[timepoint == 0])
            ordinal_data <- ordinal_data %>% filter(is.na(base_adj) == FALSE)
            summary_df<-ordinal_data %>%
              group_by(id) %>%
              summarise(
                time = if (any(base_adj == 3)) min(timepoint[base_adj == 3]) else max(time_last_report),
                event = as.integer(any(base_adj == 3))  # 1 if grade 3 occurred, 0 if censored
              ) 
              summary_df<-cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))
              colnames(summary_df)[5] <- "dose"
              summary_df          
  })

 
  
  # print(summary(ordinal_data)) 
  #Define the relevant variables
  #no_dose <- reactive({
             #         as.numeric(input$numdose)})    #K# i think there is an issue in reading the data
  
  #ordinal_data <- ordinal_data %>% filter(dose %in% no_dose)
  output$graphA<-renderPlot({
    req(input$upload)
    survfit2(Surv(time, event) ~ dose, data = summary_df()) %>%
    ggsurvfit(linewidth = 1.5) +
    add_censor_mark() +
    add_confidence_interval() +
    scale_ggsurvfit() + add_risktable()
  })}
 
##################################################################################
shinyApp(ui, server)