library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
#library(bslib)

ui <- dashboardPage(
  title = "shiny",
  dashboardHeader(title = span(tags$img(src = "ICR_logo.png"),
                               "OPTIMISE-AR"),
                  titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("App", tabName = "app", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("question")),
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
            column(2,  radioButtons("method", label = "Structure",
                                    choices = c("AlphaFold", "1kzy"), selected = "AlphaFold", inline = TRUE)),
            column(6,  radioButtons("hue", label = "Hue",
                                    choices = c("aa", "element", "bfactor"), selected = "aa", inline = TRUE)),
          ),
          fluidRow(
            column(4,  plotOutput("plot", width = "400px")),
            column(4,  plotOutput("plotyz", width = "400px")),
            column(4,  plotOutput("plotzx", width = "400px")),
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      tabItem(tabName = "about",
        h4("About this site"),
        p("This is a demo shiny-app"),
        p("Authors: Rachel Alcraft"),
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
  ordinal_data <- reactive({
                            req(input$upload)})


  #Define the relevant variables
  no_dose <- reactive({
                      req(input$numdose)})

  ordinal_data <- ordinal_data %>% filter(dose %in% no_dose)
  ordinal_data <- ordinal_data %>% group_by(id, toxicity) %>% mutate(base_adj = grade-grade[timepoint == 0])
  ordinal_data <- ordinal_data %>% filter(is.na(base_adj) == FALSE)

  summary_df <- ordinal_data %>%
    group_by(id) %>%
    summarise(
      time = if (any(base_adj == 3)) min(timepoint[base_adj == 3]) else max(time_last_report),
      event = as.integer(any(base_adj == 3))  # 1 if grade 3 occurred, 0 if censored
    )

  summary_df <- cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))

  colnames(summary_df)[5] <- "dose"
  summary_df

  output$graphA <- survfit2(Surv(time, event) ~ dose, data = summary_df) %>%
    ggsurvfit(linewidth = 1.5) +
    add_censor_mark() +
    add_confidence_interval() +
    scale_ggsurvfit() + add_risktable()
}
##################################################################################
shinyApp(ui, server)