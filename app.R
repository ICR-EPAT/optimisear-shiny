library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(magrittr)
library(dplyr)
library(ggsurvfit)
library(ggh4x)
#library(bslib)

ui <- dashboardPage(
  title = "shiny",
  dashboardHeader(title = span(tags$img(src = "ICR_logo.png"),
                               "OPTIMISE-AR"),
                  titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("App", tabName = "app", icon = icon("table")),
      menuItem("GraphB", tabName = "GraphB", icon = icon("sun")),
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
                                                 selected = NULL, inline = TRUE)), ),
          fluidRow(
            column(1,  plotOutput("graphA", width = "600px"))
          ),
          fluidPage(
            fileInput("upload", "Upload data file", accept = c(".csv"))
          ),
           fluidPage(
             DTOutput("dynamic")
           ),
        )
      ),
      ############################################ GRAPH B 
      tabItem(tabName = "GraphB",
        fluidRow(
          textOutput("textHeader")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdose1", label = "Dosage",
                                                 selected = NULL, inline = TRUE)), ),
          fluidRow(
            column(3, sliderInput("tpoint", label = "Endpoint of interest", min = 1, max = 1, value = 0, step = 1))),
          fluidRow(
            column(3, selectInput("toxic", label = "Toxicity of interest", choices = NULL ))),
          fluidRow(
            column(1,  plotOutput("graphB", width = "600px"))
          ),
          fluidPage(
            fileInput("upload", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      ###################################################graph b 
      tabItem(tabName = "description",
        h4("About this site"),
        p("We used the skeleton of the GenAlpha Shiny app to run a trail for only Graph A (KP graph) using the dummy variable"),
        p("Authors: George (Server) and Karrar (UI)"),
        p("Maintainers: Rachel Alcraft, ICR Scientific Computing"),
      ),
      tabItem(tabName = "history",
              p("v1.0 - Internal ICR release 18/01/2025"))

    )
  ),
)
###############################################################################
server <- function(input, output, session) {
  output$textHeader <- renderText(
                                  {"Kaplan-Meier Graph"})
  
  #Import the data
  ordinal_data <- reactive({
    req(input$upload)
    ordinal_data <- read.csv(input$upload$datapath, header = TRUE, sep = ",")[, -1]
    ordinal_data
  })

  #Update the selection boxes based on dataset
  observe({
    req(input$upload)
    data <- ordinal_data()
    numdose <- unique(data$dose)
    updateCheckboxGroupInput(session, "numdose", label = "Dosage", choices = numdose, selected = NULL)
  })


  #Define the relevant variables
  no_dose <- reactive({
                       req(input$numdose)
                       as.numeric(input$numdose)})
  #Filter the data
  summary_df <- reactive({
                          req(input$upload)
                          req(input$numdose)
                          ordinal_data <- ordinal_data()
                          ordinal_data <- ordinal_data %>% filter(dose %in% no_dose())
                          ordinal_data <- ordinal_data %>%
                            group_by(id, toxicity) %>%
                            mutate(base_adj = grade-grade[timepoint == 0])
                          ordinal_data <- ordinal_data %>% filter(is.na(base_adj) == FALSE)
                          summary_df <- ordinal_data %>%
                            group_by(id) %>%
                            summarise(
                              time = if (any(base_adj == 3)) min(timepoint[base_adj == 3]) else max(time_last_report),
                              event = as.integer(any(base_adj == 3))  # 1 if grade 3 occurred, 0 if censored
                            )
                          summary_df <- cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))
                          colnames(summary_df)[5] <- "dose"
                          summary_df})

  output$graphA <- renderPlot({
                               req(input$upload)
                               survfit2(Surv(time, event) ~ dose, data = summary_df()) %>%
                                 ggsurvfit(linewidth = 1.5) +
                                 add_censor_mark() +
                                 add_confidence_interval() +
                                 scale_ggsurvfit() + add_risktable()})

################################################ graph B
#Import the data
ordinal_data <- reactive({
  req(input$upload)
  ordinal_data <- read.csv(input$upload$datapath, header = TRUE, sep = ",")[, -1]
  ordinal_data
})

#Update the selection boxes based on dataset
observe({
  req(input$upload)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdose1", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpoint", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateSelectInput(session, "toxic", label = "Toxicity of interest", choices = toxic, selected = NULL)
})

#Define the relevant variables
no_doseb <- reactive({
                     req(input$numdose1)
                     as.numeric(input$numdose1)})

final_t <- reactive({
  req(input$tpoint)
  as.numeric(input$tpoint)
})

toxic <- reactive({
  req(input$toxic)
  as.numeric(input$toxic)
})

#Filter the data

prop <- reactive({
  req(input$numdose1)
  req(input$tpoint)
  req(input$upload)
  ordinal_data <- ordinal_data()
  ordinal_data$grade[is.na(ordinal_data$grade)] <- "Unreported"

  n_pat <- ordinal_data %>%
    filter(timepoint == 0, toxicity == toxic()) %>%
    group_by(dose) %>%
    summarise(n.pat = n())

  prop <- ordinal_data %>%
    group_by(toxicity, timepoint, dose) %>%
    count(grade) %>%
    left_join(n_pat, by = "dose") %>%
    mutate(proportion = n / n.pat) %>%
    dplyr::select(-n)

  colnames(prop) <- c("toxicity", "timepoint", "dose", "grade", "n.pat", "proportion")

  prop <- prop %>% dplyr::select(-n.pat)

  missing_layer <- prop %>%
    group_by(toxicity, timepoint, dose) %>%
    summarise(proportion = 1 - sum(proportion), .groups = "drop") %>%
    filter(proportion > 0)

  missing_layer <- cbind(missing_layer, "Discontinued")
  colnames(missing_layer) <- c("toxicity", "timepoint", "dose", "proportion", "grade")

  missing_layer <- missing_layer %>%
    dplyr::select(names(prop))

  prop$grade <- as.character(prop$grade)
  prop <- bind_rows(prop, missing_layer)

  prop$toxicity <- as.factor(prop$toxicity)
  prop$grade <- as.factor(prop$grade)
  prop$dose <- as.factor(prop$dose)
  prop$timepoint <- as.factor(prop$timepoint)

  prop$dose <- factor(prop$dose, levels = no_doseb())
  prop$grade <- factor(prop$grade, levels = c("Discontinued", "Unreported", 5:1))
  prop <- prop %>% filter(dose %in% no_doseb())
  prop <- prop %>% filter(timepoint %in% c(0:final_t()))
  prop
})


#Graph labels

no_patient <- reactive({
  req(input$upload)
  ordinal_data <- ordinal_data()
  no_patient <- ordinal_data %>%
    filter(timepoint == 0, toxicity == toxic()) %>%
    group_by(dose) %>%
    summarise(n.pat = n())
  no_patient
})
facet_labels <- reactive({
  req(input$upload)
  req(input$numdose1)
  no_patient<-no_patient()
  facet_labels <- paste("Dose level", no_doseb(), sep = " ")
  facet_labels <- paste(facet_labels,"(N=", sep = " ")
  facet_labels <- paste(facet_labels, no_patient$n.pat, sep = "")
  facet_labels <- paste(facet_labels, ")", sep = "")
  facet_labels
})


#Graph output
output$graphB <- renderPlot({
  req(input$upload)
  prop<-prop()
  prop %>% filter(toxicity == toxic()) %>% ggplot(aes(x = as.factor(timepoint))) +
    geom_bar(aes(x = as.factor(timepoint),
                 y = proportion,
                 fill = as.factor(grade)),
             stat = "identity",
             position = "stack") +
    scale_fill_manual(values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "grey", "grey30")),
                      labels = rev(
                        c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported", "Discontinued")
                      ),
                      guide = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 14) +
    facet_wrap2(~as.factor(dose),
                nrow = 3,
                strip.position = "left",
                axes = "all",
                labeller = as_labeller(facet_labels())) +
    labs(x = "Timepoint",
         y = "Proportion",
         fill = "Severity Grade")
})
 }
##################################################################################

shinyApp(ui, server)
