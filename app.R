library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(magrittr)
library(dplyr)
library(ggsurvfit)
library(ggh4x)
library(tidyr)
library(gt)
library(svglite)
library(bslib)

ui <- div(page_navbar(
  title = span(tags$img(src = "ICR_logo.png"), "OPTIMISE-AR"),
  ############################################################################################# Information
  tabPanel("Overview", icon = icon("house"),
  h1("OPTIMISE-AR"),
  card(card_body(
  h3("Work-flow"),
  p("1. Identify the type of data  (Ordinal/Continuous)"),
  p("2. Read about the graph choices on this page"),
  p("3. Go to the data tab"),
  p("- Read about how to format data"),
  p("- Format data and upload OR select the dummy datasets"),
  p("4. Select relevant graphs under the Ordinal or Continuous dropdown"),
  p("5. Adjust the variables to suit your required outputs"),
  p("6. Click download to export any relevant graphs as a .svg file")
  )),
  card(card_body(
  h3("About the graphs")),
  card_body(
  page_navbar(
    title = "Select data type",
    id = "about",
    fillable = TRUE,
    sidebar = sidebar(
      conditionalPanel(
        "input.about == 'Ordinal'",
        radioButtons("about_ordinal", label = NULL, choices = c("GraphA", "GraphB", "GraphH", "GraphI"))
      ),
      conditionalPanel(
        "input.about == 'Continuous'",
        radioButtons("about_continuous", label = NULL, choices = c("GraphC", "GraphD", "GraphE", "GraphF", "GraphG"))
      )
    ),
    nav_panel("Ordinal", fillable = TRUE,
    layout_columns(col_widths = c(9, 3), height = "900px",
      card(card_header("Graph"), card_body(imageOutput("about_ordinal"), height = "800px"), full_screen = TRUE, fillable = TRUE),
      card(card_header("Information"), fill = TRUE,
        card_body(card_title("About"), htmlOutput("ordinal_information_about")),
        card_body(card_title("Advantages"), htmlOutput("ordinal_pros")),
        card_body(card_title("Disadvantages"), htmlOutput("ordinal_cons"))
        ))),
    nav_panel("Continuous", fillable = TRUE,
    layout_columns(col_widths = c(9, 3), height = "900px",
      card(card_header("Graph"), card_body(imageOutput("about_continuous", height = "800px")), full_screen = TRUE, fill = TRUE),
      card(card_header("Information"),
        card_body(card_title("About"), htmlOutput("continuous_information_about")),
        card_body(card_title("Advantages"), htmlOutput("continuous_pros")),
        card_body(card_title("Disadvantages"), htmlOutput("continuous_cons"))
        ))))))),
  ####################################################################################################### Data Sorting + Upload
  tabPanel("Data", icon = icon("arrow-up-from-bracket"),
  card(card_header(strong("How to format data")),
  card_body(
    p("Data must be input as a .csv file with column titles matching the listed below."),
    p("Any columns not fitting the format will be ignored - if not all columns are provided, the app may not run."),
    p("- If a patient doesn't report at a specific timepoint, do not enter any data."),
    p("- If a patient partially responds to a questionnaire at a timepoint, enter all data and use NA in place of an unreported score or grade."),
    p("For an example of data, you can toggle the dummy data on - please note to use your own data, the dummy dataset must be toggled off."),
   layout_columns(
    card(card_header(strong("Ordinal datasets")),
    card_body(
      p(strong("Required columns, {column name} and [input types]:")),
      p("- Patient ID, {id}, [numeric]"),
      p("- Toxicity type, {toxicity}, [numeric - looking to allow characters in new version]"),
      p("- Grade of response, {grade}, [1,2,3,4,5]"),
      p("- Timepoint of current report, {timepoint}, [numeric - must begin with timepoint 0]"),
      p("- Dose Level, {dose}, [numeric]"),
      p("- Final reported time by the patient, {time_last_report}, [numeric]"),
    )),
    card(card_header(strong("Continuous datasets")),
    card_body(
       p(strong("Required columns and input types:")),
       p("- Patient ID, {id}, [numeric]"),
       p("- Functionality characteristic, {functioning}, [numeric - looking to allow characters in new version]"),
       p("- Score in the functionality category, {score}, [numeric]"),
       p("- Timepoint of current report, {timepoint}, [numeric- must begin with timepoint 0]"),
       p("- Dose Level, {dose}, [numeric]"),
    ))
  ))),
  page_sidebar(
    title = "Input data",
  sidebar = sidebar(
  card(card_header("Upload data file"), card_body(fileInput("upload", "Ordinal data", accept = c(".csv"))),
  card_body(fileInput("upload_cts", "Continuous data", accept = c(".csv")))),
  card(card_header("Dummy data"), card_body("For those with no data wishing to test the app, you can select these to test with."),
  card_body(input_switch("dummy_ordinal", "Dummy Ordinal data")),
  card_body(input_switch("dummy_continuous", "Dummy Continuous data")))),
  card(card_header("Ordinal data"), card_body(tableOutput("ordinal_table")), max_height = 250),
  card(card_header("Continuous data"), card_body(tableOutput("continuous_table")), max_height = 250))),
  navbarMenu("Ordinal",
############################################################################################################ A UI
            tabPanel("GraphA", tabName = "GraphA", icon = icon("sun"),
            fluidRow(
          textOutput("textHeaderA")
        ),
        page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(checkboxGroupInput("numdose", label = "Dosage",
                                                 selected = NULL, inline = TRUE)))
               , card(card_header("Download .svg file"), card_body(downloadButton("downloadA")))),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("editA", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popA",
             placement = "right",
             textInput("titleA", "Enter graph title"),
             actionButton("updateA", "Update")
          )), card_body(plotOutput("graphA")))
          )),
  ############################################################################################################ B UI
            tabPanel("GraphB", tabName = "GraphB", icon = icon("sun"),
          fluidRow(
          textOutput("textHeaderB")
        ),
        page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseB", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointB", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    selectInput("toxicB", label = "Toxicity of interest", choices = NULL ))
          ), card(card_header("Download .svg file"), card_body(downloadButton("downloadB"))))
          ,
          card(plotOutput("graphB"))
            )),
############################################################################################################ H UI
         
            tabPanel("GraphH", tabName = "GraphH", icon = icon("sun"), 
            fluidRow(
          textOutput("textHeaderH")
        ),
        page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseH", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointH", label = "Start and endpoint for comparison", min = 0, max = 1, value = c(0,1), step = 1),
                    checkboxGroupInput("toxicH", label = "Toxicity of interest", choices = NULL , inline = TRUE)
          )), 
          card(card_header("Download .svg file"), card_body(downloadButton("downloadH")))),
          card(plotOutput("graphH"))
            )),
############################################################################################################ I UI
            tabPanel("GraphI", tabName = "GraphI", icon = icon("sun"),
            fluidRow(
          textOutput("textHeaderI")
        ),
    page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseI", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointI", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    checkboxGroupInput("toxicI", label = "Toxicity of interest", selected = NULL, inline = TRUE))
          ), 
          card(card_header("Download .svg file"), card_body(downloadButton("downloadI")))),
          card(plotOutput("graphI"))
            ))),
  navbarMenu("Continuous",############################################################################################################ C UI
             tabPanel("GraphC", tabName = "GraphC", icon = icon("moon"),
             fluidRow(
          textOutput("textHeaderC")
        ),
                page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseC", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                   sliderInput("tpointC", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                   selectInput("functioningC", label = "Functioning of interest", choices = NULL ))), 
                   card(card_header("Download .svg file"), card_body(downloadButton("downloadC")))
          ),
          card(plotOutput("graphC"))
            )),
############################################################################################################ D UI
             tabPanel("GraphD", tabName = "GraphD", icon = icon("moon"), fluidRow(
          textOutput("textHeaderD")
        ),
            page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    sliderInput("tpointD", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    checkboxGroupInput("numdoseD", label = "Dosage",choices = NULL),
                    uiOutput("select_idD"),
                    selectInput("functioningD", label = "Functioning of interest", choices = NULL))),
                    card(card_header("Download .svg file"), card_body(downloadButton("downloadD")))),
                    card(plotOutput("graphD"))
        )),
############################################################################################################ E UI
              
             tabPanel("GraphE", tabName = "GraphE", icon = icon("moon"),  fluidRow(
          textOutput("textHeaderE")
        ),
         page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
            checkboxGroupInput("numdoseE", label = "Dosage", selected = NULL, inline = TRUE),
            sliderInput("tpointE", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            selectInput("functioningE", label = "Functioning of interest", choices = NULL ))),
            card(card_header("Download .svg file"), card_body(downloadButton("downloadE")))),
            card(plotOutput("graphE"))
        )),
############################################################################################################ F UI
           tabPanel("GraphF", tabName = "GraphF", icon = icon("moon"),     fluidRow(
          textOutput("textHeaderF")
        ),
          page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                   checkboxGroupInput("numdoseF", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
          sliderInput("tpointF", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
          checkboxGroupInput("functioningF", label = "Functioning of interest", selected = NULL, inline = TRUE))),
          card(card_header("Download .svg file"), card_body(downloadButton("downloadF")))),
           card(gt_output("graphF"))
           )),
           ############################################################################################################ G UI 
    
             tabPanel("GraphG", tabName = "GraphG", icon = icon("moon"),          fluidRow(
          textOutput("textHeaderG")
        ),
          page_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
          checkboxGroupInput("numdoseG", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
          sliderInput("tpointG", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
          checkboxGroupInput("functioningG", label = "Functioning of interest", selected = NULL, inline = TRUE))),
          card(card_header("Download .svg file"), card_body(downloadButton("downloadG")))),
          card(plotOutput("graphG"))
      ))),
################################################################################################################## description tab
      tabPanel("Description", tabName = "description", icon = icon("book"), h4("About this site"),
      layout_columns(
      card(card_header("Background"), card_body(
        p("Recently, there has been an increasing interest in the use of PRO data to supplement assessment of tolerability in early phase dose-finding trials."),
        p("However, there are no guidelines on how analysis should be conducted or what visualisations should be made."),
        p("This app aims to provide and easy to use platform to assist in analysis and visualisations using current recommended methods."))),
        card(card_header("OPTIMISE-AR"), card_body(
        p("This is an early public release of the app with a work in progress UI. There are a few known bugs, if you find any additional, please do get in touch so they can be patched."),
        p("Authors: George Knight, Emily Alger, Karrar Shareiyat"),
        p("Maintainers: Rachel Alcraft, ICR Scientific Computing"))))),
################################################################################################################## Version history
      tabPanel("Version History", tabName = "history", icon = icon("history"), 
      card(card_header("Versions"),
      card_body(p("v1.0 - Internal ICR release 06/08/2025")))), bg = "DarkSeaGreen"))

#######################################################################################################################   SERVER SIDE 

server <- function(input, output, session) {
  output$textHeaderA <- renderText({"Kaplan-Meier Graph"})
  output$textHeaderB <- renderText({"Proportional severity of doses across timepoints plot"})
  output$textHeaderC <- renderText({"Mean change from baseline reported value across timepoints plot"})
  output$textHeaderD <- renderText({"Patient experience plot"})
  output$textHeaderE <- renderText({"Tolerability Boxplot"})
  output$textHeaderF <- renderText({"Change from baseline table"})
  output$textHeaderG <- renderText({"Mean change in functionalities plot"})
  output$textHeaderH <- renderText({"Butterfly plot"})
  output$textHeaderI <- renderText({"Toxicity % plot"})
  ############################################################################################Overview page 
about_ordinal <- reactive({input$about_ordinal})
about_continuous <- reactive({input$about_continuous})


############################################# GRAPH IMAGE
output$about_ordinal <- renderImage({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "GraphA"){
    list(src = "www/GraphExamples/graphA.jpeg")
  }
  else if (about_ordinal == "GraphB") {
     list(src = "www/GraphExamples/graphB.jpeg", height = "800px")
  }
  else if (about_ordinal == "GraphH") {
     list(src = "www/GraphExamples/graphH.svg", height = "800px")
  }
  else if (about_ordinal == "GraphI") {
     list(src = "www/GraphExamples/graphI.svg", height = "800px")
  }
}, deleteFile = FALSE)

output$about_continuous <- renderImage({
  about_continuous <- about_continuous()
  if (about_continuous == "GraphC"){
    list(src = "www/GraphExamples/graphC.jpeg")
  }
  else if (about_continuous == "GraphD") {
     list(src = "www/GraphExamples/graphD.svg", height = "800px")
  }
  else if (about_continuous == "GraphE") {
     list(src = "www/GraphExamples/graphE.svg", height = "800px")
  }
  else if (about_continuous == "GraphF") {
    list(src = "www/GraphExamples/graphF.svg", height = "800px")
  }
  else if (about_continuous == "GraphG") {
    list(src = "www/GraphExamples/graphG.svg", height = "800px")
  }
}, deleteFile = FALSE)

#####################################################GRAPH INFO

output$ordinal_information_about <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "GraphA"){
    HTML("The Kaplan-Meier graph shows the survival percentage over time across dosages.<br>For this graph, we assume a change from baseline of 3 to represent an event.<br> <br> This app allows you to select: <br>- Dose levels")
  }
  else if (about_ordinal == "GraphB") {
     html("Graph B shows the proportion of patients at each severity across time on a given dose.<br> <br> This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Toxicity of interest")
  }
  else if (about_ordinal == "GraphH") {
     html("Graph H is a butterfly plot, it allows you to compare the change in severity grades between two timepoints for a dose and toxicity of interest.<br> <br> This app allows you to select: <br>- Dose levels <br>- Start timepoint <br>- Final timepoint <br>- Toxicities of interest")
  }
  else if (about_ordinal == "GraphI") {
     html("Graph I shows the percentage of patients at each severity for a given dose level, timepoint and SAE. The graph also shows the number of patients who reported. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Toxicities of interest")
  }
})

output$continuous_information_about <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "GraphC"){
    html("Graph C shows the mean change from baseline functioning across time given dose level and a functionality of interest. The graph includes 95% confidence intervals for each timepoint.<br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest")
  }
  else if (about_continuous == "GraphD") {
     html("Graph D allows viewing of individual patients trends in functionality across time. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest <br> - Patient IDs ")
  }
  else if (about_continuous == "GraphE") {
     html("Graph E creates boxplots for the tolerability based on scores in functionality categories. This allows you to compare between times and doses with a fixed functionality type. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest")
  }
  else if (about_continuous == "GraphF") {
    html("Graph F outputs a table which shows: <br> - Mean change from baseline <br> - Standard error <br> - Number of reported patients <br> These are calculated for a fixed time period from the baseline for selected functionalities and doses. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionalities of interest")
  }
  else if (about_continuous == "GraphG") {
    html("Graph G shows the mean change from baseline to a fixed timepoint given a fuctioning category with 95% confidence intervals. Doses can be compared with each other and multiple functioning categories may be shown. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionalities of interest ")
  }
})

############################################################# GRAPH PROS
output$ordinal_pros <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "GraphA"){
    HTML("- Easily compare selected doses <br> - Provides a table with number of patients")
  }
  else if (about_ordinal == "GraphB") {
    HTML("B <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_ordinal == "GraphH") {
    HTML("H <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_ordinal == "GraphI") {
     HTML("I <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
})
output$continuous_pros <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "GraphC"){
     HTML("C <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_continuous == "GraphD") {
     HTML("D <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_continuous == "GraphE") {
     HTML("E <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_continuous == "GraphF") {
     HTML("F <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
  else if (about_continuous == "GraphG") {
     HTML("G <br> -pro 1 <br> -pro 2 <br> -pro 3")
  }
})


############################################################ GRAPH CONS
output$ordinal_cons <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "GraphA"){
     HTML("- Currently only allows baseline change of 3 as an event <br> - Missing patients aren't denoted on graph")
  }
  else if (about_ordinal == "GraphB") {
     HTML("B <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_ordinal == "GraphH") {
     HTML("H <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_ordinal == "GraphI") {
    HTML("I <br> -con 1 <br> -con 2 <br> -con 3")
  }
})
output$continuous_cons <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "GraphC"){
    HTML("C <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "GraphD") {
     HTML("D <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "GraphE") {
     HTML("E <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "GraphF") {
    HTML("F <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "GraphG") {
    HTML("G <br> -con 1 <br> -con 2 <br> -con 3")
  }
})


#############################################################################################################Graph A
  #Import the data
  ordinal_data <- reactive({
    if (input$dummy_ordinal == FALSE) {
      req(input$upload)
       data <- read.csv(input$upload$datapath, header = TRUE, sep = ",")
       data <- data %>% select(id, toxicity, grade, timepoint, dose, time_last_report)
       data
    }
    else if (input$dummy_ordinal == TRUE) {
          read.csv("www/DummyData/dummy_ordinal.csv", header = TRUE, sep = ",")[,-1]
}
})

output$ordinal_table <- renderTable(ordinal_data())

output$continuous_table <- renderTable(continuous_data())

  #Update the selection boxes based on dataset
  observe({
    req(input$upload)
    ordinal_data <- ordinal_data()
    numdose <- unique(ordinal_data$dose)
    updateCheckboxGroupInput(session, "numdose", label = "Dosage", choices = numdose, selected = NULL)
  })
  observe({
    req(input$dummy_ordinal)
    ordinal_data <- ordinal_data()
    numdose <- unique(ordinal_data$dose)
    updateCheckboxGroupInput(session, "numdose", label = "Dosage", choices = numdose, selected = NULL)
  })


  #Define the relevant variables
  no_dose <- reactive({
                       req(input$numdose)
                       as.numeric(input$numdose)})
  #Filter the data
  summary_df <- reactive({
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
                              time = if (any(base_adj == 3)) min(timepoint[base_adj == 3]) else max(timepoint),
                              event = as.integer(any(base_adj == 3))  # 1 if grade 3 occurred, 0 if censored
                            )
                          summary_df <- cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))
                          colnames(summary_df)[5] <- "dose"
                          summary_df})

  output$graphA <- renderPlot({
                               survfit2(Surv(time, event) ~ dose, data = summary_df()) %>%
                                 ggsurvfit(linewidth = 1.5) +
                                 add_censor_mark() +
                                 add_confidence_interval() +
                                 scale_ggsurvfit() + add_risktable()})

################################################ graph B


#Update the selection boxes based on dataset
observe({
  req(input$upload)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseB", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointB", label = "Final timepoint of interest", max = length(tpoint) - 1)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateSelectInput(session, "toxicB", label = "Toxicity of interest", choices = toxic, selected = NULL)
})
####### for dummy data
observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseB", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointB", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateSelectInput(session, "toxicB", label = "Toxicity of interest", choices = toxic, selected = NULL)
})

#Define the relevant variables
no_doseb <- reactive({
                     req(input$numdoseB)
                     as.numeric(input$numdoseB)})

final_t <- reactive({
  req(input$tpointB)
  as.numeric(input$tpointB)
})

toxic <- reactive({
  req(input$toxicB)
  input$toxicB
})

#Filter the data

prop <- reactive({
  req(input$numdoseB)
  req(input$tpointB)
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
  req(input$numdoseB)
  ordinal_data <- ordinal_data()
  no_pat <- ordinal_data %>%
    filter(timepoint == 0, toxicity == toxic()) %>%
    group_by(dose) %>%
    filter(dose %in% no_doseb()) %>%
    summarise(n.pat = n(),.groups = "keep")
  no_pat$n.pat
})

#Graph output
output$graphB <- renderPlot({
  prop <- prop()
  no_dose <- no_doseb()
  no_patient <- no_patient()
  formatter <- function(no_dose, no_pat = no_patient) { paste0("Dose level ", no_dose, " (N=", no_pat, ")")}
  prop %>% filter(toxicity == toxic()) %>% ggplot(aes(x = as.factor(timepoint))) +
    geom_bar(aes(x = as.factor(timepoint),
                 y = proportion,
                 fill = as.factor(grade)),
             stat = "identity",
             position = "stack") +
    scale_fill_manual(breaks = rev(c(1:5, "Unreported", "Discontinued")),
                      values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "grey", "grey30")),
                      labels = rev(
                        c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported", "Discontinued")
                      ),
                      guide = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 14) +
    facet_wrap2(~as.factor(dose),
                nrow = 3,
                strip.position = "left",
                axes = "all",
                labeller = as_labeller(formatter)) +
    labs(x = "Timepoint",
         y = "Proportion",
         fill = "Severity Grade")
})

################################################################################## GRAPH C
#### Import the data
continuous_data <- reactive({
  if (input$dummy_continuous == FALSE) {
    req(input$upload_cts)
      data <- read.csv(input$upload_cts$datapath, header = TRUE, sep = ",")
      data <- data %>% select(score, id, functioning, score, timepoint, dose)
      data
        
  }
  else if (input$dummy_continuous == TRUE) {
        read.csv("www/DummyData/dummy_continuous.csv", header = TRUE, sep = ",")[,-1]
}
})
#### Update the inputs
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseC <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseC", label = "Dosage", choices = numdoseC, selected = NULL)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  tpointC <- unique(data$timepoint)
  updateSliderInput(session, "tpointC", label = "Final timepoint of interest", max = length(tpointC) - 1)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningC", label = "Functionality of interest", choices = func, selected = NULL)
})
########## For the dummy set 
observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  numdoseC <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseC", label = "Dosage", choices = numdoseC, selected = NULL)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  tpointC <- unique(data$timepoint)
  updateSliderInput(session, "tpointC", label = "Final timepoint of interest", max = length(tpointC)-1)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningC", label = "Functionality of interest", choices = func, selected = NULL)
})


#### Reactive Defining
#Define the relevant variables
no_doseC <- reactive({
                      req(input$numdoseC)
                      as.numeric(input$numdoseC)})

final_tC <- reactive({
  req(input$tpointC)
  as.numeric(input$tpointC)
})

func_C <- reactive({
  req(input$functioningC)
  input$functioningC
})

#### Data Filtering
continuous_data_filtered <- reactive({
  req(input$functioningC)
  continuous_data <- continuous_data()
  continuous_data <- continuous_data %>% filter(functioning == func_C()) %>% dplyr::select(-functioning)# %>% filter(!is.na(score))
  continuous_data
})

diffC <- reactive({
  req(input$functioningC)
  continuous_data_f <- continuous_data_filtered()
  continuous_data_f <- continuous_data_f %>%
    group_by(id) %>%
    mutate(baseline = score[timepoint == 0], adjusted_value = score - baseline) %>%
      ungroup()
    continuous_data_f
})

summaryC <- reactive({
  diff <- diffC()
  diff <- diff %>%
    group_by(dose, timepoint) %>%
    summarise(mean = mean(adjusted_value, na.rm = TRUE), sd = sd(adjusted_value, na.rm = TRUE) / sqrt(n()), .groups = "keep")
  summary <- diff %>% mutate(l.ci = mean - (1.96 * sd)) %>% mutate(u.ci = mean + (1.96 * sd))
  summary <- summary %>% filter(timepoint %in% c(0:final_tC())) %>% filter(dose %in% no_doseC())
  summary
})

pd <- position_dodge(width = 0.2)

#### FIGURE GENERATION
output$graphC <- renderPlot({
  summaryC() %>%
    ggplot(
           aes(x = as.factor(timepoint),
               y = mean, group = as.factor(dose), shape = as.factor(dose), colour = as.factor(dose))) +
    geom_ribbon(aes(ymin = 10, ymax = 30, fill = "palegreen1"), alpha = 0.3, colour = NA) +
    geom_ribbon(aes(ymin = -30, ymax = -10, fill = "red"), alpha = 0.1, colour = NA) +
    geom_point(position = pd, size=3) + geom_line(size = 1.5) +
    geom_errorbar(position = pd, aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
    theme_minimal(base_size = 14) +
    guides(shape = guide_legend(title = "Dose"),
           colour = guide_legend(title = "Dose")) +
    theme_minimal(base_size = 14) +
    xlab("Timepoint") +
    ylab(paste0(
                "Mean change from baseline across timepoints (with 95% CI) \n 
                for EORTC QLQ-C30 Global status score for functioning ",
                func_C())) +
    scale_fill_manual(name = "Threshold", values = c("palegreen1", "red"), label = c("Improvement", "Worsening")) +
    geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = 30),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
    geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = -30),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
    geom_text(data = data.frame(x = 1.6, y = 17, label = "Improvement"), aes(x = x, y = y, label = label),
              inherit.aes = FALSE) +
    geom_text(data = data.frame(x = 1.6, y = -17, label = "Worsening"), aes(x = x, y = y, label = label),
              inherit.aes = FALSE)
})
############################################################################################# GRAPH D
observe({
  req(input$upload_cts)
  data <- continuous_data()
  tpointD <- unique(data$timepoint)
  updateSliderInput(session, "tpointD", label = "Final timepoint of interest", max = length(tpointD) - 1)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningD", label = "Functionality of interest", choices = func, selected = NULL)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseD <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseD", choices = numdoseD, selected = NULL)
})
####### for dummy set 
observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  tpointD <- unique(data$timepoint)
  updateSliderInput(session, "tpointD", label = "Final timepoint of interest", max = length(tpointD) - 1)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningD", label = "Functionality of interest", choices = func, selected = NULL)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  numdoseD <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseD", choices = numdoseD, selected = NULL)
})

#### Reactive Defining 
#Define the relevant variables
no_doseD <- reactive({
                      req(input$numdoseD)
                      as.numeric(input$numdoseD)})

final_tD <- reactive({
  req(input$tpointD)
  as.numeric(input$tpointD)
})

func_D <- reactive({
  req(input$functioningD)
  input$functioningD
})

#### ID specific UI

output$select_idD <- renderUI({
  data <- continuous_data()
  data <- data %>% filter(dose %in% no_doseD())
  id <- unique(data$id)
  selectizeInput("select_idD", "Select IDs", choices = id, multiple = TRUE, selected = NULL)
})

idD <- reactive({
  req(input$select_idD)
  input$select_idD
})

#### Filter data
continuous_data_filteredD <- reactive({
  data <- continuous_data() %>%
    filter(dose %in% no_doseD()) %>%
    filter(id %in% idD()) %>%
    filter(timepoint %in% c(0:final_tD())) %>%
    filter(functioning == func_D()) %>%
    filter(!is.na(score)) %>%
    dplyr::select(-functioning)
  data <- data.frame(data)
  data
})

labels_D <- reactive({
  data <- continuous_data_filteredD()
  paste0("ID ", data$id, " (Dose: ", data$dose, ")")
})

####Plot output

output$graphD <- renderPlot({
  data <- continuous_data_filteredD()
  data$dose <- as.factor(data$dose)
  ggplot(data = data, aes(x = as.factor(timepoint),
                          y = score, group = id, colour = labels_D(), linetype = dose)) +
      geom_line(size = 0.9) +
      labs(linetype = "Dose") +
      theme_minimal(base_size = 16) + ylim(c(0, 100)) +
      geom_point() +
      labs(
        y = paste0("Score in functionality ", func_D()),
        x = "Timepoint",
        color = "Patient (Dose)")
})


################################################################################## Graph E
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseE <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseE", label = "Dosage", choices = numdoseE, selected = NULL)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  tpointE <- unique(data$timepoint)
  updateSliderInput(session, "tpointE", label = "Final timepoint of interest", max = length(tpointE) - 1)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningE", label = "Functionality of interest", choices = func, selected = NULL)
})

########### for dummy set 
observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  numdoseE <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseE", label = "Dosage", choices = numdoseE, selected = NULL)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  tpointE <- unique(data$timepoint)
  updateSliderInput(session, "tpointE", label = "Final timepoint of interest", max = length(tpointE) - 1)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateSelectInput(session, "functioningE", label = "Functionality of interest", choices = func, selected = NULL)
})


#### Reactive Defining 
#Define the relevant variables
no_doseE <- reactive({
                      req(input$numdoseE)
                      as.numeric(input$numdoseE)})

final_tE <- reactive({
  req(input$tpointE)
  as.numeric(input$tpointE)
})

func_E <- reactive({
    req(input$functioningE)
    input$functioningE
})

#### Data Filtering
data_E <- reactive({
  req(input$functioningD)
  continuous_data <- continuous_data()
  continuous_data <- continuous_data %>% filter(functioning == func_E()) %>% dplyr::select(-functioning)
  continuous_data <- continuous_data %>%
  filter(dose %in% no_doseE()) %>%
  filter(timepoint %in% c(1:final_tE())) %>%
  filter(!is.na(score))
  continuous_data
})

output$graphE <- renderPlot({
  continuous_data <- data_E()
  ggplot(continuous_data, aes(x = as.factor(timepoint), y = score, fill = as.factor(dose))) +
    labs(fill = "Dose level") +
    geom_boxplot() +
    theme_minimal(base_size = 14) +
    xlab("Timepoint") + ylab(paste0("Score in category ", func_E())) +
    geom_segment(aes(x = 0.2, xend = 0.2, y = 0, yend = 100),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) + ylim(c(0, 100)) +
    scale_x_discrete(expand = expansion(add = c(2, 0))) +
    geom_text(aes(x = -0.45, y = 90, label = "Better \n tolerability"))
})
############################################################## Graph F

#### Update the inputs
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseF <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseF", label = "Dosage", choices = numdoseF, selected = NULL)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  tpointF <- unique(data$timepoint)
  updateSliderInput(session, "tpointF", label = "Final timepoint of interest", max = length(tpointF) - 1)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateCheckboxGroupInput(session, "functioningF", label = "Functionality of interest", choices = func, selected = NULL)
})
##### for dummy set
observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  numdoseF <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseF", label = "Dosage", choices = numdoseF, selected = NULL)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  tpointF <- unique(data$timepoint)
  updateSliderInput(session, "tpointF", label = "Final timepoint of interest", max = length(tpointF) - 1)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateCheckboxGroupInput(session, "functioningF", label = "Functionality of interest", choices = func, selected = NULL)
})

#### Reactive Defining 
#Define the relevant variables
no_doseF <- reactive({
                      req(input$numdoseF)
                      as.numeric(input$numdoseF)})

final_tF <- reactive({
  req(input$tpointF)
  as.numeric(input$tpointF)
})

func_F <- reactive({
  req(input$functioningF)
  as.character(input$functioningF)
})

###Filter and sort data

continuous_data_filteredF <- reactive({
  filtered <- continuous_data()
  filtered$functioning <- as.character(filtered$functioning)
  filtered <- filtered %>%
    filter(dose %in% no_doseF()) %>%
    filter(functioning %in% func_F()) %>%
    filter(timepoint %in% c(0, final_tF()))
  filtered
})

ice_n_patF <- reactive({
  ice_n_pat <- continuous_data_filteredF() %>%
    group_by(dose, id) %>%
    summarise(n_timepoints = n_distinct(timepoint), .groups = "drop") %>%
    group_by(dose) %>%
    mutate(max_timepoints = max(n_timepoints)) %>%
    filter(n_timepoints < max_timepoints) %>%
    summarise(n_patients_missing = n_distinct(id))
  ice_n_pat
})


dfF <- reactive({
  finalT <- as.symbol(paste0("tp_", final_tF()))
  df <- continuous_data_filteredF() %>%
    group_by(functioning) %>%
    pivot_wider(names_from = timepoint, values_from = score, names_prefix = "tp_") %>%
    mutate(score = as.numeric(!!finalT - tp_0)) %>%
    dplyr::select(id, score, dose, functioning)

  df <- df %>%
    group_by(dose, functioning) %>%
    mutate(non_na_count = sum(!is.na(score))) %>%
    mutate(na_count = sum(is.na(score))) %>%
    ungroup()
  df
})

sum.dfF <- reactive({
  sum.df <- dfF() %>%
    group_by(dose, functioning) %>%
    summarise(mean = mean(score, na.rm = TRUE),
              non_na_count = unique(non_na_count),
              se = sd(score, na.rm = TRUE) / sqrt(non_na_count), .groups = "keep")
  sum.df <- sum.df %>% mutate(l.ci = mean - (1.96 * se))
  sum.df <- sum.df %>% mutate(u.ci = mean + (1.96 * se))
  sum.df <- sum.df %>% ungroup(dose)
  sum.df
})


sum.df.widerF <- reactive({
  sum.dfF() %>% pivot_wider(id_cols = functioning, names_from = dose, values_from = c(mean,se,non_na_count))
})

sum.df.wider.gtF <- reactive({
  sum.df.wider.gt <- sum.df.widerF() %>% gt() %>%
  tab_header(title = paste0("Change from baseline to timepoint ", final_tF())) %>%
   tab_stubhead(label = "Functioning")
   for (i in unique(sum.dfF()$dose)){
      sum.df.wider.gt <- sum.df.wider.gt %>%
      tab_spanner(label = paste0("Dose ", i), columns = ends_with(as.character(!!i)))
      sum.df.wider.gt <- sum.df.wider.gt %>% cols_label(starts_with("mean") ~ "Mean",
                                                        starts_with("se") ~ "Standard Error",
                                                        starts_with("non_na_count") ~ "Number Reported")
}
  sum.df.wider.gt <- sum.df.wider.gt %>% opt_stylize(style = 5, color = "blue")
  sum.df.wider.gt
})


output$graphF <- render_gt(sum.df.wider.gtF())




################################################################# Graph G
####Update inputs
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseG <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseG", label = "Dosage", choices = numdoseG, selected = NULL)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  tpointG <- unique(data$timepoint)
  updateSliderInput(session, "tpointG", label = "Final timepoint of interest", max = length(tpointG) - 1)
})

observe({
  req(input$upload_cts)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateCheckboxGroupInput(session, "functioningG", label = "Functionality of interest", choices = func, selected = NULL)
})

########### for dummy set
observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  numdoseG <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseG", label = "Dosage", choices = numdoseG, selected = NULL)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  tpointG <- unique(data$timepoint)
  updateSliderInput(session, "tpointG", label = "Final timepoint of interest", max = length(tpointG) - 1)
})

observe({
  req(input$dummy_continuous)
  data <- continuous_data()
  func <- unique(data$functioning)
  updateCheckboxGroupInput(session, "functioningG", label = "Functionality of interest", choices = func, selected = NULL)
})

#### Reactive Defining 
#Define the relevant variables
no_doseG <- reactive({
                      req(input$numdoseG)
                      as.numeric(input$numdoseG)})

final_tG <- reactive({
  req(input$tpointG)
  as.numeric(input$tpointG)
})

func_G <- reactive({
    req(input$functioningG)
    input$functioningG
})

####Filtering
no_patientG <- reactive({
  continuous_data <- continuous_data()
  continuous_data  %>%
  filter(dose %in% no_doseG()) %>%
  filter(timepoint == 0, functioning == func_G()[1]) %>%
  count(dose)
})

continuous_data_filteredG <- reactive({
  continuous_data <- continuous_data()
  continuous_data$functioning <- as.character(continuous_data$functioning)
  continuous_data_filtered <- continuous_data %>%
  filter(dose %in% no_doseG()) %>%
  filter(functioning %in% func_G())
  continuous_data_filtered
})

ice_n_pat <- reactive({
  continuous_data <- continuous_data_filteredG()
  ice_n_pat <- continuous_data %>%
  group_by(dose, id) %>%
  summarise(n_timepoints = n_distinct(timepoint), .groups = "drop") %>%
  group_by(dose) %>%
  mutate(max_timepoints = final_tG()) %>%
  filter(n_timepoints < max_timepoints) %>%
  summarise(n_patients_missing = n_distinct(id))
  ice_n_pat
})

dfG <- reactive({
  endT <- final_tG()
  endT<- as.character(endT)
  tp_end <- sym(paste0("tp_", endT))
  df <- continuous_data_filteredG() %>%
  filter(timepoint %in% c(0, as.numeric(endT))) %>%
  pivot_wider(names_from = timepoint, values_from = score, names_prefix = "tp_") %>%
  mutate(score = as.numeric(!!tp_end - tp_0)) %>% #################this is an issue
  dplyr::select(id, score, dose, functioning)

  df <- df %>%
    group_by(dose, functioning) %>%
    mutate(non_na_count = sum(!is.na(score))) %>%
    mutate(na_count = sum(is.na(score))) %>%
    ungroup()
  df
})

unrep <- reactive({
  unrep <- dfG() %>% group_by(dose, functioning) %>% distinct(na_count) %>% ungroup()
})

sum.dfG <- reactive({
  sum.df <- dfG() %>%
    group_by(dose, functioning) %>%
    summarise(
              mean = mean(score, na.rm=TRUE),non_na_count = unique(non_na_count),
              se = sd(score, na.rm = TRUE) / sqrt(non_na_count), .groups = "keep")
  sum.df <- sum.df %>% mutate(l.ci = mean - (1.96 * se))
  sum.df <- sum.df %>% mutate(u.ci = mean + (1.96 * se))
  sum.df <- sum.df %>% left_join(unrep(), by = c("dose", "functioning")) %>%
    mutate(custom_x = paste0("Dose=", dose, "\nUR=", na_count))
  sum.df
})

thresholds <- data.frame(
  ymin = c(10, -25),
  ymax = c(25, -10),
  Threshold = factor(c("Improvement", "Worsening"))
)


arrowdf <- reactive({
  first_fn <- as.symbol(as.character((func_G()[1])))
  arrow <- tibble(functioning = as.character(first_fn))
})

output$graphG <- renderPlot({
  end_t <- final_tG()
  arrowdf <- arrowdf()
  no_patient <- unlist(no_patientG()[2])
  no_patient <- paste(no_patient, collapse = ";")
  formatterG <- function(end = end_t) {paste0("Functioning ", func_G())}
  data.frame(sum.dfG()) %>% ggplot(aes(x = custom_x, y = mean, colour = as.factor(dose))) +
  geom_point(size = 3) +
  scale_color_discrete(name = "Dose level") +
  geom_errorbar(aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
  theme_minimal(base_size = 14) + ylim(c(-25, 25)) +
  scale_fill_manual(
    name = "Threshold",
    values = c("Improvement" = "palegreen1", "Worsening" = "red")
  ) +
  geom_rect(data = thresholds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Threshold),
            alpha = 0.3, inherit.aes = FALSE) +
    labs(
    y = paste0("Mean change from baseline to timepoint ", final_tG(), "(with 95% CI) \n for EORTC QLQ-C30 functional domains"),
    x = paste0("Dose (N= ", no_patient, ")")) +
  facet_wrap(~ as.factor(functioning), nrow = 1,labeller = as_labeller(formatterG), scales = "free_x") +
  scale_x_discrete(expand = expansion(add = c(1, 1))) +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = 25),
               colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = -25),
               colour = "darkgrey", size = 0.8, arrow = arrow(type="closed",
                                                              length = unit(0.1, "inches"))) +
   geom_text(data = arrowdf, aes(x = 2, y = 22, label = "Improvement"),
              inherit.aes = FALSE) +
   geom_text(data = arrowdf, aes(x = 2, y = -22, label = "Worsening"),
              inherit.aes = FALSE)
})

################################################################################ Graph H
#Update the selection boxes based on dataset
observe({
  req(input$upload)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseH", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointH", label = "Start and endpoint for comparison", max = length(tpoint)-1)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateCheckboxGroupInput(session, "toxicH", label = "Toxicity of interest", choices = toxic, selected = NULL)
})
####### Dummy 
observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseH", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointH", label = "Start and endpoint for comparison", max = length(tpoint)-1)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateCheckboxGroupInput(session, "toxicH", label = "Toxicity of interest", choices = toxic, selected = NULL)
})

#Define the relevant variables
no_doseH <- reactive({
                     req(input$numdoseH)
                     as.numeric(input$numdoseH)})

times_H <- reactive({
  req(input$tpointH)
  as.numeric(input$tpointH)
})

toxicH <- reactive({
  req(input$toxicH)
 input$toxicH
})





####Filter and sort data

ordinal_dataH <- reactive({
  tps <- times_H()
  tp_first <- as.symbol(paste0("tp_", tps[1]))
  tp_last <- as.symbol(paste0("tp_", tps[2]))
  ordinal_dataH <- ordinal_data()
  ordinal_dataH <- ordinal_dataH %>% filter(dose %in% no_doseH()) %>% filter(toxicity %in% toxicH()) %>%
    group_by(id) %>%
    mutate(has_tp = any(timepoint == tps[2])) %>%
    filter(timepoint %in% c(tps[1], tps[2])) %>%
    pivot_wider(names_from = timepoint, values_from = grade, names_prefix = "tp_") %>%
    mutate(grade_change = case_when(
                                    has_tp == FALSE ~ "ICE",
                                    TRUE ~  as.character(!!tp_first - !!tp_last)))
  ordinal_dataH$grade_change[is.na(ordinal_dataH$grade_change) == TRUE] <- "Missing"
  ordinal_dataH
})

dose_labelsH <- reactive({
  levs <- rev(no_doseH())
  paste0("Dose ", levs)
})

propH <- reactive({
  propH <- ordinal_dataH()
  propH <- propH %>% group_by(toxicity, dose) %>%
    count(grade_change) %>%
    ungroup() %>%
    mutate(original = TRUE) %>%
    complete(toxicity, dose, grade_change, fill = list(n = 0)) %>%
    group_by(toxicity, dose) %>%
    mutate(n.pat = sum(n[grade_change != "ICE"], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(proportion = n / n.pat) %>% dplyr::select(-n)
  propH$toxicity <- as.factor(propH$toxicity)
  propH$dose <- as.factor(propH$dose)
  propH$dose <- factor(propH$dose, levels = rev(levels(propH$dose)))
  #prop$base_adj <- factor(prop$base_adj, levels = c("Unreported",5,4,3,2,1))
  propH$grade_change <- as.character(propH$grade_change)
  #prop <- prop %>%
  #  replace_na(list(grade_change = "Unreported"))
  propH
})
missing_H <- reactive({
  missing <- propH() %>%
    subset(grade_change == "Missing") %>%
    mutate(label = paste0("Unreported: ", round(proportion, 2) * 100, "%"))
  missing <- cbind(missing, rep(0.7, times = nrow(missing)))
  colnames(missing)[8] <- "loc"
  levels(missing$dose) <-  dose_labelsH()
  missing
})

prop_H_adjusted <- reactive({
  propHadj <- propH()
  propHadj <- propHadj %>% mutate(proportion = ifelse(grade_change >= 0, proportion, -proportion))
  propHadj <- propHadj %>% subset(grade_change != "Missing") %>% subset(grade_change != 0)
  propHadj <- propHadj %>% filter(original)
  propHadj
})

valH <- reactive({
  valH <- prop_H_adjusted()
  valH <- valH %>%
    group_by(dose, toxicity) %>%
    arrange(dose, toxicity, grade_change) %>%
    mutate(percent = paste0(round(abs(proportion) * 100, 1), "%")) %>%

  # Create separate cumsum columns for pos/neg
  mutate(
    cumsum_pos = if_else(proportion > 0, cumsum(if_else(proportion > 0, proportion, 0)), NA_real_),
    cumsum_neg = if_else(proportion < 0, cumsum(if_else(proportion < 0, proportion, 0)), NA_real_)
  ) %>%
  mutate(
    pos = case_when(
      proportion > 0 ~ cumsum_pos - 0.5 * proportion,
      proportion < 0 ~ cumsum_neg - 0.5 * proportion,
      TRUE ~ 0  # fallback for proportion == 0
    )
  )
  valH <- valH %>% subset(grade_change != "ICE")
  valH$grade_change <- factor(valH$grade_change)
  levels(valH$dose) <-  dose_labelsH()
  valH
})

#arrowdf <- tibble(toxicity = "10")

arrowdfH <- reactive({ 
  arrowH <- tibble(
  toxicity = factor(as.character(unique(toxicH())), levels = levels(valH()$toxicity)),  # important!
  x = 0.5,
  xend = 0.5,
  y = 0,
  yend = 1
)
arrowH
})

arrowdf.revH <- reactive({ arrowH <- tibble(
  toxicity = factor(as.character(unique(toxicH())), levels = levels(valH()$toxicity)),  # important!
  x = 0.5,
  xend = 0.5,
  y = 0,
  yend = -1
)
arrowH
})



ice_n_patH <- reactive({
  prop_H_adjusted() %>% distinct(dose, n.pat)
})

labels <- cbind(c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5),
                c("red", "deeppink", "orange", "gold", "lightyellow3", "lightblue",
                  "darkseagreen3", "darkolivegreen3", "chartreuse3", "chartreuse4"),
                rev(c("Improve by 5", "Improve by 4", "Improve by 3", "Improve by 2", "Improve by 1", "Worsen by 1",
                      "Worsen by 2", "Worsen by 3", "Worsen by 4", "Worsen by 5")))

colnames(labels) <- c("score", "colour", "leg")

output$graphH <- renderPlot({
  toxic <- toxicH()
  valH <- valH()
  arrowdfH <- arrowdfH()
  arrowdf.revH <- arrowdf.revH()
  formatterH <- function(toxic = toxic) {paste0("Symptomatic Adverse Event ", toxic)}
  ggplot(valH %>% subset(toxicity %in% toxic), aes(x = dose, y = proportion, fill = grade_change, group = dose)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_y_continuous(labels = abs, breaks = c(0), limits = c(-1, 1)) +
    theme_minimal(base_size = 15) +
    theme(
          strip.text.y = element_text(angle = 0)) +
    facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterH)) + #ylim(c(-1, 1)) +
      scale_fill_manual(breaks = labels[,1],
                    values=labels[,2],
                    labels=labels[,3])+
    geom_text(aes(y = pos, label = percent), size = 3, color = "black") +
    labs(x = paste0("Dose (N=", paste(ice_n_patH()$n.pat, collapse = "; "), ")"),
         y = "Percentage (%)",
         fill = "Change in Severity ") +
    # scale_y_continuous(breaks = c(0), limits = c(-1, 1)) +
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    geom_segment(data = arrowdfH,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "darkgrey",
                 size = 0.8,
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 inherit.aes = FALSE) +
    scale_x_discrete(expand = expansion(mult = c(1.5, 0))) +
    geom_segment(data = arrowdf.revH,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "darkgrey",
                 size = 0.8,
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 inherit.aes = FALSE) +
    geom_text(data = data.frame(x = -0.25, y = 0.5, label = "Improvement"), aes(x = x, y = y, label = label),
              inherit.aes = FALSE) +
    geom_text(data = data.frame(x = -0.25, y = -0.5, label = "Worsening"),
    aes(x = x, y = y, label = label),
              inherit.aes = FALSE) +
    geom_text(
      data = missing_H() %>% filter(toxicity %in% toxic),
      aes(x = dose, y = loc, label = label, group = dose),  # 'toxicity' must also be in 'missing'
      inherit.aes = FALSE,
      vjust = -0.5
    )
})
################################################################################################## Graph I 
#Update the selection boxes based on dataset
observe({
  req(input$upload)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseI", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointI", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateCheckboxGroupInput(session, "toxicI", label = "Toxicity of interest", choices = toxic, selected = NULL)
})
############# for the dummy set 
observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  numdose <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseI", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointI", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$dummy_ordinal)
  data <- ordinal_data()
  toxic <- unique(data$toxicity)
  updateCheckboxGroupInput(session, "toxicI", label = "Toxicity of interest", choices = toxic, selected = NULL)
})

#Define the relevant variables
no_doseI <- reactive({
                     req(input$numdoseI)
                     as.numeric(input$numdoseI)})

times_I <- reactive({
  req(input$tpointI)
  as.numeric(input$tpointI)
})

toxicI <- reactive({
  req(input$toxicI)
  input$toxicI
})


####Filter and sort data

ordinal_dataI <- reactive({
  ordinal_data <- ordinal_data()
  end <- times_I()
  tp_end <- as.symbol(paste0("tp_", end))
  ordinal_data <- ordinal_data %>%
    filter(toxicity %in% toxicI()) %>%
    filter(dose %in% no_doseI()) %>%
    group_by(id) %>%
    mutate(has_tp = any(timepoint == end)) %>%
    filter(timepoint %in% c(0, end)) %>%
    pivot_wider(names_from = timepoint, values_from = grade, names_prefix = "tp_") %>%
    mutate(grade_change = !!tp_end - tp_0) %>%
    mutate(base_adj = case_when(
                                has_tp == FALSE ~ "ICE",
                                TRUE ~  ifelse(grade_change > 0, as.character(!!tp_end), as.character(1))))

  ordinal_data$base_adj[is.na(ordinal_data$base_adj) == TRUE] <- "Unreported"
  ordinal_data
})
# Data Manipulation
#prop<- ordinal_data%>%group_by(toxicity,dose)%>%count(base_adj) %>%
#  mutate(proportion = n / n.pat)%>%dplyr::select(-n)

propI <- reactive({
  prop <- ordinal_dataI() %>%
    group_by(toxicity, dose) %>%
    count(base_adj) %>%
    ungroup() %>%
    mutate(original = TRUE) %>%
    complete(toxicity, dose, base_adj, fill = list(n = 0)) %>%
    group_by(toxicity, dose) %>%
    mutate(n.pat = sum(n[base_adj != "ICE"], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(proportion = n / n.pat) %>%
    dplyr::select(-n)

  prop$toxicity <- as.factor(prop$toxicity)
  prop$base_adj <- as.factor(prop$base_adj)
  prop$dose <- as.factor(prop$dose)
  levels(prop$base_adj) <- c(levels(prop$base_adj), "Unreported")
  prop$dose <- factor(prop$dose, levels = rev(levels(prop$dose)))
  prop <- prop %>% subset(base_adj != "ICE")
  prop$base_adj <- factor(prop$base_adj, levels = c("Unreported", 5, 4, 3, 2, 1))
  prop
})

ice_n_patI <- reactive({
  ice <- propI() %>% distinct(dose, n.pat)
  ice
})

#Final figure
output$graphI <- renderPlot({
  formatterI <- function(SAE = toxicI()) {
    paste0("Symptomatic adverse event ", SAE)
  }
  propI() %>% ggplot(aes(fill = base_adj, x = dose, y = proportion * 100)) +
    geom_bar(position = "stack", stat = "identity") + coord_flip() +
    facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterI)) +
    scale_fill_manual(breaks = rev(c(1:5, "Unreported")),
                      values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "darkgrey")),
                      labels = rev(c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported")),
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = paste0("Dose (N=", paste(ice_n_patI()$n.pat, collapse = "; "), ")"),
          y = "Percentage (%)",
          fill = "Severity Grade") +
    theme(
          strip.text.y = element_text(angle = 0))
})
##########################################################Download buttons 
output$downloadA <- downloadHandler(
  filename = function(){
    paste0("Kaplan-Meier", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadB <- downloadHandler(
  filename = function(){
    paste0("Graph-B", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadC <- downloadHandler(
  filename = function(){
    paste0("Graph-C", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadD <- downloadHandler(
  filename = function(){
    paste0("Graph-D", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadE <- downloadHandler(
  filename = function(){
    paste0("Graph-E", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadF <- downloadHandler(
  filename = function(){
    paste0("Graph-F", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = as_gtable(sum.df.wider.gtF()), device = "svg")
  }
)
output$downloadG <- downloadHandler(
  filename = function(){
    paste0("Graph-G", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadH <- downloadHandler(
  filename = function(){
    paste0("Graph-H", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
output$downloadI <- downloadHandler(
  filename = function(){
    paste0("Graph-I", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg")
  }
)
####################################### Graph re-labelling UI
}
shinyApp(ui, server)
