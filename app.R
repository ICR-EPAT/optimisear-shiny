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
  p("- Use the edit button to make changes to graphs where necessary"),
  p("6. Click download to export any relevant graphs as a .svg file"),
  p("- Changing the image size in the editor will also adjust the file for download")
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
        radioButtons("about_ordinal", label = NULL, choiceValues = c("graphA", "graphB", "graphH", "graphI"), choiceNames =  c("GraphA", "GraphB", "GraphH", "GraphI"))
      ),
      conditionalPanel(
        "input.about == 'Continuous'",
        radioButtons("about_continuous", label = NULL, choiceValues = c("graphC", "graphD", "graphE", "graphF", "graphG"), choiceNames = c("GraphC", "GraphD", "GraphE", "GraphF", "GraphG") )
      )
    ),
    nav_panel("Ordinal", fillable = TRUE,
    layout_columns(col_widths = c(9, 3), height = "900px",
      card(card_header("Graph"), card_body(imageOutput("about_ordinal"), height = "800px"), full_screen = TRUE, fillable = TRUE),
      card(card_header("Information"), fill = TRUE,
        card_body(card_title("About"), htmlOutput("ordinal_information_about")),
        card_body(card_title("Edit functionality"), htmlOutput("ordinal_edit")),
        card_body(card_title("Limitations"), htmlOutput("ordinal_lims"))
        ))),
    nav_panel("Continuous", fillable = TRUE,
    layout_columns(col_widths = c(9, 3), height = "900px",
      card(card_header("Graph"), card_body(imageOutput("about_continuous", height = "800px")), full_screen = TRUE, fill = TRUE),
      card(card_header("Information"),
        card_body(card_title("About"), htmlOutput("continuous_information_about")),
        card_body(card_title("Edit functionality"), htmlOutput("continuous_edit")),
        card_body(card_title("Limitations"), htmlOutput("continuous_lims"))
        ))))))),
  ####################################################################################################### Data Sorting + Upload
  tabPanel("Data", icon = icon("arrow-up-from-bracket"),
  card(card_header(strong("How to format data")),
  card_body(
    p("Data must be input as a .csv file with column titles matching the listed below."),
    p("Any columns not fitting the format will be ignored - if not all columns are provided, the app may not run."),
    p("- If a patient experiences an intercurrent event (e.g. discontinuation or death), do not enter any data."),
    p("- If a patient partially responds to a questionnaire at a timepoint, enter all data and use NA in place of an unreported score or grade."),
    p("- If a patient recieves treatment and does not complete a questionnaire at that timepoint, fill in all scores or grades as NA"),
    p("For an example of data, you can toggle the dummy data on - please note to use your own data, the dummy dataset must be toggled off."),
   layout_columns(
    card(card_header(strong("Ordinal datasets")),
    card_body(
      p(strong("Required columns, {column name} and [input types]:")),
      p("- Patient ID, {id}, [numeric]"),
      p("- Toxicity type, {toxicity}, [numeric or character]"),
      p("- Grade of response, {grade}, [1,2,3,4,5]"),
      p("- Dose Level, {dose}, [numeric]"),
      p("- Timepoint of current report, {timepoint},"),
      p("[numeric - must begin with timepoint 0 OR date \"YYYY-MM-DD\"]"),
      p("- Final reported time by the patient, {timepoint},"),
      p("[numeric OR date \"YYYY-MM-DD\"]"),
      p("To switch to date format use toggle:"),
      input_switch("ordinal_date", "Turn on date format")
    )),
    card(card_header(strong("Continuous datasets")),
    card_body(
       p(strong("Required columns and input types:")),
       p("- Patient ID, {id}, [numeric]"),
       p("- Functionality characteristic, {functioning}, [numeric or character]"),
       p("- Score in the functionality category, {score}, [numeric]"),
       p("- Dose Level, {dose}, [numeric]"),
       p("- Timepoint of current report, {timepoint},"),
       p("[numeric - must begin with timepoint 0 OR date \"YYYY-MM-DD\"]"),
       p("To switch to date format use toggle:"),
       input_switch("continuous_date", "Turn on date format")
    ))
  ))),
  layout_sidebar(
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
            tabPanel("GraphA",
            fluidRow(
          textOutput("textHeaderA")
        ),
        layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(checkboxGroupInput("numdose", label = "Dosage",
                                                 selected = NULL, inline = TRUE), height = 800))
               , card(card_header("Download .svg file"), card_body(downloadButton("downloadA")))),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeA", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimA",
            sliderInput("widthA", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
            sliderInput("heightA", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editA", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popA",
             placement = "right",
             textInput("titleA", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeA", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("ylab_A", label = NULL, placeholder = "Enter y-axis label"),
             textInput("xlab_A", label = NULL, placeholder = "Enter x-axis label"),
             sliderInput("axisSizeA", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             actionButton("updateA", "Update")
          )), card_body(plotOutput("graphA"), height = 1000), full_screen = TRUE)
          )),
  ############################################################################################################ B UI
            tabPanel("GraphB",
          fluidRow(
          textOutput("textHeaderB")
        ),
        layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseB", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointB", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    selectInput("toxicB", label = "Toxicity of interest", choices = NULL), height = 800)
          ), card(card_header("Download .svg file"), card_body(downloadButton("downloadB"))))
          ,
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeB", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimB",
            sliderInput("widthB", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
            sliderInput("heightB", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editB", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popB",
             placement = "right",
             textInput("titleB", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeB", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("xlab_B", label = NULL, placeholder = "Enter x-axis label"),
             sliderInput("axisSizeB", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             actionButton("updateB", "Update")
          )), card_body(plotOutput("graphB"), height = 1000), full_screen = TRUE)
            )),
############################################################################################################ H UI
         
            tabPanel("GraphH",
            fluidRow(
          textOutput("textHeaderH")
        ),
        layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseH", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointH", label = "Start and endpoint for comparison", min = 0, max = 1, value = c(0,1), step = 1),
                    checkboxGroupInput("toxicH", label = "Toxicity of interest", choices = NULL, inline = TRUE), height = 800
          )), 
          card(card_header("Download .svg file"), card_body(downloadButton("downloadH")))),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeH", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimH",
            sliderInput("widthH", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
            sliderInput("heightH", "Adjust graph height", value = 800, min = 300, max = 2000, step = 100, width = "400px")),
          popover(
            actionButton("editH", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popH",
             placement = "right",
             textInput("titleH", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeH", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             sliderInput("unrep_positionH", label = "Unreported text position", min = -1, max = 1, value = 0.7, step = 0.1),
             textInput("subtitleH", label = NULL, placeholder = "Enter subgraph label"),
             actionButton("updateH", "Update")
          )), card_body(plotOutput("graphH"), height = 1000), full_screen = TRUE)
            )),
############################################################################################################ I UI
            tabPanel("GraphI",
            fluidRow(
          textOutput("textHeaderI")
        ),
    layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseI", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                    sliderInput("tpointI", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    checkboxGroupInput("toxicI", label = "Toxicity of interest", selected = NULL, inline = TRUE), height = 800) 
          ), 
          card(card_header("Download .svg file"), card_body(downloadButton("downloadI")))),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeI", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimI",
            sliderInput("widthI", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
            sliderInput("heightI", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editI", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popI",
             placement = "right",
             textInput("titleI", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeI", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("subtitleI", label = NULL, placeholder = "Enter subgraph label"),
             actionButton("updateI", "Update")
          )), card_body(plotOutput("graphI"), height = 1000), full_screen = TRUE)
            ))),
  navbarMenu("Continuous",############################################################################################################ C UI
             tabPanel("GraphC",
             fluidRow(
          textOutput("textHeaderC")
        ),
                layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    checkboxGroupInput("numdoseC", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
                   sliderInput("tpointC", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                   selectInput("functioningC", label = "Functioning of interest", choices = NULL), height = 800)),
                   card(card_header("Download .svg file"), card_body(downloadButton("downloadC")))
          ),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeC", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimC",
            sliderInput("widthC", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightC", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
            popover(
            actionButton("editC", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popC",
             placement = "right",
             textInput("titleC", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeC", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("ylab_C", label = NULL, placeholder = "Enter y-axis label"),
             textInput("xlab_C", label = NULL, placeholder = "Enter x-axis label"),
             sliderInput("axisSizeC", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             p(strong("Note:")),
             p("Upper bound for improvement and lower bound for worsening will be taken as range"),
             sliderInput("improveC", label = "Start and endpoint for improvement", min = -100, max = 100, value = c(10, 30), step = 1),
             sliderInput("worseC", label = "Start and endpoint for worsening", min = -100, max = 100, value = c(-30, -10), step = 1),
             sliderInput("impTextC", label = "Position of Improvement text", min = -100, max = 100, value = 18, step = 2),
             sliderInput("worTextC", label = "Position of Worsening text", min = -100, max = 100, value = -18, step = 2),
             p(strong("Note:")),
             p("If value selected for bounds excludes minimum or maximum of the data, the app will instead set this as the value. This also applies to the text height."),
             actionButton("updateC", "Update"))
          ), card_body(plotOutput("graphC"), height = 1000), full_screen = TRUE)
            )),
############################################################################################################ D UI
             tabPanel("GraphD", fluidRow(
          textOutput("textHeaderD")
        ),
            layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                    sliderInput("tpointD", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
                    checkboxGroupInput("numdoseD", label = "Dosage",choices = NULL),
                    uiOutput("select_idD"),
                    selectInput("functioningD", label = "Functioning of interest", choices = NULL), height = 800)),
                    card(card_header("Download .svg file"), card_body(downloadButton("downloadD")))),
                    card(card_header(class = "d-inline-flex justify-content-end",
                    popover(
            actionButton("sizeD", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimD",
            sliderInput("widthD", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightD", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editD", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popD",
             placement = "right",
             textInput("titleD", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeD", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("ylab_D", label = NULL, placeholder = "Enter y-axis label"),
             textInput("xlab_D", label = NULL, placeholder = "Enter x-axis label"),
             sliderInput("axisSizeD", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             textInput("arrowlab_D", label = NULL, placeholder = "Enter arrow label"),
             sliderInput("rangeD", label = "Range for y-axis", min = 0, max = 100, value = c(0, 100), step = 5),
             p(strong("Note:")),
             p("If value selected for range excludes minimum or maximum of the data, the app will instead set this as the value."),
             actionButton("updateD", "Update")
          )), card_body(plotOutput("graphD"), height = 1000), full_screen = TRUE)
        )),
############################################################################################################ E UI
              
             tabPanel("GraphE",  fluidRow(
          textOutput("textHeaderE")
        ),
         layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
            checkboxGroupInput("numdoseE", label = "Dosage", selected = NULL, inline = TRUE),
            sliderInput("tpointE", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            selectInput("functioningE", label = "Functioning of interest", choices = NULL), height = 800)),
            card(card_header("Download .svg file"), card_body(downloadButton("downloadE")))),
            card(card_header(class = "d-inline-flex justify-content-end",
            popover(
            actionButton("sizeE", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimE",
            sliderInput("widthE", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightE", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editE", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popE",
             placement = "right",
             textInput("titleE", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeE", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("ylab_E", label = NULL, placeholder = "Enter y-axis label"),
             textInput("xlab_E", label = NULL, placeholder = "Enter x-axis label"),
             sliderInput("axisSizeE", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             textInput("arrowlab_E", label = NULL, placeholder = "Enter arrow label"),
             sliderInput("rangeE", label = "Range for y-axis", min = 0, max = 100, value = c(0, 100), step = 5),
             p(strong("Note:")),
             p("If value selected for range excludes minimum or maximum of the data, the app will instead set this as the value."),
             actionButton("updateE", "Update")
          )), card_body(plotOutput("graphE"), height = 1000), full_screen = TRUE)
        )),
############################################################################################################ F UI
           tabPanel("GraphF",     fluidRow(
          textOutput("textHeaderF")
        ),
          layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
                   checkboxGroupInput("numdoseF", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
          sliderInput("tpointF", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
          checkboxGroupInput("functioningF", label = "Functioning of interest", selected = NULL, inline = TRUE), height = 800)),
          card(card_header("Download .svg file"), card_body(downloadButton("downloadF")))),
           card(card_header(class = "d-inline-flex justify-content-end",
           popover(
            actionButton("sizeF", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimF",
            sliderInput("widthF", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
            sliderInput("heightF", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editF", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popB",
             placement = "right",
             textInput("titleF", label = NULL, placeholder = "Enter table title"),
             actionButton("updateF", "Update")
          )), card_body(gt_output("graphF"), height = 1000), full_screen = TRUE)
           )),
           ############################################################################################################ G UI 
    
             tabPanel("GraphG",          fluidRow(
          textOutput("textHeaderG")
        ),
          layout_sidebar(
          sidebar = sidebar(
          card(card_header("Variables"), card_body(
          checkboxGroupInput("numdoseG", label = "Dosage",
                                                 selected = NULL, inline = TRUE),
          sliderInput("tpointG", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
          checkboxGroupInput("functioningG", label = "Functioning of interest", selected = NULL, inline = TRUE), height = 800)),
          card(card_header("Download .svg file"), card_body(downloadButton("downloadG")))),
          card(card_header(class = "d-inline-flex justify-content-end",
          popover(
            actionButton("sizeG", "Size", value = FALSE, icon = icon("arrows-up-down-left-right")),
            title = "Adjust output size",
            id = "dimG",
            sliderInput("widthG", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightG", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")),
          popover(
            actionButton("editG", "Edit", value = FALSE, icon = icon("pencil-square")),
             title = "Graph Editor",
             id = "popG",
             placement = "right",
             textInput("titleG", label = NULL, placeholder = "Enter graph title"),
             sliderInput("titlesizeG", label = "Title size", min = 10, max = 50, value = 20, step = 5),
             textInput("ylab_G", label = NULL, placeholder = "Enter y-axis label"),
             sliderInput("axisSizeG", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
             textInput("sublab_G", label = NULL, placeholder = "Enter subgraph label"),
             p(strong("Note:")),
             p("Upper bound for improvement and lower bound for worsening will be taken as range"),
             sliderInput("improveG", label = "Start and endpoint for improvement", min = -100, max = 100, value = c(10, 25), step = 1),
             sliderInput("worseG", label = "Start and endpoint for worsening", min = -100, max = 100, value = c(-25, -10), step = 1),
             sliderInput("impTextG", label = "Position of Improvement text", min = -100, max = 100, value = 22, step = 2),
             sliderInput("worTextG", label = "Position of Worsening text", min = -100, max = 100, value = -22, step = 2),
             p(strong("Note:")),
             p("If value selected for bounds excludes minimum or maximum of the data, the app will instead set this as the value. This also applies to the text height."),
             actionButton("updateG", "Update")
          )), card_body(plotOutput("graphG"), height = 1000), full_screen = TRUE)
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
      card_body(p("v1.0 - Internal ICR release 06/08/2025")))), bg = "DarkSeaGreen",
      #########################################Dealing with the long popovers
      tags$head(tags$style(HTML("
      .popover {
        max-height: 1000px;
        max-width: 300px;
        overflow-y: auto;
        overflow-x: hidden;
      }
    "))),))

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
  if (about_ordinal == "graphA"){
    list(src = "www/GraphExamples/graphA.svg")
  }
  else if (about_ordinal == "graphB") {
     list(src = "www/GraphExamples/graphB.svg", height = "800px")
  }
  else if (about_ordinal == "graphH") {
     list(src = "www/GraphExamples/graphH.svg", height = "800px")
  }
  else if (about_ordinal == "graphI") {
     list(src = "www/GraphExamples/graphI.svg", height = "800px")
  }
}, deleteFile = FALSE)

output$about_continuous <- renderImage({
  about_continuous <- about_continuous()
  if (about_continuous == "graphC"){
    list(src = "www/GraphExamples/graphC.jpeg")
  }
  else if (about_continuous == "graphD") {
     list(src = "www/GraphExamples/graphD.svg", height = "800px")
  }
  else if (about_continuous == "graphE") {
     list(src = "www/GraphExamples/graphE.svg", height = "800px")
  }
  else if (about_continuous == "graphF") {
    list(src = "www/GraphExamples/graphF.svg", height = "800px")
  }
  else if (about_continuous == "graphG") {
    list(src = "www/GraphExamples/graphG.svg", height = "800px")
  }
}, deleteFile = FALSE)

#####################################################GRAPH INFO

output$ordinal_information_about <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "graphA"){
    HTML("The Kaplan-Meier graph shows the survival percentage over time across dosages.<br>For this graph, we assume a change from baseline of 3 to represent an event.<br> <br> This app allows you to select: <br>- Dose levels")
  }
  else if (about_ordinal == "graphB") {
     html("Graph B shows the proportion of patients at each severity across time on a given dose.<br> <br> This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Toxicity of interest")
  }
  else if (about_ordinal == "graphH") {
     html("Graph H is a butterfly plot, it allows you to compare the change in severity grades between two timepoints for a dose and toxicity of interest.<br> <br> This app allows you to select: <br>- Dose levels <br>- Start timepoint <br>- Final timepoint <br>- Toxicities of interest")
  }
  else if (about_ordinal == "graphI") {
     html("Graph I shows the percentage of patients at each severity for a given dose level, timepoint and SAE. The graph also shows the number of patients who reported. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Toxicities of interest")
  }
})

output$continuous_information_about <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "graphC"){
    html("Graph C shows the mean change from baseline functioning across time given dose level and a functionality of interest. The graph includes 95% confidence intervals for each timepoint.<br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest")
  }
  else if (about_continuous == "graphD") {
     html("Graph D allows viewing of individual patients trends in functionality across time. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest <br> - Patient IDs ")
  }
  else if (about_continuous == "graphE") {
     html("Graph E creates boxplots for the tolerability based on scores in functionality categories. This allows you to compare between times and doses with a fixed functionality type. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionality of interest")
  }
  else if (about_continuous == "graphF") {
    html("Graph F outputs a table which shows: <br> - Mean change from baseline <br> - Standard error <br> - Number of reported patients <br> These are calculated for a fixed time period from the baseline for selected functionalities and doses. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionalities of interest")
  }
  else if (about_continuous == "graphG") {
    html("Graph G shows the mean change from baseline to a fixed timepoint given a fuctioning category with 95% confidence intervals. Doses can be compared with each other and multiple functioning categories may be shown. <br> <br>This app allows you to select: <br>- Dose levels <br>- Final timepoint <br>- Functionalities of interest ")
  }
})

############################################################# GRAPH Editor 
output$ordinal_edit <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "graphA"){
    HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size  <br> - Graph height and width")
  }
  else if (about_ordinal == "graphB") {
    HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size <br> - X axis label  <br> - Axis label font size  <br> - Graph height and width")
  }
  else if (about_ordinal == "graphH") {
    HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size <br> - Subgraph label format  <br> - Position of unreported percentage <br> - Graph height and width")
  }
  else if (about_ordinal == "graphI") {
     HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size <br> - Subgraph label format  <br> - Position of unreported percentage <br> - Graph height and width")
  }
})
output$continuous_edit <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "graphC"){
     HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Boundaries for improvement <br> - Boundaries for worsening <br> - Position of improvement text <br> - Position of worsening text <br> - Graph height and width")
  }
  else if (about_continuous == "graphD") {
     HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Arrow label <br> - Y axis range <br> - Graph height and width")
  }
  else if (about_continuous == "graphE") {
     HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Arrow label <br> - Y axis range <br> - Graph height and width")
  }
  else if (about_continuous == "graphF") {
     HTML("This app allows you to edit: <br> - Table title <br> - Table height and width")
  }
  else if (about_continuous == "graphG") {
     HTML("This app allows you to edit: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label <br> - Axis label font size <br> - Subgraph label format <br> - Boundaries for improvement <br> - Boundaries for worsening <br> - Position of improvement text <br> - Position of worsening text <br> - Graph height and width")
  }
})


############################################################ GRAPH CONSIDERATIONS
output$ordinal_lims <- renderUI({
  about_ordinal <- about_ordinal()
  if (about_ordinal == "graphA"){
     HTML("- Currently only allows baseline change of 3 as an event <br> - Missing patients aren't denoted on graph")
  }
  else if (about_ordinal == "graphB") {
     HTML("B <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_ordinal == "graphH") {
     HTML("H <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_ordinal == "graphI") {
    HTML("I <br> -con 1 <br> -con 2 <br> -con 3")
  }
})
output$continuous_lims <- renderUI({
  about_continuous <- about_continuous()
  if (about_continuous == "graphC"){
    HTML("C <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "graphD") {
     HTML("D <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "graphE") {
     HTML("E <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "graphF") {
    HTML("F <br> -con 1 <br> -con 2 <br> -con 3")
  }
  else if (about_continuous == "graphG") {
    HTML("G <br> -con 1")
  }
})
################################################################################################################ Server side for graph editors

editA <- eventReactive(input$updateA,{
  list(title = input$titleA,
  titlesize = input$titlesizeA,
  y_lab = input$ylab_A,
  x_lab = input$xlab_A,
  axissize = input$axisSizeA)
})
editB <- eventReactive(input$updateB,{
  list(title = input$titleB,
  titlesize = input$titlesizeB,
  x_lab = input$xlab_B,
  axissize = input$axisSizeB)
})
editC <- eventReactive(input$updateC,{
  list(title = input$titleC,
  titlesize = input$titlesizeC,
  y_lab = input$ylab_C,
  x_lab = input$xlab_C,
  axissize = input$axisSizeC,
  imp_bound = input$improveC,
  wor_bound = input$worseC,
  imp_text = input$impTextC,
  wor_text = input$worTextC)
})
editD <- eventReactive(input$updateD,{
  list(title = input$titleD,
  titlesize = input$titlesizeD,
  y_lab = input$ylab_D,
  x_lab = input$xlab_D,
  axissize = input$axisSizeD,
  arrow_lab = input$arrowlab_D,
  range = input$rangeD)
})
editE <- eventReactive(input$updateE,{
 list(title = input$titleE,
  titlesize = input$titlesizeE,
  y_lab = input$ylab_E,
  x_lab = input$xlab_E,
  axissize = input$axisSizeE,
  arrow_lab = input$arrowlab_E,
  range = input$rangeE)
})
titleF <- eventReactive(input$updateF,{
  input$titleF
})
editG <- eventReactive(input$updateG,{
  list(title = input$titleG,
  titlesize = input$titlesizeG,
  y_lab = input$ylab_G,
  axissize = input$axisSizeG,
  subgraph_lab = input$sublab_G,
  imp_bound = input$improveG,
  wor_bound = input$worseG,
  imp_text = input$impTextG,
  wor_text = input$worTextG)
})
editH <- eventReactive(input$updateH,{
  list(title = input$titleH,
  titlesize = input$titlesizeH,
  subtitle = input$subtitleH,
  location = input$unrep_positionH)
})
editI <- eventReactive(input$updateI,{
    list(title = input$titleI,
  titlesize = input$titlesizeI,
  subtitle = input$subtitleI)
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
                       input$numdose})
  #Filter the data
  summary_df <- reactive({
                          req(input$numdose)
                          ordinal_data <- ordinal_data()
                          ordinal_data <- ordinal_data %>% filter(dose %in% no_dose())
                          if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
                            ordinal_data <- ordinal_data %>%
                            group_by(id, toxicity) %>%
                            mutate(base_adj = grade-grade[timepoint == min(as.Date(ordinal_data$timepoint))])  
                          }
                          else {
                            ordinal_data <- ordinal_data %>%
                            group_by(id, toxicity) %>%
                            mutate(base_adj = grade-grade[timepoint == min(ordinal_data$timepoint)])
                          }
                          ordinal_data <- ordinal_data %>% filter(is.na(base_adj) == FALSE)
                          if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
                              summary_df <- ordinal_data %>%
                            group_by(id) %>%
                            summarise(
                              time = if (any(base_adj == 3)) as.numeric(min(as.Date(timepoint[base_adj == 3])) - min(as.Date(timepoint))) else as.numeric(max(as.Date(timepoint))- min(as.Date(timepoint))),
                              event = as.integer(any(base_adj == 3))  # 1 if grade 3 occurred, 0 if censored
                            )
                            }
                            else{
                              summary_df <- ordinal_data %>%
                            group_by(id) %>%
                            summarise(
                              time = if (any(base_adj == 3)) as.numeric(min(timepoint[base_adj == 3]) - min(timepoint)) else as.numeric(max(timepoint)- min(timepoint)),
                              event = as.integer(any(base_adj == 3)))  # 1 if grade 3 occurred, 0 if censored
                            }
                          summary_df <- cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))
                          colnames(summary_df)[5] <- "dose"
                          summary_df$dose <- paste0("Dose: ", summary_df$dose)
                          summary_df})

  output$graphA <- renderPlot({
                               g <- survfit2(Surv(time, event) ~ dose, data = summary_df()) %>%
                                 ggsurvfit(linewidth = 1.5) +
                                 add_censor_mark() +
                                 add_confidence_interval() +
                                 scale_ggsurvfit() + add_risktable()
                                if (isTruthy(input$updateA)){
                                  editA <- editA()
                                  g + ggtitle(editA$title) + theme(plot.title = element_text(size = editA$titlesize, face = "bold"), axis.title = element_text(size = editA$axissize)) +
                                  labs(y = editA$y_lab , x = editA$x_lab)
                                }
                                else {
                                  g + labs(y = "Probability of first baseline-adjusted \n Grade ≥3 PRO-CTCAE score" )
                                }}, height = function() input$heightA, width = function() input$widthA)

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
  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointB", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                     input$numdoseB})

final_t <- reactive({
  req(input$tpointB)
  input$tpointB
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

  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    n_pat <- ordinal_data %>%
    filter(timepoint == min(as.Date(timepoint)), toxicity == toxic()) %>%
    group_by(dose) %>%
    summarise(n.pat = n())
    }
    else {
     n_pat <- ordinal_data %>%
    filter(timepoint == min(timepoint), toxicity == toxic()) %>%
    group_by(dose) %>%
    summarise(n.pat = n())
    }

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
  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    prop <- prop %>% filter(as.Date(timepoint) <= as.Date(final_t()))
    prop$timepoint <- as.factor(as.Date(prop$timepoint))
  }
  else {
    prop <- prop %>% filter(timepoint <= final_t())
    prop$timepoint <- as.factor(prop$timepoint)
  }
  prop$dose <- factor(prop$dose, levels = no_doseb())
  prop$grade <- factor(prop$grade, levels = c("Discontinued", "Unreported", 5:1))
  prop <- prop %>% filter(dose %in% no_doseb())

  prop
})


#Graph labels

no_patient <- reactive({
  req(input$numdoseB)
  ordinal_data <- ordinal_data()
  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    no_pat <- ordinal_data %>%
    filter(timepoint == min(as.Date(timepoint)), toxicity == toxic()) %>%
    group_by(dose) %>%
    filter(dose %in% no_doseb()) %>%
    summarise(n.pat = n(),.groups = "keep")
  }
  else {
    no_pat <- ordinal_data %>%
    filter(timepoint == min(timepoint), toxicity == toxic()) %>%
    group_by(dose) %>%
    filter(dose %in% no_doseb()) %>%
    summarise(n.pat = n(),.groups = "keep")
  }
  no_pat$n.pat
})

#Graph output
output$graphB <- renderPlot({
  prop <- prop()
  no_dose <- no_doseb()
  no_patient <- no_patient()
  formatter <- function(no_dose, no_pat = no_patient) { paste0("Dose ", no_dose, " (N=", no_pat, ")")}
  g <- prop %>% filter(toxicity == toxic()) %>% ggplot(aes(x = as.factor(timepoint))) +
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
                labeller = as_labeller(formatter))
          if (isTruthy(input$updateB)){
                                  g +
    labs(x = editB()$x_lab,
         y = "Proportion",
         fill = "Severity Grade")+ ggtitle(editB()$title) + theme(plot.title = element_text(size = editB()$titlesize, face = "bold"), axis.title = element_text(size = editB()$axissize))
                                }
                                else {
                                  g +
    labs(x = "Timepoint",
         y = "Proportion",
         fill = "Severity Grade")
                                }
}, height = function() input$heightB, width = function() input$widthB)

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
  req(input$upload)
  data <- continuous_data()
  tpoint <- unique(data$timepoint)
  if (input$continuous_date == TRUE & input$dummy_continuous == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointC", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
  updateSliderInput(session, "tpointC", label = "Final timepoint of interest", max = length(tpointC) - 1)
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
                      input$numdoseC})

final_tC <- reactive({
  req(input$tpointC)
  input$tpointC
})

func_C <- reactive({
  req(input$functioningC)
  input$functioningC
})

#### Data Filtering
continuous_data_filtered <- reactive({
  req(input$functioningC)
  req(input$tpointC)
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
  summaryC <- summaryC()
  if (isTruthy(input$updateC)){
    editC <- editC()
    impbound <- editC$imp_bound
    worbound <- editC$wor_bound
    upper <- max(editC$wor_bound[2], editC$wor_bound[2], max(diffC()$adjusted_value))
    lower <- min(editC$wor_bound[1], editC$wor_bound[1], min(diffC()$adjusted_value))
    worse_text <- if (worbound[1] < impbound[1]){
      min(max(editC$wor_text, worbound[1]), worbound[2])}
      else {
         max(min(editC$wor_text, worbound[2]), worbound[1])
      }
    improve_text <- if (worbound[1] < impbound[1]){
      max(min(editC$imp_text, impbound[2]), impbound[1])}
      else {
        min(max(editC$imp_text, impbound[1]), impbound[2])
      }
    summaryC %>%
    ggplot(
           aes(x = as.factor(timepoint),
               y = mean, group = as.factor(dose), shape = as.factor(dose), colour = as.factor(dose))) +
    geom_ribbon(aes(ymin = impbound[1], ymax = impbound[2], fill = "palegreen1"), alpha = 0.3, colour = NA) +
    geom_ribbon(aes(ymin = worbound[1], ymax = worbound[2], fill = "red"), alpha = 0.1, colour = NA) +
    geom_point(position = pd, size=3) + geom_line(size = 1.5) +
    geom_errorbar(position = pd, aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
    theme_minimal(base_size = 14) +
    guides(shape = guide_legend(title = "Dose"),
           colour = guide_legend(title = "Dose")) +
    theme_minimal(base_size = 14) +
    xlab(editC$x_lab) +
    ylab(paste0(editC$y_lab)) +
    scale_fill_manual(name = "Threshold", values = c("palegreen1", "red"), label = c("Improvement", "Worsening")) +
    geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = max(impbound[2], worbound[2])),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
    geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = min(impbound[1], worbound[1])),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
    geom_text(data = data.frame(x = 1.6, y = improve_text, label = "Improvement"), aes(x = x, y = y, label = label),
              inherit.aes = FALSE) +
    geom_text(data = data.frame(x = 1.6, y = worse_text, label = "Worsening"), aes(x = x, y = y, label = label),
              inherit.aes = FALSE) +
              ggtitle(editC$title) + theme(plot.title = element_text(size = editC$titlesize, face = "bold"), axis.title = element_text(size = editC$axissize)) + ylim(lower, upper)
                                }
  else {
    summaryC %>%
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
                                }
}, height = function() input$heightC, width = function() input$widthC)
############################################################################################# GRAPH D

observe({
  req(input$upload)
  data <- continuous_data()
  tpoint <- unique(data$timepoint)
  if (input$continuous_date == TRUE & input$dummy_continuous == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointD", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                      input$numdoseD})

final_tD <- reactive({
  req(input$tpointD)
  input$tpointD
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
  req(input$tpointD)
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
  g <- ggplot(data = data, aes(x = as.factor(timepoint),
                          y = score, group = id, colour = labels_D(), linetype = dose)) +
      geom_line(size = 0.9) +
      labs(linetype = "Dose") +
      theme_minimal(base_size = 16)
         if (isTruthy(input$updateD)){
          editD <- editD()
          lims <- editD$range
          minval <- min(data$score, na.rm = TRUE)
          maxval <- max(data$score, na.rm = TRUE)
          lower <- min(c(minval, lims[1]))
          upper <- max(c(maxval, lims[2]))
      g + ylim(c(lower, upper)) +
      geom_point() +
      labs(
        y = paste0(editD$y_lab),
        x = editD$x_lab,
        color = "Patient (Dose)") +
      ggtitle(editD$title) + theme(plot.title = element_text(size = editD$titlesize, face = "bold"), axis.title = element_text(size = editD$axissize)) +
      geom_segment(x = 0.2, xend = 0.2, y = lower, yend = upper,
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches")), inherit.aes = FALSE) +
    geom_text(x = -0.2, y = upper * 0.9, label = editD$arrow_lab, color = "black", inherit.aes = FALSE) +
    scale_x_discrete(expand = expansion(add = c(2, 0)))
}
                                else {
                                  g + ylim(c(0, 100)) +
      geom_point() +
      labs(
        y = paste0("Score in functionality ", func_D()),
        x = "Timepoint",
        color = "Patient (Dose)") +
        geom_segment(x = 0.2, xend = 0.2, y = 0, yend = 100,
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches")), inherit.aes = FALSE) +
    geom_text(x = -0.2, y = 90, label = "Better \n tolerability", color = "black", inherit.aes = FALSE) +
    scale_x_discrete(expand = expansion(add = c(2, 0)))
                                }
}, height = function() input$heightD, width = function() input$widthD)


################################################################################## Graph E
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseE <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseE", label = "Dosage", choices = numdoseE, selected = NULL)
})

observe({
  req(input$upload)
  data <- continuous_data()
  tpoint <- unique(data$timepoint)
  if (input$continuous_date == TRUE & input$dummy_continuous == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointE", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                      input$numdoseE})

final_tE <- reactive({
  req(input$tpointE)
  input$tpointE
})

func_E <- reactive({
    req(input$functioningE)
    input$functioningE
})

#### Data Filtering
data_E <- reactive({
  req(input$functioningD)
  req(input$tpointE)
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
  g <- ggplot(continuous_data, aes(x = as.factor(timepoint), y = score, fill = as.factor(dose))) +
    labs(fill = "Dose") +
    geom_boxplot() +
    theme_minimal(base_size = 14)
     if (isTruthy(input$updateE)){
      editE <- editE()
      bounds <- editE$range
      maxval <- max(continuous_data$score, na.rm = TRUE)
      minval <- min(continuous_data$score, na.rm = TRUE)
      lower <- min(c(minval, bounds[1]))
      upper <- max(c(maxval, bounds[2]))
      g + xlab(editE$x_lab) + ylab(paste0(editE$y_lab)) +
    geom_segment(aes(x = 0.2, xend = 0.2, y = lower, yend = upper),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) + ylim(c(lower, upper)) +
    scale_x_discrete(expand = expansion(add = c(2, 0))) +
    geom_text(aes(x = -0.45, y = upper * 0.9, label = editE$arrow_lab))+
    ggtitle(editE$title) +
    theme(plot.title = element_text(size = editE$titlesize, face = "bold"), axis.title = element_text(size = editE$axissize))
                                }
                                else {
                                  g + xlab("Timepoint") + ylab(paste0("Score in category ", func_E())) +
    geom_segment(aes(x = 0.2, xend = 0.2, y = 0, yend = 100),
                 colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) + ylim(c(0, 100)) +
    scale_x_discrete(expand = expansion(add = c(2, 0))) +
    geom_text(aes(x = -0.45, y = 90, label = "Better \n tolerability"))
                                }
}, height = function() input$heightE, width = function() input$widthE)
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
  req(input$upload)
  data <- continuous_data()
  tpoint <- unique(data$timepoint)
  if (input$continuous_date == TRUE & input$input$dummy_continuous == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointF", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                      input$numdoseF})

final_tF <- reactive({
  req(input$tpointF)
  input$tpointF
})

func_F <- reactive({
  req(input$functioningF)
  as.character(input$functioningF)
})

###Filter and sort data

continuous_data_filteredF <- reactive({
   req(input$numdoseF)
  req(input$functioningF)
  req(input$tpointF)
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
   tab_stubhead(label = "Functioning")
   for (i in unique(sum.dfF()$dose)){
      sum.df.wider.gt <- sum.df.wider.gt %>%
      tab_spanner(label = paste0("Dose ", i), columns = ends_with(as.character(!!i)))
      sum.df.wider.gt <- sum.df.wider.gt %>% cols_label(starts_with("mean") ~ "Mean",
                                                        starts_with("se") ~ "Standard Error",
                                                        starts_with("non_na_count") ~ "Number Reported")
}
  sum.df.wider.gt <- sum.df.wider.gt %>% opt_stylize(style = 5, color = "blue")
   if (isTruthy(input$updateF)){
                                  sum.df.wider.gt %>% tab_header(title = paste0(titleF()))
                                }
                                else {
                                  sum.df.wider.gt
                                }
})


output$graphF <- render_gt(sum.df.wider.gtF(), height = input$heightF, width = input$widthF)




################################################################# Graph G
####Update inputs
observe({
  req(input$upload_cts)
  data <- continuous_data()
  numdoseG <- unique(data$dose)
  updateCheckboxGroupInput(session, "numdoseG", label = "Dosage", choices = numdoseG, selected = NULL)
})


observe({
  req(input$upload)
  data <- continuous_data()
  tpoint <- unique(data$timepoint)
  if (input$continuous_date == TRUE & input$dummy_continuous == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointG", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                      input$numdoseG})

final_tG <- reactive({
  req(input$tpointG)
  input$tpointG
})

func_G <- reactive({
    req(input$functioningG)
    input$functioningG
})

####Filtering
no_patientG <- reactive({
  req(input$tpointG)
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
  mutate(score = as.numeric(!!tp_end - tp_0)) %>%
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


arrowdf <- reactive({
  first_fn <- as.symbol(as.character((func_G()[1])))
  arrow <- tibble(functioning = as.character(first_fn))
})

output$graphG <- renderPlot({
  end_t <- final_tG()
  arrowdf <- arrowdf()
  no_patient <- unlist(no_patientG()[2])
  no_patient <- paste(no_patient, collapse = ";")
  g <- data.frame(sum.dfG()) %>% ggplot(aes(x = custom_x, y = mean, colour = as.factor(dose))) +
  geom_point(size = 3) +
  scale_color_discrete(name = "Dose") +
  geom_errorbar(aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
  theme_minimal(base_size = 14)
      if (isTruthy(input$updateG)){
        editG <- editG()
        impbound <- editG$imp_bound
        worbound <- editG$wor_bound
        minval <- min(dfG()$score, na.rm = TRUE)
        maxval <- max(dfG()$score, na.rm = TRUE)
        upper <- max(worbound[2],impbound[2], maxval)
        lower <- min(worbound[1], impbound[1], minval)
        worse_text <- if (worbound[1] < impbound[1]){
          min(max(editG$wor_text, worbound[1]), worbound[2])}
          else {
            max(min(editG$wor_text, worbound[2]), worbound[1])
          }
        improve_text <- if (worbound[1] < impbound[1]){
          max(min(editG$imp_text, impbound[2]), impbound[1])}
          else {
            min(max(editG$imp_text, impbound[1]), impbound[2])
          }
        thresholds <- data.frame(
  ymin = c(impbound[1], worbound[1]),
  ymax = c(impbound[2], worbound[2]),
  Threshold = factor(c("Improvement", "Worsening"))
)
 formatterG <- function(end = end_t) {paste(editG$subgraph_lab, func_G(), sep = " ")}
        g + ylim(c(lower, upper)) +
  scale_fill_manual(
    name = "Threshold",
    values = c("Improvement" = "palegreen1", "Worsening" = "red")
  ) +
  geom_rect(data = thresholds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Threshold),
            alpha = 0.3, inherit.aes = FALSE) +
    labs(
    y = paste0(editG$y_lab),
    x = paste0("Dose (N= ", no_patient, ")")) +
  facet_wrap(~ as.factor(functioning), nrow = 1,labeller = as_labeller(formatterG), scales = "free_x") +
  scale_x_discrete(expand = expansion(add = c(1, 1))) +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = upper),
               colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = lower),
               colour = "darkgrey", size = 0.8, arrow = arrow(type="closed",
                                                              length = unit(0.1, "inches"))) +
   geom_text(data = arrowdf, aes(x = 2, y = improve_text, label = "Improvement"),
              inherit.aes = FALSE) +
   geom_text(data = arrowdf, aes(x = 2, y = worse_text, label = "Worsening"),
              inherit.aes = FALSE) +
              ggtitle(editG$title) + theme(plot.title = element_text(size = editG$titlesize, face = "bold"), axis.title = element_text(size = editG$axissize))
                                }
                                else {
                                  thresholds <- data.frame(
  ymin = c(10, -25),
  ymax = c(25, -10),
  Threshold = factor(c("Improvement", "Worsening"))
)
 formatterG <- function(end = end_t) {paste0("Functioning ", func_G())}
                                  g + ylim(c(-25, 25)) +
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
                                }
        
}, height = function() input$heightG, width = function() input$widthG)

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
  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointH", label = "Start and endpoint for comparison", min = min(tpoint), max = max(tpoint), value = c(min(tpoint), max(tpoint)))
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
                     input$numdoseH})

times_H <- reactive({
  req(input$tpointH)
  input$tpointH
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
    if (isTruthy(input$updateH)){
    missing <- cbind(missing, rep(editH()$location, times = nrow(missing)))
    colnames(missing)[8] <- "loc"
    levels(missing$dose) <-  dose_labelsH()
    missing
    }
    else{
  missing <- cbind(missing, rep(0.7, times = nrow(missing)))
  colnames(missing)[8] <- "loc"
  levels(missing$dose) <-  dose_labelsH()
  missing
}})

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
  g <- ggplot(valH %>% subset(toxicity %in% toxic), aes(x = dose, y = proportion, fill = grade_change, group = dose)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    scale_y_continuous(labels = abs, breaks = c(0), limits = c(-1, 1)) +
    theme_minimal(base_size = 15) +
    theme(
          strip.text.y = element_text(angle = 0))

        if (isTruthy(input$updateH)){
          formatterH <- function(toxic = toxic) {paste(editH()$subtitle, toxic, sep = " ")}
                                  g + facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterH)) + #ylim(c(-1, 1)) +
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
    ) + ggtitle(editH()$title) + theme(plot.title = element_text(size = editH()$titlesize, face = "bold"))
                                }
                                else {
                                  formatterH <- function(toxic = toxic) {paste0("Symptomatic Adverse Event ", toxic)}
                                  g +
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
                                }
}, height = function() input$heightH, width = function() input$widthH)
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
  if (input$ordinal_date == TRUE & input$dummy_ordinal == FALSE){
    tpoint <- as.Date(tpoint)
  }
  updateSliderInput(session, "tpointI", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
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
                     input$numdoseI})

times_I <- reactive({
  req(input$tpointI)
  input$tpointI
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
  g <- propI() %>% ggplot(aes(fill = base_adj, x = dose, y = proportion * 100)) +
    geom_bar(position = "stack", stat = "identity") + coord_flip()
              if (isTruthy(input$updateI)){
                formatterI <- function(SAE = toxicI()) {paste(editI()$subtitle, SAE, sep = " ")}
                                  g +
    facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterI)) +
    scale_fill_manual(breaks = rev(c(1:5, "Unreported")),
                      values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "darkgrey")),
                      labels = rev(c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported")),
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = paste0("Dose (N=", paste(ice_n_patI()$n.pat, collapse = "; "), ")"),
          y = "Percentage (%)",
          fill = "Severity Grade") +
    theme(
          strip.text.y = element_text(angle = 0)) +
                                  ggtitle(editI()$title) + theme(plot.title = element_text(size = editI()$titlesize, face = "bold"))
                                }
                                else {
                                  formatterI <- function(SAE = toxicI()) {paste0("Symptomatic adverse event ", SAE)}
                                  g +
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
                                }
}, height = function() input$heightI, width = function() input$widthI)
##########################################################Download buttons 

output$downloadA <- downloadHandler(
  filename = function(){
    paste0("Kaplan-Meier", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightA * 4, width = input$widthA * 4, units = "px")
  }
)
output$downloadB <- downloadHandler(
  filename = function(){
    paste0("Graph-B", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightB * 4, width = input$widthB * 4, units = "px")
  }
)
output$downloadC <- downloadHandler(
  filename = function(){
    paste0("Graph-C", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightC * 4, width = input$widthC * 4, units = "px")
  }
)
output$downloadD <- downloadHandler(
  filename = function(){
    paste0("Graph-D", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightD * 4, width = input$widthD * 4, units = "px")
  }
)
output$downloadE <- downloadHandler(
  filename = function(){
    paste0("Graph-E", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightE * 4, width = input$widthE * 4, units = "px")
  }
)
output$downloadF <- downloadHandler(
  filename = function(){
    paste0("Graph-F", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = as_gtable(sum.df.wider.gtF()), device = "svg", height = input$heightF * 4, width = input$widthF * 4, units = "px")
  }
)
output$downloadG <- downloadHandler(
  filename = function(){
    paste0("Graph-G", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightG * 4, width = input$widthG * 4, units = "px")
  }
)
output$downloadH <- downloadHandler(
  filename = function(){
    paste0("Graph-H", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightH * 4, width = input$widthH * 4, units = "px")
  }
)
output$downloadI <- downloadHandler(
  filename = function(){
    paste0("Graph-I", Sys.Date(), ".svg")
  },
  content = function(file){
    ggsave(file, plot = last_plot(), device = "svg", height = input$heightI * 4, width = input$widthI * 4, units = "px")
  }
)
}
shinyApp(ui, server)
