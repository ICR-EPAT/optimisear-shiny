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
library(bsicons)
library(gridExtra)
library(survminer)
# rmarkdown needs a pandoc binary to render the "download code" .Rmd reports.
# In the Docker image (and any conda env built from local.yml/environment.yml) pandoc is
# already on the PATH, so only fall back to the `pandoc` R package (which downloads a
# binary from GitHub at start-up) when none is available.
if (!rmarkdown::pandoc_available()) {
  library(pandoc)
  pandoc_install("2.11.4")
  pandoc_activate("2.11.4")
}


ui <- div(page_navbar(
  theme = bslib::bs_theme("bslib-value-box-horizontal-break-point" = "1px"),
  title = span(tags$img(src = "ICR_logo.png"), "OPTIMISE-AR"),
  ############################################################################################# Information
  tabPanel("Overview",
    icon = icon("house"),
    h1("OPTIMISE-AR"),
    layout_columns(
      card(
        card_header(h3("Background"), style = "background-color: #bfd9bf"),
        card_body(
          p("OPTIMISE-AR (incOrporating PaTIent-reported outcoMes In doSE-finding trials–Analysis Recommendations) provides this shiny app to support the generation of patient-reported outcome (PRO) data visualisations for dose-finding oncology trials (DFOT) publications. Informed by critical PRO research objectives in DFOTs[1], this toolkit supports trialists wishing to present PRO data in line with three key PRO endpoint types:"),
          tags$b("Time-to-event PRO endpoints -"),
          p("For DFOTs, a PRO objective could look to assess tolerability across doses by identifying whether patients at higher doses experience earlier undesirable PRO events e.g., symptom worsening or dose-limiting events compared to those at lower doses.  Such objectives are typically described using time-to-event endpoints."),
          tags$b("PRO endpoints across timepoints -"),
          p("For DFOTs, a PRO objective could look to assess tolerability across doses by analysing the longitudinal tolerability profile of a treatment. Such endpoints may assess whether patients at higher doses experience worse tolerability than patients at lower doses at any timepoint within the trial."),
          tags$b("PRO endpoints between timepoints -"),
          p("For DFOTs, a PRO objective could look to assess tolerability by analysing tolerability to treatment at two clinically meaningful timepoints across doses. For example, such endpoints may assess whether patients at higher doses experience worse tolerability at the final assessment timepoint than patients at lower doses when compared to baseline."),
          p("[1] Alger, Emily, et al. \"A practical toolkit with recommendations for analysing and visualising patient-reported outcomes in early phase dose-finding oncology trials (OPTIMISE-AR).\" The Lancet Oncology 27.4 (2026): e218-e230.")
        )
      ),
      card(
        card_header(h3("App workflow"), style = "background-color: #bfd9bf"),
        card_body(
          p("This section provides guidance on how best to utilise this shiny app for data visualisation generation."),
          p("1. Identify the PRO data type you wish to report: ordinal (eg. FACT-GP5, PRO-CTCAE) or continuous (eg. EORTC QLQ-C30)."),
          p("2. Learn more about each figure in the 'About the graphs' section below. The section is partitioned by PRO data type and PRO endpoint types."),
          p("3. Select your data in the ‘Data’ tab, using either the built-in dummy data or uploading your own, with formatting guidelines provided."),
          p("4. Visualise your graphs with the 'Ordinal' and 'Continuous' data tabs."),
          p(" - Customise the size and layout of figures,"),
          p(" - Customise axis and main titles."),
          p("5. Click 'Download' to export your graphs as .svg files."),
          p("6. Click 'Code' to export a .html file of the code (Note: graph aesthetics are determined by the \"Edit\" section)."),
          p("Below, we provide further details on each data visualisation approach. While the examples focus on specific PRO measures (e.g., PRO-CTCAE and EORTC QLQ-C30), the Shiny app can be applied to any PRO measure, provided the data matches the required type for each figure (Ordinal or Continuous).")
        )
      )
    ),
    card(
      card_body(
        h3("About the graphs"),
        style = "background-color: #8FBC8F"
      ),
      card_body(
        page_navbar(
          title = "Select data type",
          id = "about",
          fillable = TRUE,
          sidebar = sidebar(
            conditionalPanel(
              "input.about == 'Ordinal'",
              radioButtons("about_ordinal", label = HTML("<b>Time-to-event PRO endpoints</b>"), choiceValues = c("graphA"), choiceNames = c("Kaplan-Meier")),
              radioButtons("about_ordinal1", label = HTML("<b>PRO endpoints across time points</b>"), choiceValues = c("graphB"), choiceNames = c("Barplot across timepoints"), selected = character(0)),
              radioButtons("about_ordinal2", label = HTML("<b>PRO endpoints between time points</b>"), choiceValues = c("graphH", "graphI"), choiceNames = c("Butterfly plot", "Baseline-adjusted barplot"), selected = character(0))
            ),
            conditionalPanel(
              "input.about == 'Continuous'",
              radioButtons("about_continuous", label = HTML("<b>PRO endpoints across time points</b>"), choiceValues = c("graphC", "graphE", "graphD"), choiceNames = c("Change from baseline across timepoints", "Boxplot across timepoints", "Individual patient trajectory")),
              radioButtons("about_continuous1", label = HTML("<b>PRO endpoints between time points</b>"), choiceValues = c("graphF", "graphG"), choiceNames = c("Change from baseline across PRO domains (table)", "Change from baseline across PRO domains (figure)"), selected = character(0))
            )
          ),
          nav_panel("Ordinal",
            fillable = TRUE,
            layout_columns(
              col_widths = c(9, 3), height = "1000px",
              card(card_header("Graph", style = "background-color: #bfd9bf"), card_body(imageOutput("about_ordinal"), height = "800px"), full_screen = TRUE, fillable = TRUE),
              card(card_header("Information", style = "background-color: #bfd9bf"),
                fill = TRUE,
                card_body(
                  h5("About"), htmlOutput("ordinal_information_about"),
                  h5("Edit functionality"), htmlOutput("ordinal_edit")
                ),
                # card_body(card_title("Limitations"), htmlOutput("ordinal_lims"))
              )
            )
          ),
          nav_panel("Continuous",
            fillable = TRUE,
            layout_columns(
              col_widths = c(9, 3), height = "1000px",
              card(card_header("Graph", style = "background-color: #bfd9bf"), card_body(imageOutput("about_continuous", height = "800px")), full_screen = TRUE, fill = TRUE),
              card(
                card_header("Information", style = "background-color: #bfd9bf"),
                card_body(
                  h5("About"), htmlOutput("continuous_information_about"),
                  h5("Edit functionality"), htmlOutput("continuous_edit")
                ),
                # card_body(card_title("Limitations"), htmlOutput("continuous_lims"))
              )
            )
          )
        )
      )
    )
  ),
  ####################################################################################################### Data Sorting + Upload
  tabPanel("Data",
    icon = icon("arrow-up-from-bracket"),
    card(
      card_header(strong("How to format data"), style = "background-color: #8FBC8F"),
      card_body(
        p("Data must be input as a .csv file with column titles matching the listed below."),
        p("Any columns not fitting the format will be ignored - if not all columns are provided, the app may not run."),
        p("- If a patient experiences an intercurrent event (e.g., discontinuation or death), no further data should be entered beyond that timepoint."),
        p("- If a patient only partially reports a questionnaire at a given timepoint, record all available data and enter 'NA' for any unreported scores or grades."),
        p("- If a patient receives treatment but does not report any part of the questionnaire at that timepoint, record all scores or grades as 'NA'."),
        p("Each row in the .csv file should contain the response to one question within the PRO questionnaire for a single patient. This means that if a questionnaire contains 10 questions and is fully completed by 10 patients at 6 timepoints, the .csv file should contain 600 rows."),
        p("For examplar data, you can toggle the dummy data on - please note, to use your own data the dummy dataset must be toggled off."),
        layout_columns(
          card(
            card_header(strong("Ordinal datasets"), style = "background-color: #bfd9bf"),
            card_body(
              p(strong("Required columns, {column name} and [input types]:")),
              p("- Patient ID, {id}, [numeric]"),
              p("- PRO domain, {toxicity}, [numeric or character]"),
              p("- Grade of response, {grade}, [1,2,3,4,5]"),
              p("- Doses, {dose}, [numeric]"),
              p("- Timepoint of current report, {timepoint},"),
              p("[integer OR date \"YYYY-MM-DD\"]"),
              p("To switch to date format use toggle:"),
              input_switch("ordinal_date", "Turn on date format"),
              p("Date format will toggle automatically but can also be toggled manually")
            )
          ),
          card(
            card_header(strong("Continuous datasets"), style = "background-color: #bfd9bf"),
            card_body(
              p(strong("Required columns and input types:")),
              p("- Patient ID, {id}, [numeric]"),
              p("- PRO domain, {functioning}, [numeric or character]"),
              p("- Score for PRO domain, {score}, [numeric]"),
              p("- Doses, {dose}, [numeric]"),
              p("- Timepoint of current report, {timepoint},"),
              p("[integer OR date \"YYYY-MM-DD\"]"),
              p("To switch to date format use toggle:"),
              input_switch("continuous_date", "Turn on date format"),
              p("Date format will toggle automatically but can also be toggled manually")
            )
          )
        )
      )
    ),
    card(
      card_header(h3("Input data"), style = "background-color: #8FBC8F"),
      layout_sidebar(
        sidebar = sidebar(
          card(
            card_header("Upload data file", style = "background-color: #bfd9bf"), card_body(fileInput("upload", "Ordinal data", accept = c(".csv"))),
            card_body(fileInput("upload_cts", "Continuous data", accept = c(".csv")))
          ),
          card(
            card_header("Dummy data", style = "background-color: #bfd9bf"), card_body("For those with no data wishing to test the app, you can select these to test with."),
            card_body(input_switch("dummy_ordinal", "Dummy Ordinal data")),
            card_body(input_switch("dummy_continuous", "Dummy Continuous data")),
            card_body("Toggle date mode to view alternative data formats")
          )
        ),
        layout_columns(
          col_widths = c(9, 3),
          card(card_header("Ordinal data", style = "background-color: #bfd9bf"), card_body(tableOutput("ordinal_table")), max_height = 300),
          card(card_header("Summaries", style = "background-color: #bfd9bf"),
            value_box(
              title = "No. patients", value = textOutput("ord_summary_pat"),
              theme = value_box_theme(bg = "#d9bfd9", fg = "#000000"),
              showcase = bsicons::bs_icon("person", size = "0.75em"), showcase_layout = "left center",
              full_screen = FALSE, fill = TRUE, height = NULL
            ),
            value_box(
              title = "No. doses", value = textOutput("ord_summary_dose"), ,
              theme = value_box_theme(bg = "#d9bfd9", fg = "#000000"),
              showcase = bsicons::bs_icon("capsule-pill", size = "0.75em"), showcase_layout = "left center",
              full_screen = FALSE, fill = TRUE, height = NULL
            ),
            max_height = 300
          )
        ),
        layout_columns(
          col_widths = c(9, 3),
          card(card_header("Continuous data", style = "background-color: #bfd9bf"), card_body(tableOutput("continuous_table")), max_height = 300),
          card(card_header("Summaries", style = "background-color: #bfd9bf"),
            value_box(
              title = "No. patients", value = textOutput("cts_summary_pat"),
              theme = value_box_theme(bg = "#d9bfd9", fg = "#000000"),
              showcase = bsicons::bs_icon("person", size = "0.75em"), showcase_layout = "left center",
              full_screen = FALSE, fill = TRUE, height = NULL
            ),
            value_box(
              title = "No. doses", value = textOutput("cts_summary_dose"), ,
              theme = value_box_theme(bg = "#d9bfd9", fg = "#000000"),
              showcase = bsicons::bs_icon("capsule-pill", size = "0.75em"), showcase_layout = "left center",
              full_screen = FALSE, fill = TRUE, height = NULL
            ),
            max_height = 300
          )
        )
      )
    )
  ),
  navbarMenu(
    "Ordinal",
    ############################################################################################################ A UI
    tabPanel(
      "Kaplan-Meier",
      card(
        card_header(textOutput("textHeaderA"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
        layout_sidebar(
          sidebar = sidebar(
            card(
              card_header("Variables", style = "background-color: #bfd9bf"),
              card_body(checkboxGroupInput("numdose", label = "Dosage", selected = NULL, inline = TRUE),
                selectInput("baseChange", label = "Minimum change from baseline score to define event", choices = c(1, 2, 3, 4, 5), selected = 3),
                uiOutput("toxicA"),
                input_switch("allA", label = "Select all PRO domains"),
                height = 800
              )
            ),
            card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(
              downloadButton("downloadA", style = "background-color: #d9bfd9"),
              downloadButton("downloadAcode", style = "background-color: #d9bfd9", label = "Code")
            ))
          ),
          card(card_header(
            class = "d-inline-flex justify-content-end bg-light",
            popover(
              actionButton("sizeA", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
              title = "Adjust output size",
              id = "dimA",
              sliderInput("widthA", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
              sliderInput("heightA", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
            ),
            popover(
              actionButton("editA", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
              title = "Graph Editor",
              id = "popA",
              placement = "right",
              textInput("titleA", label = NULL, placeholder = "Enter graph title"),
              sliderInput("titlesizeA", label = "Title size", min = 10, max = 50, value = 20, step = 5),
              textInput("ylab_A", label = NULL, placeholder = "Enter y-axis label"),
              textInput("xlab_A", label = NULL, placeholder = "Enter x-axis label"),
              sliderInput("axisSizeA", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
              sliderInput("box_xA", label = "Median table x-axis location", min = -0.5, max = 20, value = -0.5, step = 0.5),
              sliderInput("box_yA", label = "Median table y-axis location", min = -0.05, max = 1, value = -0.05, step = 0.05),
              actionButton("updateA", "Update")
            )
          ), card_body(plotOutput("graphA"), height = 1000), full_screen = TRUE)
        )
      )
    ),
    ############################################################################################################ B UI
    tabPanel(
      "Barplot across timepoints",
      card(
        card_header(textOutput("textHeaderB"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
        layout_sidebar(
          sidebar = sidebar(
            card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
              checkboxGroupInput("numdoseB",
                label = "Dosage",
                selected = NULL, inline = TRUE
              ),
              sliderInput("tpointB", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
              selectInput("toxicB", label = "Toxicity of interest", choices = NULL),
              height = 800
            )), card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadB", style = "background-color: #d9bfd9"), downloadButton("downloadBcode", style = "background-color: #d9bfd9", label = "Code")))
          ),
          card(card_header(
            class = "d-inline-flex justify-content-end bg-light",
            popover(
              actionButton("sizeB", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
              title = "Adjust output size",
              id = "dimB",
              sliderInput("widthB", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
              sliderInput("heightB", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
            ),
            popover(
              actionButton("editB", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
              title = "Graph Editor",
              id = "popB",
              placement = "right",
              textInput("titleB", label = NULL, placeholder = "Enter graph title"),
              sliderInput("titlesizeB", label = "Title size", min = 10, max = 50, value = 20, step = 5),
              textInput("xlab_B", label = NULL, placeholder = "Enter x-axis label"),
              sliderInput("axisSizeB", label = "Axis title size", min = 5, max = 40, value = 10, step = 5),
              input_switch("showExtraB", label = "Show Unreported and Discontinued", value = TRUE),
              input_switch("invertB", label = "Reverse order of grades", value = FALSE),
              actionButton("updateB", "Update")
            )
          ), card_body(plotOutput("graphB"), height = 1000), full_screen = TRUE)
        )
      )
    ),
    ############################################################################################################ H UI

    tabPanel(
      "Butterfly plot",
      card(
        card_header(textOutput("textHeaderH"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
        layout_sidebar(
          sidebar = sidebar(
            card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
              checkboxGroupInput("numdoseH",
                label = "Dosage",
                selected = NULL, inline = TRUE
              ),
              sliderInput("tpointH", label = "Start and endpoint for comparison", min = 0, max = 1, value = c(0, 1), step = 1),
              checkboxGroupInput("toxicH", label = "Toxicity of interest", choices = NULL, inline = TRUE),
              height = 800
            )),
            card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadH", style = "background-color: #d9bfd9"), downloadButton("downloadHcode", style = "background-color: #d9bfd9", label = "Code")))
          ),
          card(card_header(
            class = "d-inline-flex justify-content-end bg-light",
            popover(
              actionButton("sizeH", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
              title = "Adjust output size",
              id = "dimH",
              sliderInput("widthH", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
              sliderInput("heightH", "Adjust graph height", value = 800, min = 300, max = 2000, step = 100, width = "400px")
            ),
            popover(
              actionButton("editH", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
              title = "Graph Editor",
              id = "popH",
              placement = "right",
              textInput("titleH", label = NULL, placeholder = "Enter graph title"),
              sliderInput("titlesizeH", label = "Title size", min = 10, max = 50, value = 20, step = 5),
              sliderInput("unrep_positionH", label = "Unreported text position", min = -1, max = 1, value = 0.7, step = 0.1),
              textInput("subtitleH", label = NULL, placeholder = "Enter subgraph label"),
              actionButton("updateH", "Update")
            )
          ), card_body(plotOutput("graphH"), height = 1000), full_screen = TRUE)
        )
      )
    ),
    ############################################################################################################ I UI
    tabPanel(
      "Baseline-adjusted barplot",
      card(
        card_header(textOutput("textHeaderI"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
        layout_sidebar(
          sidebar = sidebar(
            card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
              checkboxGroupInput("numdoseI",
                label = "Dosage",
                selected = NULL, inline = TRUE
              ),
              sliderInput("tpointI", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
              checkboxGroupInput("toxicI", label = "Toxicity of interest", selected = NULL, inline = TRUE),
              height = 800
            )),
            card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadI", style = "background-color: #d9bfd9"), downloadButton("downloadIcode", style = "background-color: #d9bfd9", label = "Code")))
          ),
          card(card_header(
            class = "d-inline-flex justify-content-end bg-light",
            popover(
              actionButton("sizeI", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
              title = "Adjust output size",
              id = "dimI",
              sliderInput("widthI", "Adjust graph width", value = 800, min = 300, max = 1200, step = 100, width = "400px"),
              sliderInput("heightI", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
            ),
            popover(
              actionButton("editI", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
              title = "Graph Editor",
              id = "popI",
              placement = "right",
              textInput("titleI", label = NULL, placeholder = "Enter graph title"),
              sliderInput("titlesizeI", label = "Title size", min = 10, max = 50, value = 20, step = 5),
              textInput("subtitleI", label = NULL, placeholder = "Enter subgraph label"),
              input_switch("showExtraI", label = "Show Unreported and Discontinued", value = TRUE),
              input_switch("invertI", label = "Reverse order of grades", value = FALSE),
              actionButton("updateI", "Update")
            )
          ), card_body(plotOutput("graphI"), height = 1000), full_screen = TRUE)
        )
      )
    )
  ),
  navbarMenu(
    "Continuous", ############################################################################################################ C UI
    tabPanel(
      "Change from baseline across timepoints",
      card(
        card_header(textOutput("textHeaderC"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
        layout_sidebar(
          sidebar = sidebar(
            card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
              checkboxGroupInput("numdoseC",
                label = "Dosage",
                selected = NULL, inline = TRUE
              ),
              sliderInput("tpointC", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
              selectInput("functioningC", label = "PRO domain of interest", choices = NULL),
              height = 800
            )),
            card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadC", style = "background-color: #d9bfd9"), downloadButton("downloadCcode", style = "background-color: #d9bfd9", label = "Code")))
          ),
          card(card_header(
            class = "d-inline-flex justify-content-end bg-light",
            popover(
              actionButton("sizeC", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
              title = "Adjust output size",
              id = "dimC",
              sliderInput("widthC", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
              sliderInput("heightC", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
            ),
            popover(
              actionButton("editC", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
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
              actionButton("updateC", "Update")
            )
          ), card_body(plotOutput("graphC"), height = 1000), full_screen = TRUE)
        )
      )
    ),
    ############################################################################################################ E UI

    tabPanel("Boxplot across timepoints", card(
      card_header(textOutput("textHeaderE"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
      layout_sidebar(
        sidebar = sidebar(
          card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
            checkboxGroupInput("numdoseE", label = "Dosage", selected = NULL, inline = TRUE),
            sliderInput("tpointE", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            selectInput("functioningE", label = "PRO domain of interest", choices = NULL),
            height = 800
          )),
          card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadE", style = "background-color: #d9bfd9"), downloadButton("downloadEcode", style = "background-color: #d9bfd9", label = "Code")))
        ),
        card(card_header(
          class = "d-inline-flex justify-content-end bg-light",
          popover(
            actionButton("sizeE", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
            title = "Adjust output size",
            id = "dimE",
            sliderInput("widthE", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightE", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
          ),
          popover(
            actionButton("editE", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
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
          )
        ), card_body(plotOutput("graphE"), height = 1000), full_screen = TRUE)
      )
    )),
    ############################################################################################################ D UI
    tabPanel("Individual patient trajectory", card(
      card_header(textOutput("textHeaderD"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
      layout_sidebar(
        sidebar = sidebar(
          card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
            sliderInput("tpointD", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            checkboxGroupInput("numdoseD", label = "Dosage", choices = NULL),
            uiOutput("select_idD"),
            uiOutput("allD"),
            selectInput("functioningD", label = "PRO domain of interest", choices = NULL),
            height = 800
          )),
          card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadD", style = "background-color: #d9bfd9"), downloadButton("downloadDcode", style = "background-color: #d9bfd9", label = "Code")))
        ),
        card(card_header(
          class = "d-inline-flex justify-content-end bg-light",
          popover(
            actionButton("sizeD", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
            title = "Adjust output size",
            id = "dimD",
            sliderInput("widthD", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightD", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
          ),
          popover(
            actionButton("editD", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
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
          )
        ), card_body(plotOutput("graphD"), height = 1000), full_screen = TRUE)
      )
    )),

    ############################################################################################################ F UI
    tabPanel("Change from baseline across PRO domains (table)", card(
      card_header(textOutput("textHeaderF"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
      layout_sidebar(
        sidebar = sidebar(
          card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
            checkboxGroupInput("numdoseF",
              label = "Dosage",
              selected = NULL, inline = TRUE
            ),
            sliderInput("tpointF", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            checkboxGroupInput("functioningF", label = "PRO domain of interest", selected = NULL, inline = TRUE),
            height = 800
          )),
          card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadF", style = "background-color: #d9bfd9"), downloadButton("downloadFcode", style = "background-color: #d9bfd9", label = "Code")))
        ),
        card(card_header(
          class = "d-inline-flex justify-content-end bg-light",
          popover(
            actionButton("sizeF", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
            title = "Adjust output size",
            id = "dimF",
            sliderInput("widthF", "Adjust graph width", value = 600, min = 300, max = 1200, step = 100, width = "400px")
          ),
          popover(
            actionButton("editF", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
            title = "Graph Editor",
            id = "popB",
            placement = "right",
            textInput("titleF", label = NULL, placeholder = "Enter table title"),
            textInput("column1F", label = NULL, placeholder = "Enter first column name"),
            actionButton("updateF", "Update")
          )
        ), card_body(gt_output("graphF"), height = 1000), full_screen = TRUE)
      )
    )),
    ############################################################################################################ G UI

    tabPanel("Change from baseline across PRO domains (figure)", card(
      card_header(textOutput("textHeaderG"), style = "background-color: #bfd9bf; font-size: 25px; height: 50px; vertical-align: middle; padding: 12.5px"),
      layout_sidebar(
        sidebar = sidebar(
          card(card_header("Variables", style = "background-color: #bfd9bf"), card_body(
            checkboxGroupInput("numdoseG",
              label = "Dosage",
              selected = NULL, inline = TRUE
            ),
            sliderInput("tpointG", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1),
            checkboxGroupInput("functioningG", label = "PRO domain of interest", selected = NULL, inline = TRUE),
            height = 800
          )),
          card(card_header("Download .svg file", style = "background-color: #bfd9bf"), card_body(downloadButton("downloadG", style = "background-color: #d9bfd9"), downloadButton("downloadGcode", style = "background-color: #d9bfd9", label = "Code")))
        ),
        card(card_header(
          class = "d-inline-flex justify-content-end bg-light",
          popover(
            actionButton("sizeG", "Size", value = FALSE, icon = icon("arrows-up-down-left-right"), style = "background-color: #bfd9bf"),
            title = "Adjust output size",
            id = "dimG",
            sliderInput("widthG", "Adjust graph width", value = 800, min = 300, max = 1600, step = 100, width = "400px"),
            sliderInput("heightG", "Adjust graph height", value = 800, min = 300, max = 1000, step = 100, width = "400px")
          ),
          popover(
            actionButton("editG", "Edit", value = FALSE, icon = icon("pencil-square"), style = "background-color: #bfd9bf"),
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
            actionButton("updateG", "Update")
          )
        ), card_body(plotOutput("graphG"), height = 1000), full_screen = TRUE)
      )
    ))
  ),
  ################################################################################################################## description tab
  tabPanel("Code",
    tabName = "description", icon = icon("book"), h4("About this site"),
    layout_columns(
      card(card_header("Code availability", style = "background-color: #bfd9bf"), card_body(
        p("This app accompanies the OPTIMISE-AR project."),
        p("If you wish to utilise figures curated for OPTIMISE-AR for your own publications, the 'code download' feature may be used. The original R code accompanying the OPTIMISE-AR project is provided here: https://github.com/alemily100/optimise-ar"),
        p("We also provide code for the Shiny app itself, which can be accessed here: https://github.com/ICR-EPAT/optimisear-shiny.")
      )),
      card(card_header("OPTIMISE-AR", style = "background-color: #bfd9bf"), card_body(
        p("This is an early public release of the app. There are a few known bugs, if you find any additional, please do get in touch so they can be patched."),
        p("Authors: George Knight, Emily Alger, Karrar Shareiyat"),
        p("Maintainers: Rachel Alcraft, ICR Scientific Computing")
      ))
    )
  ),
  ################################################################################################################## Version history
  tabPanel("Version History",
    tabName = "history", icon = icon("history"),
    card(
      card_header("Versions", style = "background-color: #bfd9bf"),
      card_body(
        p("v1.0 - Internal ICR release 06/08/2025"),
        p("v1.1 - Internal ICR re-release 27/08/2026")
      )
    )
  ), bg = "#8FBC8F",
  ######################################### Dealing with the long popovers
  tags$head(tags$style(HTML("
      .popover {
        max-height: 600px;
        max-width: 300px;
        overflow-y: auto;
        overflow-x: hidden;
      }
      .shiny-output-error-style{
      color: red;
      font-size: 20px;
      font-weight: bold;
      }
      .bslib-value-box
      .value-box-title {font-size:14px;}")))
))

#######################################################################################################################   SERVER SIDE

server <- function(input, output, session) {
  output$textHeaderA <- renderText({
    "Kaplan-Meier"
  })
  output$textHeaderB <- renderText({
    "Barplot across timepoints"
  })
  output$textHeaderC <- renderText({
    "Change from baseline across timepoints"
  })
  output$textHeaderD <- renderText({
    "Individual patient trajectory"
  })
  output$textHeaderE <- renderText({
    "Boxplot across timepoints"
  })
  output$textHeaderF <- renderText({
    "Change from baseline across PRO domains (table)"
  })
  output$textHeaderG <- renderText({
    "Change from baseline across PRO domains (figure)"
  })
  output$textHeaderH <- renderText({
    "Butterfly plot"
  })
  output$textHeaderI <- renderText({
    "Baseline-adjusted barplot"
  })
  ############################################################################################ Overview page
  about_ordinal <- reactive({
    if (isTruthy(input$about_ordinal)) {
      input$about_ordinal
    } else if (isTruthy(input$about_ordinal1)) {
      input$about_ordinal1
    } else if (isTruthy(input$about_ordinal2)) {
      input$about_ordinal2
    }
  })

  about_continuous <- reactive({
    if (isTruthy(input$about_continuous)) {
      input$about_continuous
    } else if (isTruthy(input$about_continuous1)) {
      input$about_continuous1
    }
  })

  observe({
    req(input$about_ordinal)
    updateRadioButtons(session, "about_ordinal1", selected = character(0))
    updateRadioButtons(session, "about_ordinal2", selected = character(0))
  })
  observe({
    req(input$about_ordinal1)
    updateRadioButtons(session, "about_ordinal", selected = character(0))
    updateRadioButtons(session, "about_ordinal2", selected = character(0))
  })
  observe({
    req(input$about_ordinal2)
    updateRadioButtons(session, "about_ordinal", selected = character(0))
    updateRadioButtons(session, "about_ordinal1", selected = character(0))
  })

  observe({
    req(input$about_continuous)
    updateRadioButtons(session, "about_continuous1", selected = character(0))
  })
  observe({
    req(input$about_continuous1)
    updateRadioButtons(session, "about_continuous", selected = character(0))
  })

  ############################################# GRAPH IMAGE
  output$about_ordinal <- renderImage(
    {
      about_ordinal <- about_ordinal()
      if (about_ordinal == "graphA") {
        list(src = "www/GraphExamples/graphA.svg")
      } else if (about_ordinal == "graphB") {
        list(src = "www/GraphExamples/graphB.svg", height = "800px")
      } else if (about_ordinal == "graphH") {
        list(src = "www/GraphExamples/graphH.svg", height = "800px")
      } else if (about_ordinal == "graphI") {
        list(src = "www/GraphExamples/graphI.svg", height = "800px")
      }
    },
    deleteFile = FALSE
  )

  output$about_continuous <- renderImage(
    {
      about_continuous <- about_continuous()
      if (about_continuous == "graphC") {
        list(src = "www/GraphExamples/graphC.svg")
      } else if (about_continuous == "graphD") {
        list(src = "www/GraphExamples/graphD.svg", height = "800px")
      } else if (about_continuous == "graphE") {
        list(src = "www/GraphExamples/graphE.svg", height = "800px")
      } else if (about_continuous == "graphF") {
        list(src = "www/GraphExamples/graphF.png", height = "500px")
      } else if (about_continuous == "graphG") {
        list(src = "www/GraphExamples/graphG.svg", height = "800px")
      }
    },
    deleteFile = FALSE
  )

  ##################################################### GRAPH INFO

  output$ordinal_information_about <- renderUI({
    about_ordinal <- about_ordinal()
    if (about_ordinal == "graphA") {
      HTML("Kaplan-Meier figures present tolerability across doses by identifying whether patients at higher doses experience earlier undesirable PRO events (e.g., symptom worsening or dose-limiting events) compared to those at lower doses.
    This Kaplan-Meier visualises the time to first baseline-adjusted Grade >= a selected score change. <br> In cases where a patient's post-baseline PRO score increases by at least one compared to baseline, the baseline-adjusted score is this post-baseline value. Otherwise, if the patient's baseline and post-baseline score are equal, their baseline-adjusted score is 0 (or 'None').
    <br><br> This app allows you to select: <br>- The doses you wish to display <br>- The minimum change from baseline score to define event (this is set to 3 by default) <br>- The PRO domain(s) defining event")
    } else if (about_ordinal == "graphB") {
      html("This figure presents the proportion of patients experiencing each ordinal score across timepoints as well as the proportion of patients with unreported PRO data or who experience an intercurrent event.<br> <br> This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint you wish to display, <br>- Which PRO domain you wish to display.")
    } else if (about_ordinal == "graphH") {
      html("This figure presents differences between ordinal PRO scores as both a magnitude of worsening/improvement between two timepoints of interest across doses and PRO domains. This figure also indicates the proportion of patients with unreported PRO data at timepoints of interest. <br> <br> This app allows you to select: <br>- The doses you wish to display, <br>- The two timepoints you wish to compare, <br>- Which PRO domains you wish to display.")
    } else if (about_ordinal == "graphI") {
      html("This figure presents maximum baseline-adjusted PRO score between the baseline and a timepoint of interest across doses and PRO domains.
     In cases where a patient's post-baseline PRO score increases by at least one compared to baseline, the maximum baseline-adjusted score is this maximum post-baseline value. Otherwise, if the patient's baseline and post-baseline score are always equal, their maximum baseline-adjusted score is 0 (or 'None')  <br> <br>This app allows you to select: <br>- The doses you wish to display, <br>- The final time point for analysis <br>- Which PRO domains you wish to display.")
    }
  })

  output$continuous_information_about <- renderUI({
    about_continuous <- about_continuous()
    if (about_continuous == "graphC") {
      html("This figure evaluates mean change from baseline across time for continuous PRO scores. Associated improvement and worsening thresholds as well 95% confidence intervals for each timepoint are indicated.
    <br> <br>This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint you wish to display, <br>- The PRO domain you wish to display.")
    } else if (about_continuous == "graphD") {
      html("This figure presents individual patient trajectories for continuous PRO scores. <br> <br>This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint to display, <br>- The PRO domain you wish to display, <br> - The patients you wish to display. ")
    } else if (about_continuous == "graphE") {
      html("This figure presents boxplots summarising the distribution of continuous PRO scores across time. <br> <br>This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint to display, <br>- The PRO domain you wish to display.")
    } else if (about_continuous == "graphF") {
      html("This table indicates mean change from baseline in continuous PRO score between the baseline and a selected timepoint with standard error across different PRO domains. The number of patients who reported PROs at each dose is indicated.<br> <br> This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint for analysis, <br>- Which PRO domains you wish to display.")
    } else if (about_continuous == "graphG") {
      html("This figure indicates mean change from baseline in continuous PRO score between the baseline and a selected timepoint with standard error across different PRO domains. The number of patients who reported PROs at each dose is indicated.<br> <br> This app allows you to select: <br>- The doses you wish to display, <br>- The final timepoint for analysis, <br>- Which PRO domains you wish to display.")
    }
  })

  ############################################################# GRAPH Editor
  output$ordinal_edit <- renderUI({
    about_ordinal <- about_ordinal()
    if (about_ordinal == "graphA") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size  <br> - Graph height and width")
    } else if (about_ordinal == "graphB") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size <br> - X axis label  <br> - Axis label font size  <br> - Graph height and width <br> - Toggle Unreported & Discontinued <br> - Invert the order of grades")
    } else if (about_ordinal == "graphH") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size <br> - Subgraph label format  <br> - Position of unreported percentage <br> - Graph height and width")
    } else if (about_ordinal == "graphI") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size <br> - Subgraph label format  <br> - Graph height and width <br> - Toggle Unreported & Discontinued <br> - Invert the order of grades")
    }
  })
  output$continuous_edit <- renderUI({
    about_continuous <- about_continuous()
    if (about_continuous == "graphC") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Boundaries for improvement <br> - Boundaries for worsening <br> - Position of improvement text <br> - Position of worsening text <br> - Graph height and width")
    } else if (about_continuous == "graphD") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Arrow label <br> - Y axis range <br> - Graph height and width")
    } else if (about_continuous == "graphE") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label  <br> - X axis label  <br> - Axis label font size <br> - Arrow label <br> - Y axis range <br> - Graph height and width")
    } else if (about_continuous == "graphF") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Table title <br> - First column name <br> - Table width")
    } else if (about_continuous == "graphG") {
      HTML("This app supports the customising of the figure to your needs, including: <br> - Graph title  <br> - Graph title font size  <br> - Y axis label <br> - Axis label font size <br> - Subgraph label format <br> - Boundaries for improvement <br> - Boundaries for worsening <br> - Position of improvement text <br> - Position of worsening text <br> - Graph height and width")
    }
  })


  ############################################################ GRAPH CONSIDERATIONS
  output$ordinal_lims <- renderUI({
    about_ordinal <- about_ordinal()
    if (about_ordinal == "graphA") {
      HTML("- Currently only allows baseline change of 3 as an event <br> - Missing patients aren't denoted on graph")
    } else if (about_ordinal == "graphB") {
      HTML("B <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_ordinal == "graphH") {
      HTML("H <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_ordinal == "graphI") {
      HTML("I <br> -con 1 <br> -con 2 <br> -con 3")
    }
  })
  output$continuous_lims <- renderUI({
    about_continuous <- about_continuous()
    if (about_continuous == "graphC") {
      HTML("C <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_continuous == "graphD") {
      HTML("D <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_continuous == "graphE") {
      HTML("E <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_continuous == "graphF") {
      HTML("F <br> -con 1 <br> -con 2 <br> -con 3")
    } else if (about_continuous == "graphG") {
      HTML("G <br> -con 1")
    }
  })
  ################################################################################################################ Server side for graph editors

  editA <- eventReactive(input$updateA, {
    list(
      title = input$titleA,
      titlesize = input$titlesizeA,
      y_lab = input$ylab_A,
      x_lab = input$xlab_A,
      axissize = input$axisSizeA,
      box_x = input$box_xA,
      box_y = input$box_yA
    )
  })
  editB <- eventReactive(input$updateB, {
    list(
      title = input$titleB,
      titlesize = input$titlesizeB,
      x_lab = input$xlab_B,
      axissize = input$axisSizeB,
      showExtra = input$showExtraB,
      invert = input$invertB
    )
  })
  editC <- eventReactive(input$updateC, {
    list(
      title = input$titleC,
      titlesize = input$titlesizeC,
      y_lab = input$ylab_C,
      x_lab = input$xlab_C,
      axissize = input$axisSizeC,
      imp_bound = input$improveC,
      wor_bound = input$worseC,
      imp_text = input$impTextC,
      wor_text = input$worTextC
    )
  })
  editD <- eventReactive(input$updateD, {
    list(
      title = input$titleD,
      titlesize = input$titlesizeD,
      y_lab = input$ylab_D,
      x_lab = input$xlab_D,
      axissize = input$axisSizeD,
      arrow_lab = input$arrowlab_D,
      range = input$rangeD
    )
  })
  editE <- eventReactive(input$updateE, {
    list(
      title = input$titleE,
      titlesize = input$titlesizeE,
      y_lab = input$ylab_E,
      x_lab = input$xlab_E,
      axissize = input$axisSizeE,
      arrow_lab = input$arrowlab_E,
      range = input$rangeE
    )
  })
  titleF <- eventReactive(input$updateF, {
    input$titleF
  })
  editG <- eventReactive(input$updateG, {
    list(
      title = input$titleG,
      titlesize = input$titlesizeG,
      y_lab = input$ylab_G,
      axissize = input$axisSizeG,
      subgraph_lab = input$sublab_G,
      imp_bound = input$improveG,
      wor_bound = input$worseG,
      imp_text = input$impTextG,
      wor_text = input$worTextG
    )
  })
  editH <- eventReactive(input$updateH, {
    list(
      title = input$titleH,
      titlesize = input$titlesizeH,
      subtitle = input$subtitleH,
      location = input$unrep_positionH
    )
  })
  editI <- eventReactive(input$updateI, {
    list(
      title = input$titleI,
      titlesize = input$titlesizeI,
      subtitle = input$subtitleI,
      showExtra = input$showExtraI,
      invert = input$invertI
    )
  })
  ############################################################################################################# Graph A
  # Import the data
  ordinal_data <- reactive({
    if (input$dummy_ordinal == FALSE) {
      req(input$upload)
      if (tools::file_ext(input$upload$name) != "csv") {
        validate("Invalid upload: not .csv file", errorClass = "style")
      }
      data <- read.csv(input$upload$datapath, header = TRUE, sep = ",")
      if (all(c("id", "toxicity", "grade", "timepoint", "dose") %in% colnames(data)) == FALSE) {
        validate(
          need("id" %in% colnames(data), "Missing id column"),
          need("toxicity" %in% colnames(data), "Missing toxicity column"),
          need("grade" %in% colnames(data), "Missing grade column"),
          need("timepoint" %in% colnames(data), "Missing timepoint column"),
          need("dose" %in% colnames(data), "Missing dose column"),
          errorClass = "style"
        )
      }
      data <- data %>% select(id, toxicity, grade, timepoint, dose)
      data
    } else if (input$dummy_ordinal == TRUE & input$ordinal_date == FALSE) {
      read.csv("www/DummyData/dummy_ordinal.csv", header = TRUE, sep = ",")[, -1]
    } else if (input$dummy_ordinal == TRUE & input$ordinal_date == TRUE) {
      read.csv("www/DummyData/dummy_ordinal_dates.csv", header = TRUE, sep = ",")[, -1]
    }
  })
  observeEvent(input$upload, {
    if (class(ordinal_data()$timepoint[1]) == "character") {
      showNotification("Timepoint of format \"YYYY-MM-DD\" detected in input, auto-switching ON date mode. Switch back OFF if not a date.", type = "warning")
    } else if (class(ordinal_data()$timepoint[1]) == "integer") {
      showNotification("Timepoint of format integer detected in input, auto-switching OFF date mode. Switch back ON if a date.", type = "warning")
    }
  })
  observe({
    req(input$upload)
    if (class(ordinal_data()$timepoint[1]) == "character") {
      update_switch(session = session, id = "ordinal_date", value = TRUE)
    } else if (class(ordinal_data()$timepoint[1]) == "integer") {
      update_switch(session = session, id = "ordinal_date", value = FALSE)
    }
  })

  output$ord_summary_pat <- renderText(
    length(unique(ordinal_data()$id))
  )

  output$ord_summary_dose <- renderText(
    length(unique(ordinal_data()$dose))
  )

  output$ordinal_table <- renderTable(
    ordinal_data()
  )

  output$continuous_table <- renderTable(
    continuous_data()
  )

  # Update the selection boxes based on dataset
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

  output$toxicA <- renderUI({
    if (isTruthy(input$upload)) {
      ordinal_data <- ordinal_data()
      toxic <- unique(ordinal_data$toxicity)
      if (input$allA == FALSE) {
        selectizeInput("toxicA", label = "PRO domain(s) defining event", choices = toxic, selected = NULL, multiple = TRUE)
      } else {
        selectizeInput("toxicA", label = "PRO domain(s) defining event", choices = toxic, selected = toxic, multiple = TRUE)
      }
    } else if (isTruthy(input$dummy_ordinal)) {
      ordinal_data <- ordinal_data()
      toxic <- unique(ordinal_data$toxicity)
      if (input$allA == FALSE) {
        selectizeInput("toxicA", label = "PRO domain(s) defining event", choices = toxic, selected = NULL, multiple = TRUE)
      } else {
        selectizeInput("toxicA", label = "PRO domain(s) defining event", choices = toxic, selected = toxic, multiple = TRUE)
      }
    }
  })

  observe({
    if (isTruthy(input$upload)) {
      if (input$ordinal_date == FALSE) {
        ordinal_data <- ordinal_data()
        maxTime <- max(as.numeric(ordinal_data$timepoint)) - min(as.numeric(ordinal_data$timepoint))
        updateSliderInput(session, "box_xA", min = -0.5, max = 0.25 * maxTime, value = 0.1 * maxTime, step = 0.5)
      } else {
        ordinal_data <- ordinal_data()
        maxTime <- as.numeric(max(as.Date(ordinal_data$timepoint)) - min(as.Date(ordinal_data$timepoint)))
        updateSliderInput(session, "box_xA", min = -0.5, max = 0.25 * maxTime, value = 0.1 * maxTime, step = 0.5)
      }
    } else if (isTruthy(input$dummy_ordinal)) {
      if (input$ordinal_date == FALSE) {
        ordinal_data <- ordinal_data()
        maxTime <- max(as.numeric(ordinal_data$timepoint)) - min(as.numeric(ordinal_data$timepoint))
        updateSliderInput(session, "box_xA", min = -0.5, max = 0.25 * maxTime, value = 0.1 * maxTime, step = 0.5)
      } else {
        ordinal_data <- ordinal_data()
        maxTime <- as.numeric(max(as.Date(ordinal_data$timepoint)) - min(as.Date(ordinal_data$timepoint)))
        updateSliderInput(session, "box_xA", min = -0.5, max = 0.25 * maxTime, value = 0.1 * maxTime, step = 0.5)
      }
    }
  })
  # Define the relevant variables
  no_dose <- reactive({
    req(input$numdose)
    input$numdose
  })

  toxicA <- reactive({
    req(input$toxicA)
    input$toxicA
  })

  baseA <- reactive({
    req(input$baseChange)
    as.numeric(input$baseChange)
  })
  # Filter the data
  summary_df <- reactive({
    req(input$numdose)
    req(input$baseChange)
    req(input$toxicA)
    change <- baseA()
    ordinal_data <- ordinal_data()
    ordinal_data <- ordinal_data %>%
      filter(dose %in% no_dose()) %>%
      filter(toxicity %in% toxicA())
    if (input$ordinal_date == TRUE) {
      ordinal_data <- ordinal_data %>%
        group_by(id, toxicity) %>%
        mutate(base_adj = grade - grade[timepoint == min(as.Date(ordinal_data$timepoint))])
    } else {
      ordinal_data <- ordinal_data %>%
        group_by(id, toxicity) %>%
        mutate(base_adj = grade - grade[timepoint == min(ordinal_data$timepoint)])
    }
    ordinal_data <- ordinal_data %>% filter(is.na(base_adj) == FALSE)
    if (input$ordinal_date == TRUE) {
      summary_df <- ordinal_data %>%
        group_by(id) %>%
        summarise(
          time = if (any(base_adj >= change)) as.numeric(min(as.Date(timepoint[base_adj >= change])) - min(as.Date(timepoint))) else as.numeric(max(as.Date(timepoint)) - min(as.Date(timepoint))),
          event = as.integer(any(base_adj >= change)) # 1 if grade 3 occurred, 0 if censored
        )
    } else {
      summary_df <- ordinal_data %>%
        group_by(id) %>%
        summarise(
          time = if (any(base_adj >= change)) as.numeric(min(timepoint[base_adj >= change]) - min(timepoint)) else as.numeric(max(timepoint) - min(timepoint)),
          event = as.integer(any(base_adj >= change))
        ) # 1 if grade 3 occurred, 0 if censored
    }
    summary_df <- cbind(summary_df, ordinal_data %>% group_by(id) %>% dplyr::summarise(max(dose)))
    colnames(summary_df)[5] <- "dose"
    summary_df
  })

  fitA <- reactive({
    survfit2(Surv(time, event) ~ dose, data = summary_df())
  })

  median_dfA <- reactive({
    median_df <- survminer::surv_median(fitA()) %>%
      dplyr::select(strata, median) %>%
      rename(
        Dose = strata,
        "Median time \n to event" = median
      )
    median_df[, 1] <- paste0("Dose: ", unique(summary_df()$dose))
    median_df
  })
  tbl_grob <- reactive({
    tableGrob(
      median_dfA(),
      rows = NULL,
      theme = ttheme_default(
        core = list(
          fg_params = list(cex = 0.7, col = "black"), # text
          bg_params = list(fill = NA, col = "black") # no fill, black borders
        ),
        colhead = list(
          fg_params = list(cex = 0.8, fontface = "bold", col = "black"),
          bg_params = list(fill = NA, col = "black") # no fill, black borders
        )
      )
    )
  })
  summary_dfA <- reactive({
    data <- summary_df()
    data$dose <- paste0("Dose: ", data$dose)
    data
  })

  output$graphA <- renderPlot(
    {
      g <- survfit2(Surv(time, event) ~ dose, data = summary_dfA()) %>%
        ggsurvfit(linewidth = 1.5) +
        add_censor_mark() +
        add_confidence_interval() +
        scale_ggsurvfit() + add_risktable()
      if (isTruthy(input$updateA)) {
        editA <- editA()
        g + ggtitle(editA$title) + theme(plot.title = element_text(size = editA$titlesize, face = "bold"), axis.title = element_text(size = editA$axissize)) +
          labs(y = editA$y_lab, x = editA$x_lab) +
          annotation_custom(
            tbl_grob(),
            xmin = editA$box_x, xmax = editA$box_x + 2, # x-position range for table
            ymin = editA$box_y, ymax = editA$box_y + 0.3 # y-position range for table
          )
      } else {
        g + labs(y = paste0("No. first baseline-adjusted \n Grade ≥", baseA(), " PRO-CTCAE score")) +
          annotation_custom(
            tbl_grob(),
            xmin = -0.5, xmax = 1.75, # x-position range for table
            ymin = 0.05, ymax = 0.35 # y-position range for table
          )
      }
    },
    height = function() input$heightA,
    width = function() input$widthA
  )

  ################################################ graph B


  # Update the selection boxes based on dataset
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
    if (input$ordinal_date == TRUE) {
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
    if (input$ordinal_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointB", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
  })

  observe({
    req(input$dummy_ordinal)
    data <- ordinal_data()
    toxic <- unique(data$toxicity)
    updateSelectInput(session, "toxicB", label = "Toxicity of interest", choices = toxic, selected = NULL)
  })

  # Define the relevant variables
  no_doseb <- reactive({
    req(input$numdoseB)
    input$numdoseB
  })

  final_t <- reactive({
    req(input$tpointB)
    input$tpointB
  })

  toxic <- reactive({
    req(input$toxicB)
    input$toxicB
  })

  # Filter the data

  prop <- reactive({
    req(input$numdoseB)
    req(input$tpointB)
    ordinal_data <- ordinal_data()
    ordinal_data$grade[is.na(ordinal_data$grade)] <- "Unreported"

    ordinal_data <- ordinal_data %>%
      filter(dose %in% no_doseb()) %>%
      filter(toxicity %in% toxic())

    if (input$ordinal_date == TRUE) {
      n_pat <- ordinal_data %>%
        filter(timepoint == min(as.Date(timepoint)), toxicity == toxic()) %>%
        group_by(dose) %>%
        summarise(n.pat = n())
    } else {
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

    if (isTruthy(input$updateB)) {
      if (editB()$showExtra == FALSE) {
        ordinal_data <- ordinal_data %>% filter(grade != "Unreported")
        if (input$ordinal_date == TRUE) {
          n_pat <- ordinal_data %>%
            filter(toxicity == toxic()) %>%
            group_by(dose, timepoint) %>%
            summarise(n.pat = n())
        } else {
          n_pat <- ordinal_data %>%
            filter(toxicity == toxic()) %>%
            group_by(dose, timepoint) %>%
            summarise(n.pat = n())
        }

        prop <- ordinal_data %>%
          group_by(toxicity, timepoint, dose) %>%
          count(grade) %>%
          left_join(n_pat, by = c("dose", "timepoint")) %>%
          mutate(proportion = n / n.pat) %>%
          dplyr::select(-n)
      }
    }

    colnames(prop) <- c("toxicity", "timepoint", "dose", "grade", "n.pat", "proportion")

    prop <- prop %>% dplyr::select(-n.pat)

    missing_layer <- prop %>%
      group_by(toxicity, timepoint, dose) %>%
      summarise(proportion = 1 - sum(proportion), .groups = "drop") %>%
      filter(proportion > 0)
    if (nrow(missing_layer) != 0) {
      missing_layer <- cbind(missing_layer, "Discontinued")
      colnames(missing_layer) <- c("toxicity", "timepoint", "dose", "proportion", "grade")

      missing_layer <- missing_layer %>%
        dplyr::select(names(prop))

      prop$grade <- as.character(prop$grade)
      prop <- bind_rows(prop, missing_layer)
    }

    prop$toxicity <- as.factor(prop$toxicity)
    prop$grade <- as.factor(prop$grade)
    prop$dose <- as.factor(prop$dose)
    prop$dose <- factor(prop$dose, levels = no_doseb())
    if (isTruthy(input$updateB)) {
      if (editB()$invert == TRUE) {
        prop$grade <- factor(prop$grade, levels = rev(c("Discontinued", "Unreported", 5:1)))
      } else {
        prop$grade <- factor(prop$grade, levels = c("Discontinued", "Unreported", 5:1))
      }
    } else {
      prop$grade <- factor(prop$grade, levels = c("Discontinued", "Unreported", 5:1))
    }

    if (input$ordinal_date == TRUE) {
      prop <- prop %>% filter(as.Date(timepoint) <= as.Date(final_t()))
      prop$timepoint <- as.factor(as.Date(prop$timepoint))
    } else {
      prop <- prop %>% filter(timepoint <= final_t())
      prop$timepoint <- as.factor(prop$timepoint)
    }

    prop
  })


  # Graph labels

  no_patient <- reactive({
    req(input$numdoseB)
    ordinal_data <- ordinal_data()
    if (input$ordinal_date == TRUE) {
      no_pat <- ordinal_data %>%
        filter(timepoint == min(as.Date(timepoint)), toxicity == toxic()) %>%
        group_by(dose) %>%
        filter(dose %in% no_doseb()) %>%
        summarise(n.pat = n(), .groups = "keep")
    } else {
      no_pat <- ordinal_data %>%
        filter(timepoint == min(timepoint), toxicity == toxic()) %>%
        group_by(dose) %>%
        filter(dose %in% no_doseb()) %>%
        summarise(n.pat = n(), .groups = "keep")
    }
    no_pat$n.pat
  })
  formatter <- reactive({
    if (isTruthy(input$updateB)) {
      if (isFALSE(editB()$showExtra)) {
        ordinal_data <- ordinal_data() %>%
          filter(dose %in% no_doseb()) %>%
          filter(toxicity %in% toxic()) %>%
          filter(grade != "Unreported")
        if (input$ordinal_date == TRUE) {
          n_pat <- ordinal_data %>%
            filter(toxicity == toxic()) %>%
            filter(as.Date(timepoint) <= final_t()) %>%
            group_by(dose, timepoint) %>%
            summarise(n.pat = n())
        } else {
          n_pat <- ordinal_data %>%
            filter(toxicity == toxic()) %>%
            filter(timepoint <= final_t()) %>%
            group_by(dose, timepoint) %>%
            summarise(n.pat = n())
        }
        groupsizes <- c()
        for (i in no_doseb()) {
          pat_numbs <- n_pat %>%
            filter(dose == !!i) %>%
            distinct(timepoint, n.pat) %>%
            group_by(dose, timepoint)
          groupsizes <- append(groupsizes, paste(unlist(pat_numbs$n.pat), collapse = "; "))
        }
        function(no_dose = no_doseb(), no_pat = groupsizes) {
          paste0("Dose ", no_dose, " (N=", no_pat, ")")
        }
      } else {
        function(no.dose = no_doseb(), no.pat = no_patient()) {
          paste0("Dose ", no.dose, " (N=", no.pat, ")")
        }
      }
    } else {
      function(no.dose = no_doseb(), no.pat = no_patient()) {
        paste0("Dose ", no.dose, " (N=", no.pat, ")")
      }
    }
  })


  # Graph output
  output$graphB <- renderPlot(
    {
      prop <- prop()
      no_dose <- no_doseb()
      no_patient <- no_patient()
      g <- prop %>%
        filter(toxicity == toxic()) %>%
        ggplot(aes(x = as.factor(timepoint))) +
        geom_bar(
          aes(
            x = as.factor(timepoint),
            y = proportion,
            fill = as.factor(grade)
          ),
          stat = "identity",
          position = "stack"
        ) +
        scale_fill_manual(
          breaks = rev(c(1:5, "Unreported", "Discontinued")),
          values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "grey", "grey30")),
          labels = rev(
            c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported", "Discontinued")
          ),
          guide = guide_legend(reverse = TRUE)
        ) +
        theme_bw(base_size = 14) +
        facet_wrap2(~ as.factor(dose),
          nrow = 3,
          strip.position = "left",
          axes = "all",
          labeller = as_labeller(formatter())
        )
      if (isTruthy(input$updateB)) {
        g +
          labs(
            x = editB()$x_lab,
            y = "Proportion",
            fill = "Severity Score"
          ) + ggtitle(editB()$title) + theme(plot.title = element_text(size = editB()$titlesize, face = "bold"), axis.title = element_text(size = editB()$axissize))
      } else {
        g +
          labs(
            x = "Timepoint",
            y = "Proportion",
            fill = "Severity Score"
          )
      }
    },
    height = function() input$heightB,
    width = function() input$widthB
  )

  ################################################################################## GRAPH C
  #### Import the data
  continuous_data <- reactive({
    if (input$dummy_continuous == FALSE) {
      req(input$upload_cts)
      if (tools::file_ext(input$upload_cts$name) != "csv") {
        validate("Invalid upload: not .csv file", errorClass = "style")
      }
      data <- read.csv(input$upload_cts$datapath, header = TRUE, sep = ",")
      if (all(c("score", "id", "functioning", "timepoint", "dose") %in% colnames(data)) == FALSE) {
        validate(
          need("id" %in% colnames(data), "Missing id column"),
          need("score" %in% colnames(data), "Missing score column"),
          need("functioning" %in% colnames(data), "Missing functioning column"),
          need("timepoint" %in% colnames(data), "Missing timepoint column"),
          need("dose" %in% colnames(data), "Missing dose column"),
          errorClass = "style"
        )
      }
      data <- data %>% select(score, id, functioning, timepoint, dose)
      data
    } else if (input$dummy_continuous == TRUE & input$continuous_date == FALSE) {
      read.csv("www/DummyData/dummy_continuous.csv", header = TRUE, sep = ",")[, -1]
    } else if (input$dummy_continuous == TRUE & input$continuous_date == TRUE) {
      read.csv("www/DummyData/dummy_continuous_dates.csv", header = TRUE, sep = ",")[, -1]
    }
  })
  observeEvent(input$upload_cts, {
    if (class(continuous_data()$timepoint[1]) == "character") {
      showNotification("Timepoint of format \"YYYY-MM-DD\" detected in input, auto-switching ON date mode. Switch back OFF if not a date.", type = "warning")
    } else if (class(continuous_data()$timepoint[1]) == "integer") {
      showNotification("Timepoint of format integer detected in input, auto-switching OFF date mode. Switch back ON if a date.", type = "warning")
    }
  })
  observe({
    req(input$upload_cts)
    if (class(continuous_data()$timepoint[1]) == "character") {
      update_switch(session = session, id = "continuous_date", value = TRUE)
    } else if (class(continuous_data()$timepoint[1]) == "integer") {
      update_switch(session = session, id = "continuous_date", value = FALSE)
    }
  })

  output$cts_summary_pat <- renderText(length(unique(continuous_data()$id)))
  output$cts_summary_dose <- renderText(length(unique(continuous_data()$dose)))
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
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointC", label = "Final timepoint of interest", min = sort(tpoint)[2], max = max(tpoint))
  })

  observe({
    req(input$upload_cts)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningC", label = "PRO domain of interest", choices = func, selected = NULL)
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
    if (input$continuous_date == TRUE) {
      tpointC <- as.Date(tpointC)
    }
    updateSliderInput(session, "tpointC", label = "Final timepoint of interest", min = tpointC[2], max = max(tpointC))
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningC", label = "PRO domain of interest", choices = func, selected = NULL)
  })

  observe({
    req(input$improveC)
    bounds <- input$improveC
    updateSliderInput(session, "impTextC", min = bounds[1] + 2, max = bounds[2] - 2, value = bounds[2] - 2)
  })
  observe({
    req(input$worseC)
    bounds <- input$worseC
    updateSliderInput(session, "worTextC", min = bounds[1] + 2, max = bounds[2] - 2, value = bounds[2] - 2)
  })
  #### Reactive Defining
  # Define the relevant variables
  no_doseC <- reactive({
    req(input$numdoseC)
    input$numdoseC
  })

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
    continuous_data <- continuous_data %>%
      filter(functioning == func_C()) %>%
      dplyr::select(-functioning) # %>% filter(!is.na(score))
    if (input$continuous_date == TRUE) {
      continuous_data$timepoint == as.Date(continuous_data$timepoint)
    }
    continuous_data
  })

  diffC <- reactive({
    req(input$functioningC)
    continuous_data_f <- continuous_data_filtered()
    continuous_data_f <- continuous_data_f %>%
      group_by(id) %>%
      mutate(baseline = score[timepoint == min(timepoint)], adjusted_value = score - baseline) %>%
      ungroup()
    continuous_data_f
  })

  summaryC <- reactive({
    req(input$numdoseC)
    diff <- diffC()
    diff <- diff %>%
      group_by(dose, timepoint) %>%
      summarise(mean = mean(adjusted_value, na.rm = TRUE), sd = sd(adjusted_value, na.rm = TRUE) / sqrt(n()), .groups = "keep")
    summary <- diff %>%
      mutate(l.ci = mean - (1.96 * sd)) %>%
      mutate(u.ci = mean + (1.96 * sd))
    summary <- summary %>%
      filter(timepoint <= final_tC()) %>%
      filter(dose %in% no_doseC())
    summary
  })

  pd <- position_dodge(width = 0.2)

  #### FIGURE GENERATION
  output$graphC <- renderPlot(
    {
      summaryC <- summaryC()
      if (isTruthy(input$updateC)) {
        editC <- editC()
        impbound <- editC$imp_bound
        worbound <- editC$wor_bound
        upper <- max(editC$imp_bound[2], editC$wor_bound[2], max(diffC()$adjusted_value))
        lower <- min(editC$imp_bound[1], editC$wor_bound[1], min(diffC()$adjusted_value))
        worse_text <- if (worbound[1] < impbound[1]) {
          min(max(editC$wor_text, worbound[1]), worbound[2])
        } else {
          max(min(editC$wor_text, worbound[2]), worbound[1])
        }
        improve_text <- if (worbound[1] < impbound[1]) {
          max(min(editC$imp_text, impbound[2]), impbound[1])
        } else {
          min(max(editC$imp_text, impbound[1]), impbound[2])
        }
        summaryC %>%
          ggplot(
            aes(
              x = as.factor(timepoint),
              y = mean, group = as.factor(dose), shape = as.factor(dose), colour = as.factor(dose)
            )
          ) +
          geom_ribbon(aes(ymin = impbound[1], ymax = impbound[2], fill = "palegreen1"), alpha = 0.3, colour = NA) +
          geom_ribbon(aes(ymin = worbound[1], ymax = worbound[2], fill = "red"), alpha = 0.1, colour = NA) +
          geom_point(position = pd, size = 3) +
          geom_line(size = 1.5) +
          geom_errorbar(position = pd, aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
          theme_minimal(base_size = 14) +
          guides(
            shape = guide_legend(title = "Dose"),
            colour = guide_legend(title = "Dose")
          ) +
          theme_minimal(base_size = 14) +
          xlab(editC$x_lab) +
          ylab(paste0(editC$y_lab)) +
          scale_fill_manual(name = "Threshold", values = c("palegreen1", "red"), label = c("Improvement", "Worsening")) +
          geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = max(impbound[2], worbound[2])),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = min(impbound[1], worbound[1])),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_text(
            data = data.frame(x = 1.6, y = improve_text, label = "Improvement"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = 1.6, y = worse_text, label = "Worsening"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          ggtitle(editC$title) +
          theme(plot.title = element_text(size = editC$titlesize, face = "bold"), axis.title = element_text(size = editC$axissize)) +
          ylim(lower, upper)
      } else {
        summaryC %>%
          ggplot(
            aes(
              x = as.factor(timepoint),
              y = mean, group = as.factor(dose), shape = as.factor(dose), colour = as.factor(dose)
            )
          ) +
          geom_ribbon(aes(ymin = 10, ymax = 30, fill = "palegreen1"), alpha = 0.3, colour = NA) +
          geom_ribbon(aes(ymin = -30, ymax = -10, fill = "red"), alpha = 0.1, colour = NA) +
          geom_point(position = pd, size = 3) +
          geom_line(size = 1.5) +
          geom_errorbar(position = pd, aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
          theme_minimal(base_size = 14) +
          guides(
            shape = guide_legend(title = "Dose"),
            colour = guide_legend(title = "Dose")
          ) +
          theme_minimal(base_size = 14) +
          xlab("Timepoint") +
          ylab(paste0(
            "Mean change from baseline across timepoints (with 95% CI) \n
                in PRO domain ",
            func_C()
          )) +
          scale_fill_manual(name = "Threshold", values = c("palegreen1", "red"), label = c("Improvement", "Worsening")) +
          geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = 30),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_segment(aes(x = 0.8, xend = 0.8, y = 0, yend = -30),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_text(
            data = data.frame(x = 1.6, y = 17, label = "Improvement"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = 1.6, y = -17, label = "Worsening"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          )
      }
    },
    height = function() input$heightC,
    width = function() input$widthC
  )
  ############################################################################################# GRAPH D

  observe({
    req(input$upload_cts)
    data <- continuous_data()
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointD", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
  })

  observe({
    req(input$upload_cts)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningD", label = "PRO domain of interest", choices = func, selected = NULL)
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
    if (input$continuous_date == TRUE) {
      tpointD <- as.Date(tpointD)
    }
    updateSliderInput(session, "tpointD", label = "Final timepoint of interest", min = min(tpointD), max = max(tpointD))
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningD", label = "PRO domain of interest", choices = func, selected = NULL)
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    numdoseD <- unique(data$dose)
    updateCheckboxGroupInput(session, "numdoseD", choices = numdoseD, selected = NULL)
  })

  #### Reactive Defining
  # Define the relevant variables
  no_doseD <- reactive({
    req(input$numdoseD)
    input$numdoseD
  })

  final_tD <- reactive({
    req(input$tpointD)
    input$tpointD
  })

  func_D <- reactive({
    req(input$functioningD)
    input$functioningD
  })

  #### ID specific UI
  output$allD <- renderUI({
    if (isTruthy(input$numdoseD) == TRUE) {
      input_switch("allD", label = "Select all IDs")
    }
  })

  output$select_idD <- renderUI({
    data <- continuous_data()
    data <- data %>% filter(dose %in% no_doseD())
    id <- unique(data$id)
    if (isFALSE(input$allD)) {
      selectizeInput("select_idD", "Select IDs", choices = id, multiple = TRUE, selected = NULL)
    } else if (isTRUE(input$allD)) {
      selectizeInput("select_idD", "Select IDs", choices = id, multiple = TRUE, selected = id)
    }
  })

  idD <- reactive({
    req(input$select_idD)
    input$select_idD
  })

  #### Filter data
  continuous_data_filteredD <- reactive({
    req(input$tpointD)
    data <- continuous_data()
    if (input$continuous_date == TRUE) {
      data$timepoint == as.Date(data$timepoint)
    }
    data <- data %>%
      filter(dose %in% no_doseD()) %>%
      filter(id %in% idD()) %>%
      filter(timepoint <= final_tD()) %>%
      filter(functioning == func_D()) %>%
      filter(!is.na(score)) %>%
      dplyr::select(-functioning)
    data <- data.frame(data) %>%
      arrange(dose) %>%
      group_by(dose) %>%
      arrange(id, .by_group = TRUE) %>%
      ungroup()
    data
  })

  labels_D <- reactive({
    data <- continuous_data_filteredD()
    labels <- factor(paste0("ID ", data$id, " (Dose: ", data$dose, ")"), levels = unique(paste0("ID ", data$id, " (Dose: ", data$dose, ")")))
    labels
  })

  #### Plot output

  output$graphD <- renderPlot(
    {
      data <- continuous_data_filteredD()
      data$dose <- as.factor(data$dose)
      if (input$continuous_date == TRUE) {
        g <- ggplot(data = data, aes(
          x = as.Date(timepoint),
          y = score, group = id, colour = labels_D(), linetype = dose
        )) +
          scale_x_date()
        min_t <- min(as.Date(data$timepoint))
      } else {
        g <- ggplot(data = data, aes(
          x = as.factor(timepoint),
          y = score, group = id, colour = labels_D(), linetype = dose
        )) +
          scale_x_discrete(expand = expansion(add = c(2, 0)))
        min_t <- min(as.numeric(data$timepoint))
      }
      g <- g +
        geom_line(size = 0.9) +
        labs(linetype = "Dose") +
        theme_minimal(base_size = 16)
      if (isTruthy(input$updateD)) {
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
            color = "Patient (Dose)"
          ) +
          ggtitle(editD$title) + theme(plot.title = element_text(size = editD$titlesize, face = "bold"), axis.title = element_text(size = editD$axissize)) +
          geom_segment(
            x = min_t, xend = min_t, y = lower, yend = upper,
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            ), inherit.aes = FALSE
          ) +
          geom_text(x = min_t - 0.2, y = upper * 0.9, label = editD$arrow_lab, color = "black", inherit.aes = FALSE)
      } else {
        g + ylim(c(0, 100)) +
          geom_point() +
          labs(
            y = paste0("Score in PRO domain ", func_D()),
            x = "Timepoint",
            color = "Patient (Dose)"
          ) +
          geom_segment(
            x = min_t, xend = min_t, y = 0, yend = 100,
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            ), inherit.aes = FALSE
          ) +
          geom_text(x = min_t - 0.5, y = 90, label = "Better \n tolerability", color = "black", inherit.aes = FALSE)
      }
    },
    height = function() input$heightD,
    width = function() input$widthD
  )


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
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointE", label = "Final timepoint of interest", min = min(tpoint), max = max(tpoint))
  })

  observe({
    req(input$upload_cts)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningE", label = "PRO domain of interest", choices = func, selected = NULL)
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
    if (input$continuous_date == TRUE) {
      tpointE <- as.Date(tpointE)
    }
    updateSliderInput(session, "tpointE", label = "Final timepoint of interest", min = min(tpointE), max = max(tpointE))
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateSelectInput(session, "functioningE", label = "PRO domain of interest", choices = func, selected = NULL)
  })


  #### Reactive Defining
  # Define the relevant variables
  no_doseE <- reactive({
    req(input$numdoseE)
    input$numdoseE
  })

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
    req(input$functioningE)
    req(input$tpointE)
    continuous_data <- continuous_data()
    if (input$continuous_date == TRUE) {
      continuous_data$timepoint <- as.Date(continuous_data$timepoint)
    }
    continuous_data <- continuous_data %>%
      filter(functioning == func_E()) %>%
      dplyr::select(-functioning)
    continuous_data <- continuous_data %>%
      filter(dose %in% no_doseE()) %>%
      filter(timepoint <= final_tE()) %>%
      filter(!is.na(score))
    continuous_data
  })

  output$graphE <- renderPlot(
    {
      continuous_data <- data_E()
      g <- ggplot(continuous_data, aes(x = as.factor(timepoint), y = score, fill = as.factor(dose))) +
        labs(fill = "Dose") +
        geom_boxplot() +
        theme_minimal(base_size = 14)
      if (isTruthy(input$updateE)) {
        editE <- editE()
        bounds <- editE$range
        maxval <- max(continuous_data$score, na.rm = TRUE)
        minval <- min(continuous_data$score, na.rm = TRUE)
        lower <- min(c(minval, bounds[1]))
        upper <- max(c(maxval, bounds[2]))
        g + xlab(editE$x_lab) + ylab(paste0(editE$y_lab)) +
          geom_segment(aes(x = 0.2, xend = 0.2, y = lower, yend = upper),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) + ylim(c(lower, upper)) +
          scale_x_discrete(expand = expansion(add = c(2, 0))) +
          geom_text(aes(x = -0.45, y = upper * 0.9, label = editE$arrow_lab)) +
          ggtitle(editE$title) +
          theme(plot.title = element_text(size = editE$titlesize, face = "bold"), axis.title = element_text(size = editE$axissize))
      } else {
        g + xlab("Timepoint") + ylab(paste0("Score in PRO domain ", func_E())) +
          geom_segment(aes(x = 0.2, xend = 0.2, y = 0, yend = 100),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) + ylim(c(0, 100)) +
          scale_x_discrete(expand = expansion(add = c(2, 0))) +
          geom_text(aes(x = -0.45, y = 90, label = "Better \n tolerability"))
      }
    },
    height = function() input$heightE,
    width = function() input$widthE
  )
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
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointF", label = "Final timepoint of interest", min = sort(tpoint)[2], max = max(tpoint))
  })


  observe({
    req(input$upload_cts)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateCheckboxGroupInput(session, "functioningF", label = "PRO domain of interest", choices = func, selected = NULL)
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
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointF", label = "Final timepoint of interest", min = tpoint[2], max = max(tpoint))
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateCheckboxGroupInput(session, "functioningF", label = "PRO domain of interest", choices = func, selected = NULL)
  })

  #### Reactive Defining
  # Define the relevant variables
  no_doseF <- reactive({
    req(input$numdoseF)
    input$numdoseF
  })

  observe({
    req(input$numdoseF)
    doses <- no_doseF()
    minwidth <- 200 * length(unique(doses))
    maxwidth <- 400 * length(unique(doses))
    updateSliderInput(session, "widthF", min = minwidth, max = maxwidth, value = minwidth + 200, step = 100)
  })

  final_tF <- reactive({
    req(input$tpointF)
    input$tpointF
  })

  func_F <- reactive({
    req(input$functioningF)
    as.character(input$functioningF)
  })

  ### Filter and sort data

  continuous_data_filteredF <- reactive({
    req(input$numdoseF)
    req(input$functioningF)
    req(input$tpointF)
    filtered <- continuous_data()
    filtered$functioning <- as.character(filtered$functioning)
    if (input$continuous_date == TRUE) {
      filtered$timepoint <- as.Date(filtered$timepoint)
    }
    filtered <- filtered %>%
      filter(dose %in% no_doseF()) %>%
      filter(functioning %in% func_F()) %>%
      filter(timepoint <= final_tF())
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
    data <- continuous_data_filteredF()
    final_list <- unique(data %>% filter(timepoint <= final_tF()) %>% select(timepoint))
    final_in_list <- max(final_list$timepoint)
    first_in_list <- min(unique(data$timepoint))
    finalT <- as.symbol(paste0("tp_", final_in_list))
    firstT <- as.symbol(paste0("tp_", first_in_list))
    df <- data %>%
      group_by(functioning) %>%
      pivot_wider(names_from = timepoint, values_from = score, names_prefix = "tp_") %>%
      mutate(score = !!finalT - !!firstT) %>%
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
      summarise(
        mean = mean(score, na.rm = TRUE),
        non_na_count = unique(non_na_count),
        se = sd(score, na.rm = TRUE) / sqrt(non_na_count), .groups = "keep"
      )
    sum.df <- sum.df %>% mutate(l.ci = mean - (1.96 * se))
    sum.df <- sum.df %>% mutate(u.ci = mean + (1.96 * se))
    sum.df <- sum.df %>% ungroup(dose)
    sum.df
  })


  sum.df.widerF <- reactive({
    sum.dfF() %>% pivot_wider(id_cols = functioning, names_from = dose, values_from = c(mean, se, non_na_count))
  })

  sum.df.wider.gtF <- reactive({
    sum.df.wider.gt <- sum.df.widerF() %>%
      gt(row_group_as_column = TRUE) %>%
      tab_stubhead(label = if(isTruthy(input$updateF)){input$column1F}else{"Functioning"})
    for (i in unique(sum.dfF()$dose)) {
      sum.df.wider.gt <- sum.df.wider.gt %>%
        tab_spanner(label = paste0("Dose ", i), columns = ends_with(as.character(!!i)))
      sum.df.wider.gt <- sum.df.wider.gt %>% cols_label(
        starts_with("mean") ~ "Mean",
        starts_with("se") ~ "Standard Error",
        starts_with("non_na_count") ~ "Number Reported"
      )
    }
    sum.df.wider.gt <- sum.df.wider.gt %>% opt_stylize(style = 5, color = "blue")
    if (isTruthy(input$updateF)) {
      sum.df.wider.gt <- sum.df.wider.gt %>% tab_header(title = paste0(titleF()))
    }
    sum.df.wider.gt <- sum.df.wider.gt %>% tab_options(table.width = px(input$widthF))
  })


  output$graphF <- render_gt({
    sum.df.wider.gtF()
  })


  ################################################################# Graph G
  #### Update inputs
  observe({
    req(input$upload_cts)
    data <- continuous_data()
    numdoseG <- unique(data$dose)
    updateCheckboxGroupInput(session, "numdoseG", label = "Dosage", choices = numdoseG, selected = NULL)
  })


  observe({
    req(input$upload_cts)
    data <- continuous_data()
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointG", label = "Final timepoint of interest", min = sort(tpoint)[2], max = max(tpoint))
  })

  observe({
    req(input$upload_cts)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateCheckboxGroupInput(session, "functioningG", label = "PRO domain of interest", choices = func, selected = NULL)
  })

  observe({
    req(input$improveG)
    bounds <- input$improveG
    updateSliderInput(session, "impTextG", min = bounds[1] + 2, max = bounds[2] - 2, value = bounds[2] - 2)
  })
  observe({
    req(input$worseG)
    bounds <- input$worseG
    updateSliderInput(session, "worTextG", min = bounds[1] + 2, max = bounds[2] - 2, value = bounds[2] - 2)
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
    tpoint <- unique(data$timepoint)
    if (input$continuous_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointG", label = "Final timepoint of interest", min = tpoint[2], max = max(tpoint))
  })

  observe({
    req(input$dummy_continuous)
    data <- continuous_data()
    func <- unique(data$functioning)
    updateCheckboxGroupInput(session, "functioningG", label = "PRO domain of interest", choices = func, selected = NULL)
  })

  #### Reactive Defining
  # Define the relevant variables
  no_doseG <- reactive({
    req(input$numdoseG)
    input$numdoseG
  })

  final_tG <- reactive({
    req(input$tpointG)
    input$tpointG
  })

  func_G <- reactive({
    req(input$functioningG)
    input$functioningG
  })

  #### Filtering
  no_patientG <- reactive({
    req(input$tpointG)
    continuous_data <- continuous_data()
    continuous_data %>%
      filter(dose %in% no_doseG()) %>%
      filter(timepoint == min(timepoint), functioning == func_G()[1]) %>%
      count(dose)
  })

  continuous_data_filteredG <- reactive({
    continuous_data <- continuous_data()
    if (input$continuous_date == TRUE) {
      continuous_data$timepoint <- as.Date(continuous_data$timepoint)
    }
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
    endT_list <- unique(continuous_data() %>% filter(timepoint <= final_tG()) %>% select(timepoint))
    endT <- max(endT_list$timepoint)
    startT <- min(unique(continuous_data_filteredG()$timepoint))
    tp_end <- sym(paste0("tp_", endT))
    tp_start <- sym(paste0("tp_", startT))
    df <- continuous_data_filteredG() %>%
      filter(timepoint <= endT) %>%
      pivot_wider(names_from = timepoint, values_from = score, names_prefix = "tp_") %>%
      mutate(score = !!tp_end - !!tp_start) %>%
      dplyr::select(id, score, dose, functioning)

    df <- df %>%
      group_by(dose, functioning) %>%
      mutate(non_na_count = sum(!is.na(score))) %>%
      mutate(na_count = sum(is.na(score))) %>%
      ungroup()
    df
  })

  unrep <- reactive({
    unrep <- dfG() %>%
      group_by(dose, functioning) %>%
      distinct(na_count) %>%
      ungroup()
  })

  sum.dfG <- reactive({
    sum.df <- dfG() %>%
      group_by(dose, functioning) %>%
      summarise(
        mean = mean(score, na.rm = TRUE), non_na_count = unique(non_na_count),
        se = sd(score, na.rm = TRUE) / sqrt(non_na_count), .groups = "keep"
      )
    sum.df <- sum.df %>% mutate(l.ci = mean - (1.96 * se))
    sum.df <- sum.df %>% mutate(u.ci = mean + (1.96 * se))
    sum.df <- sum.df %>%
      left_join(unrep(), by = c("dose", "functioning")) %>%
      mutate(custom_x = paste0("Dose=", dose, "\nUR=", na_count))
    sum.df$functioning <- as.factor(sum.df$functioning)
    sum.df
  })


  arrowdf <- reactive({
    first_fn <- as.symbol(as.character((sum.dfG()$functioning[1])))
    arrow <- tibble(functioning = as.factor(as.character(first_fn)))
    arrow
  })

  output$graphG <- renderPlot(
    {
      end_t <- final_tG()
      arrowdf <- arrowdf()
      no_patient <- unlist(no_patientG()[2])
      no_patient <- paste(no_patient, collapse = ";")
      g <- data.frame(sum.dfG()) %>% ggplot(aes(x = custom_x, y = mean, colour = as.factor(dose))) +
        geom_point(size = 3) +
        scale_color_discrete(name = "Dose") +
        geom_errorbar(aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
        theme_minimal(base_size = 14)
      if (isTruthy(input$updateG)) {
        editG <- editG()
        impbound <- editG$imp_bound
        worbound <- editG$wor_bound
        minval <- min(dfG()$score, na.rm = TRUE)
        maxval <- max(dfG()$score, na.rm = TRUE)
        upper <- max(worbound[2], impbound[2], maxval)
        lower <- min(worbound[1], impbound[1], minval)
        worse_text <- if (worbound[1] < impbound[1]) {
          min(max(editG$wor_text, worbound[1]), worbound[2])
        } else {
          max(min(editG$wor_text, worbound[2]), worbound[1])
        }
        improve_text <- if (worbound[1] < impbound[1]) {
          max(min(editG$imp_text, impbound[2]), impbound[1])
        } else {
          min(max(editG$imp_text, impbound[1]), impbound[2])
        }
        thresholds <- data.frame(
          ymin = c(impbound[1], worbound[1]),
          ymax = c(impbound[2], worbound[2]),
          Threshold = factor(c("Improvement", "Worsening"))
        )
        formatterG <- function(functioning = sum.dfG()$functioning) {
          paste(editG$subgraph_lab, functioning, sep = " ")
        }
        g + ylim(c(lower, upper)) +
          scale_fill_manual(
            name = "Threshold",
            values = c("Improvement" = "palegreen1", "Worsening" = "red")
          ) +
          geom_rect(
            data = thresholds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Threshold),
            alpha = 0.3, inherit.aes = FALSE
          ) +
          labs(
            y = paste0(editG$y_lab),
            x = paste0("Dose (N= ", no_patient, ")")
          ) +
          facet_wrap(~ as.factor(functioning), nrow = 1, labeller = as_labeller(formatterG), scales = "free_x") +
          scale_x_discrete(expand = expansion(add = c(1, 1))) +
          geom_segment(
            data = arrowdf,
            aes(x = 0.5, xend = 0.5, y = 0, yend = upper),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_segment(
            data = arrowdf,
            aes(x = 0.5, xend = 0.5, y = 0, yend = lower),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_text(
            data = arrowdf, aes(x = 2, y = improve_text, label = "Improvement"),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = arrowdf, aes(x = 2, y = worse_text, label = "Worsening"),
            inherit.aes = FALSE
          ) +
          ggtitle(editG$title) + theme(plot.title = element_text(size = editG$titlesize, face = "bold"), axis.title = element_text(size = editG$axissize))
      } else {
        thresholds <- data.frame(
          ymin = c(10, -25),
          ymax = c(25, -10),
          Threshold = factor(c("Improvement", "Worsening"))
        )
        formatterG <- function(functioning = sum.dfG()$functioning) {
          paste0("PRO domain ", functioning)
        }
        g + ylim(c(-25, 25)) +
          scale_fill_manual(
            name = "Threshold",
            values = c("Improvement" = "palegreen1", "Worsening" = "red")
          ) +
          geom_rect(
            data = thresholds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Threshold),
            alpha = 0.3, inherit.aes = FALSE
          ) +
          labs(
            y = paste0("Mean change from baseline to timepoint ", final_tG(), "(with 95% CI) \n for PRO domain"),
            x = paste0("Dose (N= ", no_patient, ")")
          ) +
          facet_wrap(~ as.factor(functioning), nrow = 1, labeller = as_labeller(formatterG), scales = "free_x") +
          scale_x_discrete(expand = expansion(add = c(1, 1))) +
          geom_segment(
            data = arrowdf,
            aes(x = 0.5, xend = 0.5, y = 0, yend = 25),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_segment(
            data = arrowdf,
            aes(x = 0.5, xend = 0.5, y = 0, yend = -25),
            colour = "darkgrey", size = 0.8, arrow = arrow(
              type = "closed",
              length = unit(0.1, "inches")
            )
          ) +
          geom_text(
            data = arrowdf, aes(x = 2, y = 22, label = "Improvement"),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = arrowdf, aes(x = 2, y = -22, label = "Worsening"),
            inherit.aes = FALSE
          )
      }
    },
    height = function() input$heightG,
    width = function() input$widthG
  )

  ################################################################################ Graph H
  # Update the selection boxes based on dataset
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
    if (input$ordinal_date == TRUE) {
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
    if (input$ordinal_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointH", label = "Start and endpoint for comparison", min = min(tpoint), max = max(tpoint), value = c(min(tpoint), max(tpoint)))
  })

  observe({
    req(input$dummy_ordinal)
    data <- ordinal_data()
    toxic <- unique(data$toxicity)
    updateCheckboxGroupInput(session, "toxicH", label = "Toxicity of interest", choices = toxic, selected = NULL)
  })

  # Define the relevant variables
  no_doseH <- reactive({
    req(input$numdoseH)
    input$numdoseH
  })

  times_H <- reactive({
    req(input$tpointH)
    input$tpointH
  })

  toxicH <- reactive({
    req(input$toxicH)
    input$toxicH
  })


  #### Filter and sort data

  ordinal_dataH <- reactive({
    tps <- times_H()
    ordinal_dataH <- ordinal_data()
    list_T <- unique(ordinal_dataH %>% filter(timepoint <= tps[2] & timepoint >= tps[1]) %>% select(timepoint))
    tps[1] <- min(list_T$timepoint)
    tp_first <- as.symbol(paste0("tp_", tps[1]))
    tps[2] <- max(list_T$timepoint)
    tp_last <- as.symbol(paste0("tp_", tps[2]))
    ordinal_dataH <- ordinal_dataH %>%
      filter(dose %in% no_doseH()) %>%
      filter(toxicity %in% toxicH()) %>%
      group_by(id) %>%
      mutate(has_tp = any(timepoint == tps[2])) %>%
      filter(timepoint %in% c(tps[1], tps[2])) %>%
      pivot_wider(names_from = timepoint, values_from = grade, names_prefix = "tp_") %>%
      mutate(grade_change = case_when(
        has_tp == FALSE ~ "ICE",
        TRUE ~ as.character(!!tp_first - !!tp_last)
      ))
    ordinal_dataH$grade_change[is.na(ordinal_dataH$grade_change) == TRUE] <- "Missing"
    ordinal_dataH
  })

  dose_labelsH <- reactive({
    levs <- rev(no_doseH())
    paste0("Dose ", levs)
  })

  propH <- reactive({
    propH <- ordinal_dataH()
    propH <- propH %>%
      group_by(toxicity, dose) %>%
      count(grade_change) %>%
      ungroup() %>%
      mutate(original = TRUE) %>%
      complete(toxicity, dose, grade_change, fill = list(n = 0)) %>%
      group_by(toxicity, dose) %>%
      mutate(n.pat = sum(n[grade_change != "ICE"], na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(proportion = n / n.pat) %>%
      dplyr::select(-n)
    propH$toxicity <- as.factor(propH$toxicity)
    propH$dose <- as.factor(propH$dose)
    propH$dose <- factor(propH$dose, levels = rev(levels(propH$dose)))
    # prop$base_adj <- factor(prop$base_adj, levels = c("Unreported",5,4,3,2,1))
    propH$grade_change <- as.character(propH$grade_change)
    # prop <- prop %>%
    #  replace_na(list(grade_change = "Unreported"))
    propH
  })
  missing_H <- reactive({
    missing <- propH() %>%
      subset(grade_change == "Missing") %>%
      mutate(label = paste0("Unreported: ", round(proportion, 2) * 100, "%"))
    if (isTruthy(input$updateH)) {
      missing <- cbind(missing, rep(editH()$location, times = nrow(missing)))
      colnames(missing)[8] <- "loc"
      levels(missing$dose) <- dose_labelsH()
      missing
    } else {
      missing <- cbind(missing, rep(0.7, times = nrow(missing)))
      colnames(missing)[8] <- "loc"
      levels(missing$dose) <- dose_labelsH()
      missing
    }
  })

  prop_H_adjusted <- reactive({
    propHadj <- propH()
    propHadj <- propHadj %>% mutate(proportion = ifelse(grade_change >= 0, proportion, -proportion))
    propHadj <- propHadj %>%
      subset(grade_change != "Missing") %>%
      subset(grade_change != 0)
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
          TRUE ~ 0 # fallback for proportion == 0
        )
      )
    valH <- valH %>% subset(grade_change != "ICE")
    valH$grade_change <- factor(valH$grade_change)
    levels(valH$dose) <- dose_labelsH()
    valH
  })

  # arrowdf <- tibble(toxicity = "10")

  arrowdfH <- reactive({
    arrowH <- tibble(
      toxicity = factor(as.character(unique(toxicH())), levels = levels(valH()$toxicity)), # important!
      x = 0.5,
      xend = 0.5,
      y = 0,
      yend = 1
    )
    arrowH
  })

  arrowdf.revH <- reactive({
    arrowH <- tibble(
      toxicity = factor(as.character(unique(toxicH())), levels = levels(valH()$toxicity)), # important!
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

  labels <- cbind(
    c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5),
    c(
      "red", "deeppink", "orange", "gold", "lightyellow3", "lightblue",
      "darkseagreen3", "darkolivegreen3", "chartreuse3", "chartreuse4"
    ),
    rev(c(
      "Improve by 5", "Improve by 4", "Improve by 3", "Improve by 2", "Improve by 1", "Worsen by 1",
      "Worsen by 2", "Worsen by 3", "Worsen by 4", "Worsen by 5"
    ))
  )

  colnames(labels) <- c("score", "colour", "leg")

  output$graphH <- renderPlot(
    {
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
          strip.text.y = element_text(angle = 0)
        )

      if (isTruthy(input$updateH)) {
        formatterH <- function(toxic = toxic) {
          paste(editH()$subtitle, toxic, sep = " ")
        }
        g + facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterH)) + # ylim(c(-1, 1)) +
          scale_fill_manual(
            breaks = labels[, 1],
            values = labels[, 2],
            labels = labels[, 3]
          ) +
          geom_text(aes(y = pos, label = percent), size = 3, color = "black") +
          labs(
            x = paste0("Dose (N=", paste(ice_n_patH()$n.pat, collapse = "; "), ")"),
            y = "Percentage (%)",
            fill = "Change in Severity "
          ) +
          # scale_y_continuous(breaks = c(0), limits = c(-1, 1)) +
          geom_hline(yintercept = 0, color = "grey", size = 1) +
          geom_segment(
            data = arrowdfH,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = "darkgrey",
            size = 0.8,
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            inherit.aes = FALSE
          ) +
          scale_x_discrete(expand = expansion(mult = c(1.5, 0))) +
          geom_segment(
            data = arrowdf.revH,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = "darkgrey",
            size = 0.8,
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = -0.25, y = 0.5, label = "Improvement"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = -0.25, y = -0.5, label = "Worsening"),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = missing_H() %>% filter(toxicity %in% toxic),
            aes(x = dose, y = loc, label = label, group = dose), # 'toxicity' must also be in 'missing'
            inherit.aes = FALSE,
            vjust = -0.5
          ) + ggtitle(editH()$title) + theme(plot.title = element_text(size = editH()$titlesize, face = "bold"))
      } else {
        formatterH <- function(toxic = toxic) {
          paste0("Symptomatic Adverse Event ", toxic)
        }
        g +
          facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterH)) + # ylim(c(-1, 1)) +
          scale_fill_manual(
            breaks = labels[, 1],
            values = labels[, 2],
            labels = labels[, 3]
          ) +
          geom_text(aes(y = pos, label = percent), size = 3, color = "black") +
          labs(
            x = paste0("Dose (N=", paste(ice_n_patH()$n.pat, collapse = "; "), ")"),
            y = "Percentage (%)",
            fill = "Change in Severity "
          ) +
          # scale_y_continuous(breaks = c(0), limits = c(-1, 1)) +
          geom_hline(yintercept = 0, color = "grey", size = 1) +
          geom_segment(
            data = arrowdfH,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = "darkgrey",
            size = 0.8,
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            inherit.aes = FALSE
          ) +
          scale_x_discrete(expand = expansion(mult = c(1.5, 0))) +
          geom_segment(
            data = arrowdf.revH,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = "darkgrey",
            size = 0.8,
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = -0.25, y = 0.5, label = "Improvement"), aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = data.frame(x = -0.25, y = -0.5, label = "Worsening"),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE
          ) +
          geom_text(
            data = missing_H() %>% filter(toxicity %in% toxic),
            aes(x = dose, y = loc, label = label, group = dose), # 'toxicity' must also be in 'missing'
            inherit.aes = FALSE,
            vjust = -0.5
          )
      }
    },
    height = function() input$heightH,
    width = function() input$widthH
  )
  ################################################################################################## Graph I
  # Update the selection boxes based on dataset
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
    if (input$ordinal_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointI", label = "Final timepoint of interest", min = sort(tpoint)[2], max = max(tpoint))
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
    if (input$ordinal_date == TRUE) {
      tpoint <- as.Date(tpoint)
    }
    updateSliderInput(session, "tpointI", label = "Final timepoint of interest", min = tpoint[2], max = max(tpoint))
  })

  observe({
    req(input$dummy_ordinal)
    data <- ordinal_data()
    toxic <- unique(data$toxicity)
    updateCheckboxGroupInput(session, "toxicI", label = "Toxicity of interest", choices = toxic, selected = NULL)
  })

  # Define the relevant variables
  no_doseI <- reactive({
    req(input$numdoseI)
    input$numdoseI
  })

  times_I <- reactive({
    req(input$tpointI)
    input$tpointI
  })

  toxicI <- reactive({
    req(input$toxicI)
    input$toxicI
  })

  # Custom max function to help calculate baseline adjusted score
  custom.max <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(max(x, na.rm = TRUE))
    }
  }

  #### Filter and sort data
  ordinal_dataI <- reactive({
    ordinal_data <- ordinal_data()
    end_list <- unique(ordinal_data %>% filter(timepoint <= times_I()) %>% select(timepoint))
    end <- max(end_list$timepoint)
    tp_end <- as.symbol(paste0("tp_", end))
    start <- min(unique(ordinal_data$timepoint))
    tp_start <- as.symbol(paste0("tp_", start))
    ordinal_data <- ordinal_data %>%
      filter(toxicity %in% toxicI()) %>%
      filter(dose %in% no_doseI()) %>%
      group_by(id) %>%
      mutate(has_tp = any(timepoint == end)) %>%
      filter(timepoint <= end) %>%
      pivot_wider(names_from = timepoint, values_from = grade, names_prefix = "tp_")
    tp_cols <- which(grepl("tp_", colnames(ordinal_data)))
    ordinal_data$max_tp <- apply(ordinal_data[, tp_cols], custom.max, MARGIN = 1)
    ordinal_data <- ordinal_data %>%
      mutate(grade_change = max_tp - !!tp_start) %>%
      mutate(base_adj = case_when(
        has_tp == FALSE ~ "ICE",
        TRUE ~ ifelse(grade_change > 0, as.character(max_tp), as.character(1))
      ))

    ordinal_data$base_adj[is.na(ordinal_data$base_adj) == TRUE] <- "Unreported"
    if (isTruthy(input$updateI)) {
      if (editI()$showExtra == FALSE) {
        ordinal_data <- ordinal_data %>% filter(base_adj != "Unreported")
      }
    }
    ordinal_data
  })
  # Data Manipulation
  # prop<- ordinal_data%>%group_by(toxicity,dose)%>%count(base_adj) %>%
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
    if (isTruthy(input$updateI)) {
      if (editI()$invert == TRUE) {
        prop$base_adj <- factor(prop$base_adj, levels = rev(c("Unreported", 5, 4, 3, 2, 1)))
      } else {
        prop$base_adj <- factor(prop$base_adj, levels = c("Unreported", 5, 4, 3, 2, 1))
      }
    } else {
      prop$base_adj <- factor(prop$base_adj, levels = c("Unreported", 5, 4, 3, 2, 1))
    }

    prop
  })

  ice_n_patI <- reactive({
    ice <- propI() %>% distinct(dose, n.pat)
    ice
  })

  # Final figure
  output$graphI <- renderPlot(
    {
      g <- propI() %>% ggplot(aes(fill = base_adj, x = dose, y = proportion * 100)) +
        geom_bar(position = "stack", stat = "identity") +
        coord_flip()
      if (isTruthy(input$updateI)) {
        if (editI()$showExtra == FALSE) {
          groupsizes <- c()
          for (i in toxicI()) {
            pat_numbs <- propI() %>%
              filter(toxicity == !!i) %>%
              distinct(dose, n.pat) %>%
              select(n.pat)
            groupsizes <- append(groupsizes, paste(unlist(pat_numbs), collapse = "; "))
          }
          formatterI <- function(SAE = toxicI()) {
            paste(editI()$subtitle, SAE, paste0("(N=", groupsizes, ")"), sep = " ")
          }
        } else {
          formatterI <- function(SAE = toxicI()) {
            paste(editI()$subtitle, SAE, sep = " ")
          }
        }
        g +
          facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterI)) +
          scale_fill_manual(
            breaks = rev(c(1:5, "Unreported")),
            values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "darkgrey")),
            labels = rev(c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported")),
            guide = guide_legend(reverse = TRUE)
          ) +
          labs(
            x = if (editI()$showExtra == FALSE) {
              paste0("Dose")
            } else {
              paste0("Dose (N=", paste(ice_n_patI()$n.pat, collapse = "; "), ")")
            },
            y = "Percentage (%)",
            fill = "Severity Score"
          ) +
          theme(
            strip.text.y = element_text(angle = 0)
          ) +
          ggtitle(editI()$title) + theme(plot.title = element_text(size = editI()$titlesize, face = "bold"))
      } else {
        formatterI <- function(SAE = toxicI()) {
          paste0("Symptomatic adverse event ", SAE)
        }
        g +
          facet_grid(toxicity ~ ., scales = "free_y", labeller = as_labeller(formatterI)) +
          scale_fill_manual(
            breaks = rev(c(1:5, "Unreported")),
            values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "darkgrey")),
            labels = rev(c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported")),
            guide = guide_legend(reverse = TRUE)
          ) +
          labs(
            x = paste0("Dose (N=", paste(ice_n_patI()$n.pat, collapse = "; "), ")"),
            y = "Percentage (%)",
            fill = "Severity Score"
          ) +
          theme(
            strip.text.y = element_text(angle = 0)
          )
      }
    },
    height = function() input$heightI,
    width = function() input$widthI
  )
  ########################################################## Download buttons

  output$downloadA <- downloadHandler(
    filename = function() {
      paste0("Kaplan-Meier", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightA * 4, width = input$widthA * 4, units = "px")
    }
  )
  output$downloadAcode <- downloadHandler(
    filename = function() {
      paste0("Kaplan_Meier_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Adownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        ordinal_data = ordinal_data(),
        no_dose = input$numdose,
        toxicA = input$toxicA,
        baseA = input$baseChange,
        ordinal_date = input$ordinal_date,
        graphtitle = input$titleA,
        titlesize = input$titlesizeA,
        x_lab = input$xlab_A,
        axissize = input$axisSizeA,
        box_x = input$box_xA,
        box_y = input$box_yA
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadB <- downloadHandler(
    filename = function() {
      paste0("barplot_across_timepoints", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightB * 4, width = input$widthB * 4, units = "px")
    }
  )
  output$downloadBcode <- downloadHandler(
    filename = function() {
      paste0("barplot_across_timepoints_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Bdownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        ordinal_data = ordinal_data(),
        no_doseb = input$numdoseB,
        toxic = input$toxicB,
        final_t = input$tpointB,
        ordinal_date = input$ordinal_date,
        showExtra = input$showExtraB,
        invert = input$invertB,
        title = input$titleB,
        titlesize = input$titlesizeB,
        x_lab = input$xlab_B,
        axissize = input$axisSizeB
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadC <- downloadHandler(
    filename = function() {
      paste0("change_baseline_timepoints", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightC * 4, width = input$widthC * 4, units = "px")
    }
  )
  output$downloadCcode <- downloadHandler(
    filename = function() {
      paste0("change_baseline_timepoints_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Cdownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        continuous_data = continuous_data(),
        no_doseC = input$numdoseC,
        func_C = input$functioningC,
        final_tC = input$tpointC,
        continuous_date = input$continuous_date,
        title = input$titleC,
        titlesize = input$titlesizeC,
        axissize = input$axisSizeC,
        x_lab = input$xlab_C,
        y_lab = input$ylab_C,
        improveC = input$improveC,
        worseC = input$worseC,
        impTextC = input$impTextC,
        worTextC = input$worTextC
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadD <- downloadHandler(
    filename = function() {
      paste0("individual_patient_trajectory", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightD * 4, width = input$widthD * 4, units = "px")
    }
  )
  output$downloadDcode <- downloadHandler(
    filename = function() {
      paste0("individual_patient_trajectory", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Ddownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        continuous_data = continuous_data(),
        no_doseD = input$numdoseD,
        func_D = input$functioningD,
        final_tD = input$tpointD,
        idD = input$select_idD,
        continuous_date = input$continuous_date,
        title = input$titleD,
        titlesize = input$titlesizeD,
        axissize = input$axisSizeD,
        x_lab = input$xlab_D,
        y_lab = input$ylab_D,
        arrow_lab = input$arrowlab_D,
        range = input$rangeD
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadE <- downloadHandler(
    filename = function() {
      paste0("boxplot_timepoints", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightE * 4, width = input$widthE * 4, units = "px")
    }
  )
  output$downloadEcode <- downloadHandler(
    filename = function() {
      paste0("boxplot_timepoints_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Edownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        continuous_data = continuous_data(),
        no_doseE = input$numdoseE,
        func_E = input$functioningE,
        final_tE = input$tpointE,
        continuous_date = input$continuous_date,
        title = input$titleE,
        titlesize = input$titlesizeE,
        axissize = input$axisSizeE,
        x_lab = input$xlab_E,
        y_lab = input$ylab_E,
        arrow_lab = input$arrowlab_E,
        range = input$rangeE
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadF <- downloadHandler(
    filename = function() {
      paste0("change_baseline_functionings_table", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = as_gtable(sum.df.wider.gtF()), device = "svg", width = input$widthF * 6, units = "px")
    }
  )
  output$downloadFcode <- downloadHandler(
    filename = function() {
      paste0("change_baseline_functionings_table_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Fdownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        continuous_data = continuous_data(),
        no_doseF = input$numdoseF,
        func_F = input$functioningF,
        final_tF = input$tpointF,
        continuous_date = input$continuous_date,
        title = input$titleF,
        column1 = input$column1F,
        width = input$widthF
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadG <- downloadHandler(
    filename = function() {
      paste0("change_baseline_functionings_figure", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightG * 4, width = input$widthG * 4, units = "px")
    }
  )
  output$downloadGcode <- downloadHandler(
    filename = function() {
      paste0("change_baseline_timepoints_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Gdownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        continuous_data = continuous_data(),
        no_doseG = input$numdoseG,
        func_G = input$functioningG,
        final_tG = input$tpointG,
        continuous_date = input$continuous_date,
        title = input$titleG,
        titlesize = input$titlesizeG,
        axissize = input$axisSizeG,
        subtitle = input$sublab_G,
        y_lab = input$ylab_G,
        improveG = input$improveG,
        worseG = input$worseG,
        impTextG = input$impTextG,
        worTextG = input$worTextG
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadH <- downloadHandler(
    filename = function() {
      paste0("butterfly", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightH * 4, width = input$widthH * 4, units = "px")
    }
  )
  output$downloadHcode <- downloadHandler(
    filename = function() {
      paste0("butterfly_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Hdownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        ordinal_data = ordinal_data(),
        no_doseH = input$numdoseH,
        toxicH = input$toxicH,
        times_H = input$tpointH,
        ordinal_date = input$ordinal_date,
        title = input$titleH,
        titlesize = input$titlesizeH,
        location = input$unrep_positionH,
        subtitle = input$subtitleH
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  output$downloadI <- downloadHandler(
    filename = function() {
      paste0("baseline_adjusted_barplot", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "svg", height = input$heightI * 4, width = input$widthI * 4, units = "px")
    }
  )
  output$downloadIcode <- downloadHandler(
    filename = function() {
      paste0("baseline_adjusted_barplot_code", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("codeDownload/Idownload.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        ordinal_data = ordinal_data(),
        no_doseI = input$numdoseI,
        toxicI = input$toxicI,
        times_I = input$tpointI,
        ordinal_date = input$ordinal_date,
        showExtra = input$showExtraI,
        invert = input$invertI,
        title = input$titleI,
        titlesize = input$titlesizeI,
        subtitle = input$subtitleI
      )
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
}
shinyApp(ui, server)
