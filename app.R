library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(magrittr)
library(dplyr)
library(ggsurvfit)
library(ggh4x)
library(tidyr)
#library(bslib)

ui <- dashboardPage(
  title = "shiny",
  dashboardHeader(title = span(tags$img(src = "ICR_logo.png"),
                               "OPTIMISE-AR"),
                  titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("GraphA", tabName = "GraphA", icon = icon("sun")),
      menuItem("GraphB", tabName = "GraphB", icon = icon("sun")),
      menuItem("GraphC", tabName = "GraphC", icon = icon("moon")),
      menuItem("GraphD", tabName = "GraphD", icon = icon("moon")),
      menuItem("GraphE", tabName = "GraphE", icon = icon("moon")),
      menuItem("GraphG", tabName = "GraphG", icon = icon("moon")),
      menuItem("GraphH", tabName = "GraphH", icon = icon("sun")),
      menuItem("GraphI", tabName = "GraphI", icon = icon("sun")),
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
      tabItem(tabName = "GraphA",
        fluidRow(
          textOutput("textHeaderA")
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
          textOutput("textHeaderB")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseB", label = "Dosage",
                                                 selected = NULL, inline = TRUE)), ),
          fluidRow(
            column(3, sliderInput("tpointB", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          fluidRow(
            column(3, selectInput("toxicB", label = "Toxicity of interest", choices = NULL ))),
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
      ############################################ GRAPH C 
      tabItem(tabName = "GraphC",
        fluidRow(
          textOutput("textHeaderC")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseC", label = "Dosage",
                                                 selected = NULL, inline = TRUE))),
          fluidRow(
            column(3, sliderInput("tpointC", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          fluidRow(
            column(3, selectInput("functioningC", label = "Functioning of interest", choices = NULL ))),
          fluidRow(
            column(1,  plotOutput("graphC", width = "600px"))
          ),
          fluidPage(
            fileInput("upload_cts", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      ############################################ GRAPH D 
      tabItem(tabName = "GraphD",
        fluidRow(
          textOutput("textHeaderD")
        ),
        fluidRow(
          fluidRow(
            column(3, sliderInput("tpointD", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          #Need to add a patient selection here in order to be able to select patients of interest 
          fluidRow(
            column(4, selectizeInput("idD", label = "Select patients ID",
                           choices = NULL, multiple = TRUE),
                   uiOutput("select_dose"))),
          fluidRow(
            column(3, selectInput("functioningD", label = "Functioning of interest", choices = NULL))),
          fluidRow(
            column(1,  plotOutput("graphD", width = "600px"))
          ),
          fluidPage(
            fileInput("upload_cts", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      ############################################ GRAPH E 
      tabItem(tabName = "GraphE",
        fluidRow(
          textOutput("textHeaderE")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseE", label = "Dosage",
                                                 selected = NULL, inline = TRUE))),
          fluidRow(
            column(3, sliderInput("tpointE", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          fluidRow(
            column(3, selectInput("functioningE", label = "Functioning of interest", choices = NULL ))),
          fluidRow(
            column(1,  plotOutput("graphE", width = "600px"))
          ),
          fluidPage(
            fileInput("upload_cts", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      ########################################################### Graph G
            tabItem(tabName = "GraphG",
        fluidRow(
          textOutput("textHeaderG")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseG", label = "Dosage",
                                                 selected = NULL, inline = TRUE))),
          fluidRow(
            column(3, sliderInput("tpointG", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          fluidRow(
            column(3, checkboxGroupInput("functioningG", label = "Functioning of interest", selected = NULL, inline = TRUE))),
          fluidRow(
            column(1,  plotOutput("graphG", width = "600px"))
          ),
          fluidPage(
            fileInput("upload_cts", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      #########################################################Graph H
      tabItem(tabName = "GraphH",
        fluidRow(
          textOutput("textHeaderH")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseH", label = "Dosage",
                                                 selected = NULL, inline = TRUE)), ),
          fluidRow(
            column(3, sliderInput("tpointH", label = "Start and endpoint for comparison", min = 0, max = 1, value = c(0,1), step = 1))),
          fluidRow(
            column(3, checkboxGroupInput("toxicH", label = "Toxicity of interest", selected = NULL, inline = TRUE))),
          fluidRow(
            column(1,  plotOutput("graphH", width = "600px"))
          ),
          fluidPage(
            fileInput("upload", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      #########################################################Graph I
      tabItem(tabName = "GraphI",
        fluidRow(
          textOutput("textHeaderI")
        ),
        fluidRow(
          fluidRow(
                   column(3,  checkboxGroupInput("numdoseI", label = "Dosage",
                                                 selected = NULL, inline = TRUE)), ),
          fluidRow(
            column(3, sliderInput("tpointI", label = "Endpoint of interest", min = 1, max = 1, value = 1, step = 1))),
          fluidRow(
            column(3, checkboxGroupInput("toxicI", label = "Toxicity of interest", selected = NULL, inline = TRUE))),
          fluidRow(
            column(1,  plotOutput("graphI", width = "600px"))
          ),
          fluidPage(
            fileInput("upload", "Upload data file", accept = c(".csv"))
          ),
          fluidPage(
            DTOutput("dynamic")
          ),
        )
      ),
      ################################################### Description tab
      tabItem(tabName = "description",
        h4("About this site"),
        p("The beginnings of the OPTIMISE-AR app still to be reviewed"),
        p("Authors: George (Server) and Karrar (UI)"),
        p("Maintainers: Rachel Alcraft, ICR Scientific Computing"),
      ),
      tabItem(tabName = "history",
              p("v1.0 - Internal ICR release 20/07/2025"))

    )
  ),
)
###############################################################################
server <- function(input, output, session) {
  output$textHeaderA <- renderText({"Kaplan-Meier Graph"})
  output$textHeaderB <- renderText({"Proportional severity of doses across timepoints plot"})
  output$textHeaderC <- renderText({"Mean change from baseline reported value across timepoints plot"})
  output$textHeaderD <- renderText({"Patient experience plot"})
  output$textHeaderE <- renderText({"Tolerability Boxplot"})
  output$textHeaderG <- renderText({"Mean change in functionalities plot"})
  output$textHeaderH <- renderText({"Butterfly plot"})
  #Import the data
  ordinal_data <- reactive({
    req(input$upload)
    ordinal_data <- read.csv(input$upload$datapath, header = TRUE, sep = ",")[, -1]
    ordinal_data
  })

  #Update the selection boxes based on dataset
  observe({
    req(input$upload)
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
                              time = if (any(base_adj == 3)) min(timepoint[base_adj == 3]) else max(timepoint),
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
  updateCheckboxGroupInput(session, "numdoseB", label = "Dosage", choices = numdose, selected = NULL)
})

observe({
  req(input$upload)
  data <- ordinal_data()
  tpoint <- unique(data$timepoint)
  updateSliderInput(session, "tpointB", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$upload)
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
  as.numeric(input$toxicB)
})

#Filter the data

prop <- reactive({
  req(input$numdoseB)
  req(input$tpointB)
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
  req(input$upload)
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
                labeller = as_labeller(formatter)) +
    labs(x = "Timepoint",
         y = "Proportion",
         fill = "Severity Grade")
})

################################################################################## GRAPH C
#### Import the data
continuous_data <- reactive({
  req(input$upload_cts)
  continuous_data <- read.csv(input$upload_cts$datapath, header = TRUE, sep = ",")[, -1]
  continuous_data
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
  updateSliderInput(session, "tpointC", label = "Final timepoint of interest", max = length(tpointC)-1)
})

observe({
  req(input$upload_cts)
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
  as.numeric(input$functioningC)
})

#### Data Filtering
continuous_data_filtered <- reactive({
  req(input$upload_cts)
  req(input$functioningC)
  continuous_data <- continuous_data()
  continuous_data <- continuous_data %>% filter(functioning == func_C()) %>% dplyr::select(-functioning)# %>% filter(!is.na(score))
  continuous_data
})

diffC <- reactive({
  req(input$upload_cts)
  req(input$functioningC)
  continuous_data_f <- continuous_data_filtered()
  continuous_data_f <- continuous_data_f %>%
    group_by(id) %>%
    mutate(baseline = score[timepoint == 0], adjusted_value = score - baseline) %>%
      ungroup()
    continuous_data_f
})

summaryC <- reactive({
  req(input$upload_cts)
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
  req(input$upload_cts)
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
                for EORTC QLQ-C30 Global status score for functioning",
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
  updateSliderInput(session, "tpointD", label = "Final timepoint of interest", max = length(tpointD)-1)
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
  updateSelectizeInput(session, "idD",
                       choices = data$id,
                       server = TRUE)
})





idD <- reactive({
  req(input$idD)
  as.numeric(input$idD)
})


output$select_dose <- renderUI({
  data <- continuous_data()
  data <- data %>% filter(id %in% idD())
  dose <- unique(data$dose)
  selectizeInput("select_dose", "dose", choices = dose, multiple = TRUE, selected = NULL)
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
  tpointD <- unique(data$timepoint)
  updateSliderInput(session, "tpointE", label = "Final timepoint of interest", max = length(tpointD)-1)
})

observe({
  req(input$upload_cts)
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
    as.numeric(input$functioningE)
})

#### Data Filtering
data_E <- reactive({
  req(input$upload_cts)
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
  req(input$upload_cts)
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
  updateSliderInput(session, "tpointG", label = "Final timepoint of interest", max = length(tpointG)-1)
})

observe({
  req(input$upload_cts)
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
    as.numeric(input$functioningG)
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
  req(input$upload_cts)
  continuous_data_filtered <- continuous_data() %>%
  filter(dose %in% no_doseG()) %>%
  filter(functioning %in% func_G())
  continuous_data_filtered
})

ice_n_pat <- reactive({
  req(input$upload_cts)
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
  req(input$upload_cts)
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
  updateSliderInput(session, "tpointH", label = "Final timepoint of interest", max = length(tpoint)-1)
})

observe({
  req(input$upload)
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
  as.numeric(input$toxicH)
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
      scale_fill_manual(values=labels[match(as.numeric(levels(valH$grade_change)), labels[,1]),2],
                    labels=labels[match(as.numeric(levels(valH$grade_change)), labels[,1]),3])+
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
  as.numeric(input$toxicI)
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
    scale_fill_manual(values = rev(c("darkgreen", "lightgreen", "yellow", "orange", "red", "darkgrey")),
                      labels = rev(c("None", "Mild", "Moderate", "Severe", "Very Severe", "Unreported")),
                      guide = guide_legend(reverse = TRUE)) +
    labs(x = paste0("Dose (N=", paste(ice_n_patI()$n.pat, collapse = "; "), ")"),
          y = "Percentage (%)",
          fill = "Severity Grade") +
    theme(
          strip.text.y = element_text(angle = 0))
})

}
shinyApp(ui, server)
