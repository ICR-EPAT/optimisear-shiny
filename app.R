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
      menuItem("App", tabName = "app", icon = icon("table")),
      menuItem("GraphB", tabName = "GraphB", icon = icon("sun")),
      menuItem("GraphC", tabName = "GraphC", icon = icon("sun")),
      menuItem("GraphD", tabName = "GraphD", icon = icon("sun")),
      menuItem("GraphE", tabName = "GraphE", icon = icon("sun")),
      menuItem("GraphG", tabName = "GraphG", icon = icon("sun")),
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
          textOutput("textHeader")
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
          textOutput("textHeader")
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
          textOutput("textHeader")
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
          textOutput("textHeader")
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
      ################################################### Description tab
      tabItem(tabName = "description",
        h4("About this site"),
        p("We used the skeleton of the GenAlpha Shiny app to run a trail for only Graph A (KP graph) using the dummy variable"),
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
    summarise(n.pat = n())
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
  continuous_data <- continuous_data %>% filter(functioning == func_C()) %>% dplyr::select(-functioning) %>% filter(!is.na(score))
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
    summarise(mean = mean(adjusted_value, na.rm = TRUE), sd = sd(adjusted_value, na.rm = TRUE) / sqrt(n()))
  summary <- diff %>% mutate(l.ci = mean - (1.96 * sd)) %>% mutate(u.ci = mean + (1.96 * sd))
  summary <- summary %>% filter(timepoint %in% c(1:final_tC())) %>% filter(dose %in% no_doseC())
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
    geom_text(aes(x = 1.6, y = 17, label = "Improvement"),
              inherit.aes = FALSE) +
    geom_text(aes(x = 1.6, y = -17, label = "Worsening"),
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
    xlab("Timepoint") + ylab(paste0("EORTC QLQ-C30 global health score in category ", func_E())) +
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


continuous_data_filtered <- reactive({
  req(input$upload_cts)
  continuous_data_filtered <- continuous_data() %>%
  filter(dose %in% no_doseG()) %>%
  filter(functioning %in% func_G())
  continuous_data_filtered
})

ice_n_pat <- reactive({
  req(input$upload_cts)
  continuous_data <- continuous_data_filtered()
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
  tp_end <- sym(paste0("tp_", endT))
  df <- continuous_data_filtered() %>%
  filter(timepoint %in% c(0, endT)) %>%
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
              se = sd(score, na.rm = TRUE) / sqrt(non_na_count))
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

output$graphG<- renderPlot({
  end_t <- final_tG()
  arrowdf <- arrowdf()
  formatterG <- function(end = end_t) {paste0("Functioning ", end)}
  data.frame(sum.dfG()) %>% ggplot(aes(x = custom_x, y = mean, colour = as.factor(dose))) +
  geom_point(size = 3) +
  scale_color_discrete(name = "Dose level") +
  geom_errorbar(aes(ymin = l.ci, ymax = u.ci), width = 0.2, size = 1.2) +
  theme_minimal(base_size = 14) + ylim(c(-25, 25)) +
  geom_rect(data = thresholds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Threshold),
            alpha = 0.3, inherit.aes = FALSE)+
  scale_fill_manual(
    name = "Threshold",
    values = c("Improvement" = "palegreen1", "Worsening" = "red")
  ) + 
    labs(
    y = paste0("Mean change from baseline to timepoint ", final_tG(), "(with 95% CI) \n for EORTC QLQ-C30 functional domains"),
    x = paste0("Dose (N= ) & Unreported Patients (N= )")) +
  facet_wrap(~ as.factor(functioning), nrow = 1,labeller = as_labeller(formatterG), scales = "free_x") +
  scale_x_discrete(expand = expansion(add = c(1, 1))) +
  theme(fill = "Threshold") +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = 25),
               colour = "darkgrey", size = 0.8, arrow = arrow(type = "closed",
                                                                length = unit(0.1, "inches"))) +
  geom_segment(data = arrowdf,
               aes(x = 0.5, xend = 0.5, y = 0, yend = -25), 
               colour = "darkgrey", size = 0.8, arrow = arrow(type="closed",
                                                              length = unit(0.1, "inches")))+
  geom_text(data = arrowdf, aes(x = 2, y = 25, label = "Improvement"),
            inherit.aes = FALSE) +
  geom_text(data = arrowdf, aes(x = 2, y = -25, label = "Worsening"),
            inherit.aes = FALSE)
})
}
shinyApp(ui, server)
