library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
#library(bslib)

ui <- dashboardPage(
  title = "shiny",
  dashboardHeader(title = span(tags$img(src = "ICR_logo.png"),
                               "Demo shiny-app"),
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
                                  {"TP53 AlphaFold or crystal structure"})
  df <- reactive({
    if (input$method == "AlphaFold") {
      plot_data <- "data/tp53-af.csv"
      plot_df <- read.csv(plot_data)
      val <- plot_df
    } else {
      plot_data <- "data/tp53-1kzy.csv"
      plot_df <- read.csv(plot_data)
      paste(plot_df)
      val <- plot_df
    }
    return(val)
  })
  output$plot <- renderPlot(
                            {ggplot(df(), aes(x, y, colour = get(input$hue))) +
                               geom_point() +
                               if (input$hue == "bfactor") scale_colour_viridis_b() else scale_colour_discrete()},
                            res = 76)
  output$plotyz <- renderPlot(
                              {ggplot(df(), aes(y, z, colour = get(input$hue))) +
                                 geom_point() +
                                 if (input$hue == "bfactor") scale_colour_viridis_b() else scale_colour_discrete()},
                              res = 76)
  output$plotzx <- renderPlot(
                              {ggplot(df(), aes(z, x, colour = get(input$hue))) +
                                 geom_point() +
                                 if (input$hue == "bfactor") scale_colour_viridis_b() else scale_colour_discrete()},
                              res = 76)

  output$dynamic <- renderDT(df(), options = list(pageLength = 5))
}
##################################################################################
shinyApp(ui, server)