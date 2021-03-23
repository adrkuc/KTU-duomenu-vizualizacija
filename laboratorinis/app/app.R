library(shiny)
library(tidyverse)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Imones kodas", choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    )
  )
)
server <- function(input, output, session) {
  data <- read_csv("https://github.com/adrkuc/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  data <- data %>% filter(ecoActCode == 960900)
  updateSelectizeInput(session, "imones_kodas", choices = data$name, server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(name == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(name == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line()      
  )
}
shinyApp(ui, server)