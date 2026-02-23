pacman::p_load(shiny, tidyverse)


exam <- read_csv("data/Exam_data.csv")

#print(exam)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pupils Exam Results Dashboard"),
# total width is always 12. by default it's 4 for sidebar and 8 for main panel
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"), # default choice
      sliderInput(inputId = "bins",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value = 10)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

  
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data = exam, 
           aes_string(x = input$variable)) +
             geom_histogram(bins = input$bins,
                            color = "black",
                            fill = "light blue")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
