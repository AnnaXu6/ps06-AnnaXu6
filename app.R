library(shiny)
library(ggplot2)
library(dplyr)

# Load data
data <- read_delim('UAH-lower-troposphere-long.csv')

# Define UI
ui <- fluidPage(
  
  # App title and explanatory text
  navbarPage("My Shiny App",
  
    # Tabset panel with multiple pages
    tabsetPanel(
      tabPanel("General Info",
               br(),
               p("This app uses satellite temperature data from",
                 strong("UAH")
               ),
               p("This shiny app allows you to interactively explore the dataset by selecting a variable,"
               ), 
               p("selecting a range of values, and filtering out missing values. "
               ),
               p("The app displays a scatterplot and a table based on your inputs."
               ),
               p("Here is the summary about the UAH dataset from", 
                 em("1978 - 2023:")
               ),
              # Display dataset summary
              verbatimTextOutput("summary")),
      tabPanel("Plot",
              sidebarLayout(
                # Sidebar with plot type and variable selection
                sidebarPanel(
                  selectInput("plot_type", "Select plot type:",
                              choices = c("Scatterplot", "Boxplot", "Histogram")),
                  selectInput("x_var", "Select x-axis variable:",
                              choices = colnames(data)),
                  selectInput("y_var", "Select y-axis variable:",
                              choices = colnames(data)),
                  actionButton("update_plot", "Update plot")
                ),
                # Main panel with plot output
                mainPanel(
                  plotOutput("plot"),
                  verbatimTextOutput("plot_info")
                )
              )),
      tabPanel("Table",
              sidebarLayout(
                # Sidebar with variable selection
                sidebarPanel(
                  selectInput("table_var", "Select variable:",
                              choices = colnames(data)),
                  sliderInput("n_rows", "Number of rows to display:",
                              min = 1, max = nrow(data), value = 10),
                  actionButton("update_table", "Update table")
                ),
                 # Main panel with table output
                mainPanel(
                  tableOutput("table"),
                  verbatimTextOutput("table_info")
                )
              ))
    )
  )
)
# Define server
server <- function(input, output) {
  
  # General info page
  output$summary <- renderPrint({
    summary(data)
  })
  
  # Plot page
  plot_data <- reactive({
    data %>%
      select(input$x_var, input$y_var)
  })
  
  output$plot <- renderPlot({
    if (input$plot_type == "Scatterplot") {
      ggplot(plot_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_point()
    } else if (input$plot_type == "Boxplot") {
      ggplot(plot_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_boxplot()
    } else if (input$plot_type == "Histogram") {
      ggplot(plot_data(), aes(x = !!sym(input$x_var))) +
        geom_histogram()
    }
  })
  
  output$plot_info <- renderText({
    paste("The plot displays", input$x_var, "on the x-axis and", input$y_var, "on the y-axis.")
  })
  
  # Table page
  table_data <- reactive({
    data %>%
      select(input$table_var) %>%
      head(input$n_rows)
  })
  
  output$table <- renderTable({
    table_data()
  })
  
  output$table_info <- renderText({
    paste("The table displays the first", input$n_rows, "rows of", input$table_var)
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
