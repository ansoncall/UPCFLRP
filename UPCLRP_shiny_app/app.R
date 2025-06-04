# plot mapper
# a shiny application to visualize the location of trees at a plot
library(tidyverse)
library(shiny)

d <- read_csv("overstory_tidy.csv")
d_ntrees <- read_csv("ntrees.csv")
# fix na in dbh column

# shiny ui ####
ui <- fluidPage(
  titlePanel("Plot Mapper"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", "Select a plot:", choices = unique(d$id),
                selected = unique(d_ntrees$id)[1]),
      uiOutput("secondSelection"),
      uiOutput("thirdSelection")
    ),
    mainPanel(
      plotOutput("plot_map"),
      tableOutput("table")
    )
  )
)


# shiny server ####
server <- function(input, output) {
  output$secondSelection <- renderUI({
    selectInput("User", "visit", choices = d %>%
                  filter(id == input$plot) %>%
                  pull(t_idxn) %>%
                  unique(),
                multiple = TRUE,
                selected = d %>%
                  filter(id == input$plot) %>%
                  pull(t_idxn) %>%
                  unique() %>%
                  head(1))
  })
  output$thirdSelection <- renderUI({
    sliderInput("User2", "dbh",
                min = d %>%
                  filter(id == input$plot) %>%
                  pull(dbh) %>%
                  min(),
                max = d %>%
                  filter(id == input$plot) %>%
                  pull(dbh) %>%
                  max(),
                value = c(0, 1000))
  })
  output$plot_map <- renderPlot({
    # filter data based on user input
    filtered_data <- d %>%
      filter(id == input$plot & t_idxn %in% input$User &
               dbh >= input$User2[1] & dbh <= input$User2[2])

    # create the plot
    ggplot(filtered_data, aes(x = x, y = y, fill = t_idxn, size = dbh)) +
      geom_point(shape = 21) +
      labs(title = paste("Plot", input$plot, "Visit", input$species),
           x = "Midline distance",
           y = "N/S distance") +
      theme_minimal() +
      coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))

  })
  # ntrees table output
  output$table <- renderTable({
    d_ntrees
  })


}
# run the app
shinyApp(ui = ui, server = server)

