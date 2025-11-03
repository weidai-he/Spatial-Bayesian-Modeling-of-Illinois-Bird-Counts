#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readr)

bird_data <- read_csv("INHS_Spring_Bird_Count.csv")

bird_long <- bird_data |>
  pivot_longer(
    cols = -c(species, count_year, count_hrs, birdsperhr),
    names_to = "county_name",
    values_to = "count_total"
  ) |> 
  select(count_year, county_name, count_total)

name_fix <- c(
  "De Kalb" = "DeKalb",
  "Du Page" = "DuPage",
  "St Clair" = "St. Clair",
  "La Salle" = "LaSalle"
)

bird_long <- bird_long %>%
  mutate(county_name = recode(county_name, !!!name_fix))

# UI
ui <- fluidPage(
  titlePanel("Illinois Spring Blue Jay Bird Count Trends by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Select County:",
                  choices = sort(unique(bird_long$county_name)),
                  multiple = TRUE,
                  selected = "Cook")
    ),
    mainPanel(
      plotOutput("trend_plot", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$trend_plot <- renderPlot({
    req(input$county) 
    
    bird_selected <- bird_long %>%
      filter(county_name %in% input$county)
    
    ggplot(bird_selected, aes(x = count_year, y = count_total,
                              color = county_name, group = county_name)) +
      geom_line(size = 1, alpha = 0.8) +
      labs(x = "Year", y = "Count Total",
           title = "Blue Jay Count Trends (1975–2017)",
           color = "County") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
