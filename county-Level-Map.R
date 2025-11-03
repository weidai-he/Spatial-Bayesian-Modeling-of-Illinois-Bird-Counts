#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(tigris)


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


il_counties <- counties(state = "IL", cb = TRUE, class = "sf")

# UI
ui <- fluidPage(
  titlePanel("Illinois Spring Blue Jay Bird Count: County-Level Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  min = min(bird_long$count_year),
                  max = max(bird_long$count_year),
                  value = min(bird_long$count_year),
                  step = 1,
                  sep = "")
    ),
    mainPanel(
      plotOutput("bird_map", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$bird_map <- renderPlot({
    bird_selected <- bird_long %>%
      filter(count_year == input$year)
    
    il_map <- il_counties %>%
      left_join(bird_selected, by = c("NAME" = "county_name"))
    
    ggplot(il_map) +
      geom_sf(aes(fill = count_total), color = "white", size = 0.2) +
      scale_fill_gradient(low = "#e5f5e0", high = "#238b45", na.value = "grey90") +
      labs(title = paste(input$year, "Illinois County Bird Counts"),
           fill = "Bird Count") +
      theme_light() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    
  })
}

shinyApp(ui = ui, server = server)

