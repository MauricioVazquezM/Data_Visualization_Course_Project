"
Libraries
"
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(viridis)
library(bslib) 


"
Data read
"
data <- read.csv("C:\\Users\\mauva\\OneDrive\\Documents\\ITAM\\10mo Semestre\\VISUALIZACION DE LA INFORMACION\\CODIGO\\PROJECT REPOSITORY\\Data_Visualization_Course_Project\\DATA\\ESGData_transformed.csv")


"
Web App code source
"

# UI
ui <- page_fluid(
  navset_tab(
    id = "tab",  # Útil si luego usas input$tab
    nav_panel("Intrducción",
              h2("Introducción"),
              textOutput("intro")  # Placeholder
    ),
    nav_panel("Mundial",
              h2("Visualización Mundial"),
              plotOutput("fertility_map")
    ),
    nav_panel("Por continente",
              h2("Visualización por continente"),
              textOutput("continente_msg")  # Placeholder
    ),
    nav_panel("Por país",
              h2("Visualización por país"),
              textOutput("pais_msg")  # Placeholder
    )
  )
)

# Server
server <- function(input, output) {
  
  # Example
  output$fertility_map <- renderPlot({
    fertilidad_2010 <- data %>%
      filter(year == 2010,
             indicator_name == "Fertility rate, total (births per woman)") %>%
      select(country_name, country_code, value)
    
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    mapa_datos <- mapa_mundo %>%
      left_join(fertilidad_2010, by = c("iso_a3" = "country_code"))
    
    ggplot(mapa_datos) +
      geom_sf(aes(fill = value), color = "gray70", size = 0.2) +
      scale_fill_viridis_c(option = "viridis", na.value = "lightgray", name = "Tasa de fertilidad") +
      theme_minimal() +
      labs(title = "Tasa de fertilidad por país (2010)",
           subtitle = "Nacimientos por mujer",
           caption = "Fuente: World Bank") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
  })
  
  # Placeholders para otras pestañas
  output$intro <- renderText("Introduccion web app")
  output$continente_msg <- renderText("Por continente.")
  output$pais_msg <- renderText("Por país.")
}

# Execution APP
shinyApp(ui, server)