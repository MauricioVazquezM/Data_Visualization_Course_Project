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


"
Data read
"
data <- read.csv("C:\\Users\\mauva\\OneDrive\\Documents\\ITAM\\10mo Semestre\\VISUALIZACION DE LA INFORMACION\\CODIGO\\PROJECT REPOSITORY\\Data_Visualization_Course_Project\\DATA\\ESGData_transformed.csv")


"
Web App code source
"

# UI
ui <- fluidPage(
  titlePanel("Tasa de fertilidad por país (2010)"),
  mainPanel(
    plotOutput("fertility_map")
  )
)

# Server
server <- function(input, output) {
  output$fertility_map <- renderPlot({
    
    # Filtrar datos
    fertilidad_2010 <- data %>%
      filter(year == 2010,
             indicator_name == "Fertility rate, total (births per woman)") %>%
      select(country_name, country_code, value)
    
    # Mapa base
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Join con datos de fertilidad
    mapa_datos <- mapa_mundo %>%
      left_join(fertilidad_2010, by = c("iso_a3" = "country_code"))
    
    # Plot
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
}

# Ejecutar app
shinyApp(ui = ui, server = server)