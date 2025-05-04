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
library(bsicons)


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
    nav_panel(tagList(bs_icon("app-indicator"), "Introducción"),
              h2("Introducción"),
              textOutput("intro")  # Placeholder
    ),
    nav_panel(tagList(bs_icon("globe"), "Mundial"),
              h2("Visualización Mundial"),
              
              # Layout en dos columnas: izquierda (inputs), derecha (mapa)
              fluidRow(
                # Columna izquierda con inputs
                column(
                  width = 4,
                  selectInput("indicador_mundial", "Selecciona un indicador:",
                              choices = unique(data$indicator_name),
                              selected = "Fertility rate, total (births per woman)"),
                  
                  sliderInput("anio_mundial", "Selecciona un año:",
                              min = min(data$year, na.rm = TRUE),
                              max = max(data$year, na.rm = TRUE),
                              value = 2010,
                              sep = "")
                ),
                
                # Columna derecha con la visualización
                column(
                  width = 8,
                  plotOutput("fertility_map", height = "600px")  # puedes ajustar el alto si quieres
                )
              )
    ),
    nav_panel(tagList(bs_icon("map"), "Por continente"),
              h2("Visualización por continente"),
              textOutput("continente_msg")  # Placeholder
    ),
    nav_panel(tagList(bs_icon("pin-map"), "Por país"),
              h2("Visualización por país"),
              textOutput("pais_msg")  # Placeholder
    )
  )
)

# Server
server <- function(input, output) {
  
  # Example
  output$fertility_map <- renderPlot({
    req(input$indicador_mundial, input$anio_mundial)
    
    datos_filtrados <- data %>%
      filter(year == input$anio_mundial,
             indicator_name == input$indicador_mundial) %>%
      select(country_name, country_code, value)
    
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    mapa_datos <- mapa_mundo %>%
      left_join(datos_filtrados, by = c("iso_a3" = "country_code"))
    
    ggplot(mapa_datos) +
      geom_sf(aes(fill = value), color = "gray70", size = 0.2) +
      scale_fill_viridis_c(option = "viridis", na.value = "lightgray", name = input$indicador_mundial) +
      theme_minimal() +
      labs(title = paste(input$indicador_mundial, "-", input$anio_mundial),
           subtitle = "Visualización mundial",
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