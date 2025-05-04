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
              h2("Indicadores \n(Mundial)"),
              # Layout: izquierda (inputs), derecha (mapa)
              fluidRow(
                # Columna izquierda
                column(
                  width = 2,
                  div(
                    style = "background-color: #f0f0f0; padding: 15px; border-radius: 8px;",
                    
                    selectInput("indicador_mundial", "Selecciona indicador:",
                                choices = unique(data$indicator_name),
                                selected = "Fertility rate, total (births per woman)"),
                    
                    sliderInput("anio_mundial", "Selecciona año:",
                                min = min(data$year, na.rm = TRUE),
                                max = max(data$year, na.rm = TRUE),
                                value = 2010,
                                sep = "")
                  )
                ),
                # Columna derecha 
                column(
                  width = 8,
                  fluidRow(
                    column(width = 8,
                           plotOutput("map", height = "600px")),
                    column(width = 4,
                           h4("Top 10 países"),
                           dataTableOutput("top10_table"))
                  )
                ),
                
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
  
  # Mapa
  output$map <- renderPlot({
    req(input$indicador_mundial, input$anio_mundial)
    
    # Filtrando datos
    datos_filtrados <- data %>%
      filter(year == input$anio_mundial,
             indicator_name == input$indicador_mundial) %>%
      select(country_name, country_code, value)
    
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    mapa_datos <- mapa_mundo %>%
      left_join(datos_filtrados, by = c("iso_a3" = "country_code"))
    
    # Mapa de visualizacion mapa mundial
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
  
  output$top10_table <- DT::renderDataTable({
    req(input$indicador_mundial, input$anio_mundial)
    
    top10 <- data %>%
      filter(year == input$anio_mundial,
             indicator_name == input$indicador_mundial,
             !is.na(value)) %>%
      arrange(desc(value)) %>%
      select(País = country_name, Valor = value) %>%
      slice_head(n = 10)
    
    DT::datatable(
      top10,
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        order = list(list(1, 'desc')),
        dom = 'Bfrtip',
        buttons = c('csv')
      ),
      rownames = FALSE,
      extensions = 'Buttons'
    )
  })
  
  # Placeholders para otras pestañas
  output$intro <- renderText("Introduccion web app")
  output$continente_msg <- renderText("Por continente.")
  output$pais_msg <- renderText("Por país.")
}

# Execution APP
shinyApp(ui, server)