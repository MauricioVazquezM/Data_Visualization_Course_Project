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
library(leaflet)


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
              h2("Introducción", style = "background-color: #007acc; color: white; font-weight: bold; padding: 10px; border-radius: 6px;"),
              textOutput("intro")  # Placeholder
    ),
    nav_panel(tagList(bs_icon("globe"), "Mundial"),
              h2("Indicadores a nivel mundial", style = "background-color: #007acc; color: white; font-weight: bold; padding: 10px; border-radius: 6px;"),
              
              fluidRow(
                # Columna izquierda: Inputs + tabla
                column(
                  width = 3,
                  div(
                    style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
                    # Contenedor centrado
                    div(
                      style = "text-align: center;",
                      
                      # selectInput centrado
                      div(style = "display: inline-block; width: 90%;",
                          selectInput("indicador_mundial", "Selecciona indicador:",
                                      choices = unique(data$indicator_name_es),
                                      selected = "Fertility rate, total (births per woman)")),
                      
                      # sliderInput centrado
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          sliderInput("anio_mundial", "Selecciona año:",
                                      min = min(data$year, na.rm = TRUE),
                                      max = max(data$year, na.rm = TRUE),
                                      value = 2010,
                                      sep = ""))
                    ),
                    
                    # Título y tabla debajo
                    h4("Top 50 países"),
                    dataTableOutput("top10_table")
                  )
                ),
                
                # Columna derecha: Mapa
                column(
                  width = 9,
                  div(
                    style = "background-color: #e6f2ff; padding: 20px; border-radius: 10px;",
                    plotOutput("map", height = "650px"),
                    br(),
                    h4("Mapa interactivo (Leaflet)"),
                    leafletOutput("leaflet_map", height = "600px")
                  )
                )
              )
    ),
    nav_panel(tagList(bs_icon("map"), "Continente"),
              h2("Indicadores a nivel continental", style = "background-color: #007acc; color: white; font-weight: bold; padding: 10px; border-radius: 6px;"),
              
              fluidRow(
                # Columna izquierda: widgets y tabla
                column(
                  width = 3,
                  div(
                    style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
                    div(
                      style = "text-align: center;",
                      
                      # Select continente
                      div(style = "display: inline-block; width: 90%;",
                          selectInput("continente_input", "Selecciona un continente:",
                                      choices = sort(unique(na.omit(data$continent))),
                                      selected = "Europe")),
                      
                      # Select indicador
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          selectInput("indicador_cont", "Selecciona indicador:",
                                      choices = unique(data$indicator_name_es),
                                      selected = "Tasa de fertilidad, total (nacimientos por mujer)")),
                      
                      # Slider de año
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          sliderInput("anio_cont", "Selecciona año:",
                                      min = min(data$year, na.rm = TRUE),
                                      max = max(data$year, na.rm = TRUE),
                                      value = 2010,
                                      sep = ""))
                    ),
                    
                    # Tabla debajo
                    h4("Top 15 países"),
                    dataTableOutput("top_cont_table")
                  )
                ),
                
                # Columna derecha: mapa
                column(
                  width = 9,
                  div(
                    style = "background-color: #e6f2ff; padding: 20px; border-radius: 10px;",
                    plotOutput("continent_map", height = "650px")
                  )
                )
              )
    ),
    nav_panel(tagList(bs_icon("pin-map"), "País"),
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
             indicator_name_es == input$indicador_mundial) %>%
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
  
  # Leaflet mundial map
  output$leaflet_map <- renderLeaflet({
    req(input$indicador_mundial, input$anio_mundial)
    
    # Filtrar y preparar datos
    datos_leaflet <- data %>%
      filter(year == input$anio_mundial,
             indicator_name_es == input$indicador_mundial,
             !is.na(value)) %>%
      select(country_name, country_code, value)
    
    # Cargar geometrías con coordenadas
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Unir geometría con datos
    mapa_datos <- mapa_mundo %>%
      left_join(datos_leaflet, by = c("iso_a3" = "country_code"))
    
    # Crear paleta de colores
    pal <- colorNumeric("viridis", domain = mapa_datos$value, na.color = "#f2f2f2")
    
    # Crear mapa interactivo
    leaflet(mapa_datos) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        label = ~paste0(country_name, ": ", round(value, 1)),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = input$indicador_mundial,
                opacity = 1)
  })
  
  
  # Tabla dinamica top 10 mundial
  output$top10_table <- DT::renderDataTable({
    req(input$indicador_mundial, input$anio_mundial)
    
    top10 <- data %>%
      filter(year == input$anio_mundial,
             indicator_name_es == input$indicador_mundial,
             !is.na(value)) %>%
      arrange(desc(value)) %>%
      mutate(value = round(value, 1)) %>%
      select(País = country_name, Valor = value) %>%
      slice_head(n = 50)
    
    DT::datatable(
      top10,
      options = list(
        pageLength = 10,
        dom = 'tip',
        searching = FALSE,
        lengthChange = FALSE,
        order = list(list(1, 'desc'))
      ),
      rownames = FALSE
    )
  })
  
  # Mapa nivel continente
  output$continent_map <- renderPlot({
    req(input$continente_input, input$indicador_cont, input$anio_cont)
    
    datos_cont <- data %>%
      filter(year == input$anio_cont,
             continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             !is.na(value)) %>%
      select(country_name, country_code, value)
    
    mapa_cont <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(continent == input$continente_input)
    
    mapa_datos_cont <- mapa_cont %>%
      left_join(datos_cont, by = c("iso_a3" = "country_code"))
    
    ggplot(mapa_datos_cont) +
      geom_sf(aes(fill = value), color = "gray70", size = 0.2) +
      scale_fill_viridis_c(option = "magma", na.value = "lightgray", name = input$indicador_cont) +
      theme_minimal() +
      labs(title = paste(input$indicador_cont, "-", input$anio_cont),
           subtitle = input$continente_input,
           caption = "Fuente: World Bank") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.position = "right"
      )
  })
  
  # tabla continente
  output$top_cont_table <- DT::renderDataTable({
    req(input$continente_input, input$indicador_cont, input$anio_cont)
    
    top50_cont <- data %>%
      filter(year == input$anio_cont,
             continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             !is.na(value)) %>%
      arrange(desc(value)) %>%
      mutate(value = round(value, 1)) %>%
      select(País = country_name, Valor = value) %>%
      slice_head(n = 15)
    
    DT::datatable(
      top50_cont,
      options = list(
        pageLength = 10,
        dom = 'tip',
        searching = FALSE,
        lengthChange = FALSE,
        order = list(list(1, 'desc'))
      ),
      rownames = FALSE
    )
  })
  
  
  
  # Placeholders para otras pestañas
  output$intro <- renderText("Introduccion web app")
  # output$continente_msg <- renderText("Por continente.")
  output$pais_msg <- renderText("Por país.")
}

# Execution APP
shinyApp(ui, server)