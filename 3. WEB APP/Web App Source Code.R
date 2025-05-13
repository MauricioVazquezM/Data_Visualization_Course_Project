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
library(plotly)


"
Data read
"
data <- read.csv("C:\\Users\\mauva\\OneDrive\\Documents\\ITAM\\10mo Semestre\\VISUALIZACION DE LA INFORMACION\\CODIGO\\PROJECT REPOSITORY\\Data_Visualization_Course_Project\\DATA\\ESGData_transformed.csv")


"
Web App code source
"

###### UI ######
ui <- page_fluid(
  navset_tab(
    id = "tab",  # Útil si luego usas input$tab
    nav_panel(tagList(bs_icon("app-indicator"), "Inicio"),
              h2("Indicadores del World Bank", 
                 style = "background-color: #007acc; color: white; font-weight: bold; padding: 10px; border-radius: 6px;"),
              
              div(
                style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
                
                # Logo centrado
                div(
                  style = "text-align: center; margin-top: 30px;",
                  img(src = "The_World_Bank_logo.png", height = "150px")
                ),
                
                # Texto descriptivo e indicadores
                div(
                  style = "margin-top: 30px; font-size: 15px; line-height: 1.5;",
                  HTML("
        <h4 style='font-weight: bold;'>🧭 Objetivo del proyecto</h4>
        <p>Este proyecto tiene como objetivo desarrollar una interfaz gráfica interactiva que permita a los usuarios explorar de manera visual una serie de indicadores socioeconómicos y ambientales proporcionados por el Banco Mundial. A través de mapas georreferenciados y gráficos dinámicos, la herramienta facilita la identificación de patrones espaciales y temporales en variables como la tasa de fertilidad, las emisiones de CO₂, el acceso a la educación, entre otros indicadores clave del desarrollo.</p>
        
        <h4 style='font-weight: bold;'>🌐 ¿Qué es el World Bank?</h4>
        <p>El Banco Mundial es una organización internacional que proporciona financiamiento, asesoramiento y asistencia técnica a países en desarrollo con el objetivo de reducir la pobreza y fomentar el desarrollo sostenible. A través de su plataforma de datos abiertos, recopila y publica indicadores económicos, sociales y ambientales que permiten evaluar y comparar el progreso de los países a lo largo del tiempo. Este panel interactivo utiliza una selección de esos indicadores para facilitar su visualización y análisis.</p>
        
        <h4 style='font-weight: bold; margin-top: 25px;'>📊 Descripción de indicadores</h4>
        <ul>
          <li><b>Ahorro ajustado: agotamiento neto de bosques (% del INB):</b> representa el valor económico de la pérdida de bosques en relación al ingreso nacional bruto, indicando presión sobre los recursos naturales.</li>
          <li><b>Tierra agrícola (% del área total):</b> porcentaje del territorio de un país utilizado para agricultura, incluyendo tierras cultivables y pastos permanentes.</li>
          <li><b>Valor agregado por agricultura, silvicultura y pesca (% del PIB):</b> mide la contribución directa de estas actividades al Producto Interno Bruto, sin contar los efectos indirectos o secundarios.</li>
          <li><b>Emisiones de CO₂ (toneladas métricas per cápita):</b> promedio de toneladas métricas de dióxido de carbono emitidas por persona en un país, derivadas principalmente del uso de combustibles fósiles.</li>
          <li><b>Tasa de fertilidad total (nacimientos por mujer):</b> número promedio de hijos que tendría una mujer a lo largo de su vida si se mantuvieran las tasas de natalidad actuales.</li>
          <li><b>Índice de producción de alimentos (2004-2006 = 100):</b> indicador del cambio en la producción de alimentos en comparación con el promedio del periodo base 2004-2006.</li>
          <li><b>Crecimiento del PIB (anual %):</b> tasa de variación porcentual anual del Producto Interno Bruto, que refleja el desempeño económico de un país.</li>
          <li><b>Esperanza de vida al nacer (años):</b> número promedio de años que se espera viva una persona desde su nacimiento, bajo las condiciones de mortalidad actuales.</li>
          <li><b>Emisiones de metano (equivalente CO₂ per cápita):</b> mide la cantidad de metano emitida, ajustada a su equivalente en CO2, dividida entre la población total.</li>
          <li><b>Tasa de mortalidad menores de 5 años (por 1,000 nacidos):</b> número de niños menores de cinco años que mueren por cada mil nacimientos vivos, indicador clave del desarrollo y salud infantil.</li>
          <li><b>Emisiones de óxido nitroso (equivalente CO₂ per cápita):</b> indica las emisiones de óxido nitroso convertidas a su equivalente en CO2 por persona, típicamente asociadas a la agricultura.</li>
          <li><b>Población de 65 años o más (% del total):</b> proporción de personas mayores de 65 años respecto a la población total, útil para entender el envejecimiento demográfico.</li>
          <li><b>Densidad de población (personas/km²):</b> número promedio de habitantes por kilómetro cuadrado de superficie terrestre, indicador de presión poblacional.</li>
          <li><b>Prevalencia de sobrepeso (% de adultos):</b> porcentaje de adultos con sobrepeso según el índice de masa corporal, relevante en temas de salud pública.</li>
          <li><b>Matrícula escolar en primaria (% bruta):</b> porcentaje de niños matriculados en la educación primaria respecto a la población en edad oficial de asistir, incluso si hay repitentes o fuera de rango de edad.</li>
        </ul>
        
        <h4 style='font-weight: bold; margin-top: 25px;'>📚 Referencias</h4>
        <ul>
          <li>World Bank Open Data. Recuperado de <a href='https://data.worldbank.org/' target='_blank'>https://data.worldbank.org/</a></li>
          <li>Kaggle - ESG Dataset: <a href='https://www.kaggle.com/datasets/tunguz/environment-social-and-governance-data/data?select=ESGData.csv' target='_blank'>https://www.kaggle.com/datasets/tunguz/environment-social-and-governance-data</a></li>
          <li>R Shiny. Documentación oficial: <a href='https://shiny.posit.co/' target='_blank'>https://shiny.posit.co/</a></li>
          <li>Plotly para R: <a href='https://plotly.com/r/' target='_blank'>https://plotly.com/r/</a></li>
          <li>Gráficos espaciales en R: <a href='https://r-charts.com/es/espacial/mapas-interactivos-leaflet/' target='_blank'>https://r-charts.com/es/espacial/mapas-interactivos-leaflet/</a></li>
        </ul>
      ")
                )
              )
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
                                      selected = "Tasa de fertilidad total (nacimientos por mujer)")),
                      
                      # sliderInput centrado
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          sliderInput("anio_mundial", "Selecciona año:",
                                      min = min(data$year, na.rm = TRUE),
                                      max = max(data$year, na.rm = TRUE),
                                      value = 2010,
                                      step=1,
                                      sep = "")),
                      div(style = "display: inline-block; width: 90%;",
                          selectInput(
                            inputId = "palette_choice",
                            label = "Selecciona paleta de colores (Mapa):",
                            choices = c(
                              "Viridis (verde-amarillo-azul)" = "viridis",
                              "Plasma (rosado-amarillo-morado)" = "plasma",
                              "Magma (negro-rojo-naranja)" = "magma",
                              "Inferno (oscuro-rojizo)" = "inferno",
                              "Azul-verde" = "YlGnBu",
                              "Rojo-azul divergente" = "RdYlBu",
                              "Verdes monocromáticos" = "Greens",
                              "Azules monocromáticos" = "Blues"
                            ),
                            selected = "viridis",
                            selectize = TRUE
                        )
                      ),
                      div(style = "display: inline-block; width: 90%;",
                          selectInput(
                            inputId = "palette_choice_box",
                            label = "Selecciona paleta de colores (Boxplot):",
                            choices = c(
                              "Colores pastel suaves 1" = "Pastel1",
                              "Colores pastel suaves 2" = "Pastel2",
                              "Colores vibrantes moderados" = "Set2"
                            ),
                            selected = "Pastel2",
                            selectize = TRUE
                          )
                      ),
                      # Slider de velocidad de animacion
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          sliderInput(
                            inputId = "velocidad_anim_mun",
                            label = "Velocidad de animación",
                            min = 100,
                            max = 1000,
                            value = 200,
                            step = 100
                          )
                      )
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
                    uiOutput("leaflet_titulo"),
                    leafletOutput("leaflet_map", height = "600px"),
                    # Separador
                    tags$hr(),
                    # Boxplot
                    uiOutput("boxplot_titulo"),
                    plotOutput("boxplot", height = "400px"),
                    # Separador
                    tags$hr(),
                    # Animacion
                    h4("Evolución temporal por continente", style = "margin-top: 20px; font-weight: bold;"),
                    plotlyOutput("scatter_animado_mundial", height = "400px"),
                    tags$p(
                      "🔄 Esta visualización muestra la evolución del valor promedio del indicador seleccionado por continente a lo largo del tiempo.",
                      style = "font-size: 14px; color: #333; margin-top: 10px;"
                    )
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
                                      selected = "North America")),
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
                                      step=1,
                                      sep = "")),
                      div(style = "display: inline-block; width: 90%;",
                          selectInput(
                            inputId = "palette_choice_cont",
                            label = "Selecciona paleta de colores (Mapa):",
                            choices = c(
                              "Viridis (verde-amarillo-azul)" = "viridis",
                              "Plasma (rosado-amarillo-morado)" = "plasma",
                              "Magma (negro-rojo-naranja)" = "magma",
                              "Inferno (oscuro-rojizo)" = "inferno",
                              "Azul-verde" = "YlGnBu",
                              "Rojo-azul divergente" = "RdYlBu",
                              "Verdes monocromáticos" = "Greens",
                              "Azules monocromáticos" = "Blues"
                            ),
                            selected = "plasma",
                            selectize = TRUE
                          )
                      ),
                      div(style = "display: inline-block; width: 90%;",
                          selectInput(
                            inputId = "color_choice_box",
                            label = "Selecciona color (gráfico de barras):",
                            choices = c(
                              "Azul suave" = "#5b8fd2",
                              "Verde musgo" = "#7da27d",
                              "Amarillo arena" = "#e1c36d",
                              "Púrpura apagado" = "#a086b7"
                            ),
                            selected = "#5b8fd2",
                            selectize = TRUE
                          )
                      ),
                      # Slider de velocidad de animacion
                      div(style = "display: inline-block; width: 90%; margin-top: 10px;",
                          sliderInput(
                            inputId = "velocidad_anim",
                            label = "Velocidad de animación",
                            min = 100,
                            max = 1000,
                            value = 200,
                            step = 100
                          )
                        )
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
                    uiOutput("leaflet_titulo_cont"),
                    leafletOutput("leaflet_continent_map", height = "600px"),
                    # Separador
                    tags$hr(),
                    # Boxplot
                    uiOutput("barplot_titulo"),
                    plotOutput("continent_barplot", height = "400px"),
                    # Separador
                    tags$hr(),
                    # Animacion
                    h4("Evolución temporal por país", style = "margin-top: 20px; font-weight: bold;"),
                    plotlyOutput("scatter_animado_cont", height = "400px"),
                    tags$p(
                      "🔄 Esta visualización muestra cómo ha cambiado el indicador en los 10 países con los valores más altos del continente seleccionado.",
                      style = "font-size: 14px; color: #333; margin-top: 10px;"
                    )
                    
                  )
                )
              )
    )
  )
)



###### Server ######
server <- function(input, output) {
  
  
  # Leaflet mundial map
  output$leaflet_map <- renderLeaflet({
    req(input$indicador_mundial, input$anio_mundial)
    
    # Filtrar datos
    datos_leaflet <- data %>%
      filter(year == input$anio_mundial,
             indicator_name_es == input$indicador_mundial,
             !is.na(value)) %>%
      select(country_name, country_code, value)
    
    # Coordenadas
    mapa_mundo <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Unir geometría con datos
    mapa_datos <- mapa_mundo %>%
      left_join(datos_leaflet, by = c("iso_a3" = "country_code"))
    
    # Crear paleta de colores
    pal <- colorNumeric(input$palette_choice, 
                        domain = mapa_datos$value, 
                        na.color = "lightgray")
    
    # Mapa interactivo mundial
    leaflet(mapa_datos, options = leafletOptions(worldCopyJump = FALSE)) %>%
      setView(lng = -20, lat = 20, zoom = 2.5) %>% 
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
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~value,
        title = "Valor del indicador",  
        opacity = 1,
        labFormat = labelFormat(digits = 1),
        na.label = "Sin dato"  
      )
  })
  
  # Tabla dinamica mundial
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
  
  # Titulo dinamico leaflet
  output$leaflet_titulo <- renderUI({
    req(input$indicador_mundial, input$anio_mundial)
    indicador_mod <- sub("\\(", paste0(" en ", input$anio_mundial, "<br>("), input$indicador_mundial)
    HTML(paste0("<h4 style='font-weight: bold;'>", indicador_mod, "</h4>"))
  })
  
  
  # Boxplot por continente
  output$boxplot <- renderPlot({
    datos_box <- data %>%
      filter(year == input$anio_mundial,
             indicator_name_es == input$indicador_mundial,
             !is.na(value))
    
    # Calcular orden de continentes por mediana
    orden_continentes <- datos_box %>%
      group_by(continent) %>%
      summarise(mediana = median(value, na.rm = TRUE)) %>%
      arrange(desc(mediana)) %>%
      pull(continent)
    
    # Convertir continente en factor con orden deseado
    datos_box$continent <- factor(datos_box$continent, levels = orden_continentes)
    
    ggplot(datos_box, aes(x = continent, y = value, fill = continent)) +
      geom_boxplot() +
      scale_fill_brewer(palette = input$palette_choice_box)+
      labs(x = "Continente", y = "Valor") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#e6f2ff", color = NA),   
        panel.background = element_rect(fill = "#e6f2ff", color = NA),  
        legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_blank()  # ✅ sin título
      )
  })
  
  # Titulo para boxplots
  output$boxplot_titulo <- renderUI({
    req(input$anio_mundial)
    h4(paste("Distribución por continente en", input$anio_mundial),
       style = "margin-top: 20px; font-weight: bold;")
  })
  
  # Animacion mundial scatterplot
  output$scatter_animado_mundial <- renderPlotly({
    req(input$indicador_mundial)
    
    datos_anim_mundial <- data %>%
      filter(indicator_name_es == input$indicador_mundial,
             !is.na(value),
             !is.na(continent)) %>%
      group_by(continent, year) %>%
      summarise(valor = mean(value, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      data = datos_anim_mundial,
      x = ~year,
      y = ~valor,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~continent,
      # symbol = ~continent,
      frame = ~year,
      text = ~paste("Continente:", continent, "<br>Año:", year, "<br>Valor:", round(valor, 2)),
      hoverinfo = "text",
      marker = list(size = 12),
      line = list(simplify = FALSE)
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = list(text = "Año", font = list(family = "Arial", size = 16, color = "black"))
        ),
        yaxis = list(
          title = list(text = "Valor del indicador", font = list(family = "Arial", size = 16, color = "black"))
        ),
        showlegend = TRUE,
        plot_bgcolor = "#e6f2ff",
        paper_bgcolor = "#e6f2ff"
      ) %>%
      animation_opts(frame = input$velocidad_anim, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Año: "))
  })
  
  
  

  ##### Mapa nivel continente #####
  # Tabla continente
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
  
  # Leaflet continent map
  output$leaflet_continent_map <- renderLeaflet({
    req(input$continente_input, input$indicador_cont, input$anio_cont)
    
    datos_leaflet <- data %>%
      filter(year == input$anio_cont,
             continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             !is.na(value)) %>%
      select(country_name, country_code, value)
    
    mapa_cont <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(continent == input$continente_input)
    
    mapa_datos_leaflet <- mapa_cont %>%
      left_join(datos_leaflet, by = c("iso_a3" = "country_code")) 
    
    pal <- colorNumeric(input$palette_choice_cont, 
                        domain = mapa_datos_leaflet$value, 
                        na.color = "#f2f2f2")
    
    leaflet(mapa_datos_leaflet) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -100, lat = 40, zoom = 2.5) %>% 
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
      addLegend("bottomright", 
                pal = pal, 
                values = ~value,
                title = "Valor del indicador",
                na.label = "Sin dato",
                opacity = 1)
  })
  
  # Titulo dinamico leaflet
  output$leaflet_titulo_cont <- renderUI({
    req(input$indicador_cont, input$anio_cont)
    indicador_mod <- sub("\\(", paste0(" en ", input$anio_cont, "<br>("), input$indicador_cont)
    HTML(paste0("<h4 style='font-weight: bold;'>", indicador_mod, "</h4>"))
  })
  
  # Barplot para pestala de continente
  output$continent_barplot <- renderPlot({
    req(input$continente_input, input$indicador_cont, input$anio_cont)
    
    datos_bar <- data %>%
      filter(year == input$anio_cont,
             continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             !is.na(value)) %>%
      select(country_name, value) %>%
      arrange(desc(value)) %>%
      slice_head(n = 10)
    
    ggplot(datos_bar, aes(x = reorder(country_name, value), y = value)) +
      geom_bar(stat = "identity", fill = input$color_choice_box) +  
      coord_flip() +
      labs(x = "País", y = "Valor del indicador") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#e6f2ff", color = NA),
        panel.background = element_rect(fill = "#e6f2ff", color = NA),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Titulo de barplot con año
  output$barplot_titulo <- renderUI({
    req(input$anio_cont)
    h4(paste("Comparación entre países (Top 10) en", input$anio_cont),
       style = "margin-top: 20px; font-weight: bold;")
  })
  
  # Animación de scatterplot
  output$scatter_animado_cont <- renderPlotly({
    req(input$continente_input, input$indicador_cont, input$anio_cont)
    
    # 1. Top 10 países del año seleccionado
    top_paises <- data %>%
      filter(year == input$anio_cont,
             continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             !is.na(value)) %>%
      arrange(desc(value)) %>%
      slice_head(n = 10) %>%
      pull(country_name)
    
    # 2. Datos solo para esos países, en todos los años
    datos_anim <- data %>%
      filter(continent == input$continente_input,
             indicator_name_es == input$indicador_cont,
             country_name %in% top_paises,
             !is.na(value)) %>%
      select(pais = country_name, year, valor = value)
    
    plot_ly(
      data = datos_anim,
      x = ~year,
      y = ~valor,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~pais,
      # symbol = ~pais,  
      frame = ~year,
      text = ~paste("País:", pais, "<br>Año:", year, "<br>Valor:", round(valor, 2)),
      hoverinfo = "text",
      marker = list(size = 10),
      line = list(simplify = FALSE)  
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = list(text = "Año", font = list(family = "Arial", size = 16, color = "black"))
        ),
        yaxis = list(
          title = list(text = "Valor del indicador", font = list(family = "Arial", size = 16, color = "black"))
        ),
        showlegend = TRUE,
        plot_bgcolor = "#e6f2ff",
        paper_bgcolor = "#e6f2ff"
      ) %>%
      animation_opts(frame = input$velocidad_anim, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Año: "))
  })
  
  
  
  # Placeholders para otras pestañas
  output$intro <- renderText("Introduccion web app")
  
}

# Execution APP
shinyApp(ui, server)