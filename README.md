# 🌍 Visualización de Indicadores Globales del Banco Mundial con Shiny

Este proyecto implementa una aplicación interactiva en **R Shiny** que permite explorar la evolución temporal y geográfica de indicadores socioeconómicos y ambientales proporcionados por el **Banco Mundial**. La aplicación facilita el análisis visual de patrones por continente, país y región, ayudando a investigadores, estudiantes y tomadores de decisiones a comprender mejor las dinámicas globales.

---

## 📌 Objetivos del Proyecto

- Diseñar una interfaz gráfica intuitiva e interactiva usando Shiny.
- Visualizar múltiples indicadores del Banco Mundial a través del tiempo y el espacio.
- Permitir al usuario personalizar vistas mediante filtros, animaciones, paletas de colores, y herramientas de exploración geoespacial.
- Promover el uso de datos abiertos para el análisis económico, ambiental y social a escala global.

---

## 🧰 Tecnologías Utilizadas

- **R**: Manipulación de datos, visualización y backend de la app.
- **Shiny**: Framework para apps web interactivas en R.
- **ggplot2** y **plotly**: Gráficos estáticos y animados.
- **leaflet**: Visualización de mapas interactivos.
- **dplyr**, **tidyr**: Limpieza y transformación de datos.
- **shinyWidgets**, **bslib**: Personalización del diseño de interfaz.

---

## 📊 Indicadores Visualizados

Los datos provienen directamente del repositorio del Banco Mundial (World Development Indicators). Algunos de los indicadores incluidos en la app son:

- **Ahorro ajustado: agotamiento neto de bosques (% del INB):** representa el valor económico de la pérdida de bosques en relación al ingreso nacional bruto, indicando presión sobre los recursos naturales.
- **Tierra agrícola (% del área total):** porcentaje del territorio de un país utilizado para agricultura, incluyendo tierras cultivables y pastos permanentes.
- **Valor agregado por agricultura, silvicultura y pesca (% del PIB):** mide la contribución directa de estas actividades al Producto Interno Bruto, sin contar los efectos indirectos o secundarios.
- **Emisiones de CO₂ (toneladas métricas per cápita):** promedio de toneladas métricas de dióxido de carbono emitidas por persona en un país, derivadas principalmente del uso de combustibles fósiles.
- **Tasa de fertilidad total (nacimientos por mujer):** número promedio de hijos que tendría una mujer a lo largo de su vida si se mantuvieran las tasas de natalidad actuales.
- **Índice de producción de alimentos (2004–2006 = 100):** indicador del cambio en la producción de alimentos en comparación con el promedio del periodo base 2004–2006.
- **Crecimiento del PIB (anual %):** tasa de variación porcentual anual del Producto Interno Bruto, que refleja el desempeño económico de un país.
- **Esperanza de vida al nacer (años):** número promedio de años que se espera viva una persona desde su nacimiento, bajo las condiciones de mortalidad actuales.
- **Emisiones de metano (equivalente CO₂ per cápita):** mide la cantidad de metano emitida, ajustada a su equivalente en CO₂, dividida entre la población total.
- **Tasa de mortalidad menores de 5 años (por 1,000 nacidos):** número de niños menores de cinco años que mueren por cada mil nacimientos vivos, indicador clave del desarrollo y salud infantil.
- **Emisiones de óxido nitroso (equivalente CO₂ per cápita):** indica las emisiones de óxido nitroso convertidas a su equivalente en CO₂ por persona, típicamente asociadas a la agricultura.
- **Población de 65 años o más (% del total):** proporción de personas mayores de 65 años respecto a la población total, útil para entender el envejecimiento demográfico.
- **Densidad de población (personas/km²):** número promedio de habitantes por kilómetro cuadrado de superficie terrestre, indicador de presión poblacional.
- **Prevalencia de sobrepeso (% de adultos):** porcentaje de adultos con sobrepeso según el índice de masa corporal, relevante en temas de salud pública.
- **Matrícula escolar en primaria (% bruta):** porcentaje de niños matriculados en la educación primaria respecto a la población en edad oficial de asistir, incluso si hay repitentes o fuera de rango de edad.


---

## 🧭 Dashboard

### Inicio

![alt text](/IMAGES/image.png)

### Indicadores a nivel mundial

![alt text](/IMAGES/image-1.png)

### Indicadores a nivel continente

![alt text](/IMAGES/image-2.png)










