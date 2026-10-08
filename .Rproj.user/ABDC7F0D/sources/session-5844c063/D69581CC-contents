library(shiny)
library(tidyquant)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(rsconnect)
library(RColorBrewer)
library(shinyWidgets)
library(shinyBS)

tickers <- c(
  'co1 comdty', 'cl1 comdty',
  'xb1 comdty', 'ho1 comdty',
  'ng1 comdty', 'tzt1 comdty'
)
nombres <- c(
  "Crudo Brent", "Crudo WTI",
  "Gasolina RBOB", "Diésel ULSD",
  "Gas natural HH", "Gas natural TTF"
)

precios <- read_xlsx("datos.xlsx") %>%
  mutate(
    value = ifelse(ticker == "tzt1 comdty", value / 3.412141633, value),
    ticker = factor(ticker, levels = tickers, labels = nombres)
    )

# Datos

# UI
ui <- fluidPage(
  tags$head(
    
    tags$style(HTML("
    
    body, .container-fluid, .main-panel {
      background-color: transparent !important;
      margin: 0 !important;
      padding-bottom: 0 !important;
    }

  "))
  ),
  
  tags$h4(
    "Precios Globales de Energía",
    tags$span(
      icon("question-circle"), 
      id = "info_icon", 
      style = "cursor: pointer; color: #007BFF;"
    )
  ),
  bsTooltip(
    id = "info_icon",
    title = paste(
      "Series corresponden a los principales contratos de referencia en Estados Unidos,",
      "excepto por el crudo Brent, que es de referencia internacional,",
      "y el gas natural TTF, que es de referencia en Europa."
    ),
    placement = "right",
    trigger = "click"
  ),
  
  absolutePanel(
    top=10, right=10, fixed=TRUE, draggable=FALSE,
    radioGroupButtons(
      inputId="intervalo",
      label=NULL,
      choices=c(
        "12 meses"="1",
        "5 años"="5",
        "10 años"="10"
      ),
      selected="1",
      size="xs",
      direction="vertical"
    )
  ),
  
  prettyRadioButtons(
    inputId="grupo",
    choices=c(
      "Petróleo"="crudo",
      "Refinados"="refin",
      "Gas"="gas"
    ),
    selected="crudo",
    inline=TRUE,
    label=NULL
  ),
  
  fluidRow(
    column(width = 12,
           div(
             style = "position: relative; left: -20px;",
             plotlyOutput("grafico", height = "300px", width = "100%")
           ),
           tags$div(
             style = "display: flex; justify-content: space-between; align-items: center;
                    margin-top: 0px;",
             tags$div(
               style = "font-size: 11px; color: black; display: flex; flex-direction: column; align-items: flex-start;",
               tags$img(src = "icono_flecha.svg", height = "30px", style = "margin-bottom: 2px;"),
               "Fuente: Bloomberg"
             ),
             tags$img(src = "footer.png", height = "35px")
             )
           )
    )
)

# Server
server <- function(input, output) {
  
  output$grafico <- renderPlotly({
    
    colores <- setNames(
      brewer.pal(length(nombres), "Dark2"),
      nombres
    )
    titulo_yaxis <- case_when(
      input$grupo == "crudo" ~ "Dólares por barril (US$)",
      input$grupo == "refin" ~ "Centavos por galón (USd)",
      TRUE ~ "Dólares por millón de BTU (US$)"
      )
    filtrado <- precios %>%
      filter(date >= max(date) - years(as.numeric(input$intervalo)))
    
    if (input$grupo == "crudo") {
      procesado <- filtrado %>%
        filter(ticker %in% nombres[1:2])
    } else if (input$grupo == "refin") {
      procesado <- filtrado %>%
        filter(ticker %in% nombres[3:4]) %>%
        mutate(value = value * 1e2)
    } else {
        procesado <- filtrado %>%
          filter(ticker %in% nombres[5:6])
        }
    
    plot_ly(
      procesado,
      x=~date,
      y=~value,
      color=~ticker,
      colors=colores,
      type="scatter",
      mode="lines",
      hovertemplate="%{fullData.name}: %{y:,.1f}<extra></extra>"
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        legend = list(
          orientation = "h",
          y = 1.2, x = 0
          ),
        yaxis = list(
          title = titulo_yaxis,
          tickformat = ",.0f"
          ),
        xaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(
        displayModeBar = FALSE,
        showTips = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        staticPlot = FALSE,
        displaylogo = FALSE,
        responsive = TRUE,
        locale = "es"
      )
  })
}

shinyApp(ui, server)