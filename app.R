library(dplyr)
library(spdplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readxl)
library(echarts4r)
library(shiny)
library(shinyalert)
library(katexR)
library(bslib)

theme <- bs_theme(
    bg = "#000000", fg = "#B8BCC2",
    "input-border-color" = "#a6a6a6"
)

# Datos
shp_mex <- read_sf("México_Estados/México_Estados.shp")
ing_mex <- read_excel("ingresos_mex.xlsx")

# Uniendo las bases
shp_mex <- shp_mex %>%
    left_join(ing_mex, by = c("ESTADO" = "ESTADO"))
# Cuadro de texto

mytext <- paste(
    "Estado: ", shp_mex$ESTADO,"<br/>", 
    "Decil más bajo: ", shp_mex$`Decil 1`, "<br/>", 
    "Decil más alto: ", shp_mex$`Decil 10`, "<br/>", 
    "Total: ", shp_mex$Total, 
    sep="") %>%
    lapply(htmltools::HTML)

ui <- bootstrapPage(
    theme = theme,
    useShinyalert(),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  selectInput("variable",
                              "Escoge un decil:",
                              c("Total" = "Total",
                                "Decil 1" = "Decil 1",
                                "Decil 2" = "Decil 2",
                                "Decil 3" = "Decil 3",
                                "Decil 4" = "Decil 4",
                                "Decil 5" = "Decil 5",
                                "Decil 6" = "Decil 6",
                                "Decil 7" = "Decil 7",
                                "Decil 8" = "Decil 8",
                                "Decil 9" = "Decil 9",
                                "Decil 10" = "Decil 10")),
                  echarts4rOutput('hist_plot', height = '350px', width = '400px'),
                  echarts4rOutput('pie', height = '300px', width = '350px'),
                  div(style = "display:inline-block; float:right", actionButton("do", icon = icon( "info"), label = ""  ))
    )
)

server <- function(input, output, session) {
    
    dato <- reactive({
        
        ing2 <- ing_mex %>%
            select(ESTADO, nuevo = {input$variable})
        
        left_join(shp_mex, ing2, by = "ESTADO")
    })
    
    output$map <- renderLeaflet({
        
        mypalette <- colorBin(palette="RdGy", domain=dato()$nuevo, na.color="transparent", bins = 4)
        
        leaflet() %>%
            addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
                     attribution = paste(
                         "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                         "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
                     ))  %>%
            setView( lat=23, lng=-100 , zoom=5.3) %>%
            addPolygons(
                data = dato(),
                fillColor = ~mypalette(nuevo),
                stroke=TRUE,
                fillOpacity = 0.85,
                color="white",
                weight=0.3,
                label = mytext,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            )  %>%
            addLegend(pal = mypalette, values = ~nuevo, opacity = 0.9, title = "Ingresos en México", position = "bottomleft", data = dato())
        
    })
    
    output$hist_plot <- renderEcharts4r({
        dato() %>%
            e_charts() %>%
            e_histogram(nuevo, name = "Distribución",breaks = "freedman-diaconis") %>%
            e_tooltip(trigger = "axis") %>%
            e_color(color = "#753732")
    })
    
    dato2 <- reactive({ dato() |> 
            mutate(nuevo2 = nuevo/sum(nuevo)*100  ) |> 
            arrange(desc(nuevo2)) |> 
            top_n(8, nuevo2) 
        
    })
    
    output$pie <- renderEcharts4r({
        dato2() |> 
            e_charts(ESTADO) |>
            e_pie(nuevo2, name = "Porcentaje (top 8)", radius = c("50%", "83%") ) |>
            e_tooltip(trigger = "item") |> 
            e_legend(FALSE) |> 
            e_color(color = c("#0466c8", "#0353a4","#023e7d","#002855","#001845",
                              "#001233","#33415c","#5c677d","#7d8597", "979dac")) 
    })
    
    observeEvent(input$do, {
        shinyalert(
            title = "Los ingresos en México por deciles",
            text = p("Esta app considera los ingresos en México, desde el más bajo 
      hasta el más alto. Los deciles consideran una serie de datos ordenados 
      de menor a mayor, los cuales consideran un 10% de la información por cada
      decil. Los deciles se pueden calcular mediante la fórmula siguiente:", hr(katex("D_1= \\frac{(n+1)}{100}, \\, D_2= \\frac{50(n+1)}{100}, \\, D_3= \\frac{99(n+1)}{100}")), 
                     hr("O bien, para datos no agrupados, usaríamos la expresión:"), 
                     hr(katex("Q_n , D_n , P_n = L_i + [\\frac{f_{Qn}, D_n, P_n, - f_a}{f_{Q,D,P}}]*C ")), 
                     hr("Por su parte, el histograma nos muestra la forma en la que se distribuyen los ingresos por decil, utilizando
                  la regla de Friedman-Diaconis para el número de intervalos. El gráfico de pastel
                  nos muestra un top 8 de los estados que representan un ingreso más alto por decil.") ),
            size = "m",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = TRUE,
            type = "info",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
        )
    })
    
}

shinyApp(ui, server)





