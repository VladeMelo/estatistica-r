library(shiny)
library(shinythemes)
library(ggplot2)

setwd('..')
setwd('..')
setwd('C:/Users/vlade/Desktop/projeto-r')

dados <- read.csv("DailyDelhiClimate4Months.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Projeto",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h2("Input"),
                             tags$h4("01/01/2017 - 24/04/2017", style="color:gray"),
                             
                             selectInput("classe", "Classe",
                                         c("Temperatura" = "meantemp",
                                           "Umidade" = "humidity",
                                           "Velocidade do vento" = "wind_speed",
                                           "Pressao atmosferica" = "meanpressure")),
                             numericInput("dia1", "Dia de Inicio", 1, min=1, max=31),
                             numericInput("mes1", "Mes do Inicio", 1, min=1, max=12),
                             numericInput("dia2", "Dia de Termino", 1, min=1, max=31),
                             numericInput("mes2", "Mes de Termino", 1, min=1, max=12),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("result1"),
                             
                             h4("Output 2"),
                             verbatimTextOutput("result2"),
                             
                             h4("Output 3"),
                             dataTableOutput("result3"),
                             
                             h4("Output 4"),
                             plotOutput("result4"),
                             
                             h4("Output 5"),
                             plotOutput("result5"),
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", 
                           sidebarPanel(
                             tags$h2("Input"),
                             tags$h4("01/01/2017 - 24/04/2017", style="color:gray"),
                             
                             selectInput("classe1", "Classe 1",
                                         c("Temperatura" = "meantemp",
                                           "Umidade" = "humidity",
                                           "Velocidade do vento" = "wind_speed",
                                           "Pressao atmosferica" = "meanpressure")),
                             selectInput("classe2", "Classe 2",
                                         c("Temperatura" = "meantemp",
                                           "Umidade" = "humidity",
                                           "Velocidade do vento" = "wind_speed",
                                           "Pressao atmosferica" = "meanpressure")),
                             numericInput("dia3", "Dia de Inicio", 1, min=1, max=31),
                             numericInput("mes3", "Mes do Inicio", 1, min=1, max=12),
                             numericInput("dia4", "Dia de Termino", 1, min=1, max=31),
                             numericInput("mes4", "Mes de Termino", 1, min=1, max=12),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Nada"),
                           ) # mainPanel
                           
                  ) # Navbar 2, tabPanel
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$result1 <- renderText({
    
    paste(input$dia1, input$mes1, 2017, sep="-")
  })
  
  output$result2 <- renderText({
    
    paste(input$dia2, input$mes2, 2017, sep="-")
  })
  
  output$result3 <- renderDataTable({
    mode <- function(column) {
      a <- unique(column)
      a[which.max(tabulate(match(column, a)))]
    }
    
    meses = c(0, 31, 59, 90)
    # se o mes e janeiro, aumenta 0
    # se o mes e fevereiro, aumenta 0 + 31 = 31
    # se o mes e marco, aumenta 0 + 31 + 28 = 59
    # se o mes e abril, aumenta 0 + 31 + 28 + 31 = 90
    
    inicio = input$dia1 + meses[input$mes1]
    fim = input$dia2 + meses[input$mes2]
    
    tabela <- data.frame(Moda = mode(dados[inicio:fim, input$classe]),
                         Media = mean(dados[inicio:fim, input$classe]),
                         Mediana = median(dados[inicio:fim, input$classe]),
                         DesvioPadrao = sd(dados[inicio:fim, input$classe]),
                         Minimo = min(dados[inicio:fim, input$classe]),
                         Maximo = max(dados[inicio:fim, input$classe]))
  })
  
  output$result4 <- renderPlot({
    
    classe = input$classe
    
    meses = c(0, 31, 59, 90)
    
    inicio = input$dia1 + meses[input$mes1]
    fim = input$dia2 + meses[input$mes2]
    
    if (classe == "meantemp") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = meantemp) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Temperatura")
    } else if (classe == "humidity") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = humidity) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Humidade")
    } else if (classe == "wind_speed") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = wind_speed) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Velocidade do vento")
    } else {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = meanpressure) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Pressao")
    }
    
  })
  
  output$result5 <- renderPlot({
    
    classe = input$classe
    
    meses = c(0, 31, 59, 90)
    
    inicio = input$dia1 + meses[input$mes1]
    fim = input$dia2 + meses[input$mes2]
    
    if (classe == "meantemp") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = meantemp) + geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) + labs(x = "Datas", y = "Temperatura")
    } else if (classe == "humidity") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = humidity) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Humidade")
    } else if (classe == "wind_speed") {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = wind_speed) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Velocidade do vento")
    } else {
      ggplot(dados[inicio:fim, ]) + aes(x = date, y = meanpressure) + geom_bar(stat = 'sum') + labs(x = "Datas", y = "Pressao")
    }
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
