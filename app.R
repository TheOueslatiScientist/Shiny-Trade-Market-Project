# ------------------------
# PACKAGES NECESSAIRES
# ------------------------
library(shiny)
library(quantmod)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)
library(scales)
library(DT)


# ------------------------
# FONCTIONS
# ------------------------

# Fonction Volatilité
calcul_volatilite <- function(symbole, from, to, event_date) {
  data <- getSymbols(symbole, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  prix <- Cl(data)
  returns <- dailyReturn(prix, type = "log")
  
  vol_before <- sd(returns[paste0("/", event_date - 1)], na.rm = TRUE)
  vol_after  <- sd(returns[paste0(event_date, "/")], na.rm = TRUE)
  
  data.frame(
    Indice = symbole,
    Vol_Avant = round(vol_before, 5),
    Vol_Apres = round(vol_after, 5),
    Variation = round(vol_after - vol_before, 5),
    Ratio = round(vol_after / vol_before, 2)
  )
}

# Fonction Rendement
calcul_rendement <- function(symbole, from, to, event_date) {
  data <- getSymbols(symbole, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  prix <- Cl(data)
  returns <- dailyReturn(prix, type = "log")
  
  rend_before <- mean(returns[paste0("/", event_date - 1)], na.rm = TRUE)
  rend_after  <- mean(returns[paste0(event_date, "/")], na.rm = TRUE)
  
  data.frame(
    Indice = symbole,
    Rendement_Avant = round(rend_before, 5),
    Rendement_Apres = round(rend_after, 5),
    Variation = round(rend_after - rend_before, 5)
  )
}

# ------------------------
# UI
# ------------------------

ui <- fluidPage(
  
  titlePanel("Dashboard des Marchés Financiers"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indice", "Choisir un indice :",
                  choices = c("NASDAQ" = "^IXIC",
                              "Euro Stoxx 50" = "^STOXX50E",
                              "Forex EUR/USD" = "EURUSD=X",
                              "Nikkei 225" = "^N225",
                              "Or (Gold)" = "GC=F",
                              "Obligations US 10Y" = "^TNX")),
      dateInput("from", "Date début", value = "2025-03-10"),
      dateInput("to", "Date fin", value = "2025-04-10")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cours + MM5", plotOutput("plot_mm")),
        tabPanel("Histogramme Rendement", plotOutput("plot_rendement")),
        tabPanel("Density", plotOutput("plot_density")),
        tabPanel("Boxplot + Points", plotOutput("plot_box")),
        tabPanel("Rendements Cumulés", plotOutput("plot_cum")),
        tabPanel("Volume Transactions", plotOutput("plot_volume")),
        tabPanel("Tendance LOESS", plotOutput("plot_loess")),
        tabPanel("Tableau Volatilité", DT::dataTableOutput("tab_vol")),
        tabPanel("Tableau Rendement", DT::dataTableOutput("tab_rend"))
      )
    )
  )
)

# ------------------------
# SERVER
# ------------------------

server <- function(input, output) {
  
  event_date <- as.Date("2025-04-01")
  
  noms_indices <- c(
    "^IXIC" = "NASDAQ",
    "^STOXX50E" = "Euro Stoxx 50",
    "EURUSD=X" = "Forex EUR/USD",
    "^N225" = "Nikkei 225",
    "GC=F" = "Or (Gold)",
    "^TNX" = "Obligations US 10Y"
  )
  
  indices <- names(noms_indices)
  
  data <- reactive({
    getSymbols(input$indice, src = "yahoo", from = input$from, to = input$to, auto.assign = FALSE)
  })
  
  df <- reactive({
    df_xts <- data()
    prix <- Cl(df_xts)
    volume <- Vo(df_xts)
    rendement <- c(NA, diff(log(as.numeric(prix))))
    
    data.frame(
      date = index(prix),
      prix = as.numeric(prix),
      mm5 = rollmean(as.numeric(prix), 5, fill = NA, align = "right"),
      volume = as.numeric(volume[index(prix)]),
      rendement = rendement,
      cum_return = cumsum(replace_na(rendement, 0)),
      periode = ifelse(index(prix) < event_date, "Avant", "Après")
    )
  })
  
  # ------------------------
  # GRAPHIQUES
  # ------------------------
  
  output$plot_mm <- renderPlot({
    ggplot(df(), aes(x = date)) +
      geom_line(aes(y = prix, color = "Prix")) +
      geom_line(aes(y = mm5, color = "MM5")) +
      geom_vline(xintercept = as.numeric(event_date), linetype = "dashed", color = "red") +
      scale_color_manual(values = c("Prix" = "blue", "MM5" = "orange")) +
      labs(title = "Cours avec Moyenne Mobile 5 jours", x = "Date", y = "Prix", color = "Légende") +
      theme_minimal()
  })
  
  output$plot_rendement <- renderPlot({
    ggplot(df(), aes(x = rendement, fill = rendement > 0)) +
      geom_histogram(bins = 15, color = "white") +
      scale_fill_manual(values = c("red", "green"), guide = FALSE) +
      geom_vline(xintercept = mean(df()$rendement, na.rm = TRUE), color = "blue", linetype = "dashed") +
      labs(title = "Histogramme Rendements Journaliers", x = "Rendement", y = "Fréquence") +
      theme_minimal()
  })
  
  output$plot_density <- renderPlot({
    moyennes <- df() %>% group_by(periode) %>% summarise(moy = mean(rendement, na.rm = TRUE))
    ggplot(df(), aes(x = rendement, fill = periode)) +
      geom_density(alpha = 0.4) +
      geom_vline(data = moyennes, aes(xintercept = moy, color = periode), linetype = "dashed") +
      scale_fill_manual(values = c("Avant" = "blue", "Après" = "orange")) +
      labs(title = "Density des rendements", x = "Rendement", y = "Densité") +
      theme_minimal()
  })
  
  output$plot_box <- renderPlot({
    ggplot(df(), aes(x = periode, y = rendement, fill = periode)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.5) +
      geom_jitter(width = 0.15, color = "black") +
      scale_fill_manual(values = c("Avant" = "blue", "Après" = "orange")) +
      labs(title = "Boxplot Rendement", x = "Période", y = "Rendement") +
      theme_minimal()
  })
  
  output$plot_cum <- renderPlot({
    ggplot(df(), aes(x = date, y = cum_return)) +
      geom_area(fill = "darkgreen", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(event_date), linetype = "dashed", color = "red") +
      labs(title = "Rendement Cumulé", x = "Date", y = "Cumul") +
      theme_minimal()
  })
  
  output$plot_volume <- renderPlot({
    ggplot(df(), aes(x = date, y = volume)) +
      geom_col(fill = "steelblue") +
      geom_vline(xintercept = as.numeric(event_date), linetype = "dashed", color = "red") +
      labs(title = "Volume des transactions", x = "Date", y = "Volume") +
      theme_minimal()
  })
  
  output$plot_loess <- renderPlot({
    ggplot(df(), aes(x = date, y = prix)) +
      geom_point(color = "gray") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      geom_vline(xintercept = as.numeric(event_date), linetype = "dashed", color = "red") +
      labs(title = "Tendance LOESS", x = "Date", y = "Prix") +
      theme_minimal()
  })
  
  # ------------------------
  # TABLEAUX
  # ------------------------
  
  output$tab_vol <- DT::renderDataTable({
    df_vol <- bind_rows(lapply(indices, function(ind) calcul_volatilite(ind, input$from, input$to, event_date)))
    df_vol$Indice <- noms_indices[df_vol$Indice]
    datatable(df_vol)
  })
  
  output$tab_rend <- DT::renderDataTable({
    df_rend <- bind_rows(lapply(indices, function(ind) calcul_rendement(ind, input$from, input$to, event_date)))
    df_rend$Indice <- noms_indices[df_rend$Indice]
    datatable(df_rend, options = list(pageLength = 6)) %>%
      formatStyle('Variation', color = styleInterval(0, c('red', 'green')))
  })
  
}

# ------------------------
# RUN APP
# ------------------------
shinyApp(ui, server)

  
  
  