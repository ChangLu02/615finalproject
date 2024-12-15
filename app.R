library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)
library(geosphere)

# UI
ui <- dashboardPage(
  skin = "blue",  
  dashboardHeader(title = "Diving Into Maldives"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("globe")),
      menuItem("GDP Analysis", tabName = "gdp_analysis", icon = icon("chart-line")),
      menuItem("Comparison with Sri Lanka", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Current Challenges", tabName = "challenges_maldives", icon = icon("exclamation-triangle"))
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        body { font-size: 15px; }
        h1, h2, h3 { color: #007bff; font-weight: bold; }
        .box { border-radius: 8px; box-shadow: 2px 2px 5px #aaa; }
        p { font-size: 17px; line-height: 1.5; text-align: justify; }
      "))
    ),
    
    tabItems(
      # Introduction Tab
      tabItem(
        tabName = "intro",
        fluidRow(
          box(
            title = "Introduction to Maldives", width = 12,
            p("The Maldives, a jewel in the Indian Ocean, is an archipelago of 1,200 stunning islands scattered 
              like pearls across turquoise waters. Known for its pristine white sandy beaches, crystal-clear 
              lagoons, and vibrant coral reefs, the Maldives offers unparalleled natural beauty and tranquility."),
            p("However, beneath its serene beauty lies a challenge – the Maldives is one of the most climate-vulnerable 
              island nations, facing rising sea levels and environmental pressures.")
          )
        ),
        fluidRow(
          box(
            title = "Beautiful Views of Maldives", width = 12,
            column(3, img(src = "mal1.jpg", style = "width: 100%; border-radius: 8px;")),
            column(3, img(src = "mal2.jpg", style = "width: 100%; border-radius: 8px;")),
            column(3, img(src = "mal3.jpg", style = "width: 100%; border-radius: 8px;")),
            column(3, img(src = "mal4.jpg", style = "width: 100%; border-radius: 8px;"))
          )
        )
      ),
      
      # GDP Analysis Tab
      tabItem(
        tabName = "gdp_analysis",
        fluidRow(
          box(
            title = "Maldives GDP Analysis", width = 12,
            tabsetPanel(
              tabPanel("GDP Over Time", 
                       img(src = "MALgdp.png", style = "width: 100%;"),
                       p("The graph shows the trend in GDP with a LOESS smooth line indicating consistent growth.")),
              tabPanel("Data After Differencing",
                       img(src = "diffGDP.png", style = "width: 100%;"),
                       p("The ADF test result indicates non-stationarity, requiring transformations.")),
              tabPanel("Log-Transformation",
                       img(src = "logdiffGDP.png", style = "width: 100%;"),
                       p("After log-transformation, the ADF test confirms stationarity of the series.")),
              tabPanel("ACF & PACF Analysis",
                       img(src = "ACFPACF.png", style = "width: 100%;"),
                       p("The ARIMA(1,1,1) model is suggested based on ACF and PACF patterns.")),
              tabPanel("Model Fit",
                       img(src = "ARIMA.png", style = "width: 100%;"),
                       p("Model diagnostics confirm the ARIMA(1,1,1) model as a good fit.")),
              tabPanel("Forecasting",
                       img(src = "forecast.png", style = "width: 100%;"),
                       p("Forecasts indicate rapid GDP growth in the coming years."))
            )
          )
        )
      ),
      
      # Comparison Tab
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Comparison Between Maldives and Sri Lanka", width = 12,
            tabsetPanel(
              tabPanel("GDP Comparison", 
                       img(src = "GDPcomp.png", style = "width: 100%;"),
                       p("Sri Lanka shows larger economic growth, while Maldives displays steady growth driven by tourism.")),
              tabPanel("Population Comparison", 
                       img(src = "popcomp.png", style = "width: 100%;"),
                       p("Sri Lanka has a significantly larger population compared to the Maldives.")),
              tabPanel("Life Expectancy", 
                       img(src = "lifecomp.png", style = "width: 100%;"),
                       p("Both countries have shown improvements, with Maldives surpassing Sri Lanka recently.")),
              tabPanel("Intentional Homicides", 
                       img(src = "homicomp.png", style = "width: 100%;"),
                       p("Sri Lanka's decline reflects stability, while Maldives' rates remain low with occasional spikes."))
            )
          )
        )
      ),
      
      # Current Challenges Tab
      tabItem(
        tabName = "challenges_maldives",
        fluidRow(
          box(
            title = "Current Challenges in the Maldives", width = 12,
            tabsetPanel(
              tabPanel("Major Coral Bleaching Events",
                       img(src = "bleaching.png", style = "width: 100%; border-radius: 8px;"),
                       p("The Maldives has faced severe coral bleaching events in 1998, 2016, and 2020. 
                          These events, often triggered by rising sea surface temperatures, threaten 
                          marine biodiversity and tourism-dependent livelihoods.")
              ),
              tabPanel("Projected Sea-Level Rise",
                       img(src = "sealevel.png", style = "width: 100%; border-radius: 8px;"),
                       p("With over 80% of the Maldives' land area less than 1 meter above sea level, 
                          projections suggest sea levels could rise by 1.32 meters by 2150. This poses 
                          significant threats to the nation's existence and its economic stability.")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Render Coral Bleaching Image with renderUI
  output$bleachingContent <- renderUI({
    tags$img(
      src = "www/bleaching.png", 
      style = "width: 100%; border-radius: 8px;",
      alt = "Major Coral Bleaching Events"
    )
  })
  
  # Render Sea-Level Rise Image with renderUI
  output$sealevelContent <- renderUI({
    tags$img(
      src = "www/sealevel.png", 
      style = "width: 100%; border-radius: 8px;",
      alt = "Projected Sea-Level Rise"
    )
  })
  
  
  
  # GDP Analysis Images
  output$gdpImages <- renderUI({
    tags$img(src = "MALgdp.png", style = "width: 100%;")
  })
  
  # Dynamic Tabs for GDP Analysis Images
  output$tabGDP <- renderUI({
    tabsetPanel(
      tabPanel("GDP Over Time", 
               img(src = "MALgdp.png", style = "width: 100%;"),
               p("The graph shows GDP trends with LOESS smooth lines.")),
      tabPanel("Differencing",
               img(src = "diffGDP.png", style = "width: 100%;"),
               p("The series requires differencing to ensure stationarity.")),
      tabPanel("Log-Transformation",
               img(src = "logdiffGDP.png", style = "width: 100%;"),
               p("Log-transformation helps achieve stationarity.")),
      tabPanel("Forecasting",
               img(src = "forecast.png", style = "width: 100%;"),
               p("Forecast values indicate future growth."))
    )
  })
  
  # Comparison Images
  output$comparisonContent <- renderUI({
    tabsetPanel(
      tabPanel("GDP Comparison", 
               img(src = "GDPcomp.png", style = "width: 100%;"),
               p("Sri Lanka's GDP is larger, but Maldives shows consistent growth.")),
      tabPanel("Population Comparison", 
               img(src = "popcomp.png", style = "width: 100%;"),
               p("Sri Lanka has a much larger population compared to the Maldives.")),
      tabPanel("Life Expectancy", 
               img(src = "lifecomp.png", style = "width: 100%;"),
               p("Maldives now exceeds Sri Lanka in life expectancy.")),
      tabPanel("Homicide Rates", 
               img(src = "homicomp.png", style = "width: 100%;"),
               p("Sri Lanka experienced a decline, while Maldives remains low with occasional spikes."))
    )
  })
  
}

# Run the App
shinyApp(ui, server)
