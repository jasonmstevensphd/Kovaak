

# Import Libraries and Files ----------------------------------------------


library(tidyverse)
library(zoo)
library(shiny)
library(shinydashboard)
library(plotly)

df <- read_csv("Kovaak - Sheet1.csv")

colnames(df) <- c("Date", "Challenge", "Score", "Notes")
# Column Mutations --------------------------------------------------------

summary <- df %>% group_by(Challenge) %>% summarize(counts = n()) %>%
  arrange(desc(counts))

challenges <- summary$Challenge

# UI ----------------------------------------------------------------------


ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Jay's Kovaak Data"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Challenge selection", tabName = "Selection", icon = icon("th")), # End menuItem
        menuItem("Progress over time", tabName = "DateProgress", icon = icon("th")), # End menuItem
        menuItem("Moving average", tabName = "MovingAverage", icon = icon("th")), # End menuItem
        menuItem("Score Distribution", tabName = "Distribution", icon = icon("th")) # End menuItem
      ) # End sidebarMenu
    ), # End dashboardSidebar
    dashboardBody(
      tabItems(
        tabItem(tabName = "Selection",
                        h2("Select your Kovaak challenge"),
                        fluidRow(
                          box("Selection",
                              selectizeInput("Selection",
                                             label = "Select from the list of challenges",
                                             choices = challenges) # End SelectizeInput
                              ) # End box
                          ) # End fluidRow
                        ), # End tabItem
        tabItem(tabName = "DateProgress",
                        h2("Improvement over time"),
                        box("DateProgress",
                            plotlyOutput("Progress") # End SelectizeInput
                            ) # End box
                      ), # End tabItem
        tabItem(tabName = "MovingAverage",
                        h2("Smoothed average of scores"),
                        box("Moving Average",
                            plotlyOutput("MovingAverage") # End SelectizeInput
                            ) # End box
                        ), # End tabItem
        tabItem(tabName = "Distribution",
                        h2("Distribution of scores"),
                        box("Distribution",
                            plotlyOutput("Distribution") # End SelectizeInput
                            ) # End box
                        ) # End tabItem
            ) # End tabItems
      ) # End dashboardBody

) # End dashboardPage


# Server ------------------------------------------------------------------


server <- function(input, output) {


# Reactive Functions ------------------------------------------------------


  # Input$Challenge ---------------------------------------------------------

    df_R <- reactive({
      
      df <- df %>% filter(Challenge == input$Selection)
      return(df)
        
    })
    
    
  # Moving Average Plot -----------------------------------------------------
    
    # Set up the Math
    
    df_MovingAverage <- reactive({
      
      MovAvg <- df %>% filter(Challenge == input$Selection)
      
      MovAvg$Average <- rollmean(MovAvg$Score, 5, fill = NA)
      
      MovAvg$Entry <- c(1:nrow(MovAvg))
      
      return(MovAvg)
      
    })
        

# Outputs -----------------------------------------------------------------

    output$Progress <- renderPlotly({
        
        df <- df_R()
        
        Plot <- ggplot(df, aes(x = Date, y = Score)) +
            geom_point()
        
        Plot <- ggplotly(Plot)
        
        Plot

    })
    
    output$Distribution <- renderPlotly({
        
        df <- df_R()
        
        Plot <- ggplot(df, aes(x = Score)) +
            geom_density()
        
        Plot <- ggplotly(Plot)
        
        Plot
        
    })
    
    output$MovingAverage <- renderPlotly({
      
      Plot <- ggplot(df_MovingAverage(), aes(x = Entry, y = Average)) +
        geom_point()
      
      Plot <- ggplotly(Plot)
      
      Plot
      
    })  
    
    
    
}

# Run the Application -----------------------------------------------------

shinyApp(ui = ui, server = server)

# End of script