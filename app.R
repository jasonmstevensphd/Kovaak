

# Import Libraries and Files ----------------------------------------------


library(tidyverse)
library(zoo)
library(shiny)

df <- read_csv("Kovaak - Sheet1.csv")
# Sync with github, did it work?


# Setup Lists for Downstream Filtration -----------------------------------

challenges <- df$Challenge


# Column Mutations --------------------------------------------------------

summary <- df %>% group_by(Challenge) %>% summarize(counts = n())


# Define UI for application that draws a histogram
ui <- fluidPage(

# UI ----------------------------------------------------------------------


    # Application title
    titlePanel("Jay's Kovaak Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Challenge",
                        "Select a Challenge:",
                        choices = challenges)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Progress"),
           plotOutput("Distribution"),
           plotOutput("MovingAverage")
        )
    )
)


# Server ------------------------------------------------------------------


server <- function(input, output) {


# Reactive Functions ------------------------------------------------------


  # Input$Challenge ---------------------------------------------------------

    df_R <- reactive({
      
      df <- df %>% filter(Challenge == input$Challenge)
      return(df)
        
    })
    
    
  # Moving Average Plot -----------------------------------------------------
    
    # Set up the Math
    
    df_MovingAverage <- reactive({
      
      MovAvg <- df %>% filter(Challenge == input$Challenge)
      
      MovAvg$Average <- rollmean(MovAvg$Score, 5, fill = NA)
      
      MovAvg$Entry <- c(1:nrow(MovAvg))
      
      return(MovAvg)
      
    })
        

# Outputs -----------------------------------------------------------------

    output$Progress <- renderPlot({
        
        df <- df_R()
        
        ggplot(df, aes(x = Date, y = Score)) +
            geom_point()

    })
    
    output$Distribution <- renderPlot({
        
        df <- df_R()
        
        ggplot(df, aes(x = Score)) +
            geom_density()
        
    })
    
    output$MovingAverage <- renderPlot({
      
      ggplot(df_MovingAverage(), aes(x = Entry, y = Average)) +
        geom_point()
      
    })  
    
    
    
}

# Run the Application -----------------------------------------------------

shinyApp(ui = ui, server = server)


# Import Libraries and Files ----------------------------------------------


library(tidyverse)
library(zoo)
library(shiny)

df <- read_csv("Kovaak - Sheet1.csv")



# Setup Lists for Downstream Filtration -----------------------------------

challenges <- df$Challenge


# Column Mutations --------------------------------------------------------

summary <- df %>% group_by(Challenge) %>% summarize(counts = n())


# Define UI for application that draws a histogram
ui <- fluidPage(

# UI ----------------------------------------------------------------------


    # Application title
    titlePanel("Jay's Kovaak Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Challenge",
                        "Select a Challenge:",
                        choices = challenges)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Progress"),
           plotOutput("Distribution"),
           plotOutput("MovingAverage")
        )
    )
)


# Server ------------------------------------------------------------------


server <- function(input, output) {


# Reactive Functions ------------------------------------------------------


  # Input$Challenge ---------------------------------------------------------

    df_R <- reactive({
      
      df <- df %>% filter(Challenge == input$Challenge)
      return(df)
        
    })
    
    
  # Moving Average Plot -----------------------------------------------------
    
    # Set up the Math
    
    df_MovingAverage <- reactive({
      
      MovAvg <- df %>% filter(Challenge == input$Challenge)
      
      MovAvg$Average <- rollmean(MovAvg$Score, 5, fill = NA)
      
      MovAvg$Entry <- c(1:nrow(MovAvg))
      
      return(MovAvg)
      
    })
        

# Outputs -----------------------------------------------------------------

    output$Progress <- renderPlot({
        
        df <- df_R()
        
        ggplot(df, aes(x = Date, y = Score)) +
            geom_point()

    })
    
    output$Distribution <- renderPlot({
        
        df <- df_R()
        
        ggplot(df, aes(x = Score)) +
            geom_density()
        
    })
    
    output$MovingAverage <- renderPlot({
      
      ggplot(df_MovingAverage(), aes(x = Entry, y = Average)) +
        geom_point()
      
    })  
    
    
    
}

# Run the Application -----------------------------------------------------

shinyApp(ui = ui, server = server)
