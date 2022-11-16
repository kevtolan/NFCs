library(shiny)
library(ggplot2)
library(tidyverse)
library(rsconnect)


# Data prep
df <- readxl::read_xlsx("TolanK_Norwich_NFC_Data.xlsx")

df[is.na(df)] <- 0

df2 <- 
    df %>% 
    pivot_longer(cols = -Date, names_to = "Species", values_to = "Count") 

date_min = min(df2$Date) 
date_max = max(df2$Date) 


ui <- fluidPage(
    
    titlePanel("NFCs"),
    
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("date_range", h3("Select Date Range"),
                           min = date_min, max = date_max,
                           start = date_min, end = date_max, autoclose = T),
            checkboxGroupInput("categories", 
                               h3("Species"), 
                               #multiple = TRUE,
                               choices = list(
                                   
                                   "Hermit Thrush" = "Hermit Thrush",
                                   "Swainson's Thrush" = "Swainson's Thrush",
                                   "White-throated Sparrow" = "White-throated Sparrow",
                                   "Northern Parula" = "Northern Parula",
                                   "American Redstart" = "American Redstart"
                                   
                               ),
                               selected = "Hermit Thrush")),
        mainPanel(
            plotOutput("place_plot")
            
        )
    )
)

server <- function(input, output) {
    place_plot <- reactive({
        df2 %>% 
            # Here I filter on 
            # - The categories the user selected
            # - The date range the user selected
            filter(Species %in% input$categories,
                   Date >= input$date_range[[1]] & Date <= input$date_range[[2]]
            ) %>% 
            ggplot(
                aes(x=Date, y=Count, group=Species)) +
            geom_line(aes(color=Species)) #+ 
        # geom_rect(xmin = as.Date1(date_min, "%Y-%m-%d"), 
        #            xmax = as.Date(date_max,  "%Y-%m-%d")) + 
        #  scale_x_date(date_breaks = "1 week") 
        
        
    })
    
    output$place_plot <- renderPlot({
        place_plot()
    })
    
}


shinyApp(ui = ui, server = server)
