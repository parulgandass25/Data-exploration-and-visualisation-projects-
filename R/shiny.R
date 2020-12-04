library(shiny)
library(datasets)
library(ggplot2) 
library(dplyr)
library(leaflet)

# SHINY UI
ui <- fluidPage( 
  
  titlePanel(h1("Assignment 2 - R programming")),
  h2("Coral Bleaching"),
  
  sidebarPanel(
    selectInput("corals", "Choose a Coral Type", 
                c("Blue Corals" = "blue corals", 
                  "Hard Corals" = "hard corals",
                  "Sea Fans" = "sea fans",
                  "Sea Pens" = "sea pens", 
                  "Soft Corals" = "soft corals"
                )
    ),
    radioButtons("smoothing", "Choose a Smoother", 
                 c("Auto" = "auto", 
                   "Linear Model(LM)" = "lm",
                   "Generalised LM" = "glm",
                   "Additive LM" = "gam", 
                   "Locally Weighted Least Squares " = "loess"
                 )
    )
    
  ),
  
  
  mainPanel( 
    h3("Select Tab"),
    tabsetPanel(type = "tab",
                tabPanel("Static Visual",h4("Site ordered by Latitude"),plotOutput("Plot1")),
                tabPanel("Interactive Visual",h4(textOutput("caption")),plotOutput("Plot2")),
                tabPanel("Location of sites",leafletOutput("Map"))
    )
    
  )
)

# SHINY SERVER
server <- function(input,output,session) {
  
  #reading data into r for plots
  data <- read.csv("assignment-02-data-formated.csv")
  
  #converting data type of value column from character to numeric
  data =data %>% mutate(new_value=  gsub("%", "", paste(data$value)))
  data=transform(data,new_value = as.numeric(new_value))  
  
  #static plot for bleaching varies over the years for each type of coral and for each site
  output$Plot1 <- renderPlot({
    ggplot(data,aes(year,new_value,color=location)) + geom_point() + facet_grid(coralType~reorder(location, latitude)) + geom_smooth() + scale_color_manual(values = c("tomato","orange3", "olivedrab", "springgreen3",  "turquoise3", "dodgerblue", "darkorchid2","maroon1"))
  })
  
  #interactive visualisation for each coral type with different smoothing
  output$caption <- renderText({ paste("Coral Bleaching for ", input$corals)})
  output$Plot2 <- renderPlot({coral=filter(data,coralType==input$corals)
  ggplot(coral,aes(year,new_value,color=location)) + geom_point() + facet_grid(input$corals~reorder(location, latitude))+ geom_smooth(method = input$smoothing) 
  })  
  
  #color palette
  pal <- colorFactor(c("tomato","orange3", "olivedrab", "springgreen3",  "turquoise3", "dodgerblue", "darkorchid2","maroon1"),
                     domain = c("site01", "site02","site03","site04", "site05","site06","site07","site08"))
  
  #leaflet map for site location
  output$Map <- renderLeaflet({ 
    leaflet(data) %>% addTiles() %>% 
      addCircleMarkers(~longitude, ~latitude, label = ~as.character(location),labelOptions = labelOptions(noHide = T),color = ~pal(location))
  })
  
}


shinyApp(ui, server)