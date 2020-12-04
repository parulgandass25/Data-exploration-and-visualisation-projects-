library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(shiny)
library(leaflet)
library(shinythemes)
library(RColorBrewer)
library(shinydashboard)

####### UI FUNCTION
ui <- navbarPage("REAL ESTATE MARKET ANALYSIS",theme = shinytheme("darkly"),
        #######################################tab 1 -> About#########################################
        tabPanel("About",
                 h1("Melbourne Housing"),
                 h3("Melbourne is considered as the number one city according to the global liveability surveys because of its thriving art and welcoming culture. Melbourne is beloved by young and independent people for the buzz in the city, bustling cosmopolitan atmosphere and everything at doorsteps. Melbourne offers a unique network of inner-city laneways with one of the most extensive tram systems, suburbs on boundaries with waterfront appeal. This makes the Real Estate Business in Melbourne to be one of the most busy and successful Industry."),
                 h3("The Real Estate business is growing, the customers are no doubt struggling to find and choose a perfect house according to their budget and needs. Housing prices are fluctuating over past few years. There are numerous agents in the market trying to provide best deals with a percentage of brokerage. Some well-known brokers have set foots well in the market. This application shows some visualisation on melbourne housing data publicly available online. this application will help the customers to get a better understanding of the market and housing prices in different areas, so that customer can make a smart decision based on their needs and budget.")
                 ),
        
        #######################################tab 2 -> Overview of data ################################
          tabPanel("Overview",
                   
                   #leafle map output
                   leafletOutput("mymap",width="1900px",height = "1400px"),
                   
                   #filter panel
                   absolutePanel(id = "controls",style="opacity: 0.80", class = "panel panel-default", fixed = FALSE,
                                 draggable = TRUE, top = 275, left = 20, right = 20, bottom = "auto",
                                 width = 350, height = "auto",
                                 
                                 #panel tittle
                                 h2(textOutput("caption")),
                                 
                                 #filter of Local Government area
                                 selectInput("variable", "Choose a Council Area", 
                                             c("All" = "All",
                                               'Banyule' = 'Banyule',
                                               'Bayside' = 'Bayside',
                                               'Boroondara' = 'Boroondara',
                                               'Brimbank' = 'Brimbank',
                                               'Cardinia' = 'Cardinia',
                                               'Casey' = 'Casey',
                                               'Darebin' = 'Darebin',
                                               'Frankston' = 'Frankston',
                                               'Glen Eira' = 'Glen Eira',
                                               'Greater Dandenong' = 'Greater Dandenong',
                                               'Hobsons Bay' = 'Hobsons Bay',
                                               'Hume' = 'Hume',
                                               'Kingston' = 'Kingston',
                                               'Knox' = 'Knox',
                                               'Macedon Ranges' = 'Macedon Ranges',
                                               'Manningham' = 'Manningham',
                                               'Maribyrnong' = 'Maribyrnong',
                                               'Maroondah' = 'Maroondah',
                                               'Melbourne' = 'Melbourne',
                                               'Melton' = 'Melton',
                                               'Mitchell' = 'Mitchell',
                                               'Monash' = 'Monash',
                                               'Moonee Valley' = 'Moonee Valley',
                                               'Moorabool' = 'Moorabool',
                                               'Moreland' = 'Moreland',
                                               'Nillumbik' = 'Nillumbik',
                                               'Port Phillip' = 'Port Phillip',
                                               'Stonnington' = 'Stonnington',
                                               'Whitehorse' = 'Whitehorse',
                                               'Whittlesea' = 'Whittlesea',
                                               'Wyndham' = 'Wyndham',
                                               'Yarra' = 'Yarra',
                                               'Yarra Ranges' = 'Yarra Ranges'
                                             )
                                 ),
                                 p("Untick Show Clusters to have detailed view of individual property.")
                                 ,
                                 
                                 #filter for clustering
                                 checkboxInput("clusters", "Show clusters", TRUE),
                                 
                                 #filter for type of property
                                 checkboxGroupInput("type" , "Select Property Type :",           # creating checkbox inouts
                                                    c("House" = "h",
                                                      "Town House" = "t",
                                                      "Unit" = "u"),
                                                    selected = c("h","t","u" )),
                                 
                                 #filter for price range 
                                 sliderInput("price","Select Price Range:",           # slidebar input
                                             min = 85000.0, max = 11200000.0, value = c(85000.0,11200000.0)),
                                 
                                 #filter for distance from CBD
                                 sliderInput("distance", "Select Distance from CBD", 
                                             min = 0.0, max = 48.1, value = c(0.0,48.1)),
                                 
                                 #message for drag
                                 p("  *You can drag this panel anywhere, for clear view!*")
        ),
        
        #panel for user guide
        absolutePanel(id = "Guide",style="opacity: 0.80", class = "panel panel-default", fixed = FALSE,
                      draggable = TRUE, top = 85, left = "auto", right = 20, bottom = "auto",
                      width = 350, height = "auto",
                      h3(" User Guide"),
                        p("-Use filter pannel for advanced search"),
                        p("-Select any Local Government area for narrow view."),
                        p("-Click on clusters to zoom into it."),
                        p("-Click on circle marker for information about the property."),
                        p("-Property type house, townhouse and unit are in colour maroon, orange and dark blue respectively")
        )
    ),
    
    ##############################################tab 3 ######################################################
    tabPanel("Detailed analysis",
             
             #row 1
             fluidRow(column(width = 10,
                             #plotly output of avg. price by council area
                             box(tags$h4("Average Property Price and Rate of incidence report by Local Government Area"),                #Box to show line plot for HandGun and LongGun sales
                                 width = NULL,solidHeader = TRUE,
                                 plotlyOutput("fig1",height = 500)
                             )),
                      
                      #User guide box
                      column(width = 2,
                             box(tags$h4("User Guide"),                #Box to show line plot for HandGun and LongGun sales
                                 width = NULL,solidHeader = TRUE,
                                 p("-Select any Council area for analysis by suburb in that Local Government area."),
                                 p("-Use filter pannel for advanced analysis."),
                                 p("-Hover over the plots to see more details."),
                                 p("-Boxplot for each Suburb shows the distribution of property price in that area."),
                                 p("-Size of the bubble in plot by by quarter shows the number of sales by quarter."),
                                 p("-Numbers 1 to 12 in polar bar (circular) plot corresponds to each month."),
                                 p("-Enjoy playing with filters :-)")
                             ))
             ),
             
            #row 2
            fluidRow(column(width = 8,
                            #plotly output og price ananlysis  by each suburb
                            box(width = NULL, solidHeader = TRUE,tags$h4("Property Price Analysis by Suburb"),       #Main Leaflet 'mymap'
                                plotlyOutput("fig2",height = 500)
                            )
            ),
            
            #filter panel
            column(width = 4,
                   
                     #filter for council area
                     selectInput("var", h3("Choose a Council Area"), 
                                 c("All" = "All",
                                   'Banyule' = 'Banyule',
                                   'Bayside' = 'Bayside',
                                   'Boroondara' = 'Boroondara',
                                   'Brimbank' = 'Brimbank',
                                   'Cardinia' = 'Cardinia',
                                   'Casey' = 'Casey',
                                   'Darebin' = 'Darebin',
                                   'Frankston' = 'Frankston',
                                   'Glen Eira' = 'Glen Eira',
                                   'Greater Dandenong' = 'Greater Dandenong',
                                   'Hobsons Bay' = 'Hobsons Bay',
                                   'Hume' = 'Hume',
                                   'Kingston' = 'Kingston',
                                   'Knox' = 'Knox',
                                   'Macedon Ranges' = 'Macedon Ranges',
                                   'Manningham' = 'Manningham',
                                   'Maribyrnong' = 'Maribyrnong',
                                   'Maroondah' = 'Maroondah',
                                   'Melbourne' = 'Melbourne',
                                   'Melton' = 'Melton',
                                   'Mitchell' = 'Mitchell',
                                   'Monash' = 'Monash',
                                   'Moonee Valley' = 'Moonee Valley',
                                   'Moorabool' = 'Moorabool',
                                   'Moreland' = 'Moreland',
                                   'Nillumbik' = 'Nillumbik',
                                   'Port Phillip' = 'Port Phillip',
                                   'Stonnington' = 'Stonnington',
                                   'Whitehorse' = 'Whitehorse',
                                   'Whittlesea' = 'Whittlesea',
                                   'Wyndham' = 'Wyndham',
                                   'Yarra' = 'Yarra',
                                   'Yarra Ranges' = 'Yarra Ranges'
                                 )
                   ),
                   
                       #slider to select disttance range 
                       sliderInput("dist", h3("Select Distance from CBD"), 
                                   min = 0.0, max = 48.1, value = c(0.0,48.1)),
                  
                   #
                   selectInput("room" , h3("Select number of Bedrooms :"),           # creating checkbox inouts
                               c("All",1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16)),
                   
                   #box with check butttons to select to property types
                   checkboxGroupInput("t" , "Select Property Type :",           # creating checkbox inouts
                                      c("House" = "h",
                                        "Town House" = "t",
                                        "Unit" = "u"),
                                      selected = c("h","t","u" ))
                   
                   )
            ),
            
            #row 3
            fluidRow(column(width = 7,
                            #plotly output of avg price by quarter of time according to selected filters 
                            box(tags$h4("Average Price and sales trend by Quarter"),                #Box to show bubble plot for avg. price and sales in that period
                                width = NULL,solidHeader = TRUE,
                                plotlyOutput("fig3",height = 500)
                            )),
                     column(width = 5,
                            box(tags$h4("Number of sales each month every year"),                #Box to show polar bar chart to shoe sales by month each year
                                width = NULL,solidHeader = TRUE,
                                plotOutput("fig4",height = 500)
                            ))
                     
            )
            
    )
  
)

##### SERVER FUNCTION
server <- function(input, output) {
  
  #read data from the fiule and extract useful information from existing columns.
  data <- read.csv("final_data.csv")
  data$month= (str_match(data$Date, '/(.*?)/')[,2])
  data$Year= (str_match(data$Date, '/.*/(.*)')[,2])
  
  #Plot 1: bar plot and line plott combined for avg. price and incident reported rate by Council area respectively. 
  output$fig1 <- renderPlotly({
    
    #if selected all number of rooms, filter data only based on other selected filters  
    if(input$room == "All"){
      data1 <- filter(data,Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))
    }
    #filter data based on selected number of rooms as well as other filters
    else{
      data1 <- filter(data, Rooms == input$room & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))
    }
    
    #group data by council area for average price by area
    data1 <- data1  %>% group_by(CouncilArea) %>%
      summarise(Avg.Price=(mean(Price)), incidence = mean(Rate.per.100000.population))
   
    # bar and line plot by council area for avg price and incidence reported 
    plot_ly(data1) %>% add_trace(x = ~CouncilArea, y = ~Avg.Price, type = 'bar', name = 'Average price',
                                 marker = list(color = '#C9EFF9'),
                                 hoverinfo = "text",
                                 text = ~paste('Avg. Price: ', Avg.Price)) %>% add_trace(x = ~CouncilArea, y = ~incidence, type = 'scatter', mode = 'lines', name = 'Criminal incidences', yaxis = 'y2',
                                 line = list(color = '#45171D'),
                                 hoverinfo = "text",
                                 text = ~paste('Criminal incidences:', incidence)) %>% layout(title = 'Average price and incidence rate by council area',
                                 xaxis = list(title = "Council Area"),
                                 yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE),
                                 yaxis2 = list(side = 'right', overlaying = "y", title = 'incidence rate per 100000 population', showgrid = FALSE, zeroline = FALSE))
  })
  
  
  #Plot 2: Box plot for price distrubution in each suburb
  output$fig2 <- renderPlotly({
    
    if(input$var == "All"){
      if (input$room == 'All') {
        #if selected all council area and all number of rooms, no filter on council area and rooms column.
        data2 <- filter(data, Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected all council area and particular number of rooms, no filter on council area column, filter data only on other column.
        data2 <- filter(data, Rooms == input$room & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      
    }
    else{
      #if selected particular council area and all number of rooms, no filter on rooms column, filter data for that council area and other columns
      if (input$room == 'All') {
        data2 <- filter(data,CouncilArea==input$var & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected particular council area and number of rooms, filter on council area and rooms column based on selected value along with others
        data2 <- filter(data,CouncilArea==input$var & Rooms == input$room & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
    }
    
    #box plot based on filtered data 
    plot_ly(data2, y = ~Price, color = ~Suburb, type = "box")

  })
  
  #Plot 3: Bubble plot
  output$fig3 <- renderPlotly({
    if(input$var == "All"){
      if (input$room == 'All') {
        #if selected all council area and all number of rooms, no filter on council area and rooms column.
        data3 <- filter(data, (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected all council area and particular number of rooms, no filter on council area column, filter data only on other column.
        data3 <- filter(data, Rooms == input$room & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      
    }
    else{
      if (input$room == 'All') {
        #if selected particular council area and all number of rooms, no filter on rooms column, filter data for that council area and other columns
        data3 <- filter(data,CouncilArea==input$var  & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected particular council area and number of rooms, filter on council area and rooms column based on selected value along with others
        data3 <- filter(data,CouncilArea==input$var & Rooms == input$room & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }   
    }
    
    #Group by data based on quarter and type of property with avg, aggregate on price and count of number of sales by counting rows.
    data3 <- data3 %>% group_by(Quarter, Type) %>%
      summarise(p_mean=(mean(Price)), sales = n())
    
    #bubble plot with avg price on x-axis and quarter by year on y-axis, and size of the bubble proportional to the number of sales in that time span, different colour of bubble for different type of property.
    plot_ly(data3, x = ~Quarter, y = ~p_mean, type = 'scatter', mode = 'markers', size = ~sales, color = ~Type, colors = 'Paired',
            sizes = c(10, 50),
            marker = list(opacity = 0.5, sizemode = 'diameter'),
            hoverinfo = 'text',
            text = ~paste('Period:', Quarter, '<br>Type:', Type,'<br>Average Price:', p_mean,'<br>Number of Sales:', sales))%>% layout(title = 'Price and sales trend by type',
                                                                                                                                       xaxis = list(showgrid = FALSE),
                                                                                                                                       yaxis = list(showgrid = FALSE),
                                                                                                                                       showlegend = FALSE)
  })
  
  #Plot 4:Polar bar chart
  output$fig4 <- renderPlot({
    
    if(input$var == "All"){
      if (input$room == 'All') {
        #if selected all council area and all number of rooms, no filter on council area and rooms column.
        data4 <- filter(data, Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected all council area and particular number of rooms, no filter on council area column, filter data only on other column.
        data4 <- filter(data, Rooms == input$room & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      
    }
    else{
      #if selected particular council area and all number of rooms, no filter on rooms column, filter data for that council area and other columns
      if (input$room == 'All') {
        data4 <- filter(data,CouncilArea==input$var & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }
      else{
        #if selected particular council area and number of rooms, filter on council area and rooms column based on selected value along with others
        data4 <- filter(data,CouncilArea==input$var & Rooms == input$room & Type %in% input$t & (Distance > input$dist[1] & Distance < input$dist[2]))      
      }   
    }
    
    #Group by data based on month and year of sale of property with aggregate count of number of sales by counting rows.
    data4 <- data4 %>% group_by(month, Year) %>%
      summarise(p_mean=(mean(Price)), sales = n())
    
    #plot with number 1-12 correspons to each month and colour for each year and area proportional to number of sales..
    ggplot(data=data4,aes(x=month,y=sales,fill=Year))+
      geom_bar(stat="identity")+coord_polar()+
      scale_fill_brewer(palette="Greens")+xlab("")+ylab("")+ theme_minimal()
    
  })
  
  #color pallet for different type of property
  colour =c("maroon","orange", "darkblue" )  # define the colours
  pal =colorFactor(palette = colour ,domain = data$Type)
  
  #text output for filter panel
  output$caption <- renderText( {paste("Listings in ", input$variable)})
  
  #Plot 5: leaflet map plot for each property in the data 
  output$mymap <- renderLeaflet({ 
    
    #if all Local government area is seleced, filter data based on other filters except area
    if(input$variable == "All")   #for all listings plot leaflet
    {
      area=filter(data,Type %in% input$type & (Price > input$price[1] & Price < input$price[2]) & (Distance > input$distance[1] & Distance < input$distance[2]))
    }
    
    #if a particular Local government area is seleced, filter data for that Local governemt area and other filters selected.
    else{
      area=filter(data,CouncilArea==input$variable & Type %in% input$type & (Price > input$price[1] & Price < input$price[2]) & (Distance > input$distance[1] & Distance < input$distance[2]))
    }
    
    # leaflet based on choices made by the user
    #draw leaflet
      if(input$clusters == TRUE)#Show clusters
      {
        #if selected cluster true, make clusters
        leaflet(area) %>% addTiles() %>% 
          addCircleMarkers(~Longtitude, ~Lattitude, radius = 8, color= ~pal(Type),popup = paste(h5("Address:"), area$Address, "<br>",h5("Type :"), area$Type, "<br>",h5("Number of Rooms:"), area$Rooms, "<br>",                                                                                            
          h5("Price:"), area$Price,"<br>",h5("Distance from CBD"), area$Distance, "<br>",h5("Local Goveernment Area"), area$CouncilArea  ) , 
          clusterOptions = markerClusterOptions()) %>%   
          addLegend("topleft",pal = pal, values = ~Type , title = "Property Type")
      }
      
      else
      {
        #if selected cluster to be false (untick cluster), plot each property individuallyu
        leaflet(area) %>% addTiles() %>% 
          addCircleMarkers(~Longtitude, ~Lattitude, radius = 8, color= ~pal(Type),popup = paste(h5("Address:"), area$Address, "<br>",h5("Type :"), area$Type, "<br>",h5("Number of Rooms:"), area$Rooms, "<br>",                                                                                            
          h5("Price:"), area$Price,"<br>",h5("Distance from CBD"), area$Distance, "<br>",h5("Local Goveernment Area"), area$CouncilArea  ))  %>%   
          addLegend("topleft",pal = pal, values = ~Type , title = "Property Type")
      }
    
  })
  
}

#call app functtion
shinyApp(ui, server)
