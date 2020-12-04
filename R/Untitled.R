library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

# The dataset is provided in the gapminder library
data <- read.csv("final_data.csv")
data$month= (str_match(data$Date, '/(.*?)/')[,2])
data$Year= (str_match(data$Date, '/.*/(.*)')[,2])




####### tab 1
data4 <- data %>% group_by(CouncilArea) %>%
  summarise(p_mean=(mean(Price)))




fig3 <- plot_ly(data, y = ~Price, color = ~CouncilArea, type = "box")

data1 <- data %>% group_by(month, Year) %>%
  summarise(p_mean=(mean(Price)), sales = n())

ggplot(data=data1,aes(x=month,y=sales,fill=Year))+
  geom_bar(stat="identity")+coord_polar()+
  scale_fill_brewer(palette="Greens")+xlab("")+ylab("")

######### tab 2
data2 <- data %>% group_by(Quarter, Type) %>%
  summarise(p_mean=(mean(Price)), sales = n())
plot_ly(data2, x = ~Quarter, y = ~p_mean, type = 'scatter', mode = 'markers', size = ~sales, color = ~Type, colors = 'Paired',
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter'),
               hoverinfo = 'text',
               text = ~paste('Period:', Quarter, '<br>Type:', Type,'<br>Average Price:', p_mean,'<br>Number of Sales:', sales)) %>% layout(title = 'Price and sales trend by type',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)

data3 <- data %>% group_by(CouncilArea) %>%
  summarise(Avg.Price=(mean(Price)), incidence = mean(Rate.per.100000.population))

plot_ly(data3) %>% add_trace(x = ~CouncilArea, y = ~Avg.Price, type = 'bar', name = 'Average price',
                         marker = list(color = '#C9EFF9'),
                         hoverinfo = "text",
                         text = ~paste('Avg. Price: ', Avg.Price)) %>% add_trace(x = ~CouncilArea, y = ~incidence, type = 'scatter', mode = 'lines', name = 'Criminal incidences', yaxis = 'y2',
                         line = list(color = '#45171D'),
                         hoverinfo = "text",
                         text = ~paste('Criminal incidences:', incidence)) %>% layout(title = 'Average price and incidence rate by council area',
                      xaxis = list(title = "Council Area"),
                      yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE),
                      yaxis2 = list(side = 'right', overlaying = "y", title = 'incidence rate per 100000 population', showgrid = FALSE, zeroline = FALSE))
###############################

mapStates <- maps::map("world.cities", fill = TRUE, plot = FALSE)
spliteNames <- strsplit(mapStates$names, ":")
firstPartNames <- lapply(spliteNames, function(x) x[1])