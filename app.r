all = ls()
rm(all)

# loading all the required packages
library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(wordcloud)

# Reading various input files 

# for keywords
cc = read.csv('cheap_count.csv')
ec = read.csv('expensive_count.csv')

# for amenities
am = read.csv('amenities.csv')
filtered_am = am[order(am$freq),]

# for leafletMap
clean = read.csv('cleaned.csv')
places = read.csv('places.csv')

# creating temporary data frames for plotting the  closest 15 airbnbs
colnames(places) = c('place', 'lat', 'lon')

temp_lat = places$lat[places$place == "statue_of_liberty"]
temp_lon = places$lon[places$place == "statue_of_liberty"]
clean$distance_sol = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "central_park"]
temp_lon = places$lon[places$place == "central_park"]
clean$distance_cp = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "empire_state_building"]
temp_lon = places$lon[places$place == "empire_state_building"]
clean$distance_esb = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "times_square"]
temp_lon = places$lon[places$place == "times_square"]
clean$distance_ts = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "museum_of_modern_art"]
temp_lon = places$lon[places$place == "museum_of_modern_art"]
clean$distance_mma = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "brooklyn_bridge"]
temp_lon = places$lon[places$place == "brooklyn_bridge"]
clean$distance_bb = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "rockefeller_center"]
temp_lon = places$lon[places$place == "rockefeller_center"]
clean$distance_rc = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp_lat = places$lat[places$place == "high_line"]
temp_lon = places$lon[places$place == "high_line"]
clean$distance_hl = abs(abs(clean$longitude) - abs(temp_lon)) + abs(abs(clean$latitude) - abs(temp_lat))

temp1 = clean[order(clean$distance_sol),]
temp1 = temp1[1:15,]

temp2 = clean[order(clean$distance_cp),]
temp2 = temp2[1:15,]

temp3 = clean[order(clean$distance_esb),]
temp3 = temp3[1:15,]

temp4 = clean[order(clean$distance_ts),]
temp4 = temp4[1:15,]

temp5 = clean[order(clean$distance_mma),]
temp5 = temp5[1:15,]

temp6 = clean[order(clean$distance_bb),]
temp6 = temp6[1:15,]

temp7 = clean[order(clean$distance_rc),]
temp7 = temp7[1:15,]

temp8 = clean[order(clean$distance_hl),]
temp8 = temp8[1:15,]

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # using the cyborg theme
  theme = shinytheme("united"),
  
  # App title ----
  titlePanel("Airbnb Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
  sidebarPanel(
    tabsetPanel(id = "viz",
                tabPanel("Keywords and Amenities", id = "1", value=1),
                tabPanel("Hot spots", id = "2", value=2), 
                tabPanel("Overview", id = "3", value=3)
    ),
    conditionalPanel(
      condition = "input.viz==1",
      radioButtons("cost", "Select your preference for keywords",
                   c("Keywords for Cheap listings / Common Amenities" = "cheap",
                     "Keywords for Expensive listings / Rare Amenities" = "expensive"))
    ),

    
    conditionalPanel(
      condition = "input.viz==1",
      checkboxInput("amenitiesType", label = "Show Keywords instead of Amenities", value = TRUE)
    ),
    
    conditionalPanel(
      condition = "input.viz==2",
      radioButtons("place", "Select your prefered location",
                   c("Statue of Liberty" = "distance_sol",
                     "Central Park" = "distance_cp",
                     "Empire State Building" = "distance_esb",
                     "Times Square" = "distance_ts",
                     "Museum of Modern Art" = "distance_mma",
                     "Brooklyn Bridge" = "distance_bb",
                     "Rockefeller Center" = "distance_rc",
                     "High Line" = "distance_hl"))
    ),
    
    conditionalPanel(
      condition = "input.viz==3",
      sliderInput("priceRange", label = h3("Slider Range"), min = 0, 
                  max = 1000, value = c(50, 350))
      
    )
  ),
    
  # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", 
                         conditionalPanel(condition="input.viz==1",
                                          fluidRow(plotOutput("Word_Cloud", height = 700))),
                         conditionalPanel(condition="input.viz==2",
                                          fluidRow(leafletOutput("Leaflet_Map", height = 700))),
                         conditionalPanel(condition="input.viz==3",
                                          fluidRow(plotOutput("Overview1"), plotOutput("Overview2")))
                         ),
                tabPanel("Explanation", 
                         conditionalPanel(condition = "input.viz==1",
                            verbatimTextOutput("Word_Cloud_Exp")),
                         conditionalPanel(condition = "input.viz==2",
                            verbatimTextOutput("Leaflet_Map_Exp")),
                         conditionalPanel(condition = "input.viz==3",
                            verbatimTextOutput("Overview_Exp")),
                         )
  
  )
))
)


server <- function(input, output) {
  # Keyords and amenities (PLOT)
  output$Word_Cloud = reactivePlot(function(){
    if (input$amenitiesType == TRUE)
    {
      if(input$cost == 'cheap')
      {
        wc = wordcloud(words = cc$word, freq = cc$freq, min.freq = 1,
                  max.words=75, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        print(wc)
        
      }
      else if(input$cost == 'expensive')
      {
        wc = wordcloud(words = ec$word, freq = ec$freq, min.freq = 1,
                  max.words=75, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        print(wc)

      }
    }
    else
    {
      if(input$cost == 'cheap')
      {
        wc = barplot(am[1:40,]$freq, las = 2, names.arg = am[1:40,]$amenities,
                     col ="lightblue", main ="Most frequent words",
                     ylab = "Word frequencies")
        
        print(wc)

      }
      else if(input$cost == 'expensive')
      {
        
        wc = barplot(filtered_am[1:40,]$freq, las = 2, names.arg = filtered_am[1:40,]$amenities,
                     col ="lightblue", main ="Most frequent words")

        print(wc)

      }
    }
      
  })
  
  
# Leaflet (PLOT)
output$Leaflet_Map = renderLeaflet({
  
  color = c('orange', 'green', 'blue')

  if(input$place == 'distance_sol'){
    leaflet(temp1) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp1$price, "<br>",
        "name -", temp1$name, "<br>",
        "id -", temp1$id, "<br>",
        "room type -", temp1$room_type
      ),
      data = temp1, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
      ) %>%
            addProviderTiles(providers$CartoDB.Positron)
  }

  else if(input$place == 'distance_cp'){
    leaflet(temp2) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp2$price, "<br>",
        "name -", temp2$name, "<br>",
        "id -", temp2$id, "<br>",
        "room type -", temp2$room_type
      ),
      data = temp2, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_esb'){
    leaflet(temp3) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp3$price, "<br>",
        "name -", temp3$name, "<br>",
        "id -", temp3$id, "<br>",
        "room type -", temp3$room_type
      ),
      data = temp3, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_ts'){
    leaflet(temp4) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp4$price, "<br>",
        "name -", temp4$name, "<br>",
        "id -", temp4$id, "<br>",
        "room type -", temp4$room_type
      ),
      data = temp4, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_mma'){
    leaflet(temp5) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp5$price, "<br>",
        "name -", temp5$name, "<br>",
        "id -", temp5$id, "<br>",
        "room type -", temp5$room_type
      ),
      data = temp5, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_bb'){
    leaflet(temp6) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp6$price, "<br>",
        "name -", temp6$name, "<br>",
        "id -", temp6$id, "<br>",
        "room type -", temp6$room_type
      ),
      data = temp6, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_rc'){
    leaflet(temp7) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp7$price, "<br>",
        "name -", temp7$name, "<br>",
        "id -", temp7$id, "<br>",
        "room type -", temp7$room_type
      ),
      data = temp7, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
  else if(input$place == 'distance_hl'){
    leaflet(temp8) %>% addTiles() %>% addCircleMarkers(
      popup = paste(
        "price -", temp8$price, "<br>",
        "name -", temp8$name, "<br>",
        "id -", temp8$id, "<br>",
        "room type -", temp8$room_type
      ),
      data = temp8, lng = ~longitude, lat = ~latitude, label = ~name, labelOptions(noHide = T, direction = "top"), radius = 5, fill = ~(color), color = ~(color), fillOpacity = 1
      
    ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
})
  
# density plots (PLOT)
output$Overview1 = renderPlot(
  {
    inp = input$priceRange
    tempo1 = clean[clean$price >= as.numeric(inp[1]), ]
    tempo1 = tempo1[tempo1$price <= as.numeric(inp[2]),]
    dp = ggplot(tempo1, aes(x = price)) +
      geom_density(color = 'black', fill = 'red') + facet_wrap(~neighbourhood_group_cleansed)
    dp
  }
)

# Pie chart (PLOT)
output$Overview2 = renderPlot(
  {
    inp = input$priceRange
    tempo2 = clean[clean$price >= as.numeric(inp[1]), ]
    tempo2 = tempo2[tempo2$price <= as.numeric(inp[2]),]
    tempo2 = as.data.frame(table(tempo2$room_type))
    piee <- ggplot(tempo2, aes(x="", y=Freq, fill=Var1))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0)
    piee
  }
)

# Explanation for Keywords and Amenities
output$Word_Cloud_Exp <- reactiveText(function()
  {
  if (input$amenitiesType == TRUE)
  {
    if(input$cost == 'cheap')
    {
      print('The keywords used in cheap listings are words used to describe the property. A few examples being Cozy, spacious, beautiful, clean etc.')
    }
    else if(input$cost == 'expensive')
    {
      print('The keywords used in expensive listings on the other hand also include words that describe the location of the AIRBNB indicating that location does affect the cost of AIRBNBs.')
    }
  }
  else
  {
    if(input$cost == 'cheap')
    {
      print('The common amenities provided are Wi-Fi, dryer, heating, TV, Kitchen etc.')
    }
    else if(input$cost == 'expensive')
    {
      print('The costlier and rare amenities are Fruits, Snacks, Fridge, Bar, Bluetooth Speaker, Bathrobes etc which help in developing a feeling of living in a post hotel.')
    } 
  }
})

# Leaflet Explanation
output$Leaflet_Map_Exp <- reactiveText(function()
{
  print("Selecting the place of your choice enables the application to show the closest Airbnbs to that spot. Also, hovering over a point give you the name of that listing. Click on it to get a detailed description of the place.")
})

# Overview Explanation
output$Overview_Exp <- reactiveText(function()
{
  print("Selecting a price range displays a density plot for airbnbs in that price range. Also, the pie chart helps understand the ratio of room types within that price range.")
})


}


# Running the application 
shinyApp(ui = ui, server = server)
