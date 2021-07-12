# Calling all the required libraries
require(plotly)
require(packcircles)
require(ggmap)
require(ggplot2)
require(maps)
require(shiny)
require(leaflet)
require(dplyr)


ui <- fluidPage(
  # Generating title
  titlePanel("Custom panel"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Creating widget to select year range
      selectInput("years",
                  label = "Year Range",
                  choices = c("1688-1888" , "1889-1989","1990-2020"),
                  selected = "1889-1989"),
      
      # Creating widget to select discovery type
      checkboxGroupInput("fall",
                  label = "Discovery Type",
                  choices = c("Fell", "Found"),
                  selected = "Found")
      
    ),
    
    # Generating main panel output
    mainPanel(
      h1("Analysis of the Meteorite Landings"),
      p("To update visualizations please select appropriate Year Range and Discovery Type. The 
      'Fell' discovery type refers to the meteorite falls and the 'Found' discovery type refer 
        to the meteorite finds."),
      h3("Projection of the Meteorite Landings"),
      p("The map below shows the places where meteortes were discovered and the size of the bubble 
        is proportional to the mass of the meteorite."),
      leafletOutput("mymap"),
      h3("Share of the Classes of meteorites"),
      p("The bubbles below represent the distinct classes of meteorites and the size of eah bubble 
        is proportional to the number of meteorites fouund in that particular classs.Please use the
        zoom button on top right side to focus."),
      plotlyOutput("pack_bubb"),
      h3("Trend of number of meteorites discovered over the years"),
      p("The line graph below shows the trend of the number of meteorites observed each year. Please use the
        zoom button on top right side to focus."),
      plotlyOutput("my_graph")
      
    ) 
  )
)


# Generating server function
server <- function(input, output, session) {
  
  
  # Reading the data file
  meteor <- read.csv("Clean_meteor_data.csv")
  meteor$Year <- as.numeric(meteor$Year)
 
  # Filtering the data based on the input of the widget and checkbox 
  filter <- reactive({
    validate(
      need(input$fall != "", "Please select a 'Discovery Type'")
    )
    if(input$years == "1688-1888"){
      subset(meteor, meteor$fall == input$fall & meteor$Year >= 1688 & meteor$Year <= 1888)
    } else if(input$years == "1889-1989"){
      subset(meteor, meteor$fall == input$fall & meteor$Year >= 1889 & meteor$Year <= 1989)
    } else if(input$years == "1990-2020"){
      subset(meteor, meteor$fall == input$fall & meteor$Year >= 1990 & meteor$Year <= 2020)
    }
  })
  
  
  
  # Generating leaflet plot for mymap
  # Referred from:https://rstudio.github.io/leaflet/shiny.html
  #               https://www.r-graph-gallery.com/19-map-leafletr.html
  #               https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html
  
  output$mymap <- renderLeaflet({ 
    
    # Creating a color palette
    pal <- colorFactor(c("purple","red"), levels = c("Fell", "Found"))
    
    
    # Generating leaflet plot with the filter data
    leaflet(data=filter()) %>%
      addTiles() %>%
      addCircles(~reclong,~reclat, 
                 radius = ~sqrt(mass) * 100,    # relating size of bubble to mass 
                 color = ~pal(fall),            # using palette for colors
                 fillColor = ~pal(fall), fillOpacity = 0.7, 
                 popup = ~paste("Mass: ", mass,"gm", "<br>", "Class: ", recclass),  # info on click
                 stroke = FALSE) %>%
      # adding legend
      addLegend(pal=pal, values=~fall, opacity=0.7, title = "Discovery Type", position = "topright")
    
  })
  
  
  # Generating plot for pack_bubb 
  # Referred from: https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html 
  output$pack_bubb <- renderPlotly({
    
    filter_data <- filter()
    
    # Generating new data with count of number of meteorites in each class
    class_data <- aggregate(id ~ recclass, filter_data, function(x) length(unique(x)))
    
    # Getting top 50 popular classes
    max_class_data <- class_data %>% top_n(50)
    
    # Generating x,y coordinate and radius for the bubbles
    coordinate <- circleProgressiveLayout(max_class_data$id, sizetype='area')
    
    # Adding the coordinate information to the initial data frame
    max_class_data <- cbind(max_class_data, coordinate)
    
    
    # Going from one center, a radius to the coordinates of a circle drawn by a multitude of straight lines.
    circle_generate <- circleLayoutVertices(coordinate, npoints=70)
    
   
    # Make the plot
    bubb_chart <- ggplot() + 
      
      # Making the bubbles
      geom_polygon(data=circle_generate, mapping=aes(x=x, y=y,group = id, fill=id),
                   colour = "black", alpha = 0.6,size=.5) +
      scale_fill_distiller(palette = "BuPu", direction = 1 ) +
      
      
      # Adding text in each bubble and controlling the size
      geom_text(data = max_class_data, aes(x=x, y=y, size=id, label=recclass,
                                           text = paste("Class: ", recclass,
                                                        "\nNumber of Meteorites: ", id))) +
      
      scale_size_continuous(range = c(1,4)) +
     
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()+
      ggtitle("Meteorite class frequency")
      
    # Using plotly to add tooltip  
    bubb_chart <- ggplotly(bubb_chart,  tooltip = "text")
    bubb_chart
      
  })
  
  
  # Generating plot for pack_bubb 
  output$my_graph <- renderPlotly({
    
    filter_data <- filter()
    
    # Creating data containing count of meteorites wrt year and class
    year_count <- aggregate(id ~ Year+fall, filter_data, function(x) length(unique(x)))
    
    # Generating the plot
    line_graph <- ggplot(year_count) + 
      aes(x = Year, y = id,  group=fall, color=fall) + 
      geom_line(size=1)+
      scale_color_manual(input$fall,values = c("purple","red"), name="Discovery Type") + 
      geom_point(size=3, aes(text = paste("Number of Meteorites: ", id))) +
      ggtitle("Meteorite landing trend") + labs(y="Number of meteorite landings", x = "Year")
    
    # Using plotly to generate tooltip
    line_graph <- ggplotly(line_graph, tooltip = "text") 
    line_graph
      
  })

}

# Running the app
shinyApp(ui, server)

