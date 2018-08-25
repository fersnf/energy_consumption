#libraries ----
library("shiny")
library("shinythemes")
library("readr")
library("ggplot2")
library("ggthemes")
library("lubridate")
require("gridExtra")
require("rsconnect")

#changing the theme ----
theme_black = function(base_size = 12, base_family = "") {

  theme_grey(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(10, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#303030"),
      legend.key = element_rect(color = "white",  fill = "#303030"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "#303030", color  =  NA),
      panel.border = element_rect(fill = NA, color = "grey35"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.9, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white"),
      strip.text.y = element_text(size = base_size*0.8, color = "white", angle = -90),
      # Specify plot options
      plot.background = element_rect(color = "#303030", fill = "#303030"),
      plot.title = element_text(size = base_size*1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}

#user interface ----
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  tags$head(
    tags$style(
      HTML("
        @import url('//fonts.googleapis.com/css?family=Great+Vibes');
        
        h1 {
        font-family: 'Great Vibes', cursive;
        font-style: normal;
        text-align: center;
        color: #ff0065}
        "
      )
      )), 
  
  #app's title
  headerPanel("Energy Consumption"),
    
  #sidebar ----
  sidebarPanel(
    radioButtons(
      inputId = "timeframe",
      label = "Time frame:",
      choices = list(
        "Year" = "year",
        "Season" = "season",
        "Month" = "month",
        "Week" = "week"
      ),
      selected = "week",
      inline = FALSE,
      width = "100%"
    ),
    
    sliderInput(
      inputId = "years",
      label = "Years to plot",
      min = 2007,
      max = 2010,
      value = c(2007, 2010)
    ),
    
    checkboxGroupInput(
      inputId = "energysource",
      label = "Energy sources:",
      choices = list(
        "Global Active Power" = "GlobalActivePower",
        "Kitchen" = "Kitchen",
        "Laundry" = "Laundry",
        "Water Heater & Air Conditioning" = "WaterHeater_AirCon",
        "Missing Energy" = "MissingEnergy"
      ),
      selected = c(
        "Global Active Power" = "GlobalActivePower",
        "Kitchen" = "Kitchen",
        "Laundry" = "Laundry",
        "Water Heater & Air Conditioning" = "WaterHeater_AirCon",
        "Missing Energy" = "MissingEnergy"
      )
    )
  ),
  
  #main panel ----
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot1")),
      tabPanel("Dataset", tableOutput("table")),
      tabPanel("Info", htmlOutput("text"))),
      hr()
    )
)
  
#server ----
server <- function(input, output) {
  
  #loading dataset ----
  #opening all the files at once
  energy <- reactive(read.csv(
    paste0("data/energy_shiny_grouped_", input$timeframe, ".csv"),
    col.names = c(
      "GlobalActivePower",
      "WaterHeater_AirCon",
      "Laundry",
      "Kitchen",
      "MissingEnergy",
      "Date"
    )
  ))
  
  #plot ----  
  output$plot1 <- renderPlot({
    
    validate(need(!is.null(input$energysource), 
                  "Please select at least one energy source"))
    
    starting_date <- as.POSIXct(paste(input$years[1],"01","01", sep = "-"))
    end_date <- as.POSIXct(paste(input$years[2]+1,"01","01", sep = "-"))
    
    my_plot <- ggplot() +
      ylab("Energy Consumed (Watt/hour)") + xlab("Year") +
      ggtitle("Energy Consumed") + theme_black() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(
        name = "Legend",
        values = c(
          "GlobalActivePower" = "#ff66cc",
          "Kitchen" = "#f47835",
          "Laundry" = "#66ccff",
          "WaterHeater_AirCon" = "#ccff66",
          "MissingEnergy" = "#f9d62e"
        )) + xlim(starting_date, end_date)
  
      
   if("GlobalActivePower" %in% input$energysource){
     my_plot <- my_plot + geom_line(data = energy(),
                                    aes(
                                      x = as.POSIXct(Date),
                                      y = GlobalActivePower,
                                      color = "GlobalActivePower"
                                    ))
   }
    
    if("Kitchen" %in% input$energysource){
      my_plot <- my_plot + geom_line(data = energy(),
                                     aes(
                                       x = as.POSIXct(Date),
                                       y = Kitchen,
                                       color = "Kitchen"
                                     ))
    } 
    
    if("Laundry" %in% input$energysource){
      my_plot <- my_plot + geom_line(data = energy(),
                                     aes(
                                       x = as.POSIXct(Date),
                                       y = Laundry,
                                       color = "Laundry"
                                     ))
    } 
    
    if("WaterHeater_AirCon" %in% input$energysource){
      my_plot <- my_plot + geom_line(data = energy(),
                                     aes(
                                       x = as.POSIXct(Date),
                                       y = WaterHeater_AirCon,
                                       color = "WaterHeater_AirCon"
                                     ))
    }  
    
    if("MissingEnergy" %in% input$energysource){
      my_plot <- my_plot + geom_line(data = energy(),
                                     aes(
                                       x = as.POSIXct(Date),
                                       y = MissingEnergy,
                                       color = "MissingEnergy"
                                     ))
    }  
    
    return(my_plot)
  })
  
  #other tabs ----
  output$table <- renderTable({
    energy()
  })
  
  output$text <- renderText({
    "<br>
Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years.
    <br>
    <br>
    Dataset Source: https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption"
    })
  
}

#running
shinyApp(ui = ui, server = server)