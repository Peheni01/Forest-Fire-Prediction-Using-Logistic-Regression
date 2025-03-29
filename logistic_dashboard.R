# Load required libraries

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)



# Read the data

mydata <- read.csv("Forest_fire.csv")
mydata$Classes <- as.factor(mydata$Classes)

# UI for application

ui <- fluidPage(
  
  # Added CSS style for background image and gap between plots
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-image: url('https://img.freepik.com/premium-photo/autumn-forest-is-alight-with-fire-night-creating-dramatic-scene-burning-foliage-fiery-sparks-against-darkness_127746-9907.jpg');
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
        }
        .plot-output {
          margin-top: 30px;
          margin-bottom: 30px;
        }
        "
      )
    )
  ),
  
  titlePanel(tags$span("Forest Fire Dashboard", style = "color: white;")),  # Title with white color
  
    navbarPage(
    "Dataset Analysis",
    
    tabPanel("Introduction", align = "left",  
             h2(style = "color: white;", "Northern Algerian Forest Fire - 2012"),  
             h4(style = "color: white;",
               "Forest fires, known variously as bushfires, wildland fires, or rural fires,",br(),"are among nature's most destructive forces.",
               "They spread unpredictably",br(),"and uncontrollably, wreaking havoc on ecosystems.",
               "Northern Algeria, in ",br(),"2012, bore witness to such devastation.",
               "This region faced a catastrophic ",br(),"wildfire, leaving behind a scar of destruction, devastating wildlife and vegetation.",br(),
               "This dashboard chronicles the events of this inferno that ravaged Northern Algeria in 2012." 
             )
    
            
    ),  
    
    # Plots and Select Data tab panel
    tabPanel("Plots & Select Data",
             fluidRow(
               column(4, 
                      pickerInput("select_month", label = tags$span("Choose Month", style = "color: white;"),  # Dropdown box title in white
                                  choices = unique(mydata$month),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE,
                                  selected = unique(mydata$month)[1]),
                      pickerInput("select_class", label = tags$span("Choose Class", style = "color: white;"),  # Dropdown box title in white
                                  choices = unique(mydata$Classes),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE,
                                  selected = unique(mydata$Classes)[1])
               ),
               column(4, plotOutput("boxplot_region"), style = "margin-bottom: 30px;"),
               column(4, plotOutput("histogram_temperature"), style = "margin-bottom: 30px;"),
               column(4, plotOutput("barplot_rain_month"), style = "margin-bottom: 30px;"),
               column(4, plotlyOutput("scatter_plot_2d"), style = "margin-bottom: 30px;"),
               column(4, plotlyOutput("scatter_plot_3d"), style = "margin-bottom: 30px;"),
               column(4, plotOutput("lm1_plot"), style = "margin-bottom: 30px;"),
               column(4, plotOutput("lm2_plot"), style = "margin-bottom: 30px;"),
               column(4, plotOutput("logistic_regression_plot"), style = "margin-bottom: 30px;")
             )
    )
    
  )
)

# Server logic

server <- function(input, output) {
  
  # The dataset is loaded
  mydata <- read.csv("Forest_fire.csv")
  
  # Reactive expression to filter data based on selected input
  filtered_data <- reactive({
    filtered <- mydata[mydata$month %in% input$select_month & mydata$Classes %in% input$select_class, ]
    return(filtered)
  })
  
  # Box Plot 
  output$boxplot_region <- renderPlot({
    ggplot(filtered_data(), aes(x = region, y = Temperature)) +
      geom_boxplot() +
      labs(title = "Temperature Distribution by Region",
           x = "Region", y = "Temperature")
  })
  
  # Histogram 
  output$histogram_temperature <- renderPlot({
    ggplot(filtered_data(), aes(x = Temperature)) +
      geom_histogram(binwidth = 1, fill = "orange", color = "black") +
      labs(title = "Temperature Distribution",
           x = "Temperature", y = "Frequency")
  })
  
  # Bar Plot 
  output$barplot_rain_month <- renderPlot({
    filtered_data() %>%
      group_by(month) %>%
      summarise(avg_rain = mean(Rain)) %>%
      ggplot(aes(x = factor(month), y = avg_rain)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Average Rainfall by Month",
           x = "Month", y = "Average Rainfall")
  })
  
  
  # Scatter Plot
  output$scatter_plot_2d <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Temperature, y = ~RH, color = ~Classes,
            type = "scatter", mode = "markers")
  })
  
  # 3D Scatter Plot
  output$scatter_plot_3d <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Temperature, y = ~RH, z = ~FWI, color = ~Classes,
            type = "scatter3d", mode = "markers")
  })
  
  # Model 1 - Linear Regression with Temperature and Rain
  output$lm1_plot <- renderPlot({
    lm1 <- lm(Temperature ~ Rain  , data = filtered_data())
    summary(lm1)
    ggplot(filtered_data(), aes(x = Temperature, y = Rain)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "red")
  })
  
  # Model 2 - Linear Regression with Temperature and Wind Speed (Ws)
  output$lm2_plot <- renderPlot({
    lm2 <- lm(Temperature  ~  Ws , data =  filtered_data())
    summary(lm2)
    ggplot(filtered_data(), aes(x = Temperature   , y = Ws)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "blue")
  })
  
  # Model 3 - Logistic Regression with Temperature
  output$logistic_regression_plot <- renderPlot({
    train <- filtered_data()[sample(2, nrow(filtered_data()), replace=TRUE, prob=c(0.8,0.20)), ]
    model_glm <- glm(Classes ~ Temperature, data = train, family = "binomial")
    summary(model_glm)
    filtered_data() %>% 
      mutate(Out = ifelse(Classes == "1", 1, 0)) %>% 
      ggplot(aes(Temperature, Out)) + 
      geom_point(alpha = .15) + 
      geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
      ggtitle("Logistic regression model fit") + 
      xlab("Temperature") + 
      ylab("Classes ") 
  })
}


# Runs the application

shinyApp(ui = ui, server = server)
*