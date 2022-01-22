library(shiny)
library(tidyverse)
library(caret)

## Data Preparation

path = "./cdi.csv"
cdi = read_csv(path)

cdi = cdi %>% 
  mutate(
    id = as.factor(id),
    state = as.factor(state),
    region = as.factor(region),
    region = fct_recode(region,
                        `Northeast` = "1",
                        `North central` = "2",
                        `South` = "3",
                        `West` = "4"
    )
  ) %>% 
  mutate(
    crm_1000 = (crimes / pop) * 1000,
    docs_1000 = (docs / pop) * 1000,
    beds_1000 = (beds / pop) * 1000
  ) %>% 
  mutate(
    pop = pop / 1000
  ) %>% 
  mutate(crm_1000_rt = crm_1000^0.5) %>% 
  dplyr::select(-crm_1000)

cdi = cdi[-c(1, 6),]

fit = lm(crm_1000_rt ~ pop + pop18 + poverty + pcincome + region + beds_1000 + pop*poverty + pop*pop18, data = cdi)


## Main body


### ui

ui = fluidPage(
  tags$head(
    tags$style(HTML(".selectize-input {white-space: nowrap;}.selectize-dropdown {width: 500px !important;}"))
  ),
  titlePanel("County Crime Rate Prediction"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter following information to predict the crime rate (crimes per 1000 person)"),
      
      sliderInput("popIn", "County Population (in thousands)", 0, 9000, 0, sep = ""),
      sliderInput("pop18In", "Percentage of Population between 18 and 34", 0, 100,0, sep = ""),
      sliderInput("povertyIn", "Percentage of Population with Income Below Poverty Level", 0, 100, 0, sep = ""),
      sliderInput("pcincomeIn", "Average Residence Income (in dollars)", 10000, 50000, 0, sep = ""),
      sliderInput("beds1000In", "Hospital Beds per 1000 person", 0, 20, 0, sep = ""),
      selectInput("regionIn", "Geographical Region", c("North central", "Northeast", "South", "West")),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      tags$hr(),
      textOutput("res"),
      tags$hr()
    )
  )
)

### server

server = function(input, output) {
  
  #### rating on adaptively provided movies
  
  observeEvent(input$predict, {
    # add selected movie and rating to custom data frame
    
    tmp = tibble(pop = input$popIn, 
                 pop18 = input$pop18In,
                 poverty = input$povertyIn,
                 pcincome = input$pcincomeIn,
                 region = input$regionIn,
                 beds_1000 = input$beds1000In)
    
    prediction = round(as.numeric(predict(fit, tmp) ** 2), 3)
    
    result = paste("The predicted crimes per 1000 person in the given county is ", as.character(prediction))
    
    output$res = renderText({result})
  }
  )
}


shinyApp(ui = ui, server = server)
