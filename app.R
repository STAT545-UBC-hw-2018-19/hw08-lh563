library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(colourpicker)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      #add an image into the siderbar area
      img(src = "wines.jpg", align="center",width = "110x"),
      #add parameters to the plot: use shinyjs::colorInput() to make 
      #the user decide the colors of hte bars
      div(class="h5",
      colourInput("col","What color do you want to fill in the plot", value="blue"),
      br()),
      
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      #add sort results table by price
      checkboxInput("sortInput","Check this to sort in price", value = FALSE),
      uiOutput("countryOutput"),
      downloadButton("downloadData", label = "Download")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  bcl_filtered<-reactive({
    if (input$sortInput){
    bcl %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput) %>% 
             arrange(Price)
  } else {
    bcl %>% 
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    }
  })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  output$coolplot <- renderPlot({
    bcl_filtered() %>% 
      ggplot(aes(Price)) +
      geom_histogram(fill=input$col)
  })

  output$results <- DT::renderDataTable({
    bcl_filtered()
  })

  output$downloadData<- downloadHandler(
    filename="BCLiquor",
    content=function(file){
      write.csv(bcl_filtered(),file)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
