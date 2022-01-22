# packages
library(shiny)
library(ggplot2) 
library(dplyr) # for filtering and manipulating data
library(agridat) # data

# loading data
Barley <- as.data.frame(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel("Barley Yield"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="gen",
                  label="1. Select genotype",
                  choices=c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"),
                    selected = "a"
      ),
    selectInput(inputId="colour",
                label="2. Select histogram colour",
                choices=c("blue","green","red","purple","grey"), selected = "grey"
    ),
    sliderInput(inputId="bin",
                label="3. Select number of histogram bins",
                min=1, max=25, value=c(10)
    ),
    textInput(inputId = "text",
              label="4. Enter some text to be displayed", "")
    ),
    mainPanel(
      plotOutput("myhist"),
      tableOutput("mytable"),
      textOutput("mytext")
    )
  )
)

# server.R
server <- function(input, output) {
  output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) +
                              geom_histogram(bins = input$bin, fill = input$colour, group=input$gen,
                                             data=Barley[Barley$gen == input$gen,],
                                             colour = 'black'))
  output$mytext <- renderText(input$text)
  output$mytable <- renderTable(Barley %>% filter(gen == input$gen) %>% 
                                   summarise("Mean" = mean(yield),
                                             "Median" = median(yield),
                                             "STDEV" = sd(yield),
                                             "Min" = min(yield),
                                             "Max" = max(yield)))
  }

# Run the app
shinyApp(ui=ui, server=server)

