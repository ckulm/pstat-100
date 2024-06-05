library(shiny)
library(tidyverse)
library(bslib)
MNIST_data <- read.csv(
  "~/Desktop/PSTAT 100/Mini Projects/MP03/data/mnist.csv"
)

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  title = "PSTAT 100, MP03",
  sidebar = sidebar(
  h6("Perform dimension reduction on an MNIST image, 
  reconstitute the dimension-reduced image."),
  sliderInput("dims", "Number of dimensions: ", min = 1, max = 28, 
              value = 2, ticks = T),
  helpText("This slider controls the number of dimensions 
            we first project the image into. Fewer dimensions result 
            in 'grainer' images."),
  textInput("which_row", "Which Row?", value = 1, placeholder = 1),
  helpText("This input controls which row (of the 1000 rows included) 
  to use when generating the image.")
  ),
  layout_columns(
    card(card_header("Reconstituted Image"),
         imageOutput("distPlot")), 
    card(card_header("True Classification"),
         textOutput("verb")))
)

# server function
server <- function(input, output) {
  output$verb <- renderText({ paste0(
    "The number displayed is ", 
    MNIST_data[input$which_row, 1])
  })
  
  output$distPlot <- renderPlot({
    vect <- (MNIST_data[input$which_row, -1] %>% as.numeric) / 255
    as.im <- matrix(vect,
                    nrow = 28,
                    byrow = T)
    as.im <- prcomp(as.im)$x[,1:input$dims] %*% 
      t(prcomp(as.im)$rotation[,1:input$dims])
    as.im <- scale(as.im, scale = F)
    
    as.im[nrow(as.im):1, ] %>%
      as.data.frame() %>%
      rowid_to_column(var = 'y') %>%
      pivot_longer(
        -y,
        names_to = 'x',
        values_to = "brightness"
      ) %>%
      mutate(x = parse_number(x)) %>%
      ggplot(aes(x = x, y = y, fill = brightness)) +
      geom_raster() +
      theme_void() +
      scale_fill_gradient2(low="white", high="black", guide = 
                             "none") +
      theme(
        panel.border = element_rect(linewidth = 1,
                                    fill = NA)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)