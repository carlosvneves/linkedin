# -----------------------------------------------------------------------
# Very simple Shiny App to interactively draw the Pascal´s Triangle
# Author: Carlos Eduardo Veras Neves
# -----------------------------------------------------------------------
# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)


# Define UI for application that draws a Pascal´s Triangle
ui <- fluidPage(

    # Application title
    titlePanel("TRIANGULO DE PASCAL"),
   

    # Sidebar with a slider input for number of lines 
    sidebarLayout(position = "right",
        sidebarPanel(
            sliderInput("num",
                        "Numero de linhas do Triangulo de Pascal:",
                        min = 2,
                        max = 20,
                        value = 7)
            
        ),

        # Show the output of the generated Pascal´s Triangle
        mainPanel(
          
          verbatimTextOutput("printTriang")
          
        )
        
    )
)




# Define server logic required to draw the Pascal´s Triangle
server <- function(input, output) {
    
    # call the Shiny function to draw the output
    output$printTriang <- renderPrint({
      
      # Creates an empty list with the rows defined
      create <- function(nrows){
        
        for (r in 1:(nrows)) {
          pt[[r]] <-  rep(1,r)
        } 
        
        return(pt)
        
      }
      
      # Populates the list 
      populate <- function(pt){
        
        for (r in 1:(length(pt)-1)){
          
          for (c in 1:(length(pt[[r]]))){
            
            pt[[r+1]][c+1] <- factorial(r)/(factorial(c) * factorial(r - c))
          }
          
        }
        
        return(pt)
        
      }
      
      # Wrapper function to printf C style
      printf <- function(...) {cat(sprintf(...))}
      
      # Prints the formatted triangle
      print_triang <- function(pt){
        
        inset <- as.integer((((((length(pt)) * 1.5) - 1 ) / 2) * 3))
        
        printf("\n")
        for (r in 1:(length(pt))){
          
          printf(paste0(rep(" ",inset)))
          
          for (c in 1:(length(pt[[r]]))){
            
            printf("%-3d   ", pt[[r]][c])
          }
          
          printf("\n")
          inset = inset - 2
        }
        
      }
      
      # gets user input - number of rows
      nrows <- input$num
      
      # empty list
      pt <- list()
      
      # function calls
      pt <- create(nrows)
      pt <- populate(pt)
      
      # prints the Pascal´s Triangle
      print_triang(pt)
      
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
