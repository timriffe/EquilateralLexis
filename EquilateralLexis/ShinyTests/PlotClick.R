install.packages("shiny")
library(shiny)

ui <- basicPage(
		plotOutput("plot1", click = "plot_click"),
		verbatimTextOutput("info")
)

server <- function(input, output,session) {
	output$plot1 <- renderPlot({
				plot(mtcars$wt, mtcars$mpg)
			})
	
	output$info <- renderText({
				paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
			})
	session$onSessionEnded(function() { stopApp() })
}

# need the session thing to turn off properly for now
shinyApp(ui, server)





