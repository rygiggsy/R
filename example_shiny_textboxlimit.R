library(shiny)    

ui <- fluidPage(title = "App Title",
                textInput("mytext","Input text:"),
                textOutput('helptext')
)

max_char = 10

server <- function(input, output, session) 
{
  output$helptext <- reactive({ paste0('only ', max_char-nchar(input$mytext), ' characters remaining.' ) })
  
  observeEvent(input$mytext,{
    if(nchar(input$mytext)>max_char)
    {
      updateTextInput(session,'mytext',value=substr(input$mytext,1,max_char))
      showModal(modalDialog(
        title = "Error!",
        "Character limit exceeded!",
        easyClose = TRUE
      ))
    }
  }
  )
  
}
shinyApp(ui,server)

