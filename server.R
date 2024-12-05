# Define server for application

function(input, output, session) {
  
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formula
  observe_helpers(withMathJax = TRUE)
  observe({
    if(input$datatype == 'drc') {
      source(file.path("src/DRC.R"), local = TRUE)$value
    } else {
      source(file.path("src/TE.R"), local = TRUE)$value
    }
  })
  
}



