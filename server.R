function(input, output, session) {
  observeEvent(input$button, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Started the analysis')
  })
  
#download data button
    
}