function(input, output, session) {
  observe({
    #    session$sendCustomMessage(type = 'testmessage',
    #                              message = 'Started the analysis')
    if (input$button) {
      bucketTable <<- doBuckets(playerScrapeMerge)
      output$table <- DT::renderDataTable({
        datatable(bucketTable, colnames = c(Rank = 1), fillContainer=TRUE, selection = 'multiple', 
                  extensions = c('Buttons'
                                 #,'FixedHeader'
                                 ,'Scroller')
                  , options = list(
                    #pageLength=15
                    dom = 'Bfrtip'
                    , buttons = c('csv','copy')
                    , columnDefs = list(list(
                      targets = 12:38
                      , visible=FALSE)
                      , list(
                        targets = 0:11
                        , className = 'dt-center'))
                    , scrollY = '80.0vh'
                    , scroller = TRUE
                    , scrollX = TRUE
                    , deferRender = TRUE
                  )
        ) %>% 
          formatRound(columns=11,digits=2) %>%
          formatStyle(columns=10, fontWeight = styleEqual(59000, 'bold')) %>%
          formatCurrency(columns=10,currency = "", interval = 3, mark = ",", digits=0)
      })
    }
  })
  observe({
    if (!is.null(input$table_rows_selected) & input$showSalary){
      #print(input$table_rows_selected)
      output$tableSelect = ({
        DT::renderDataTable({
          datatable(bucketTable[input$table_rows_selected,30:38], colnames = c(Rank = 1)
                    , fillContainer=TRUE, selection = 'none', filter='none'
                    , extensions = 'Scroller'
                    , options = list(
                      scroller = TRUE
                    , scrollX = TRUE ) 
                    ) %>%
          formatCurrency(columns=1:9,currency = "", interval = 3, mark = ",", digits=0)
        })
      })
    } else {
      output$tableSelect = HTML("")
    }
  })
}