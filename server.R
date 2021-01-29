library(DT)
library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output){
    
    # show illness graph
    output$illness <- renderPlot({
        ggplot(all_diphtheria_dtp1, aes(Year, total_cases)) + 
            geom_bar(data=all_diphtheria_dtp1,stat="identity") + 
            geom_line(data=all_diphtheria_dtp1, aes(y=avg*100)) + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
            })
   
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(all_diphtheria_dtp1, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
})