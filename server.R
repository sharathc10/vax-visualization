library(DT)
library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output){
    
    # show illness graph
    output$illness <- renderPlot({
        ggplot(diphtheria_by_region_2010_2019 ) + 
                geom_line(aes(x=year,y=total_cases,color=WHO_REGION)) + 
                ggtitle("Diphtheria Cases By Region") + 
                labs(x="Year",y="Total Cases") + 
                scale_x_continuous(breaks = 2010:2019)
            })
    # show illness graph
    output$vaccine <- renderPlot({
        ggplot(dtp1_by_region ) + 
            geom_line(aes(x=Year,avg,color=WHO_REGION)) + 
            ggtitle("DTP Administration By Region") + 
            labs(x="Year",y="Coverage") + 
            scale_x_continuous(breaks = 2010:2019)
    })
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(diphtheria_by_region_2010_2019, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
})