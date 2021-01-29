library(DT)
library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output){
    
    data <- reactive({
       if (input$selectedIllness == "Diphtheria") {
        dataSet <- all_diphtheria_dtp1
       } else if (input$selectedIllness == "Tetanus") {
           dataSet <- all_tetanus_dtp1
       } else if (input$selectedIllness == "Pertussis") {
           dataSet <- all_pertussis_dtp1
       }
           
        dataSet
    })
    
    scale <- reactive({
        if (input$selectedIllness == "Diphtheria") {
            scaleVal <- 10000
        } else if (input$selectedIllness == "Tetanus") {
            scaleVal <- 8000
        } else if (input$selectedIllness == "Pertussis") {
            scaleVal <- 40000
        }
        
        scaleVal
    })
    
    # show illness graph
    output$illness <- renderPlot({
        df=data()
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
            
        })
   
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(all_diphtheria_dtp1, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
})