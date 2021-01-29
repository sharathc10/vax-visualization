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
    
    output$illnessAMR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="AMR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
   
    output$illnessAFR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="AFR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
    
    output$illnessEMR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="EMR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
    
    output$illnessEUR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="EUR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
    
    output$illnessSEAR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="SEAR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
    
    output$illnessWPR <- renderPlot({
        df=data() %>% filter(WHO_REGION=="WPR")
        scaleVal=scale()
        
        ggplot(df, aes(Year, total_cases)) + 
            geom_bar(data=df,stat="identity") + 
            geom_line(data=df, aes(y=avg*scaleVal),color="red") + 
            scale_y_continuous(sec.axis= sec_axis(~./100, name="avg")) +
            facet_wrap(~ WHO_REGION)
        
    })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        df=data()
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableAFR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="AFR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableAMR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="AMR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableEMR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="EMR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableEUR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="EUR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableSEAR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="SEAR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableWPR <- DT::renderDataTable({
        df=data() %>% filter(WHO_REGION=="WPR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
})