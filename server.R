shinyServer(function(input, output){
    
    data <- reactive({
       illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
       group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
       if (illness_selected == "Diphtheria" & group_selected=="Region") {
        dataSet <- all_diphtheria_dtp1
       } else if (illness_selected == "Tetanus" & group_selected=="Region") {
           dataSet <- all_tetanus_dtp1
       } else if (illness_selected == "Pertussis" & group_selected=="Region") {
           dataSet <- all_pertussis_dtp1
       } else if (illness_selected == "Polio" & group_selected=="Region") {
           dataSet <- all_polio_ipv1
       } else if (illness_selected == "Measles" & group_selected=="Region") {
           dataSet <- all_measles_mcv1
       } else if (illness_selected == "Rubella" & group_selected=="Region") {
           dataSet <- all_rubella_rcv1
       } else if (illness_selected == "Japanese Enciphelites" & group_selected=="Region") {
           dataSet <- all_japenc
       }
           
        dataSet
    })
    
    scale <- reactive({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        scaleVal=10000
        if (illness_selected == "Diphtheria" & group_selected=="Region") {
            scaleVal <- 10000
        } else if (illness_selected == "Tetanus" & group_selected=="Region") {
            scaleVal <- 8000
        } else if (illness_selected == "Pertussis" & group_selected=="Region") {
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