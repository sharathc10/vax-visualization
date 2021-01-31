shinyServer(function(input, output){
    
    data <- reactive({
       illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
       group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
       illness_selected_heading = illness_selected
       
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
       } else if (illness_selected == "Diphtheria" & group_selected=="HDI") {
           dataSet <- diphtheria_dtp_hdi
       } else if (illness_selected == "Tetanus" & group_selected=="HDI") {
           dataSet <- tetanus_dtp_hdi
       }else if (illness_selected == "Pertussis" & group_selected=="HDI") {
           dataSet <- pertussis_dtp_hdi 
       }else if (illness_selected == "Polio" & group_selected=="HDI") {
           dataSet <-  polio_ipv_hdi
       }else if (illness_selected == "Measles" & group_selected=="HDI") {
           dataSet <-  measles_mcv_hdi
       }else if (illness_selected == "Rubella" & group_selected=="HDI") {
           dataSet <-  rubella_rcv_hdi
       }else if (illness_selected == "Japanese Enciphelites" & group_selected=="HDI") {
           dataSet <-  japenc_hdi
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
        } else if (group_selected=="HDI") {
            scaleVal=100000
        }
        
        scaleVal
    })
    
    facetresp <- reactive({
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        facetrespVal = "WHO_REGION"
        if (group_selected=="HDI") {
            facetrespVal="hdi_level"
            }
        
        facetrespVal
    })
    
    highestCountry <- reactive({
        highest_country=highest_cases_illness(diphtheria)
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
         tab_selected=input$tabset1
        if (tab_selected=="All") {
            if (illness_selected=="Tetanus") {
                highest_country=highest_cases_illness(tetanus)
            } else if (illness_selected=="Japanese Encephilites") {
                highest_country=highest_cases_illness(japenc)
            }else if (illness_selected=="Pertussis") {
                highest_country=highest_cases_illness(pertussis)
            }else if (illness_selected=="Measles") {
                highest_country=highest_cases_illness(measles)
            }else if (illness_selected=="Polio") {
                highest_country=highest_cases_illness(polio)
            }else if (illness_selected=="Rubella") {
                highest_country=highest_cases_illness(rubella)
            }
        } else {
            if (group_selected=="Region") {
                region=strsplit(tab_selected,split="/")[[1]][1]
                print(region)
                highest_country=filter_highest_cases(illness_selected,group_selected,region)
            } else {
                hdi_cat=strsplit(tab_selected,split="/")[[1]][2]
                hdi_value=map_hdi_value(hdi_cat)
                highest_country=filter_highest_cases(illness_selected,group_selected,hdi_value)
            }
        }
        highest_country
    })

    heading <- reactive({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
    })
    
    # show illness graph
    output$illness <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        scaleVal=scale()
        facetresponse = facetresp()
        plot_all_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
            
        })
    
    output$illnessAMR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        if (group_selected=="Region") {
          df=df %>% filter(WHO_REGION=="AMR")
        } else {
          df=df %>% filter(hdi_level=="High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
   
    output$illnessAFR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AFR")
        } else {
            df=df %>% filter(hdi_level=="Very High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
            
    })
    
    output$illnessEMR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EMR")
        } else {
            df=df %>% filter(hdi_level=="Medium Development")
        }
        
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessEUR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EUR")
        } else {
            df=df %>% filter(hdi_level=="Low Development")
        }
        
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessSEAR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data() %>% filter(WHO_REGION=="SEAR")
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessWPR <- renderPlot({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data() %>% filter(WHO_REGION=="WPR")
        scaleVal=scale()
        facetresponse = facetresp()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        df=data() 
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableAFR <- DT::renderDataTable({
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AFR")
        } else {
            df=df %>% filter(hdi_level=="Very High Development")
        }
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableAMR <- DT::renderDataTable({
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AMR")
        } else {
            df=df %>% filter(hdi_level=="High Development")
        }
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableEMR <- DT::renderDataTable({
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EMR")
        } else {
            df=df %>% filter(hdi_level=="Medium Development")
        }
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableEUR <- DT::renderDataTable({
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        df=data()
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EUR")
        } else {
            df=df %>% filter(hdi_level=="Low Development")
        }
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
    
    output$countryBox <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBox <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxAFR <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxAFR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxAMR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxAMR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxEMR <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxEMR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxSEAR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxSEAR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxEUR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxEUR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxWPR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountry()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxWPR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountry()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$tabHeading <- renderText({
        illness_selected = strsplit(input$selectedIllness,split="-")[[1]][1]
        group_selected = strsplit(input$selectedIllness,split="-")[[1]][2]
        tab_selected=input$tabset1
        if (group_selected=="Region") {
            if (tab_selected !="All") {
                tab_selected=map_region_value(strsplit(tab_selected,split="/")[[1]][1])
            }
        } else {
            if (tab_selected != "All") {
                tab_selected=map_hdi_value(strsplit(tab_selected,split="/")[[1]][2])
            }
        }
        build_tab_heading=paste0(illness_selected," by ")
        build_tab_heading=paste0(build_tab_heading,paste0(group_selected,"-"))
        build_tab_heading=paste0(build_tab_heading,tab_selected)
        build_tab_heading
    })
    
    
})