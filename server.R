shinyServer(function(input, output){
    
    dataRegion <- reactive({
       illness_selected = input$selectedIllness
       group_selected = "Region"
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
    
    dataHDI <- reactive({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
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
        illness_selected = input$selectedIllness
        group_selected = input$selectedgroup
        scaleVal=10000
        scaleVal
    })
    
    facetrespRegion <- reactive({
        group_selected = "Region"
        facetrespVal = "WHO_REGION"
        if (group_selected=="HDI") {
            facetrespVal="hdi_level"
            }
        
        facetrespVal
    })
    
    facetrespHDI <- reactive({
        group_selected = "HDI"
        facetrespVal = "WHO_REGION"
        if (group_selected=="HDI") {
            facetrespVal="hdi_level"
        }
        
        facetrespVal
    })
    
    highestCountryRegion <- reactive({
        highest_country=highest_cases_illness(diphtheria)
        illness_selected = input$selectedIllness
        group_selected = "Region"
        if (group_selected=="Region") {
            tab_selected=input$tabset1
        } else {
            tab_selected=input$tabset2
        }
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
                region=tab_selected
                highest_country=filter_highest_cases(illness_selected,group_selected,region)
            } else {
                hdi_cat=tab_selected
                hdi_value=map_hdi_value(hdi_cat)
                highest_country=filter_highest_cases(illness_selected,group_selected,hdi_value)
            }
        }
        highest_country
    })
    
    highestCountryHDI <- reactive({
        highest_country=highest_cases_illness(diphtheria)
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        if (group_selected=="Region") {
            tab_selected=input$tabset1
        } else {
            tab_selected=input$tabset2
        }
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
                region=tab_selected
                highest_country=filter_highest_cases(illness_selected,group_selected,region)
            } else {
                hdi_cat=tab_selected
                hdi_value=map_hdi_value(hdi_cat)
                highest_country=filter_highest_cases(illness_selected,group_selected,hdi_value)
            }
        }
        highest_country
    })

    heading <- reactive({
        illness_selected = input$selectedIllness
        group_selected = input$selectedgroup
    })
    
    # show illness graph by region
    output$illness <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
        scaleVal=scale()
        facetresponse = facetrespRegion()
        plot_all_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
            
        })
    
    output$illnessAMR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
        if (group_selected=="Region") {
          df=df %>% filter(WHO_REGION=="AMR")
        } else {
          df=df %>% filter(hdi_level=="High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
   
    output$illnessAFR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AFR")
        } else {
            df=df %>% filter(hdi_level=="Very High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
            
    })
    
    output$illnessEMR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EMR")
        } else {
            df=df %>% filter(hdi_level=="Medium Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessEUR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EUR")
        } else {
            df=df %>% filter(hdi_level=="Low Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessSEAR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion() %>% filter(WHO_REGION=="SEAR")
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessWPR <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion() %>% filter(WHO_REGION=="WPR")
        scaleVal=scale()
        facetresponse = facetrespRegion()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    # show illness graph by HDI
    output$illnesshdi <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI()
        scaleVal=scale()
        facetresponse = facetrespHDI()
        plot_all_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessHD <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AMR")
        } else {
            df=df %>% filter(hdi_level=="High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespHDI()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessVHD <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="AFR")
        } else {
            df=df %>% filter(hdi_level=="Very High Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespHDI()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessMD <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EMR")
        } else {
            df=df %>% filter(hdi_level=="Medium Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespHDI()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    output$illnessLD <- renderPlot({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI()
        if (group_selected=="Region") {
            df=df %>% filter(WHO_REGION=="EUR")
        } else {
            df=df %>% filter(hdi_level=="Low Development")
        }
        
        scaleVal=scale()
        facetresponse = facetrespHDI()
        
        plot_vax_disease_graph(
            df,
            facetresponse,
            paste0(paste0(
                paste0(illness_selected," incidences vs Vaccine coverage by "),
                group_selected),"(source:WHO)"),
            "Year",
            paste0(illness_selected," cases (people count)"),
            scaleVal) 
        
    })
    
    # show data using DataTable
    output$tableRegion <- DT::renderDataTable({
        df=dataRegion() 
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableAFR <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
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
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
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
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
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
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion()
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
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion() %>% filter(WHO_REGION=="SEAR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableWPR <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "Region"
        df=dataRegion() %>% filter(WHO_REGION=="WPR")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tablehdi <- DT::renderDataTable({
        df=dataHDI() 
        colnames(df)[colnames(df)=="avg"]="Vaccine coverage"
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableVHD <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI() %>% filter(hdi_level=="Very High Development")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableHD <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI() %>% filter(hdi_level=="High Development")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableMD <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI() %>% filter(hdi_level=="Medium Development")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$tableLD <- DT::renderDataTable({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        df=dataHDI() %>% filter(hdi_level=="Low Development")
        datatable(df, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    
    output$countryBoxill <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxill <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxAFR <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxAFR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxAMR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxAMR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxEMR <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxEMR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxSEAR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxSEAR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxEUR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxEUR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxWPR <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountryRegion()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxWPR <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryRegion()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxhdi <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryHDI()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxhdi <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryHDI()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxHD <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryHDI()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxHD <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryHDI()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxVHD <- renderInfoBox({
        infoBox(
            "Highest cases(%)",highestCountryHDI()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    output$yearBoxVHD <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryHDI()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxMD <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryHDI()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxMD <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryHDI()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$countryBoxLD <- renderInfoBox({
        infoBox(
            "Highest cases(%)", highestCountryHDI()$Country, icon = icon("hand-o-right"),
            color = "olive"
        )
    })
    
    output$yearBoxLD <- renderInfoBox({
        infoBox(
            "Year of highest cases", highestCountryHDI()$Year, icon = icon("calendar", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$tabHeadingRegion <- renderText({
        illness_selected = input$selectedIllness
        group_selected = "Region"
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
    
    output$tabHeadingHDI <- renderText({
        illness_selected = input$selectedIllness
        group_selected = "HDI"
        tab_selected=input$tabset2
        if (tab_selected != "All") {
            tab_selected=map_hdi_value(tab_selected)
        }
        
        build_tab_heading=paste0(illness_selected," by ")
        build_tab_heading=paste0(build_tab_heading,paste0(group_selected,"-"))
        build_tab_heading=paste0(build_tab_heading,tab_selected)
        build_tab_heading
    })
    
    
})