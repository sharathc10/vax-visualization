shinyUI(dashboardPage(
    dashboardHeader(title = "Vaccine Preventable Diseases Dashboard",titleWidth="90%"),
    dashboardSidebar(
        sidebarUserPanel("by Chitra Sharathchandra",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        selectizeInput("selectedIllness",
                       "Select Disease & Grouping",
                       choice_illness_group),
        sidebarMenu(id="sidebarID",
            menuItem("Charts", tabName = "illness", icon = icon("line-chart")),
            menuItem("Tables", tabName = "table", icon = icon("database"))
        )
        
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "illness",
                    fluidRow(
                        tabBox(title=textOutput("tabHeading"),
                               width=12,
                               id = "tabset1", 
                               height = "250px",
                            tabPanel(title="All",
                                     infoBoxOutput("countryBox"),tags$style("#countryBox {width:300px;}"),
                                     infoBoxOutput("yearBox"),tags$style("#yearBox {width:300px;}"),
                                     plotOutput("illness")),
                            tabPanel(title="AFR/VHD",
                                     infoBoxOutput("countryBoxAFR"),tags$style("#countryBoxAFR {width:300px;}"),
                                     infoBoxOutput("yearBoxAFR"),tags$style("#yearBoxAFR {width:300px;}"),
                                     plotOutput("illnessAFR")),
                            tabPanel(title="AMR/HD",
                                     infoBoxOutput("countryBoxAMR"),tags$style("#countryBoxAMR {width:300px;}"),
                                     infoBoxOutput("yearBoxAMR"),tags$style("#yearBoxAMR {width:300px;}"),
                                     plotOutput("illnessAMR")),
                            tabPanel(title="EMR/MD",
                                     infoBoxOutput("countryBoxEMR"),tags$style("#countryBoxEMR {width:300px;}"),
                                     infoBoxOutput("yearBoxEMR"),tags$style("#yearBoxEMR {width:300px;}"),
                                     plotOutput("illnessEMR")),
                            tabPanel(title="EUR/LD",
                                     infoBoxOutput("countryBoxEUR"),tags$style("#countryBoxEUR {width:300px;}"),
                                     infoBoxOutput("yearBoxEUR"),tags$style("#yearBoxEUR {width:300px;}"),
                                     plotOutput("illnessEUR")),
                            tabPanel(title="SEAR/None",
                                     infoBoxOutput("countryBoxSEAR"),tags$style("#countryBoxSEAR {width:300px;}"),
                                     infoBoxOutput("yearBoxSEAR"),tags$style("#yearBoxSEAR {width:300px;}"),
                                     plotOutput("illnessSEAR")),
                            tabPanel(title="WPR/None",
                                     infoBoxOutput("countryBoxWPR"),tags$style("#countryBoxWPR {width:300px;}"),
                                     infoBoxOutput("yearBoxWPR"),tags$style("#yearBoxWPR {width:300px;}"),
                                     plotOutput("illnessWPR"))
                        )
                    )

                    
            ),
            tabItem(tabName = "table",
                    fluidRow(
                        tabBox(width=12,
                               tabPanel(title="All", DT::dataTableOutput("table")),
                               tabPanel(title="AFR/VHD", DT::dataTableOutput("tableAFR")),         
                               tabPanel(title="AMR/HD", DT::dataTableOutput("tableAMR")),         
                               tabPanel(title="EMR/MD", DT::dataTableOutput("tableEMR")),         
                               tabPanel(title="EUR/LD", DT::dataTableOutput("tableEUR")),         
                               tabPanel(title="SEAR/None", DT::dataTableOutput("tableSEAR")),         
                               tabPanel(title="WPR/None", DT::dataTableOutput("tableWPR"))         
                               
                        )
                   
                    )
            )
        )
    )
))