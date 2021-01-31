shinyUI(dashboardPage(skin="blue",
    dashboardHeader(title = "Vaccine Preventable Diseases Dashboard",titleWidth="90%"),
    dashboardSidebar(
        sidebarUserPanel("by Chitra Sharathchandra",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(id="sidebarID",
            menuItem("Introduction", tabName = "introduction", icon = icon("globe")),
            menuItem("Charts", tabName = "illness", icon = icon("chart-bar")),
            menuItem("Tables", tabName = "table", icon = icon("database")),
            menuItem("Findings", tabName="findings", icon=icon("file-alt"))
        ),
        conditionalPanel(
            condition = "input.sidebarID == 'illness' || input.sidebarID == 'table'",
            selectizeInput("selectedIllness",
                           "Select Disease & Grouping",
                           choice_illness_group)
            )
        
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "introduction",
                    img(src="polio-vaccine.jpg",width="200px"),
                    h1("Overview"),
                    h2("Introduction"),
                    p("This dashboard is an attempt to study vaccine treatable diseases across the world in the years 2010-2019.  The data presented is based on datasets available at the World Health Organization(WHO) website",a(href = 'https://apps.who.int/immunization_monitoring/globalsummary', 'here'),".  Two key pieces of data have been used for the analysis (1) rates of occurrences of vaccine preventable diseases and (2) rate of inoculation for that particular disease.  The diseases studied are limited to (1) Diphtheria (2) Tetanus (3) Pertussis (4) Measles (5) Rubella (6) Polio and (7) Japanese Enciphelites." ),
                    p("This dashboard divides the disease study by",a(href = 'https://en.wikipedia.org/wiki/List_of_WHO_regions', 'WHO Regions')," and by ",a(href = 'https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index(HDI)', 'Human Development Index(HDI)'),".  Such a breakdown allows us to analyze whether disease rates are dependent on regionality or on the rate of human development in a country.  In addition to these breakdowns, the dashboard also shows how inoculation affects rates for specific diseases."),
                    h2("Data sets"),
                    p("The illness dataset included other illnesses such as Yellow Fever and Mumps.  Yellow fever was not included because there were no incidences from 2010 to 2020 and Mumps was not included because there was no data on its antigen" ),
                    p("The vaccine dataset, on the other hand, included vaccines that were for illnesses other than what were studied here. These data were automatically excluded."),
                    h2("Study limitations"),
                    p("The limitations of this study are as follows:"),
                    tags$ul(
                        tags$li("Datasets were used as is from WHO and UN websites"),
                        tags$li("Since vaccine data was provided as a percent coverage within the country, for normalization, these rates were converted as a percent of world population. "),
                        tags$li("There was insufficient vaccine data in some regions and these particular areas could not be analyzed")
                    )
                    
                    ),
            
            tabItem(tabName = "illness",
                    fluidRow(
                        tabBox(title=textOutput("tabHeading"),
                               width=12,
                               id = "tabset1", 
                               height = "650px",
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
            ),
            tabItem(tabName = "findings",
                    img(src="poliovaccine.jpeg",width="200px"),
                    h1("Data Analysis"))
        )
    )
))