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
                    h1(verbatimTextOutput("tabHeading")),
                    fluidRow(
                        tabBox(width=12,
                            tabPanel(title="All",plotOutput("illness")),
                            tabPanel(title="AFR/VHD",plotOutput("illnessAFR")),
                            tabPanel(title="AMR/HD",plotOutput("illnessAMR")),
                            tabPanel(title="EMR/MD",plotOutput("illnessEMR")),
                            tabPanel(title="EUR/LD",plotOutput("illnessEUR")),
                            tabPanel(title="SEAR/None",plotOutput("illnessSEAR")),
                            tabPanel(title="WPR/None",plotOutput("illnessWPR"))
                        )
                    ),
                    h4("Data source: WHO")
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