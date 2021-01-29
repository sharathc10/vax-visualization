library(DT)
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "Vaccine Preventable Diseases Dashboard",titleWidth="90%"),
    dashboardSidebar(
        sidebarUserPanel("by Chitra Sharathchandra",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        selectizeInput("selectedIllness",
                       "Select Disease",
                       choice_illness),
        selectizeInput("selectedRegion",
                       "Select Grouping",
                       choice_groupings),
        sidebarMenu(id="sidebarID",
            menuItem("Charts", tabName = "illness", icon = icon("line-chart")),
            menuItem("Tables", tabName = "table", icon = icon("database"))
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "illness",
                    fluidRow(
                        tabBox(width=12,
                            tabPanel(title="All",plotOutput("illness")),
                            tabPanel(title="AFR",plotOutput("illnessAFR")),
                            tabPanel(title="AMR",plotOutput("illnessAMR")),
                            tabPanel(title="EMR",plotOutput("illnessEMR")),
                            tabPanel(title="EUR",plotOutput("illnessEUR")),
                            tabPanel(title="SEAR",plotOutput("illnessSEAR")),
                            tabPanel(title="WPR",plotOutput("illnessWPR"))
                        )
                    )
            ),
            tabItem(tabName = "table",
                    fluidRow(
                        tabBox(width=12,
                               tabPanel(title="All", DT::dataTableOutput("table")),
                               tabPanel(title="AFR", DT::dataTableOutput("tableAFR")),         
                               tabPanel(title="AMR", DT::dataTableOutput("tableAMR")),         
                               tabPanel(title="EMR", DT::dataTableOutput("tableEMR")),         
                               tabPanel(title="EUR", DT::dataTableOutput("tableEUR")),         
                               tabPanel(title="SEAR", DT::dataTableOutput("tableSEAR")),         
                               tabPanel(title="WPR", DT::dataTableOutput("tableWPR"))         
                               
                        )
                   
                    )
            )
        )
    )
))