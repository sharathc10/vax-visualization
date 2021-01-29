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
                        box(width = 10,plotOutput("illness"))

                        )
                    ),
            tabItem(tabName = "table",
                    fluidRow(box(DT::dataTableOutput("table"), width = 12)))
        )
    )
))