#TITLE: Script for Data Analysis from Structural Steel Provider
##AUTHOR: Sergio Miranda (sergeimiranda@hotmail.com)
##DATE: 07-02-2022

library(shiny)
library(ggplot2)
library(bslib)
library(hrbrthemes)
source("Metal_Analysis.R")


Variables <- unique(MetalData$Seccion)  # List of unique category values

hrbrthemes::import_roboto_condensed()

# Define UI for application that draws the ScatterPlot
shinyUI(fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),

    tabsetPanel(
        tabPanel("Information", 
                 titlePanel("Structural Members Cost/Weight Analysis"),
                 # Main Panel
                 mainPanel(
                     h3("Introduction"),
                     p("The present analysis takes data from a given local supplier pricing information (Excel File) and 
                       extracts data to perform a comparison of the metallurgical members available. The comparison lies in the need
                       of having a simple way of visualizing the cost per unit of weight of the structural basic members."),
                     p("The provided Excel file is not Tidy, and a complex regex analysis is performed to extract the data. Only the structural
                       members are extracted and transformed into groups for post analysis. Dimensions and weight of the members are extracted from
                       the supplied data and the information is used to compare the current acquisition cost to weith ratio."),
                     p("All pricing data is provided in Argentinian pesos. The data is analysed using U.S. currency (Dollar) by transforming all value
                       with the official exchange rate."),

                     h3("Instructions"),
                     p("Data is processed and plots are provided in the different Tabs."), 
                     p("Select a Tab to view the plot and click on the Check Boxes to include the desired members")
                 )
    ),
            
    tabPanel("FreqPlot", 
             # Application title
             titlePanel("Fequency Plot"),
             # Sidebar with Data Selection
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput("Element1", "Select Element Type",
                                        choices = Variables,
                                        selected = Variables,
                                        choiceNames = NULL,
                                        choiceValues = NULL
                     ),
                     width =3
                 ),
                 # Show a plot of the generated ScatterPlot
                 mainPanel(
                     plotOutput("FreqPlot", height = "600px")
                 )
             )
    ),
    
    tabPanel("BoxPlot", 
             # Application title
             titlePanel("Box Plot"),
             # Sidebar with Data Selection
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput("Element2", "Select Element Type",
                                        choices = Variables,
                                        selected = Variables,
                                        choiceNames = NULL,
                                        choiceValues = NULL
                     ),
                     width =3
                 ),
                 # Show a plot of the generated ScatterPlot
                 mainPanel(
                     plotOutput("BoxPlot", height = "600px")
                 )
             )
    ),
    
    tabPanel("ScatterPlot", 
        # Application title
        titlePanel("Element Price/Weight Plot"),
        # Sidebar with Data Selection
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("Element3", "Select Element Type",
                    choices = Variables,
                    selected = Variables,
                    choiceNames = NULL,
                    choiceValues = NULL
                    ),
                width =3
            ),
        
        # Show a plot of the generated ScatterPlot
            mainPanel(
                plotOutput("ScatterPlot", height = "600px")
            )
        )
    )
    )
))




