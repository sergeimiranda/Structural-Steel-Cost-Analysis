#TITLE: Script for Data Analysis from Structural Steel Provider
##AUTHOR: Sergio Miranda (sergeimiranda@hotmail.com)
##DATE: 07-02-2022


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$FreqPlot <- renderPlot({
        PlotData1 <- MetalData %>% filter(Seccion %in% input$Element1)
        ##frequency plot (histogram) to get a view of the price range for the data set:
            ggplot(PlotData1,aes(PrecioxPeso_uss)) + geom_histogram(aes(fill = Seccion))+ geom_freqpoly()+
                labs(x = "U$S/Kg", y = "count", title = "Member Cost Distribution") +
                guides(fill = guide_legend(ncol = 1))
    })
    
    output$BoxPlot <- renderPlot({
        PlotData2 <- MetalData %>% filter(Seccion %in% input$Element2)
        ## a Boxplot give a better price distribution for each section
        ggplot(PlotData2,aes(PrecioxPeso_uss,Seccion, fill = Seccion)) + geom_boxplot() + 
            labs(x = "u$s/Kg", y = " ", title = "Cost/Weight Per Element Type") +
            theme(legend.position = "none")
    })

    output$ScatterPlot <- renderPlot({
        PlotData3 <- MetalData %>% filter(Seccion %in% input$Element3)
        ##Scatterplot
            ggplot(PlotData3,aes(1:dim(PlotData3)[1],PrecioxPeso_uss,color = Seccion)) + geom_point(size = 2) +
                guides(color = guide_legend(ncol = 1)) +
                labs(x = "Item", y = "u$s/Kg", title = "Cost/Weight Distribution") +
                theme_ft_rc() 
    })
    
})
