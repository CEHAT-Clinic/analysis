# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny,
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(plotly)
library(tidyverse)
library(zoo)
library(gridExtra)
library(PurpleAirCEHAT)
library(lubridate)
library(shinythemes)

sensors <- hourlyPA(cleanPA(read.csv("december2020_readings.csv")),FALSE)
#set the max file size to be 1000 Mb
options(shiny.maxRequestSize = 10000*1024^2)


# Define UI

ui <- fluidPage(theme = shinytheme("cerulean"),

    # App title ----
    titlePanel("Interactive Data Analysis Report"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
            tags$h3("Choose data:"),

            # Input: Select a file ----
            fileInput("file1", "Choose PurpleAir CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            fileInput("file2", "Choose AQMD CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), #sidebar panel


            selectInput("sensor", label = "View sensor:",
                        choices = sensors$names, selected = "Sensor: SCSG-14"),

            selectInput("n_breaks", label = "Number of bins:",
                        choices = c(4, 8, 16, 24), selected = 8),

            selectInput("month", label = "Choose month(s):",
                        choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "November", "December"), selected = "December"),

            uiOutput("dateRange1"),

            uiOutput("dateRange2"),

            uiOutput("date1")
        ),
        mainPanel(

            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Statistical Analysis",
                                 h2("Overview"),
                                 p("Select your "), em("datafile"), p(" to begin!"),
                                 br(),
                                 p("For an introduction and live examples, visit the ",
                                   a("Shiny homepage.",
                                     href = "http://shiny.rstudio.com")),
                                 br(),
                                 h2("Features"),
                                 p("- Build useful web applications with only a few lines of code-no JavaScript required."),
                                 p("- Shiny applications are automatically 'live' in the same way that ",
                                   strong("spreadsheets"),
                                   " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser."),

                                 h4("Output 1: Table"),

                                 #Output: Data file ----
                                 tableOutput("contents"),

                                 #Plotly plots

                                 h4("Output 2: Plots"),

                                 plotlyOutput(outputId = "density"),

                                 plotlyOutput(outputId = "overThresholdSG"),

                                 #plotOutput(outputId = "sidebyside"),

                                 fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "avgs1"), plotOutput(outputId = "avgs2"))
                                 )
                            ), #-tabPanel 1


                        tabPanel("Sensor Summaries",

                                 h2("Graphs for different Sensors"),
                                 br(),
                                 plotlyOutput(outputId="highlow"),
                                 br(),
                                 plotOutput( outputId = "hiloHist"),
                                 br(),
                                 plotlyOutput(outputId = "overThresholdSensor")
                        ),
                        tabPanel("Comparisons",
                                 h2("South Gate PM2.5 vs. other areas in LA "),
                                 br(),
                                 h2("The Data"),
                                 p("We are specifically comparing the South Gate data against AQMD data. To find datasets that you can use, visit the",
                                   a("AB 617 Community Air Monitoring website.",
                                     href = "http://xappprod.aqmd.gov/AB617CommunityAirMonitoring/")),
                                 h2("Visuals"),
                                 p("After conducting t tests on these data sets, we have found that:"),
                                 strong(textOutput("ttests")),
                                 p("We have also provided a box plot and a bar chart. Through these visuals, we hope that you can get a sense where South Gate's PM2.5 levels stand compared to levels in other parts of LA."),
                                 br(),
                                 plotlyOutput(outputId = "compareBoxplot"),
                                 br(),
                                 plotlyOutput(outputId = "compareBar")
                        ),
                        tabPanel("Interpolation and Sensor Placement",

                                 mainPanel(
                                     plotlyOutput("prediction"),

                                     plotlyOutput("variance"),

                                     plotlyOutput("stdev")

                                 )


                                 ) #- tabPanel 3
            )
        )
    )
)




# Define server function
server <- function(input, output) {

    output$dateRange1 <- renderUI({
        req(input$file1)
        dateRangeInput("dates1", "Select the date range:",
                       start =
                           as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"), # Start
                       end =
                           as.character(format(as.Date(max(PAfull()$timestamp))),"yyyy-mm-dd"), # End
                       min =
                           as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"),
                       max =
                           as.character(format(as.Date(max(PAfull()$timestamp))),"yyyy-mm-dd"),
                       format = "yyyy-mm-dd")

    })

    output$dateRange2 <- renderUI({
        req(input$file1)
        dateRangeInput("dates2", "Select the date range:",
                       start =
                           as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"), # Start
                       end =
                           as.character(format(as.Date(max(PAfull()$timestamp))),"yyyy-mm-dd"), # End
                       min =
                           as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"),
                       max =
                           as.character(format(as.Date(max(PAfull()$timestamp))),"yyyy-mm-dd"),
                       format = "yyyy-mm-dd")

    })

    output$date1 <- renderUI({
        req(input$file1)
        dateInput("date1", "Select the date range:",
                  value =
                      as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"), # Start
                  min =
                      as.character(format(as.Date(min(PAfull()$timestamp))),"yyyy-mm-dd"),
                  max =
                      as.character(format(as.Date(max(PAfull()$timestamp))),"yyyy-mm-dd"),
                  format = "yyyy-mm-dd")

    })

    PAfull <- reactive({
        req(input$file1)
        messyPA <- read.csv(input$file1$datapath)

        PAfull <- PurpleAirCEHAT::cleanPA(messyPA)

        return(PAfull)

    })
    
    newPAfull <- reactive({
        req(input$file1)
        messyPA <- read.csv(input$file1$datapath)
        
        PAfull <- PurpleAirCEHAT::newCleanPA(messyPA)
        
        return(PAfull)
    })

    PAhourly <- reactive({
        req(input$file1)
        PAfull <- PAfull()

        PAhourly <- PurpleAirCEHAT::hourlyPA(PAfull)

        return(PAhourly)
    })


    summarySG <-reactive({
        req(input$file1)
        
        
        PAhourly <- PAhourly()

        avgSG <- PurpleAirCEHAT::summarySG(PAhourly)
        return(avgSG)
    })


    PAhi_lo <- reactive({
        req(input$file1)
        PAhourly <- PAhourly()

        PAhi_lo <- PurpleAirCEHAT::highslows(PAhourly)

        return(PAhi_lo)
    })


    sensors <- reactive({
        req(input$file1)
        PAfull <- PAfull()
        sensors <- unique(PAfull[,c("longitude","latitude")])
        #adding the names of 11 sensors
        #to add another sensor, end the previous line with a comma, and input the following info for the new sensor:
        #     longitude == -118.1965 & latitude == 33.93868 ~ "Sensor: CEHAT 8",
        #     longitude == <longitude> & latitude == <latitude> ~ "<name>")


        sensors <- dplyr::mutate(sensors,
                                 names = case_when(longitude == -118.1901 & latitude == 33.94106 ~ "Sensor: SCSG-14",
                                                   longitude == -118.1953 & latitude == 33.94354 ~ "Sensor: CEHAT 7-CD",
                                                   longitude == -118.2201 & latitude == 33.94178 ~ "Sensor: CEHAT-01",
                                                   longitude == -118.1985 & latitude == 33.96063 ~ "Sensor: CEHAT 5",
                                                   longitude == -118.2184 & latitude == 33.96757 ~ "Sensor: CCA Mountainview and Olive",
                                                   longitude == -118.2146 & latitude == 33.95058 ~ "Sensor: CEHAT-St. Helens-STEM",
                                                   longitude == -118.1685 & latitude == 33.93553 ~ "Sensor: SCSG_15",
                                                   longitude == -118.1673 & latitude == 33.92019 ~ "Sensor: SCSG_20",
                                                   longitude == -118.2225 & latitude == 33.95094 ~ "Sensor: CEHAT 7-SE",
                                                   longitude == -118.1965 & latitude == 33.93868 ~ "Sensor: CEHAT 8",
                                                   longitude == -118.2181 & latitude == 33.96192 ~ "Sensor: CEHAT 3")
        )

        return(sensors)
    })


    matchingDays <- reactive({
        req(input$file1)
        req(input$file2)

        aqmd <- read.csv(input$file2$datapath)
        PAhourly <- PAhourly()
        avgSG <- summarySG()

        matchingDays <- PurpleAirCEHAT::matchingDays(avgSG,aqmd)

        return(matchingDays)

    })

    overEPAthreshold <- reactive({
        req(input$file1)
        req(input$sensor)

        PAhourly <- PAhourly()
        Sensor <- PAhourly[PAhourly$names == input$sensor,]
        overEPA <- PurpleAirCEHAT::overEPA(Sensor)

        return(overEPA)
    })

    overEPAthresholdSG <- reactive({
        req(input$file1)

        summarySG<- summarySG()
        names(summarySG)[2] <- "PM2.5"
        overEPA <- PurpleAirCEHAT::overEPA(summarySG)

        return(overEPA)
    })

    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        #messyPA <- read.csv(input$main$datapath)
        PAhourly <- PAhourly()

        return(head(PAhourly))
    })

    output$ttests <- renderText({
        req(input$file1)
        req(input$file2)
        matchingDays<- matchingDays()
        nameOfCity <- PurpleAirCEHAT::gettingCityName(read.csv(input$file2$datapath))
        result <- PurpleAirCEHAT::ttests(matchingDays$otherCityPM,matchingDays$southGatePM,nameOfCity)
        return(result)
    })

    output$highlow <- renderPlotly({
        req(input$file1)

        PAhi_lo <- PAhi_lo()

        wessy_pal <- c("high"="#C93312","low"="#899DA4")

        hilo <- ggplot(data=PAhi_lo[PAhi_lo$names == input$sensor,], aes(x=day, y=PM2.5, group = day)) +
            geom_line(lwd=1)+
            geom_point(data=PAhi_lo[PAhi_lo$type == "high" & PAhi_lo$PM2.5<=300  & PAhi_lo$names == input$sensor, ],
                       aes(x=day, y=PM2.5, group = type, col="high"), size=5)+
            geom_point(data=PAhi_lo[PAhi_lo$type == "low" & PAhi_lo$names == input$sensor, ],
                       aes(x=day, y=PM2.5, group = type, col="low"), size=5)+
            scale_color_brewer(palette="Dark2")+
            labs(x = 'Day', y = 'PM25') +
            scale_colour_manual(name="Type",values=wessy_pal, guide = guide_legend(override.aes=aes(fill=NA)) ) +    scale_fill_manual(name="Type",values=wessy_pal) +
            ggtitle(input$sensor) +
            theme_minimal()

        ggplotly(hilo)
    })

    output$hiloHist <- renderPlot({
        req(input$file1)
        PAhi_lo <- PAhi_lo()

        mean <- mean(PAhi_lo$PM2.5[PAhi_lo$names==input$sensor & PAhi_lo$type == "high"])
        std <- sqrt(var(PAhi_lo$PM2.5[PAhi_lo$names==input$sensor & PAhi_lo$type == "high"]))

        hist(PAhi_lo$PM2.5[PAhi_lo$names==input$sensor & PAhi_lo$type == "high"], probability = TRUE, breaks = as.numeric(input$n_breaks),
             xlab = "PM25", main = "Histogram of High Values")

        curve(dnorm(x, mean=mean, sd=std), col="darkblue", lwd=2, add=TRUE) #fits a normal curve to observe normaility in the distribution
    })

    output$density <- renderPlotly({
        req(input$file1)

        PAhi_lo <- PAhi_lo()

        dens <- ggplot(PAhi_lo[PAhi_lo$type == "high" & PAhi_lo$PM2.5<=300 & months(PAhi_lo$timestamp) == input$month,], aes(x=hour, group = timeofday))+
            geom_histogram(aes(y=after_stat(count/nrow(PAhi_lo[PAhi_lo$type == "high" & PAhi_lo$PM2.5<=300,])),                color=timeofday, fill=timeofday),  alpha=0.7, stat="count", bins=4, lwd=1)+
            #geom_point(aes(, ycolor=timeofday, alpha=0.01))+
            geom_density(alpha = 0.2, fill = "grey")+
            labs(x = "Hour", y = "Density") +
            ggtitle("Density of Peak PM2.5 Values Over 24-hour Period")+
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2")+
            theme_minimal()
        ggplotly(dens)
    })


    output$overThresholdSG <- renderPlotly({
        req(input$file1)

        # finding the number of days in the data frame
        summarySG<- summarySG()
        numDays <- length(unique(lubridate::mday(summarySG$timestamp)))

        # finding the days over EPA threshold
        overEPA <- overEPAthresholdSG()
        ourData <- PurpleAirCEHAT::overEPA_hist(overEPA,numDays)

        if(overEPA$timestamp > 0) {
            epahist <- ggplot(ourData, aes(x=day,y=freq)) +
                geom_bar(position="dodge", stat="identity") +
                ggtitle("Days over EPA threshold in South Gate")}

        else{ epahist <- ggtitle("No days are over the EPA threshold for this month.") }
        ggplotly(epahist)
    })


    output$overThresholdSensor <- renderPlotly({
        req(input$file1)
        req(input$sensor)

        # finding the number of days in the data frame
        PAhourly <- PAhourly()
        Sensor <- PAhourly[PAhourly$names == input$sensor,]
        numDays <- length(unique(lubridate::mday(Sensor$timestamp)))

        overEPA <- overEPAthreshold()
        ourData <- PurpleAirCEHAT::overEPA_hist(overEPA,numDays)
        if(overEPA$timestamp > 0) {
            epahist <- ggplot(ourData, aes(x=day,y=freq)) +
                geom_bar(position="dodge", stat="identity") +
                ggtitle(paste("Days over EPA threshold for",input$sensor,"in South Gate",sep=" "))}

        else{ epahist <- ggtitle("No days are over the EPA threshold for this month.") }
        ggplotly(epahist)
    })

    output$compareBoxplot<- renderPlotly({
        req(input$file1)
        req(input$file2)

        matchingDays <- matchingDays()

        nameOfCity <- PurpleAirCEHAT::gettingCityName(read.csv(input$file2$datapath))

        boxplot <- ggplot(PurpleAirCEHAT::compareDataDF(matchingDays,nameOfCity), aes(x= city, y=PM2.5)) +
            geom_boxplot(outlier.colour="red", outlier.size = 8, fill=c("darkolivegreen3","darksalmon"),notch=T) +
            ggtitle(paste("PM2.5 in",nameOfCity,"vs South Gate",sep=" "))

        ggplotly(boxplot)
    })

    output$compareBar <- renderPlotly({
        req(input$file1)
        req(input$file2)

        matchingDays <- matchingDays()

        nameOfCity <- PurpleAirCEHAT::gettingCityName(read.csv(input$file2$datapath))

        stacked <- ggplot(PurpleAirCEHAT::compareDataDF(matchingDays,nameOfCity), aes(fill=city, y=PM2.5, x=day)) +
            geom_bar(position="dodge", stat="identity") +
            geom_col(width = 0.7, position = position_dodge(0.9)) +
            ggtitle("Stacked Bar Chart of PM2.5 values")

        ggplotly(stacked)
    })

    output$avgs1 <- renderPlot({
        req(input$file1)
        req(input$dates1)
        avgSG <- summarySG()

        #saves the start and end dates as a single vector, with start at index 1 and end at 2
        dates <- c(input$dates1)

        #set title of  plot
        Title <- paste("8 Hour Averages From", paste(as.character(input$dates1), collapse = " to "))

        avgs <- zoo::rollmean(avgSG$average_PM2.5[lubridate::date(avgSG$timestamp) >= toString(dates[1]) & lubridate::date(avgSG$timestamp) <= toString(dates[2])], k=8)
        plot(avgs,type="l", main= Title)
    })

    output$avgs2 <- renderPlot({
        req(input$file1)

        avgSG <- summarySG()

        #saves the start and end dates as a single vector, with start at index 1 and end at 2
        dates <- c(input$dates2)

        #set title of  plot
        Title <- paste("8 Hour Averages From", paste(as.character(input$dates2), collapse = " to "))

        avgs <- zoo::rollmean(avgSG$average_PM2.5[lubridate::date(avgSG$timestamp) >= toString(dates[1]) & lubridate::date(avgSG$timestamp) <= toString(dates[2])], k=8)
        plot(avgs,type="l",main=Title)
    })

    output$prediction <- renderPlotly({
        req(input$date1)

        PAhourly <- PAhourly()

        autoDF <- data.frame(PurpleAirCEHAT::krigePA(PAhourly, as_datetime(input$date1)))

        autoPlot <- ggplot() + geom_tile(autoDF, mapping = aes(x,y,fill=var1.pred), alpha=0.90) +
            geom_point(sliceSP@data[,c('PM2.5',"longitude","latitude")], color ="black", size=2, pch=21, fill="red" , mapping =aes(longitude, latitude), inherit.aes = TRUE) +
            coord_equal() +
            scale_fill_continuous(type = "viridis") +
            labs(x = "longitude", y="latitude")+
            theme_bw() +
            ggtitle("Simple Kriging PM2.5 Predictions")

        ggplotly(autoPlot)

    })

    output$variance <- renderPlotly({
        req(input$date1)

        PAhourly <- PAhourly()

        autoDF <- data.frame(PurpleAirCEHAT::krigePA(PAhourly, as_datetime(input$date1)))

        autoVars <- ggplot() + geom_tile(autoDF, mapping = aes(x,y,fill=var1.var), alpha=0.90) +
            geom_point(sliceSP@data[,c('PM2.5',"longitude","latitude")], color ="black", size=2, pch=21, fill="red" , mapping =aes(longitude, latitude), inherit.aes = TRUE) +
            coord_equal() +
            scale_fill_continuous(type = "viridis") +
            labs(x = "longitude", y="latitude")+
            theme_bw() +
            ggtitle("Simple Kriging PM2.5 Variance")

        ggplotly(autoVars)

    })

    output$stdev <- renderPlotly({
        req(input$date1)

        PAhourly <- PAhourly()

        autoDF <- data.frame(PurpleAirCEHAT::krigePA(PAhourly, as_datetime(input$date1)))

        autoStDev <- ggplot() + geom_tile(autoDF, mapping = aes(x,y,fill=var1.stdev), alpha=0.90) +
            geom_point(sliceSP@data[,c('PM2.5',"longitude","latitude")], color ="black", size=2, pch=21, fill="red" , mapping =aes(longitude, latitude), inherit.aes = TRUE) +
            coord_equal() +
            scale_fill_continuous(type = "viridis") +
            labs(x = "longitude", y="latitude")+
            theme_bw() +
            ggtitle("Simple Kriging PM2.5 Standard Deviation")

        ggplotly(autoStDev)

    })


} #--server


#Create Shiny App
shinyApp(ui= ui, server = server)
