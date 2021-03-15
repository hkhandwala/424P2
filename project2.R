library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)

#setwd("/Users/hkhandwala06/Desktop/CS424/Project2")

data <- read.csv(file = "egrid2018_data.csv", fill = TRUE, sep = ",", header = TRUE, stringsAsFactors=FALSE)
data00 <- read.csv(file = "egrid2000_data.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE)
data10 <- read.csv(file = "egrid2010_data.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE)

#2018 data
filterData <- subset(data,select=c("STATE","PLANT_NAME","LAT","LON","PL_COAL","PL_OIL","PL_GAS","PL_NUCLEAR", "PL_HYDRO","PL_BIOMASS","PL_WIND", "PL_SOLAR", "PL_GEOTHERMAL","PL_OTHER","PL_NONRENEWABLE","PL_RENEWABLE","PLFUELCT"))

names(filterData)[names(filterData) == "STATE"] <- "STATE"
names(filterData)[names(filterData) == "PLANT_NAME"] <- "PLANTNAME"
names(filterData)[names(filterData) == "PL_COAL"] <- "COAL"
names(filterData)[names(filterData) == "PL_OIL"] <- "OIL"
names(filterData)[names(filterData) == "PL_GAS"] <- "GAS"
names(filterData)[names(filterData) == "PL_NUCLEAR"] <- "NUCLEAR"
names(filterData)[names(filterData) == "PL_HYDRO"] <- "HYDRO"
names(filterData)[names(filterData) == "PL_BIOMASS"] <- "BIOMASS"
names(filterData)[names(filterData) == "PL_WIND"] <- "WIND"
names(filterData)[names(filterData) == "PL_SOLAR"] <- "SOLAR"
names(filterData)[names(filterData) == "PL_GEOTHERMAL"] <- "GEOTHERMAL"
names(filterData)[names(filterData) == "PL_OTHER"] <- "OTHER"
names(filterData)[names(filterData) == "PL_NONRENEWABLE"] <- "NONRENEWABLE"
names(filterData)[names(filterData) == "PL_RENEWABLE"] <- "RENEWABLE"
names(filterData)[names(filterData) == "PLFUELCT"] <- "TYPE"


filterData$COAL <- as.numeric(gsub(",","",filterData$COAL))
filterData$OIL <- as.numeric(gsub(",","",filterData$OIL))
filterData$GAS <- as.numeric(gsub(",","",filterData$GAS))
filterData$NUCLEAR <- as.numeric(gsub(",","",filterData$NUCLEAR))
filterData$HYDRO <- as.numeric(gsub(",","",filterData$HYDRO))
filterData$BIOMASS <- as.numeric(gsub(",","",filterData$BIOMASS))
filterData$WIND <- as.numeric(gsub(",","",filterData$WIND))
filterData$SOLAR <- as.numeric(gsub(",","",filterData$SOLAR))
filterData$GEOTHERMAL <- as.numeric(gsub(",","",filterData$GEOTHERMAL))
filterData$OTHER <- as.numeric(gsub(",","",filterData$OTHER))
filterData$NONRENEWABLE <- as.numeric(gsub(",","",filterData$NONRENEWABLE))
filterData$RENEWABLE <- as.numeric(gsub(",","",filterData$RENEWABLE))
filterData$LAT = as.numeric(filterData$LAT)
filterData$LON = as.numeric(filterData$LON)
filterData$TYPE = as.factor(filterData$TYPE)

filterData <- subset(filterData, filterData$COAL >= 0)
filterData <- subset(filterData, filterData$OIL >= 0)
filterData <- subset(filterData, filterData$GAS >= 0)
filterData <- subset(filterData, filterData$NUCLEAR >= 0)
filterData <- subset(filterData, filterData$HYDRO >= 0)
filterData <- subset(filterData, filterData$BIOMASS >= 0)
filterData <- subset(filterData, filterData$WIND >= 0)
filterData <- subset(filterData, filterData$SOLAR >= 0)
filterData <- subset(filterData, filterData$GEOTHERMAL >= 0)
filterData <- subset(filterData, filterData$OTHER >= 0)
filterData <- subset(filterData, filterData$NONRENEWABLE >= 0)
filterData <- subset(filterData, filterData$RENEWABLE >= 0)
filterData <- subset(filterData, filterData$TYPE !="")

ilData <- subset(filterData, filterData$STATE == "IL")


#2010 data
filterData2 <- subset(data10,select=c("STATE","PLANT_NAME","LAT","LON","PL_COAL","PL_OIL","PL_GAS","PL_NUCLEAR", "PL_HYDRO","PL_BIOMASS","PL_WIND", "PL_SOLAR", "PL_GEOTHERMAL","PL_OTHER","PL_NONRENEWABLE","PL_RENEWABLE","PLFUELCT"))
names(filterData2)[names(filterData2) == "STATE"] <- "STATE"
names(filterData2)[names(filterData2) == "PLANT_NAME"] <- "PLANTNAME"
names(filterData2)[names(filterData2) == "PL_COAL"] <- "COAL"
names(filterData2)[names(filterData2) == "PL_OIL"] <- "OIL"
names(filterData2)[names(filterData2) == "PL_GAS"] <- "GAS"
names(filterData2)[names(filterData2) == "PL_NUCLEAR"] <- "NUCLEAR"
names(filterData2)[names(filterData2) == "PL_HYDRO"] <- "HYDRO"
names(filterData2)[names(filterData2) == "PL_BIOMASS"] <- "BIOMASS"
names(filterData2)[names(filterData2) == "PL_WIND"] <- "WIND"
names(filterData2)[names(filterData2) == "PL_SOLAR"] <- "SOLAR"
names(filterData2)[names(filterData2) == "PL_GEOTHERMAL"] <- "GEOTHERMAL"
names(filterData2)[names(filterData2) == "PL_OTHER"] <- "OTHER"
names(filterData2)[names(filterData2) == "PL_NONRENEWABLE"] <- "NONRENEWABLE"
names(filterData2)[names(filterData2) == "PL_RENEWABLE"] <- "RENEWABLE"
names(filterData2)[names(filterData2) == "PLFUELCT"] <- "TYPE"

filterData2$COAL <- as.numeric(gsub(",","",filterData2$COAL))
filterData2$OIL <- as.numeric(gsub(",","",filterData2$OIL))
filterData2$GAS <- as.numeric(gsub(",","",filterData2$GAS))
filterData2$NUCLEAR <- as.numeric(gsub(",","",filterData2$NUCLEAR))
filterData2$HYDRO <- as.numeric(gsub(",","",filterData2$HYDRO))
filterData2$BIOMASS <- as.numeric(gsub(",","",filterData2$BIOMASS))
filterData2$WIND <- as.numeric(gsub(",","",filterData2$WIND))
filterData2$SOLAR <- as.numeric(gsub(",","",filterData2$SOLAR))
filterData2$GEOTHERMAL <- as.numeric(gsub(",","",filterData2$GEOTHERMAL))
filterData2$OTHER <- as.numeric(gsub(",","",filterData2$OTHER))
filterData2$NONRENEWABLE <- as.numeric(gsub(",","",filterData2$NONRENEWABLE))
filterData2$RENEWABLE <- as.numeric(gsub(",","",filterData2$RENEWABLE))
filterData2$LAT = as.numeric(filterData2$LAT)
filterData2$LON = as.numeric(filterData2$LON)
filterData2$TYPE = as.factor(filterData2$TYPE)

filterData2 <- subset(filterData2, filterData2$COAL >= 0)
filterData2 <- subset(filterData2, filterData2$OIL >= 0)
filterData2 <- subset(filterData2, filterData2$GAS >= 0)
filterData2 <- subset(filterData2, filterData2$NUCLEAR >= 0)
filterData2 <- subset(filterData2, filterData2$HYDRO >= 0)
filterData2 <- subset(filterData2, filterData2$BIOMASS >= 0)
filterData2 <- subset(filterData2, filterData2$WIND >= 0)
filterData2 <- subset(filterData2, filterData2$SOLAR >= 0)
filterData2 <- subset(filterData2, filterData2$GEOTHERMAL >= 0)
filterData2 <- subset(filterData2, filterData2$OTHER >= 0)
filterData2 <- subset(filterData2, filterData2$NONRENEWABLE >= 0)
filterData2 <- subset(filterData2, filterData2$RENEWABLE >= 0)
filterData2 <- subset(filterData2, filterData2$TYPE !="")

ilData2 <- subset(filterData2, filterData2$STATE == "IL")

#2000 data
filterData3 <- subset(data00,select=c("STATE","PLANT_NAME","LAT","LON","PL_COAL","PL_OIL","PL_GAS","PL_NUCLEAR", "PL_HYDRO","PL_BIOMASS","PL_WIND", "PL_SOLAR", "PL_GEOTHERMAL","PL_OTHER","PL_NONRENEWABLE","PL_RENEWABLE","PLFUELCT"))
names(filterData3)[names(filterData3) == "STATE"] <- "STATE"
names(filterData3)[names(filterData3) == "PLANT_NAME"] <- "PLANTNAME"
names(filterData3)[names(filterData3) == "PL_COAL"] <- "COAL"
names(filterData3)[names(filterData3) == "PL_OIL"] <- "OIL"
names(filterData3)[names(filterData3) == "PL_GAS"] <- "GAS"
names(filterData3)[names(filterData3) == "PL_NUCLEAR"] <- "NUCLEAR"
names(filterData3)[names(filterData3) == "PL_HYDRO"] <- "HYDRO"
names(filterData3)[names(filterData3) == "PL_BIOMASS"] <- "BIOMASS"
names(filterData3)[names(filterData3) == "PL_WIND"] <- "WIND"
names(filterData3)[names(filterData3) == "PL_SOLAR"] <- "SOLAR"
names(filterData3)[names(filterData3) == "PL_GEOTHERMAL"] <- "GEOTHERMAL"
names(filterData3)[names(filterData3) == "PL_OTHER"] <- "OTHER"
names(filterData3)[names(filterData3) == "PL_NONRENEWABLE"] <- "NONRENEWABLE"
names(filterData3)[names(filterData3) == "PL_RENEWABLE"] <- "RENEWABLE"
names(filterData3)[names(filterData3) == "PLFUELCT"] <- "TYPE"

filterData3$COAL <- as.numeric(gsub(",","",filterData3$COAL))
filterData3$OIL <- as.numeric(gsub(",","",filterData3$OIL))
filterData3$GAS <- as.numeric(gsub(",","",filterData3$GAS))
filterData3$NUCLEAR <- as.numeric(gsub(",","",filterData3$NUCLEAR))
filterData3$HYDRO <- as.numeric(gsub(",","",filterData3$HYDRO))
filterData3$BIOMASS <- as.numeric(gsub(",","",filterData3$BIOMASS))
filterData3$WIND <- as.numeric(gsub(",","",filterData3$WIND))
filterData3$SOLAR <- as.numeric(gsub(",","",filterData3$SOLAR))
filterData3$GEOTHERMAL <- as.numeric(gsub(",","",filterData3$GEOTHERMAL))
filterData3$OTHER <- as.numeric(gsub(",","",filterData3$OTHER))
filterData3$NONRENEWABLE <- as.numeric(gsub(",","",filterData3$NONRENEWABLE))
filterData3$RENEWABLE <- as.numeric(gsub(",","",filterData3$RENEWABLE))
filterData3$LAT = as.numeric(filterData3$LAT)
filterData3$LON = as.numeric(filterData3$LON)
filterData3$TYPE = as.factor(filterData3$TYPE)

filterData3 <- subset(filterData3, filterData3$COAL >= 0)
filterData3 <- subset(filterData3, filterData3$OIL >= 0)
filterData3 <- subset(filterData3, filterData3$GAS >= 0)
filterData3 <- subset(filterData3, filterData3$NUCLEAR >= 0)
filterData3 <- subset(filterData3, filterData3$HYDRO >= 0)
filterData3 <- subset(filterData3, filterData3$BIOMASS >= 0)
filterData3 <- subset(filterData3, filterData3$WIND >= 0)
filterData3 <- subset(filterData3, filterData3$SOLAR >= 0)
filterData3 <- subset(filterData3, filterData3$GEOTHERMAL >= 0)
filterData3 <- subset(filterData3, filterData3$OTHER >= 0)
filterData3 <- subset(filterData3, filterData3$NONRENEWABLE >= 0)
filterData3 <- subset(filterData3, filterData3$RENEWABLE >= 0)
filterData3 <- subset(filterData3, filterData3$TYPE !="")

ilData3 <- subset(filterData3, filterData3$STATE == "IL")

col <- c("black", "darkgrey", "lightgreen", "green","blue","brown","cyan","yellow","orange", "purple")
source <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other")
mapColor <- c(source = col)

states <- c(
  "USA",
  "Alaska","Alabama","Arkansas","Arizona","California","Colorado","Connecticut", "District of Columbia","Delaware","Florida","Georgia","Hawaii",
  "Idaho","Iowa","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","MaryLand","Maine","Michigan","Minnesota","Missouri","Mississippi",
  "Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Orgeon",
  "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennesse","Texas", "Utah", "Virginia", "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming"
)
   

ui <- fluidPage(
  navbarPage("CS 424 - Project 2",
             tabPanel("Map of Illinois", 
                      fluidRow(
                        column(12,
                               fluidRow(box(title = "Illinois Power Plants in 2018", solidHeader = TRUE, 
                                            status = "primary", width = 50,
                                            column(width=10,
                                                   column(width=2,checkboxInput('AllEnergy','All',TRUE)),
                                                   column(width=2,checkboxInput('CoalEnergy','Coal',FALSE)),
                                                   column(width=2,checkboxInput('OilEnergy','Oil',FALSE)),
                                                   column(width=2,checkboxInput('GasEnergy','Gas',FALSE)),
                                                   column(width=2,checkboxInput('NuclearEnergy','Nuclear',FALSE)),
                                                   column(width=2,checkboxInput('HydroEnergy','Hydro',FALSE)),
                                                   column(width=2,checkboxInput('BiomassEnergy','Biomass',FALSE)),
                                                   column(width=2,checkboxInput('WindEnergy','Wind',FALSE)),
                                                   column(width=2,checkboxInput('SolarEnergy','Solar',FALSE)),
                                                   column(width=2,checkboxInput('GeothermalEnergy','Geothermal',FALSE)),
                                                   column(width=2,checkboxInput('OtherEnergy','Other',FALSE))),
                                                   column(width=2,checkboxInput('REnergy','Renewable',FALSE)),
                                                   column(width=2,checkboxInput('NREnergy','Non-Renewable',FALSE)),
                                            leafletOutput("plot1",height=600)))
                        )
                      )
              ),
             tabPanel("Compare 2 States", 
                      fluidRow(
                        column(6,
                               fluidRow(
                                 box(title = textOutput('title1'), solidHeader = TRUE, status = "primary", width =30,
                                     column(width=8,
                                            column(width=4,checkboxInput('AllEnergy2','All',TRUE)),
                                            column(width=4,checkboxInput('CoalEnergy2','Coal',FALSE)),
                                            column(width=4,checkboxInput('OilEnergy2','Oil',FALSE)),
                                            column(width=4,checkboxInput('GasEnergy2','Gas',FALSE)),
                                            column(width=4,checkboxInput('NuclearEnergy2','Nuclear',FALSE)),
                                            column(width=4,checkboxInput('HydroEnergy2','Hydro',FALSE)),
                                            column(width=4,checkboxInput('BiomassEnergy2','Biomass',FALSE)),
                                            column(width=4,checkboxInput('WindEnergy2','Wind',FALSE)),
                                            column(width=4,checkboxInput('SolarEnergy2','Solar',FALSE)),
                                            column(width=4,checkboxInput('GeothermalEnergy2','Geothermal',FALSE)),
                                            column(width=4,checkboxInput('OtherEnergy2','Other',FALSE))),
                                            column(width=4,checkboxInput('REnergy2','Renewable',FALSE)),
                                            column(width=4,checkboxInput('NREnergy2','Non-Renewable',FALSE)),
                                     column(4,selectInput('State', 'Select State:',
                                                          choices=states,selected = "Illinois")),
                                     column(4, selectInput('Year', 'Select Year:',
                                                           choices=c("2000","2010","2018"),selected = "2018")),
                                     
                                     leafletOutput("plot2",height = 300)
                                 )
                               )
                           ),
                        column(6,
                               fluidRow(
                                 box(title = textOutput('title2'), solidHeader = TRUE, status = "primary", width = 30,
                                     column(width=8,
                                            column(width=4,checkboxInput('AllEnergy3','All',TRUE)),
                                            column(width=4,checkboxInput('CoalEnergy3','Coal',FALSE)),
                                            column(width=4,checkboxInput('OilEnergy3','Oil',FALSE)),
                                            column(width=4,checkboxInput('GasEnergy3','Gas',FALSE)),
                                            column(width=4,checkboxInput('NuclearEnergy3','Nuclear',FALSE)),
                                            column(width=4,checkboxInput('HydroEnergy3','Hydro',FALSE)),
                                            column(width=4,checkboxInput('BiomassEnergy3','Biomass',FALSE)),
                                            column(width=4,checkboxInput('WindEnergy3','Wind',FALSE)),
                                            column(width=4,checkboxInput('SolarEnergy3','Solar',FALSE)),
                                            column(width=4,checkboxInput('GeothermalEnergy3','Geothermal',FALSE)),
                                            column(width=4,checkboxInput('OtherEnergy3','Other',FALSE))),
                                            column(width=4,checkboxInput('REnergy3','Renewable',FALSE)),
                                            column(width=4,checkboxInput('NREnergy3','Non-Renewable',FALSE)),
                                     column(4,selectInput('State1', 'Select State:',
                                                          choices=states,selected = "USA")),
                                     column(4, selectInput('Year1', 'Select Year:',
                                                           choices=c("2000","2010","2018"),selected = "2018")),
                                     leafletOutput("plot3",height = 300)
                                 )
                               )
                        )
                      )
            ),
            tabPanel("Map of USA", 
                     fluidRow(
                       column(12,
                              fluidRow(box(title = "USA Power Plants", solidHeader = TRUE, 
                                           status = "primary",width = 100,
                                           column(width=10,
                                                  column(width=2,checkboxInput('AllEnergy3','All',TRUE)),
                                                  column(width=2,checkboxInput('CoalEnergy3','Coal',FALSE)),
                                                  column(width=2,checkboxInput('OilEnergy3','Oil',FALSE)),
                                                  column(width=2,checkboxInput('GasEnergy3','Gas',FALSE)),
                                                  column(width=2,checkboxInput('NuclearEnergy3','Nuclear',FALSE)),
                                                  column(width=2,checkboxInput('HydroEnergy3','Hydro',FALSE)),
                                                  column(width=2,checkboxInput('BiomassEnergy3','Biomass',FALSE)),
                                                  column(width=2,checkboxInput('WindEnergy3','Wind',FALSE)),
                                                  column(width=2,checkboxInput('SolarEnergy3','Solar',FALSE)),
                                                  column(width=2,checkboxInput('GeothermalEnergy3','Geothermal',FALSE)),
                                                  column(width=2,checkboxInput('OtherEnergy3','Other',FALSE))),
                                                  column(width=2,checkboxInput('REnergy3','Renewable',FALSE)),
                                                  column(width=2,checkboxInput('NREnergy3','Non-Renewable',FALSE)),
                                           column(2,selectInput('State2', 'Select State:',
                                                                choices=states,selected = "USA")),
                                           column(2, selectInput('Year2', 'Select Year:',
                                                                 choices=c("2000","2010","2018"),selected = "2018")),
                                           leafletOutput("plot4",height=600)
                                  )
                              )
                      )
                  )
            ),
             tabPanel("About",
                     p("This web app was created by Herrit Khandwala on 3/13/2021. The data used in this web app is from https://www.epa.gov/egrid/download-data"),
                     p("The files are titled eGRID2018v2 Data File (XLSX), and eGRID historical files (1996-2016) zip folder"),
                     p("The web app uses leaflet maps to plot different power plants in the United States. You can filter the data based on the type of energy source, given a specific year.")
             )
  )
  )

server <- function(input, output, session){
  output$title1 <- renderText(paste("Power Plants in", input$State, "in",input$Year))
  output$title2 <- renderText(paste("Power Plants in", input$State1, "in",input$Year1))


}

shinyApp(ui, server)
  