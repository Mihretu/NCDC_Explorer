#load libraries
library(shiny)
library(shinyBS)
library(leaflet)
library(rnoaa)
library(ggplot2)
library(dygraphs)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(xts)
library(data.table)
#define user API key
options(noaakey = "<paste your API key here>")

fluidPage(
  # Application title
  titlePanel("NCDC Explorer"),
  sidebarLayout(
    # Sidebar query options
    sidebarPanel(width = 3,
                 a('Documentation',href= 'https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/'),
                 selectInput('datasets',label = 'Datasets',choices='GHCND'),
                 radioButtons('searchCriteria','Search By',choices=c('designation','coordinates'),selected = 'designation',inline = TRUE),
                 bsTooltip(id = 'searchCriteria', title = "NCDC API rate limit = 1000, if rate limit of stations is exceeded, stations will not be shown.  Zoom in to query additional stations", 
                           placement = "right", trigger = "hover"),
                 conditionalPanel("input.searchCriteria=='designation'",
                                  selectInput('country','Country',choices='United States'),
                                  conditionalPanel(
                                    condition = "input.country == 'United States'",
                                    selectInput('state','State',choices='Massachusetts')
                                  )
                 ),
                 conditionalPanel("input.searchCriteria=='coordinates'",
                                  numericInput(inputId = 'minlat','Lat Min',value = NULL),
                                  numericInput(inputId = 'maxlat','Lat Max',value = NULL),
                                  numericInput(inputId = 'minlong','Lng Min',value = NULL),
                                  numericInput(inputId = 'maxlong','Lng Max',value = NULL)
                 ),
                 helpText('Select a station in the data table or click on a station in the map to populate the available data types for a particular station'),
                 selectInput('datatypes','Datatypes',choices = NULL,multiple = TRUE),
                 dateRangeInput('dateRange',label='Date Range',start = '2010-01-01',end='2010-12-31'),
                 bsTooltip(id = 'dateRange', title = 'NORMAL Datasets use the year 2010, all others use explicit years', 
                           placement = "right", trigger = "hover"),
                 actionButton('queryData','Query Data'),
                 downloadButton('exportData','Export Data'),
                 textOutput('results')
    ),
    
    # stations returned
    mainPanel(
      tabsetPanel(id = 'tabs',
                  tabPanel(title = 'Map',value = 'Map',
                           fluidRow(column(4,sliderInput("markersize","Marker Size",min=0,max=50000,step=100,value = 5000,post = 'm')),
                                    column(4,selectInput("colors", "Color Scheme",choices =
                                                           c('viridis',rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                                         selected ='viridis')),
                                    column(4,selectInput('basemap','Basemap',choices = 
                                                           c("CartoDB.Positron","CartoDB.DarkMatter", "Esri","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Esri.WorldImagery",
                                                             "Esri.WorldTerrain","Esri.WorldShadedRelief","Esri.WorldPhysical","Esri.OceanBasemap","Esri.NatGeoWorldMap" )
                                                         ,selected = "CartoDB.Positron"))),
                           leafletOutput('map')),
                  tabPanel(title = 'Tabular',value='Tabular',div(DT::dataTableOutput("stations"),style = "font-size: 75%; width: 75%")),
                  tabPanel(title = 'Data',value='Data',dygraphOutput('plot')),
                  tabPanel(title='Citations',value='Citations',
                           p('Scott Chamberlain (2017). rnoaa: \'NOAA\' Weather Data from R. R package version 0.7.0.
                             https://CRAN.R-project.org/package=rnoaa'),
                           br(),
                           p('Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework
                             for R. R package version 1.0.3. https://CRAN.R-project.org/package=shiny'),
                           br(),
                           p('Joe Cheng, Bhaskar Karambelkar and Yihui Xie (2017). leaflet: Create Interactive Web Maps with the JavaScript
                             \'Leaflet\' Library. R package version 1.1.0. https://CRAN.R-project.org/package=leaflet'),
                           br(),
                           p('Hadley Wickham (2017). tidyverse: Easily Install and Load \'Tidyverse\' Packages. R package version 1.1.1.
                             https://CRAN.R-project.org/package=tidyverse'),
                           br(),
                           p('H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.'),
                           br(),
                           p('Matt Dowle and Arun Srinivasan (2017). data.table: Extension of `data.frame`. R package version 1.10.4.
                             https://CRAN.R-project.org/package=data.table'),
                           br(),
                           p('Dan Vanderkam, JJ Allaire, Jonathan Owen, Daniel Gromer, Petr Shevtsov and Benoit Thieurmel (2017). dygraphs:
  Interface to \'Dygraphs\' Interactive Time Series Charting Library. R package version 1.1.1.4.
  https://CRAN.R-project.org/package=dygraphs'),
                           br(),
                           p('Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61.
  https://CRAN.R-project.org/package=shinyBS'),
                           br(),
                           p('Yihui Xie (2016). DT: A Wrapper of the JavaScript Library \'DataTables\'. R package version 0.2.
  https://CRAN.R-project.org/package=DT'),
                           br(),
                           p('Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package version 1.1-2.
  https://CRAN.R-project.org/package=RColorBrewer'),
                           br(),
                           p('Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical
  Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/.'),
                           br(),
                           p('Jeffrey A. Ryan and Joshua M. Ulrich (2014). xts: eXtensible Time Series. R package version 0.9-7.
  https://CRAN.R-project.org/package=xts')
                           )
                           )
    )
)
)
# Server
