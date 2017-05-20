# Server
#get available datasets
datasets<-ncdc_datasets()$data
datasets<-datasets[!(datasets$id %in% c('NEXRAD2','NEXRAD3')),] #NEXRAD2 and NEXRAD3 don't work
#define the resolution of the data as a fraction of a daily increment
datasets$dayMultiplier<-NA
datasets$dayMultiplier<-c(1,1/31,1/365,NA,NA,NA,NA,24*4,24)

countries<-ncdc_locs(locationcategoryid = 'CNTRY',limit=1000)$data
states<-ncdc_locs(locationcategoryid = 'ST',limit=1000)$data

function(input, output,session) {
  
  #initialize reactive values
  active<-reactiveValues()
  updateSelectInput(session,'datasets',choices=datasets$id,selected='GHCND')
  updateSelectInput(session,'country',choices=countries$name,selected='United States')
  updateSelectInput(session,'state',choices=states$name,selected='Massachusetts')
  state<-reactive({input$states})
  country<-reactive({input$countries})
  basemap<-reactive({input$basemap})
  
  #initialize map
  output$map<-renderLeaflet({
    leaflet() %>% addProviderTiles('CartoDB.Positron')  %>%
      addMeasure(position='topleft',primaryLengthUnit = 'kilometers',secondaryLengthUnit = 'miles') %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 3",
        onClick=JS("function(btn, map){ map.setZoom(3); }")))
  })
  
  #change basemap
  observe({
    tile<-basemap()
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(provider = tile)
  })
  
  #update map bounds as the map is moved
  observe({
    updateNumericInput(session,inputId = 'minlat',value =input$map_bounds$south)
    updateNumericInput(session,inputId = 'maxlat',value =input$map_bounds$north)
    updateNumericInput(session,inputId = 'minlong',value =input$map_bounds$west)
    updateNumericInput(session,inputId = 'maxlong',value =input$map_bounds$east)
  })
  
  #define the location ID, if inside the US, use the state ID else use the country ID
  locID<-reactive({
    if(input$country=='United States'){
      states$id[states$name %in% input$state]
    } else{
      countries$id[countries$name %in% input$country]
    }
  })
  
  #query the stations within the user-defined search criteria
  observe({
    if(input$searchCriteria=='designation'){
      active$stations<-reactive({ncdc_stations(datasetid = input$datasets,limit=1000,locationid = locID(),sortfield = 'name')$data})
    } else {
      searchExtent<-reactive({as.numeric(c(input$minlat,input$minlong,input$maxlat,input$maxlong))})
      active$stations<-reactive({ncdc_stations(datasetid = input$datasets,limit=1000,extent =searchExtent(),sortfield = 'name')$data})
    }
    #show stations in a data table
    output$stations <- DT::renderDataTable(active$stations(),rownames=FALSE,filter='top')
    
    #show stations in a map
    pal <- colorNumeric(palette = input$colors, active$stations()$datacoverage)
    labels <- sprintf(
      "<strong>%s</strong><br/>start date: %s<br/>end date: %s",
      active$stations()$name, active$stations()$mindate,active$stations()$maxdate) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy(mapId = 'map',session = session,data = active$stations()) %>% 
      clearShapes() %>%
      clearControls() %>%
      addCircles(~longitude,~latitude, weight = 1,radius=input$markersize/2,
                 fillColor =~pal(datacoverage),color = "#777777",
                 stroke = TRUE, fillOpacity = .7,
                 highlightOptions = highlightOptions(color = "black", weight = 3,
                                                     bringToFront = TRUE),
                 label=labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto")
      )%>% 
      addLegend(position = "bottomleft",
                pal = pal, values = ~datacoverage,title = "Data Coverage [%]")
    
    # zoom to designation area
    if (input$searchCriteria=='designation'){
      leafletProxy('map',session = session,data = active$stations()) %>% 
        fitBounds(lat1 = ~min(latitude,na.rm=TRUE)-.01,
                  lat2 = ~max(latitude,na.rm=TRUE)+.01,
                  lng1 = ~min(longitude,na.rm=TRUE)-.01,
                  lng2 = ~max(longitude,na.rm=TRUE)+.01)
    }
  })
  
  #on station select, query the available active$datatypes and zoom map in
  observeEvent(input$stations_rows_selected,{
    selected.dt<-active$stations()[input$stations_rows_selected,]
    active$datatypes<-ncdc_datatypes(datasetid = input$datasets,stationid = selected.dt$id)$data
    active$datatypes<-active$datatypes[order(active$datatypes$name),]
    active$datatypes<-rename(active$datatypes,datatype=id)
    updateSelectInput(session,'datatypes',choices=active$datatypes$name,selected=active$datatypes$name[1])
    
    #update map if search criteria is designation
    if(input$searchCriteria=='designation'){
      leafletProxy("map",data=selected.dt) %>%
        fitBounds(~min(longitude,na.rm = TRUE)-.1, ~min(latitude,na.rm = TRUE)-.1,
                  ~max(longitude,na.rm = TRUE)+.1, ~max(latitude,na.rm = TRUE)+.1)
    }
  })
  
  #on map click, select station in data table and zoom map in
  observeEvent(eventExpr = input$map_shape_click,{
    selected.dt<-active$stations()[round(active$stations()$latitude,3)==round(input$map_shape_click$lat,3) & 
                                     round(active$stations()$longitude,3)==round(input$map_shape_click$lng,3),]
    active$datatypes<-ncdc_datatypes(datasetid = input$datasets,stationid = selected.dt$id)$data
    active$datatypes<-active$datatypes[order(active$datatypes$name),]
    active$datatypes<-rename(active$datatypes,datatype=id)
    updateSelectInput(session,'datatypes',choices=active$datatypes$name,selected=active$datatypes$name[1])
    #update map if search criteria is designation
    if(input$searchCriteria=='designation'){
      leafletProxy("map",data=selected.dt) %>%
        fitBounds(~min(longitude,na.rm = TRUE)-.1, ~min(latitude,na.rm = TRUE)-.1,
                  ~max(longitude,na.rm = TRUE)+.1, ~max(latitude,na.rm = TRUE)+.1)
    }
    #update data table
    data.dt<-active$stations()
    Table.dt<-data.dt
    selected<-which(Table.dt$id==selected.dt$id)
    output$stations <- DT::renderDataTable({
      DT::datatable(Table.dt,filter="top",options = list(pageLength = 10,displayStart = selected-1),selection = list(mode = "single", target = "row", selected = selected) )
    })
    updateTabsetPanel(session = session,inputId = 'tabs',selected = 'Tabular')
    
  })
  # query and  plot user-defined data
  observeEvent(input$queryData,{
    datatype<-active$datatypes$datatype[active$datatypes$name %in% input$datatypes]
    #calculate length of output observations and split up query accordingly (1-year and 1000 row limit to API)
    numChannels<-length(input$datatypes)
    dateDiff<-as.numeric(difftime(input$dateRange[2],input$dateRange[1]))
    numObs<-dateDiff*datasets$dayMultiplier[datasets$id==input$datasets]
    queryRows<-numChannels*numObs
    if((queryRows>1000 | dateDiff>365) & !is.na(queryRows)){
      queries<-expand.grid(datasetid=input$datasets,startdate=as.character(seq(input$dateRange[1],input$dateRange[2],by='year')),enddate=NA,
                           datatypeid=datatype,stationid=active$stations()$id[input$stations_rows_selected],limit=1000,stringsAsFactors = FALSE)
      queries$enddate[year(queries$startdate)!=year(input$dateRange[2])]<-
        paste(year(queries$startdate[year(queries$startdate)!=year(input$dateRange[2])]),'-12-31',sep='')
      queries$enddate[is.na(queries$enddate)]<-as.character(input$dateRange[2])
      #query sequentially
      out.ls<-list()
      withProgress(message = 'Querying', value = 0, {
        for (i in 1:nrow(queries)){
          incProgress(i/nrow(queries),message = paste('Querying ',queries$datatypeid[i],': ',queries$startdate[i],' - ',queries$enddate[i],sep=''))
          out.ls[[i]]<-data.table(ncdc(datasetid = queries$datasetid[i],datatypeid = queries$datatypeid[i],
                                       stationid=queries$stationid[i],startdate=queries$startdate[i],
                                       enddate=queries$enddate[i],limit=queries$limit[i])$data)
        }
      })
      #combine data
      active$data.dt<-bind_rows(out.ls)
      
    }else{
      active$data.dt<-data.table(ncdc(datasetid = input$datasets,datatypeid = datatype,
                                      startdate = as.character(input$dateRange[1]),enddate=as.character(input$dateRange[2]),
                                      stationid = active$stations()$id[input$stations_rows_selected],limit=1000)$data)
    }
    
    #if data exists, plot it
    if(nrow(active$data.dt)>0){
      active$data.dt$date<-as.POSIXct(active$data.dt$date,format='%Y-%m-%dT%H:%M:%S')
      active$data.dt<-merge(active$data.dt,active$datatypes[,c('name','datatype')],by='datatype',all.x=TRUE,all.y=FALSE)
      data.xts<-as.xts.data.table(spread(active$data.dt[,c('date','value','name'),with=FALSE],name,value))
      output$plot<-renderDygraph({dygraph(data.xts,
                                          main = paste(active$stations()$name[input$stations_rows_selected],' ',
                                                       paste(input$datatypes,collapse=' ; '),sep='')) %>% dyRoller() %>%dyRangeSelector()})
      updateTabsetPanel(session = session,inputId = 'tabs',selected = 'Data')
      output$results<-renderText('query successful')
    } else {output$results<-renderText('no data found')}
  })
  
  #export data to csv
  output$exportData <- downloadHandler(
    filename = function() {  paste(active$stations()$name[input$stations_rows_selected],' ',input$datatypes,'_data.csv',sep='')},
    content = function(con) { write.csv(active$data.dt, con,row.names = FALSE)}
  )
  
}