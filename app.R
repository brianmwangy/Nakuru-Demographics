#loading library
library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(raster)
#library(rsconnect)




#loading clipped areas within 25m of sewer shape file
area25<-readOGR(
  dsn=path.expand("./clip_25"),
  layer="clip_25",encoding = 'UTF-8')

#loading clipped areas within 50m of sewer shape file
area50<-readOGR(
  dsn=path.expand("./clip_50"),
  layer="clip_50",encoding = 'UTF-8')

#loading areas within 50m of sewer shape file
# area50<-readOGR(
#   dsn=path.expand("./area_50"),
#   layer="area_50m",encoding = 'UTF-8')

#loading elevation shape file
countor<-readOGR(
  dsn=path.expand("./Elevation Contours"),
  layer="25m interval rough contours",encoding = 'UTF-8')

#loading unknown areas shape file
unknown<-readOGR(
  dsn=path.expand("./unknown"),
  layer="unknown",encoding = 'UTF-8')

#adding elevation with values
unknown@data$Elevation<-c(1800,1875,1800,1900,1800)
#kn$Elevation<-c(1800,1875,1800,1900,1800)

#alternatively(raster)
#countor <- shapefile("./Elevation Contours/25m interval rough contours.shp")

#loading sewer shape file
sewer<-readOGR(
  dsn=path.expand("./Sewer map"),
  layer="Sewer line WGS84",encoding = 'UTF-8')

#loading LIAs shape file
mp<-readOGR(
  dsn=path.expand("./LIAs3"),
  layer="m3",encoding = 'UTF-8')

#filling elevation NAs with values
mp@data[8,19]<-1838
mp@data[7,19]<-1838
mp@data[12,19]<-1800
mp@data[13,19]<-1800
mp@data[16,19]<-1863
mp@data[18,19]<-1800
mp@data[25,19]<-1775
mp@data[26,19]<-1800
mp@data[27,19]<-1800
mp@data[28,19]<-1800
mp@data[29,19]<-1800
mp@data[32,19]<-1825
mp@data[33,19]<-1800
mp@data[36,19]<-1838
#mp<-mp[!is.na(mp$elevation),]

#converting columns to numeric
cols = c(11,12,13,14,15,16,17)
mp@data[,cols]=apply(mp@data[,cols],2,function(x) as.integer(as.character(x)))

#loading  water treatment plants shape file
treat<- readOGR(dsn=path.expand("./treatment plants"),
                layer="Nk_Sewer_newcrs",encoding = 'UTF-8')

#projecting the coordinates to default CRS
treat1<-spTransform(treat, CRS("+proj=longlat +datum=WGS84"))
treat1$Elevation<-1775

#Remove NAs from AreaType col
mp<-mp[!is.na(mp$AreaTyp),]
#tr<-mp@data




#loading nakuru data
nakuru<-read.csv("./naks/nak3.csv",encoding = 'UTF-8')
colnames(nakuru)[c(1,2,3,5,6,7,8,9,10,11)]=c("Location Name","Sublocation Name","Area Name",
                                             "Populationperkm2",
                                             "Percentage Dwellings with Piped Water on Plot",
                                             "Percentage Dwellings with Water Source On Plot",
                                             "Percentage Dwellings with Flush Toilets",
                                             "Percentage Dwellings with Other Improved",
                                             "Percentage Dwellings with Unimproved",
                                             "Percentage Dwellings with Open Defecation")



#slicing nakuru data for plotting options
nakuru1<-nakuru[,c("AreaType"),drop=F]
nakuru2<-nakuru[,c(5,7,8,9,10,11)]

#UI layout
ui<-dashboardPage(
  dashboardHeader(title = "NAKURU DATA",titleWidth = 275),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem("DATA EXPLORER",tabName = "data",icon = icon("th")),
      menuItem("PLOT EXPLORER",tabName = "plot",icon =icon("bar-chart-o")),
      menuItem("COLOR MAPPING",tabName = "map",icon = icon("map",class = "fas fa-map")),
      menuItem("INTERACTIVE MAPPING",tabName = "map2",icon = icon("map", class="fas fa-map-marked-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        h3("INTERACTING WITH DATA"),
        
        fluidRow(
          
          # box(title = "FILTERING",status = "primary",solidHeader = TRUE,
          #     checkboxGroupInput(inputId = "popdensity", 
          #                        label = " Population Per Km2:",
          #                        choices = list(
          #                          "< 15,000"=1, 
          #                          "15,001 - 30,000"=2 , 
          #                          ">30,000"=3
          #                        ), 
          #                        selected =list(1,2,3)
          #     ),
          #     
          #     selectInput(inputId = "area", 
          #                 label = " Area Type:",
          #                 choices = c(
          #                   "All",
          #                   unique(as.character(nakuru$AreaType))
          #                 ) 
          #     ),
          #     
          #     width = 4),
          
          
          box(title = "OUTPUT",status = "primary",solidHeader = TRUE,
              
              #datatableoutput
              DT::dataTableOutput("table"),
              width = 12
              
          ))),
      
      tabItem(
        tabName = "plot",
        h3("PLOT INTERACTION"),
        fluidRow(
          box(
            title = "AXIS VALUES",status = "primary",solidHeader = TRUE,
            radioButtons(inputId = "status",
                         label = "SELECT X VALUE:",
                         choices = colnames(nakuru1)
                         
            ),
            radioButtons(inputId = "status2",
                         label = "SELECT Y VALUE:",
                         choices = colnames(nakuru2)
                         
                         
            ),width=4
          ),
          box(
            title = "PLOT OUTPUT",status = "primary",solidHeader = TRUE,
            plotOutput("stat",click = "plot_click"),
            verbatimTextOutput("info"),
            width = 8
          ))
        
      ),
      tabItem(
        tabName = "map",
        h3("COLOR MAP"),
        fluidPage(
          
          title = "MAP DISPLAY",status = "primary",solidHeader = TRUE,
          leafletOutput("leaf",width="100%",height = 500),
          
          
          
          
          
          #h2("MAP EXPLORER",style="color:#3474A7"),
          fluidRow(
            column(6,
                   selectInput(inputId = "pop",
                               label = " Population Per km2:",
                               choices = list(
                                 "All"=1,
                                 "Population"=4
                                 #"< 15,000"=2
                                 # "15,001 - 30,000"=3 ,
                                 # ">30,001"=1
                               )
                   )),
            
            column(6,
                   selectInput(inputId = "area1", 
                               label = " AREA TYPE:",
                               choices = c(
                                 "All",
                                 unique(as.character(mp$AreaTyp))
                               ) 
                   ))),
          fluidRow(
            column(4,
                   selectInput(
                     inputId = "pw",
                     label = "WATER ACCESS:",
                     choices = c(
                       "All"=1,
                       "Piped Water On Plot"=2,
                       "Water source On Plot"=3
                     )
                   )),
            column(4,
                   checkboxGroupInput(
                     inputId = "unknown1",
                     label = "AREAS:",
                     choices=c(
                       "Unknown Areas"=1
                     )
                   )),
            column(4, 
                   selectInput(
                     inputId = "san",
                     label = "SANITATION TYPE:",
                     choices = c(
                       "All"=1,
                       "Flush Toilets"=2,
                       "Other Improved"=3,
                       "UnImproved"=4,
                       "Open Defecation"=5
                     )
                   )),
            fluidRow(
              checkboxGroupInput(
                inputId = "treat",
                label = "LAYOUT:",
                inline = TRUE,
                
                choices=c(
                  "Low Income Areas"=1,
                  "Areas Within 50m of Sewer"=2,
                  "Areas Within 25m of Sewer"=3,
                  "Unknown Areas"=4,
                  "Water Treatment Plant"=5,
                  "Sewer Line"=6,
                  "Elevation Colors"=7,
                  "Elevation Contours"=8
                  
                ),selected = 1
              )
              
              
            )
          )
          
          
          
        )
        
        
        
      ),
      tabItem(
        tabName = "map2",
        h3("INTERACTIVE MAP"),
        fluidPage(
          
          title = "MAP DISPLAY",status = "primary",solidHeader = TRUE,
          leafletOutput("leaf2",height = 500),
          
          
          
          
          
          #h2("USER EXPLORER",style="color:#3474A7"),
          fluidRow(
            column(6,
                   #slider input for population per km2  
                   sliderInput(inputId = "pop2",
                               label = "Population Per km2:",
                               min = min(mp@data$PpDnsty,na.rm =T),
                               max = max(mp@data$PpDnsty,na.rm =T),
                               value = c(min(mp@data$PpDnsty,na.rm =T),
                                         max(mp@data$PpDnsty,na.rm =T))
                   ),
                   #slider input for piped water on plot  
                   sliderInput(inputId = "pw2",
                               label = "Piped Water On Plot:",
                               min = min(mp@data$PpdWtrP,na.rm =T),
                               max = max(mp@data$PpdWtrP,na.rm =T),
                               value = c(min(mp@data$PpdWtrP,na.rm =T),
                                         max(mp@data$PpdWtrP,na.rm =T))
                   )),
            column(6,
                   #slider input for water source on plot  
                   sliderInput(inputId = "ws",
                               label = "Water Source On Plot:",
                               min = min(mp@data$WtrSrOP,na.rm =T),
                               max = max(mp@data$WtrSrOP,na.rm =T),
                               value = c(min(mp@data$WtrSrOP,na.rm =T),
                                         max(mp@data$WtrSrOP,na.rm =T))
                   ),
                   
                   #slider input for flush toilets  
                   sliderInput(inputId = "ft",
                               label = "Flush Toilets:",
                               min = min(mp@data$FlshTlt,na.rm =T),
                               max = max(mp@data$FlshTlt,na.rm =T),
                               value = c(min(mp@data$FlshTlt,na.rm =T),
                                         max(mp@data$FlshTlt,na.rm =T))
                   ))),
          fluidRow(
            column(6,
                   
                   #slider input for Other Improved
                   sliderInput(inputId = "om",
                               label = "Other Improved:",
                               min = min(mp@data$OthrImp,na.rm =T),
                               max = max(mp@data$OthrImp,na.rm =T),
                               value = c(min(mp@data$OthrImp,na.rm =T),
                                         max(mp@data$OthrImp,na.rm =T))
                   ),
                   
                   #slider input for unimproved
                   sliderInput(inputId = "um",
                               label = "Unimproved:",
                               min = min(mp@data$Unmprvd,na.rm =T),
                               max = max(mp@data$Unmprvd,na.rm =T),
                               value = c(min(mp@data$Unmprvd,na.rm =T),
                                         max(mp@data$Unmprvd,na.rm =T))
                   )
            ),
            column(6,
                   
                   #slider input for open defecation
                   sliderInput(inputId = "od",
                               label = "Open Defecation:",
                               min = min(mp@data$OpnDfct,na.rm =T),
                               max = max(mp@data$OpnDfct,na.rm =T),
                               value = c(min(mp@data$OpnDfct,na.rm =T),
                                         max(mp@data$OpnDfct,na.rm =T))
                   ),
                   
                   #slider input for elevation
                   sliderInput(inputId = "el",
                               label = "Elevation:",
                               min = min(mp@data$elevation,na.rm =T),
                               max = max(mp@data$elevation,na.rm =T),
                               value = c(min(mp@data$elevation,na.rm =T),
                                         max(mp@data$elevation,na.rm =T))
                   )
            )
            
          )
          
          
          
          
          
          
          
          
          
        )
        
        
        
      )
    )
  )
)

#server function

server<-function(input,output,session){
  
  #reactive function for datatable
  # dt<-reactive({
  #   df<-nakuru
  #   
  #   
  #   #area type function
  #   if(input$area!="All"){
  #     
  #     df<-df[df$AreaType==input$area,]
  #     
  #   }
  #   
  #   #population function    
  #   str(df)
  #   df1<-df %>% filter(Populationperkm2<=15000)
  #   df2<-df %>% filter(Populationperkm2>15000&Populationperkm2<=30000)
  #   df3<-df %>% filter(Populationperkm2>30000)
  #   temp<-data.frame()
  #   
  #   if("1" %in% input$popdensity){
  #     temp<-rbind(temp,df1)
  #   }
  #   
  #   if("2" %in% input$popdensity){
  #     temp<-rbind(temp,df2)
  #   }
  #   if("3" %in% input$popdensity){
  #     temp<-rbind(temp,df3)
  #   }
  #   temp
  #   
  # })
  output$table<-DT::renderDataTable({
    DT::datatable(nakuru,filter = "top",
                  extensions = "Buttons",options=list(scrollX=TRUE,
                    pageLength=50,dom="Bfrtip",buttons=list("copy","print",list(
                      extend="collection",buttons=c("csv","excel","pdf"),
                      text="Download"
                    ))
                  ))
    
    
  })
  
  
  #plot explorer 
  output$stat<-renderPlot({
    x<-nakuru[,c(input$status,input$status2)]
    plot(x,col = "#75AADB", pch = 19,main=paste0(input$status2,"  vs ",input$status))
    
  })
  
  #average lias info(%)
  output$info <- (renderText({
    paste0("VALUE=", round(as.numeric(input$plot_click$y,0)),
           "\n% AREA TYPE OF DWELLINGS:\nPlanned:68%\nUnplanned:18%\nMixed Area:14%",
           "\n% WATER ACCESS TYPE OF DWELLINGS:\nPiped Water On Plot:41%\nWater Source On Plot:43%",
           "\n% SANITATION TYPE OF DWELLINGS:\nFlush Toilets:19%\nUnimproved:46%\nOther Improved:35%\nOpen Defecation:0.3%"
    )
    
    
  }))
  
  
  #INTERACTIVE WEB MAPPING
  
  
  
  
  #reactive function for sanitation type
  fts<-reactive({
    dm<-mp
    if(input$san==2){
      dm[dm$FlshTlt<=25,]
    }
    if(input$san==3)
    {
      dm[dm$OthrImp<=50,]
    }
    if(input$san==4)
    {
      dm[dm$Unmprvd<=50,]
    }
    if(input$san==5)
    {
      dm[dm$OpnDfct<=50,]
    }
    
    return(dm)
  })
  
  
  
  #reactive function for water access type
  pwp<-reactive({
    dm<-mp
    
    if("2" %in% input$pw){
      dm[dm$PpdWtrP<=25,]
    }
    
    if("3" %in% input$pw){
      dm[dm$WtrSrOP<=25,]
    }
    
    dm
  })
  
  #reactive function for population per km2 
  ppd<-reactive({
    dt<-mp
    if(input$pop==1){
      dt[dt$pop<=1000,]
    } else if(input$pop==3){
      dt[dt$PpDnsty>1000&dt$PpDnsty<=2000,]
    } else if(input$pop==4){
      dt[dt$PpDnsty>2000,]
    }
    return(dt)
  })
  
  #reactive function for area type
  fdata<-reactive({
    data<-mp
    if(input$area1!="All"){
      data<-subset(data,AreaTyp %in% input$area1)
      #data<-data[data$AreaType==input$area,]
      
    }
    
    
    return(data)
  })
  
  #sliderinput reactive function for all numeric input options
  sld<-reactive({
    subset(mp,mp@data$PpDnsty>=input$pop2[1]&
             mp@data$PpDnsty<=input$pop2[2]&
             mp@data$PpdWtrP>=input$pw2[1]&
             mp@data$PpdWtrP<=input$pw2[2]&
             mp@data$WtrSrOP>=input$ws[1]&
             mp@data$WtrSrOP<=input$ws[2]& 
             mp@data$FlshTlt>=input$ft[1]&
             mp@data$FlshTlt<=input$ft[2]&
             mp@data$OthrImp>=input$om[1]&
             mp@data$OthrImp<=input$om[2]&
             mp@data$Unmprvd>=input$um[1]&
             mp@data$Unmprvd<=input$um[2]&
             mp@data$OpnDfct>=input$od[1]&
             mp@data$OpnDfct<=input$od[2]&
             mp@data$elevation>=input$el[1]&
             mp@data$elevation<=input$el[2]
           
    )
    
  })
  
  #Base map(default)
  
  output$leaf<-renderLeaflet({
    
    
    
    leaflet(mp) %>%
      
      #Initializing the map
      # setView(lng=36.092245, lat=-00.292115,zoom=15)%>%
      
      #default map
      #Add default OpenStreetMap map tiles
      addTiles()%>%
      
      # addProviderTiles("Esri.NatGeoWorldMap",group = "default")%>%  
      #addProviderTiles("CartoDB.Positron",group = "custom")%>%
      
      #nakuru lias polygons
      addPolygons(
        data = mp,
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty,
                       "<br>",
                       "<strong>Elevation:</strong>",elevation,"m"
        )
        
      ) 
    
    
    
    
  })
  
  #Base map(default)
  
  output$leaf2<-renderLeaflet({
    
    
    
    leaflet(mp) %>%
      
      #Initializing the map
      # setView(lng=36.092245, lat=-00.292115,zoom=15)%>%
      
      #default map
      #Add default OpenStreetMap map tiles
      addTiles()%>%
      
      # addProviderTiles("Esri.NatGeoWorldMap",group = "default")%>%  
      #addProviderTiles("CartoDB.Positron",group = "custom")%>%
      
      #nakuru lias polygons
      addPolygons(
        data = mp,
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty,
                       "<br>",
                       "<strong>Elevation:</strong>",elevation,"m"
        )
        
      ) 
    
    
    
    
  })
  
  #observe function for slider input numeric options
  observe({
    
    #color mapping function
    pal<-colorNumeric(palette = "plasma",domain=input$pop2)
    #pal1 <- colorBin("plasma",lias$PpDnsty, 15, pretty = TRUE)
    #pal1<- colorBin("Blues", lias$PpDnsty, 2, pretty = FALSE)
    
    
    leafletProxy("leaf2",data=sld()) %>%
      
      #Initializing the map
      #setView(lng=36.092245	, lat=-00.292115,zoom=10)%>%
      # clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        #fillColor = ~pal(input$pop2),
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty,
                       "<br>",
                       "<strong>Elevation:</strong>",elevation,"m"
                       
        )
        
       ) #%>% addLegend(
      #   title = "Values", position = "topleft",
      #   pal = pal, values = ~(input$pop2), opacity = 1
      # ) #%>% addLegend(
        #title = "Values", position = "topleft",
        #pal = pal, values = ~(input$pw2), opacity = 1
      #)
      
  })
  
  #observe function for area type
  observe({
    #color mapping function for area type
    pal<-colorFactor(rainbow(7),mp$AreaTyp)
    
    leafletProxy("leaf",data=fdata()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(AreaTyp),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty,
                       "<br>",
                       "<strong>Elevation:</strong>",elevation,"m"
                       
        )
        
      )%>%
      addLegend(title = "AreaType", position = "topleft",
                pal = pal, values = ~AreaTyp, opacity = 1)
  })
  
  #observe function for population per km2
  observe({
    
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma",mp$PpDnsty, 15, pretty = TRUE)
    
    leafletProxy("leaf",data=ppd()) %>%
      
      # clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal1(PpDnsty),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =~LIA,
        popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                       "<br>",
                       "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",OthrImp,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Unmprvd,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",OpnDfct,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",PpDnsty,
                       "<br>",
                       "<strong>Elevation:</strong>",elevation,"m"
                       
        )
        
      )%>%
      addLegend(title = "Population Per km2", position = "topleft",
                pal = pal1, values = ~PpDnsty, opacity = 1)
    
  })
  
  
  
  
  #observe function for water access type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", mp$PpdWtrP, 5, pretty = TRUE)
    pal2<-colorBin("plasma", mp$WtrSrOP, 5, pretty = TRUE)
    
    md<-leafletProxy("leaf",data=pwp()) %>% clearControls() %>% clearShapes()
    
    # clearMarkers() %>%
    if("1" %in% input$pw){
      md %>%
        
        
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
          )
          
        ) 
    }
    
    #piped water on plot if function
    if("2" %in% input$pw){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal1(PpdWtrP),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "Piped Water On Plot(%):", position = "topleft",
                  pal = pal1, values = ~PpdWtrP, opacity = 1)
    }
    
    #water source on plot if function
    if("3" %in% input$pw){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(WtrSrOP),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "WaterSource On Plot(%):", position = "topleft",
                  pal = pal2, values = ~WtrSrOP, opacity = 1)
    }
    #Unknown Areas overlay map function
    if("1" %in% input$unknown1){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay unknown map
        addPolygons(
          data = unknown,
          fillColor = "white",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",Elevation,"m"
          )
          
        )
    }
  })
  
  #observe function for sanitation type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", mp$FlshTlt, 5, pretty = TRUE)
    pal2<-colorBin("plasma", mp$OthrImp, 5, pretty = TRUE)
    pal3 <- colorBin("plasma", mp$Unmprvd, 5, pretty = TRUE)
    pal4<-colorBin("plasma", mp$OpnDfct, 5, pretty = TRUE)
    
    #leafletproxy function
    md<-leafletProxy("leaf",data=fts()) %>% clearControls() %>% clearShapes()
    
    # clearMarkers() %>%
    #nakuru lias if function
    if("1" %in% input$san){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
          )
          
        ) 
    }
    
    #flash toilets if function
    if("2" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          #data=fdata(),
          fillColor = ~pal1(FlshTlt),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "Flush Toilets (%):", position = "topleft",
                  pal = pal1, values = ~FlshTlt, opacity = 1)
    }
    
    #otherimproved if function
    if("3" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(OthrImp),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "Other Improved (%):", position = "topleft",
                  pal = pal2, values = ~OthrImp, opacity = 1)
    }
    
    #unimproved if function
    if("4" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          #data=fdata(),
          fillColor = ~pal3(Unmprvd),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "UnImproved (%):", position = "topleft",
                  pal = pal3, values = ~Unmprvd, opacity = 1)
    }
    
    #open defecation if function
    if("5" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal4(OpnDfct),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>WaterSource On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
                         
          )
          
        )%>%
        addLegend(title = "Open Defecation (%):", position = "topleft",
                  pal = pal4, values = ~OpnDfct, opacity = 1)
    }
    
    #Unknown Areas overlay map function
    if("1" %in% input$unknown1){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay unknown map
        addPolygons(
          data = unknown,
          fillColor = "white",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",Elevation,"m"
          )
          
        )
    }
  })
  
  #observe function for layout
  observe({
    
    #leafletproxy function
    md<-leafletProxy("leaf") %>% clearControls() %>% clearShapes()
    
    
    #nakuru lias if function
    if("1" %in% input$treat){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          data = mp,
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
          )
          
        ) 
    }
    
    #Areas within 50m of sewer overlay map function
    if("2" %in% input$treat){
      md %>%
        #Initializing the map
         setView(lng=36.092245	, lat=-00.292115,zoom=13)%>%
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay 50m map
        addPolygons(
          data = area50,
          fillColor = "yellow",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
                         
          )
          
        )
      
      
    }
    #Areas within 25m of sewer overlay map function
    if("3" %in% input$treat){
      md %>%
        #Initializing the map
        setView(lng=36.092245	, lat=-00.292115,zoom=13)%>%
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay 50m map
        addPolygons(
          data = area25,
          fillColor = "yellow",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty
                         
                         
          )
          
        )
      
      
    }
    
    #Unknown Areas overlay map function
    if("4" %in% input$treat){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay unknown map
        addPolygons(
          data = unknown,
          fillColor = "white",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",Elevation,"m"
          )
          
        )
    }
    
    #water treatment plants ovelay map function
    if("5" %in% input$treat){
      md%>%
        addPolygons(
          data = treat1,
          color="green",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~NAME,
          popup = ~paste("<strong>TYPE:</strong>",TYPE,
                         "<br>",
                         "<strong>USAGE:</strong>",USAGE,"m3",
                         "<br>",
                         "<strong>CAPACITY:</strong>",CAPACITY,"m3",
                         "<br>",
                         "<strong>ELEVATION:</strong>",Elevation,"m"
                         
          )
        )
    }
    
    #sewer lines overlay map function
    if("6" %in% input$treat){
      md%>%
        addPolylines(
          weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          data=sewer,
          color = "red",
          highlightOptions = highlightOptions(
            weight = 2,
            color = "yellow",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~OWNER,
          popup = ~paste("<strong>OWNER:</strong>",OWNER_2,
                         "<br>",
                         "<strong>YEAR CONSTRUCTED:</strong>",YEAR_CONST,
                         "<br>",
                         "<strong>MATERIAL:</strong>",MATERIAL,
                         "<br>",
                         "<strong>PIPE SIZE:</strong>",PIPE_SIZE,
                         "<br>",
                         "<strong>LENGTH:</strong>",LENGTH_1
                         
                         
          )
        ) 
      
    }
    
    #elevation colors overlay map function
    if("7" %in% input$treat){
      pal1 <- colorBin("plasma", mp$elevation, 5, pretty = TRUE)
      
      leafletProxy("leaf",data = mp) %>% 
        addPolygons(
          weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          #data=lias,
          fillColor=~pal1(elevation),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~LIA,
          popup = ~paste("<strong>Area Type:</strong>",AreaTyp,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",PpdWtrP,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",WtrSrOP,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",FlshTlt,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",OthrImp,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Unmprvd,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",OpnDfct,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",PpDnsty,
                         "<br>",
                         "<strong>Elevation:</strong>",elevation,"m"
          )
          
          
        )%>%
        addLegend(title = "Elevation(m)", position = "topleft",
                  pal = pal1, values = ~elevation, opacity = 1)
    }
    
    #elevation contours overlay map function
    if("8" %in% input$treat){
      md%>%
        addPolylines(
          weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          data=countor,
          color = "brown",
          highlightOptions = highlightOptions(
            weight = 2,
            color = "green",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~Interval,
          popup = ~paste("<strong>ELEVATION:</strong>",elevation,"m",
                         "<br>",
                         "<strong>INTERVAL:</strong>",Interval
          )
          
          
        ) 
      
    }
    
  })
  
}


shinyApp(ui,server)