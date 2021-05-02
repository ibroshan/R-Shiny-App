
##  INITIAL SETUP ##

setwd("home/dir")

.libPaths("/R/library")

libs <- c("DT",  "leaflet", "RColorBrewer", "shinydashboard", "data.table", "dplyr", "shiny") 
lapply(libs, require, character.only = TRUE)


df0 <- fread("geo_spatial_data.csv", stringsAsFactors = F)

str(df0)

############### UI - R shiny app #########################

colnames(df0) <- c("Id",
                   "Latitude",
                   "Longitude",
				   "Date",
				   "Address",
				   "var5")

df0 <- as.data.frame(df0)

df0 <- df0[c("Id",
             "Latitude",
             "Longitude",
			 "Date",
			 "var5")]


x <- factor(df0$Id)
df0 <- df0 %>% mutate(Cluster = (as.character(as.numeric(x))))

df <- df0 %>% filter(!is.na(Latitude) & !is.na(Longitude))

df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

# Set value for the minZoom and maxZoom settings.

leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

targetvar <- unique(df$var5)

targetvar_choices <- append(targetvar, "Select All", after = 0)

date <- unique(df$Date)

date_choices <- append(date, "Select All", after = 0)

cluster <- unique(df$Cluster)

cluster_choices <- append(cluster, "Select All", after = 0)


# create shinydashboard page 
ui <- dashboardPage( 
  
  # dashboard header 
  dashboardHeader(title = "Geo_spatial Dashboard"),
  
  # dashboard sidebar 
  dashboardSidebar( 
    # allow to select multiple categories 
    
    selectInput(inputId = "dateselected", 
                label = "Date",
                choices = date_choices,
                selected = "Select All"),
    
    selectInput(inputId = "targetvar",
                label = "targetvar Country",
                choices = targetvar_choices,
                selected = "Select All"),
    
    selectInput(inputId = "cluster",
                label = "Cluster Id",
                choices = cluster_choices,
                selected = "Select All")
    
  ),
  
  # dashboard body 
  dashboardBody( 
    
    # map 
    fluidRow( 
      box( 
        width = 12,
        title = "Area Map",  
        status = "primary",  
        solidHeader = TRUE, 
        collapsible = TRUE, 
        leafletOutput("mymap", height = 600)
        
      ) 
      
    ),
    
    # data table 
    fluidRow( 
      box( 
        width = 12,  
        title = "Area Results", 
        status = "primary", 
        solidHeader = TRUE, 
        collapsible = TRUE, 
        DT::dataTableOutput("points") 
      ) 
    )) 
) 


###########server.R###############

server <- function(input,output, session) {
  
  data <- reactive({
    x <- df
    
    
  })
  
  ########## Color palette used ######
  
  pal <- c("darkred", "blue", "green", "orange", "gray", "black", "purple", "pink", "navajowhite", "yellow", "hotpink4", "deepskyblue", "blueviolet", "burlywood3", "darkgoldenrod4", "chocolate4", "cyan4","darkorange", "khaki", "ivory", "tan", "maroon1", "magenta", "navy", "linen", "seagreen", "pink", "seashell", "salmon", "coral", "tomato", "sienna", "cyan", "wheat", "darkolivegreen4", "darkorchid4", "indianred4", "plum4", "lightslateblue", "darkgreen", "palevioletred4", "peru", "skyblue4", "brown4", "yellowgreen", "yellow4")
  
  groupcolors <- colorFactor('Dark2', domain = df$Cluster)
  
  ##### R color palette with 657 colors ############
  
  # palette <- rgb(t(col2rgb(colors()) / 255))
  # 
  # names(palette) <- colors()
  # pal <- names(palette)
  # 
  # str(pal)
  
  ### Color function to add different colors to markers based on clusters ##########
  
  color <- function(df) {
    sapply(as.integer(df$Cluster), function(Cluster) {
      c <- as.integer(unique(df[,"Cluster"]))
      
      length(pal) <- length(unique(df$Cluster))
      
      pal[which (Cluster == c)]
      
    })
  }
  
  ################### Add marker color reactivity ####################################################
  
  icons <- reactive({
    df <- data()
    
    if (input$dateselected != "Select All") {
      df <- df[grepl(input$dateselected, df$Date, fixed = TRUE), ]
      
    }
    
    if (input$targetvar != "Select All") {
      df <- df[grepl(input$targetvar, df$var5, fixed = TRUE), ]
      
    }
    
    if (input$cluster != "Select All") {
      df <- df[grepl(input$cluster, df$Cluster, fixed = TRUE), ]
    }
    
    icons <- makeAwesomeIcon(
      library = 'ion',
      markerColor = color(df))
    
    return(icons)
  })
  
  ##################################################################
  
  output$mymap <- renderLeaflet({
    
    df <- data()
    
    if (input$dateselected != "Select All") {
      df <- df[input$dateselected == df$Date,]
    }
    
    if (input$targetvar != "Select All") {
      df <- df[input$targetvar == df$var5,]
    }
    
    if (input$cluster != "Select All") {
      df <- df[input$cluster == df$Cluster,]
    }
    
    ######### Warning message if no records are found for selected filters #######
    
    # check_df <- shiny::reactive({
    #   
    #   shiny::validate(
    #     shiny::need(nrow(df) != 0, "No data available for chosen filters!")
    #   )
    #   df
    # })
    # 
    # 
    # observeEvent(c(input$dateselected, input$targetvar, input$cluster), {
    #   
    #   if((is.null(input$dateselected)) || (is.null(input$targetvar)) || (is.null(input$cluster)))
    #     
    #   {
    #     return ("NA")
    #     
    #   }})
    # 
    ######################## Add data to map #############################
    
    m <- leaflet(df) %>%   
      addTiles() %>%
      setView(133.275, -28.153, 4)  %>%
      
      ######################################################################
    
    addAwesomeMarkers(lng = df$Longitude,
                      lat =  df$Latitude,
                      popup = paste0("<strong>Cluster: </strong>", df$Cluster, "<br>","<strong>Address: </strong>", df$Address),
                      label = df$Id,
                      group = "Markers",
                      icon = icons(),
                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                      markerOptions(draggable = T),
                      labelOptions = labelOptions(noHide = F, direction = 'auto')) %>%
      
      addCircleMarkers(data = df, lng = df$Longitude, lat = df$Latitude,
                       color = ~groupcolors(df$Cluster),
                       fillColor = ~groupcolors(df$Cluster),
                       group = "Circles") %>%
      
      addLayersControl(overlayGroups = c("Markers", "Circles"), options = layersControlOptions(collapsed = FALSE)) %>%
      
      addEasyButtonBar(easyButton(
        icon = "fa-crosshairs", title = "Home view",
        onClick = JS("function(btn, map){ map.setZoom(4);}")))
    
    
  })
  
  ########### Output table of points #############################
  
  output$points <-  DT::renderDataTable(
    
    {
      df <- data()
      
      if (input$dateselected != "Select All") {
        df <- df[input$dateselected == df$Date,]
        
      }
      
      if (input$targetvar != "Select All") {
        df <- df[input$targetvar == df$var5,]
      }
      
      if (input$cluster != "Select All") {
        df <- df[input$cluster == df$Cluster,]
      }
      
      df %>% 
        select(Cluster,
               Latitude,
               Longitude,
               Id,
			   Address,
               Date,
               var5)
    }, 
    
    extensions = c('Buttons','Responsive', 'Scroller', 'ColReorder'),
    options = list(dom = 'Blfrtip', scrollY = '500px', fixedHeader = TRUE, scrollX = TRUE, colReorder = list(realtime = FALSE),
                   buttons = list('colvis', 'copy', 'print',list(extend = 'collection', buttons = list(list(extend = 'excel', buttons = 'excel'), 
                                                                                                       list(extend = 'pdf', orientation = 'landscape', pageSize = 'A3')), text = 'Download'))))


  #check_df <- shiny::reactive({
    # 
    #     shiny::validate(
    #       shiny::need(nrow(df()) != 0,
    #                   "No data available for chosen filters!")
    #     )
    #     df
    #   })
    
    ### session stops after closing the window
    
    session$onSessionEnded(stopApp)
    
  } 
  
  ### Run app ###                       
  
  shinyApp(ui, server)






