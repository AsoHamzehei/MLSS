library(shiny)
library(rMaps)
library(rCharts)
library(ggmap)
#library(markdown)

shinyServer(function(input, output){#, session) { 
  #drawmap(
   # output<-list()

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Function]: Fixing invalid multibyte strings (needed for HTML output)
  ## Source: http://tm.r-forge.r-project.org/faq.html#Encoding
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fix_mbs <- function(x) iconv(enc2utf8(x), sub = "byte")


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Function]: Setting the approximate map center
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  set_center <- function(loc = 'all') {
    if (loc == 'all') return(data.frame(lon = 10, lat = 15))

    ## By Region
    if (loc == 'Asia') return(data.frame(lon = 105, lat = 20))
    if (loc == 'Europe') return(data.frame(lon = 22, lat = 50))
    if (loc == 'North America') return(data.frame(lon = -100, lat = 40.5))
    if (loc == 'Middle East or Africa') return(data.frame(lon = 20, lat = 0))
    if (loc == 'Oceania') return(data.frame(lon = 143.5, lat = -31))
    if (loc == 'South or Central America') return(data.frame(lon = -70, lat = -18))

    ## By Country (>= 5 groups)
    if (loc == 'Australia') return(data.frame(lon = 131.5, lat = -30))
    if (loc == 'Canada') return(data.frame(lon = -100, lat = 48))
    if (loc == 'Germany') return(data.frame(lon = 10, lat = 51))
    if (loc == 'India') return(data.frame(lon = 77, lat = 20))
    if (loc == 'Japan') return(data.frame(lon = 135.5, lat = 36.5))
    if (loc == 'United Kingdom') return(data.frame(lon = -3.5, lat = 54))
    if (loc == 'United States') return(data.frame(lon = -100, lat = 35))

  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Main Function]: Create Leaflet rMaps
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  create_map <- function(map_region = NULL,
                         map_country = NULL,
                         map_zoom = 2,
                         map_type = 'Acetate.all',  # Esri.WorldGrayCanvas, OpenStreetMap.BlackAndWhite
                         map_width = 1100,
                         map_height = 600) {

    ## Load Data
    df_rugs <- read.csv("./data/mlss_updated.csv", stringsAsFactors=FALSE)

    ## Define map center and record needed
    if (is.null(map_region) & is.null(map_country)) {
      geo_center <- set_center('all')
      row_subset <- 1:nrow(df_rugs)

    } else if (is.null(map_region)) {
      geo_center <- set_center(map_country)
      row_subset <- which(df_rugs$Country %in% map_country)

    } else if (is.null(map_country)) {
      geo_center <- set_center(map_region)
      row_subset <- which(df_rugs$Region %in% map_region)
    }

    ## Subset df_rugs
    df_rugs_subset <- df_rugs[row_subset, ]

    ## Create and config map_base
    map_base <- rMaps::Leaflet$new()
    map_base$params$width <- map_width
    map_base$params$height <- map_height
    map_base$tileLayer(provider = map_type)
    map_base$setView(c(geo_center$lat, geo_center$lon), map_zoom)

    ## Add markers one by one
    for (n_rugs in 1:nrow(df_rugs_subset)) {
      tmp_city <- paste0(df_rugs_subset[n_rugs,]$City)
      tmp_name <- paste0(df_rugs_subset[n_rugs,]$Name, " (", df_rugs_subset[n_rugs,]$Email, ")")
      map_base$marker(c(df_rugs_subset[n_rugs, ]$lat, df_rugs_subset[n_rugs, ]$lon), bindPopup = tmp_name)
    }

    ## Return map_base
    return(map_base)

  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Output]: Interactive Map (All Groups)
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$map_all<- renderUI({

    ## Create map_base
    map_base <- create_map(NULL, NULL, 2)

     ## Generate HTML code, fix invalid multibyte strings and return
     html_out <- HTML(map_base$html(chartId = "map_all"))
     html_out <- fix_mbs(html_out)
     html_out
 
   })



#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ## [Output]: Download Buttons for Original Data
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   csv_ori <- reactive({
   
   read.csv("./data/MLSS_contacts.csv", stringsAsFactors = FALSE)
   
   })
   
   output$dl_ori <- downloadHandler(
     filename = function() {'MLSS_contacts.csv'},
     content = function(file) {write.csv(csv_ori(), file)}
   )
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## [Output]: Table - Original Data
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$data_original <- renderDataTable({
    
    ## Read Original CSV
    rugs <- read.csv("./data/MLSS_contacts.csv", stringsAsFactors=FALSE)
    
    ## Return
    rugs
    
  }, options = list(aLengthMenu = c(10, 25, 50, 100, 150), iDisplayLength = 10))
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## End of Shiny Server Script
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

})
