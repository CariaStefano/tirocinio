pkgs = c("sf", "shiny", "spData", "leaflet", "tidyverse", "spDataLarge", "units", "tmap", "dplyr")
invisible(lapply(pkgs, library, character.only = TRUE))


logistica_2022=readr::read_csv("../GDL06_geo_classifica_22.csv", na = "N.D.", 
                               col_types = list("RAGIONE SOCIALE"= readr::col_factor(), 
                                                "SEDE"= readr::col_factor(), 
                                                "PROV"= readr::col_factor()))

numeric_conversion= function(col){
  data= logistica_2022 %>% 
    mutate(col= gsub("\\.", "", col),
           col = as.numeric(col))
  return(data$col)
  
}

numeric_conversion2= function(col){
  data= logistica_2022 %>% 
    mutate(col= gsub("\\,", ".", col),
           col = as.numeric(col))
  return(data$col)
  
}


#min(renderText(reactive({if(input$radio_vis == "province") {ds$fatturato20}})()))


selection= c("nome"=1, "comune"=2, "provincia"=3, "fatturato20"=4, 
             "fatturato19"=5, "utile20"=6, "utile19"=7, 
             "ros20"= 8, "ros19"=9, "roe20"= 10, "roe19"= 11 )

ds = logistica_2022 %>%
  select(-`...1`,-`POS. '21`, -`POS. 22` ) %>% 
  select(selection, everything()) %>% 
  mutate(fatturato20= numeric_conversion(fatturato20),
         fatturato19= numeric_conversion(fatturato19),
         utile20 = numeric_conversion(utile20),
         utile19 = numeric_conversion(utile19),
         ros20= numeric_conversion2(ros20),
         ros19= numeric_conversion2(ros19),
         roe20= numeric_conversion2(roe20),
         roe19= numeric_conversion2(roe19))

geo_ds= st_as_sf(ds, coords = c("long", "lat"), crs = "EPSG:4326")

# Based on input coordinates finding the nearest bicycle points

ui = fluidPage( 
  div(
    style = "display:flex; align-items:flex-start",
    div(
    wellPanel(style= "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
              radioButtons("radio_vis", h3("Visualizza per:"),
                           choices = list("regioni", "province", "località"), 
                           selected = "province"),
              sliderInput("slider", h3("Filtra per fatturato:"),
                                    min = 100000 , max= 1900000000, value=0 ),
              br(),
              radioButtons("radio", h3("Tipo di aggregazione"),
                                     choices = list("fatturato20", "utile20", "roe20", "ros20" ), 
                                     selected = "fatturato20"), 
              sliderInput("slider2", h3("Filtra per ROE:"),
                                    min = -4 , max= 314 , value= -4 ),
              sliderInput("slider_utile", h3("Filtra per utile:"),
                          min = -18000000 , max= 46000000, value=-18000000 ),
              br(),
              sliderInput("slider_ros", h3("Filtra per ROS:"),
                          min = -108 , max= 116, value=-108 ),
              br()
              ),
      div( #~~ Main panel ~~#
        titlePanel("Mappa logistica 2022"),
        tmapOutput("map"),
        style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px",
        tags$style(type = "text/css", "#map {height: calc(90vh) !important; width:calc(120vh) !important; }")
      ))))
  

  
  
  # 
  # titlePanel("Mappa logistica 2022"),
  #               tmapOutput("map", height = 500, width = "50%" ),
  #               sidebarLayout(
  #                 sidebarPanel(
  #                 fluidRow(column(10,
  #                                 sliderInput("slider", h3("Seleziona per fatturato:"),
  #                                             min = 10000 , max= 1000000000, value=10000 ),
  #                                 radioButtons("radio", h3("Tipo di aggregazione"), 
  #                                              choices = list("fatturato20", "roe20"), 
  #                                              selected = "fatturato20"))),
  #                 fluidRow(column(10,
  #                                 sliderInput("slider2", h3("Seleziona per ROE:"),
  #                                             min = -3 , max= 350, value=1 ))))
  #                 
  #                 
  #                 
  #               , mainPanel(plotOutput("slider"),
  #                            plotOutput("slider2"),
  #                            plotOutput("radio"))))


# ui = fluidPage(
#   # Application title
#   titlePanel("Mappa logistica 2022"),
#   # Where leaflet map will be rendered
#   tmapOutput("map", height= 500),
#   fluidRow(column(7,
#                   sliderInput("slider", h3("seleziona per fatturato:"),
#                   min = 10000 , max= 1000000000, value=10000 ),
#                   radioButtons("radio", h3("Tipo di aggregazione"), 
#                                choices = list("fatturato20", "roe20"), 
#                                selected = "fatturato20"))),
#   fluidRow(column(7,
#                   sliderInput("slider2", h3("seleziona per ROE:"),
#                               min = -3 , max= 350, value=1 ))))


# Aggregazione per provincia

limiti_amministrativi_prov = read_sf("../Limiti01012020_g/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")

limiti_amministrativi_prov = limiti_amministrativi_prov %>% 
  select(-COD_RIP, -COD_UTS,-COD_PROV,-COD_CM,-DEN_CM,-TIPO_UTS,-SHAPE_AREA, -SHAPE_LEN, "Provincia"=DEN_UTS, -DEN_PROV) # in DEN_PROV ci sono alune osservazioni mancanti. Si è scelto di prendere DEN_UTS e rinominarlo come "Provincia"


aggregare =function(data){
  data= 
    ds %>%
    group_by(provincia) %>% 
    summarise(fatturato20= sum(fatturato20),
              utile20 = sum(na.omit(utile20)),
              roe20= mean(na.omit(roe20)),
              ros20= mean(na.omit(ros20))) %>% 
    left_join(., limiti_amministrativi_prov, by = c(provincia = "SIGLA")) 
  
  return(data)
}

fatturato_provincia = aggregare(fatturato_provincia)

ds$provincia= stringr::str_replace_all(ds$provincia, "OC", "PC")
ds$provincia= stringr::str_replace_all(ds$provincia, "OT", "SS")
ds$provincia= stringr::str_replace_all(ds$provincia, "PGG", "PG")
fatturato_provincia = aggregare(fatturato_provincia)





l_amm=st_transform(st_as_sf(fatturato_provincia %>% select(Provincia ,everything())))

geo_limiti_amministrativi= st_transform(limiti_amministrativi_prov %>% select(-COD_REG))

mappa_provinc= function(input, output){
  
  popup= c("fatturato20","Provincia")
  mappa_provincia=
    tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
    tm_shape(geo_limiti_amministrativi) +
    tmap_options(check.and.fix = TRUE) +
    tm_polygons() +
    l_amm %>% 
    dplyr::filter(fatturato20 >= input$slider,
                  roe20 >= input$slider2,
                  utile20 >= input$slider_utile) %>%
    tm_shape(.)+
    tm_fill(input$radio, breaks = seq(0, 14, by = 2) * 1e8,  popup.vars = popup) +
    #tm_fill(popup.vars = popup)+
    tm_borders()
  
  
  return(mappa_provincia)
  
  # if ( str_c(input$radio_vis)== "province"){
  #   return(mappa_provincia)
  # }
  # if ( str_c(input$radio_vis)== "aziende"){
  #   return(mappa_aziende)
  # }
    

}


# Regione


limiti_amministrativi_reg = read_sf("../Limiti01012020_g/Reg01012020_g/Reg01012020_g_WGS84.shp")
limiti_amministrativi_reg= limiti_amministrativi_reg %>% 
  select(-COD_RIP,-SHAPE_AREA, -SHAPE_LEN)
limiti_amministrativi_reg

provincia_reg= fatturato_provincia %>% 
  select(-geometry)  %>% # rimuovo geometry delle province
  left_join(.,limiti_amministrativi_reg , by = "COD_REG")

fatturato_regione = fatturato_provincia %>% 
  select(-geometry)  %>% # rimuovo geometry delle province  
  group_by(COD_REG) %>% 
  summarise(fatturato20= sum(fatturato20),
            utile20 = sum(na.omit(utile20)),
            roe20= mean(na.omit(roe20)),
            ros20= mean(na.omit(ros20))) %>% 
  left_join(.,limiti_amministrativi_reg , by = "COD_REG")

geo_fatturato_reg= st_transform(st_as_sf(fatturato_regione %>% select(DEN_REG, everything()))) #reorder??
geo_limiti_amministrativi_reg= st_transform(limiti_amministrativi_reg %>% select(DEN_REG, everything()))




  


server <- function(input, output) {
#   output$map = renderTmap({ if ("province" == "province"){
#     
#     mappa_provinc(input, output)}})
# }
  
  # output$map = renderTmap({
  #   mappa_provinc(input, output)
  # })}
  #x = reactive({input$radio_vis})

  # res = reactive({if (input$radio_vis == "province") {mappa_provinc(input, output)}})
  # 
  # output$map = renderTmap(res())
                        

  fatturato = reactive({if(input$radio_vis == "province") {min(ds$fatturato20)}})
  output$fatturato= renderText({fatturato()})
  
# mappa reattiva  
  reactive_map <- reactive({
    if(input$radio_vis == "province") {
      mappa_provinc(input, output)
    } else if (input$radio_vis == "località") {
        popup = c("fatturato20", "fatturato19", "roe20")
        geo_ds %>%
        dplyr::filter(fatturato20 >= input$slider,
                      roe20 >= input$slider2,
                      utile20 >= input$slider_utile) %>%
          tm_shape(.)+
          tm_basemap(leaflet::providers$CartoDB.DarkMatter)+
          tm_symbols(jitter = 0.5,
                     col = input$radio ,
                     size = input$radio ,
                     style= "jenks",
                     n=7,
                     popup.vars = popup
          )
      
    }else if (input$radio_vis == "regioni") {
      popup= c("fatturato20","DEN_REG")
      tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
      tm_shape(geo_limiti_amministrativi_reg)+
        tm_polygons()+
        #tm_polygons(col= "fatturato20")+
        geo_fatturato_reg %>% 
        dplyr::filter(fatturato20 >= input$slider,
                      roe20 >= input$slider2,
                      utile20 >= input$slider_utile) %>%
        tm_shape(.)+
        tm_fill(input$radio , breaks = seq(0, 70, by = 10) * 1e8, popup.vars = popup)+
        tm_borders() 
    }
})
  # Utilizzare la variabile reattiva come output
  output$map <- renderTmap({reactive_map()})
  
}



  #Findind the top distance between input coordinates and all other cycle stations, then sorting them.
# data = reactive({
#   geo_ds
# })
  #   geo_ds$fatturato20 = st_point(input_pt()) %>%  
  #     st_sfc() %>% 
  #     st_set_crs(4326) %>% 
  #     st_distance(geo_ds$geometry) %>%
  #     t() %>% 
  #     set_units(.,"euro")
  #   
  #   geo_ds[order(geo_ds$geometry),]
  # })
  
  #Filtering the distance data from above to show top 5 closest stations meeting requirement of # of bikes needed  
  # filteredData = reactive({
  #   filter(data(), fatturato20 >= input$slider) %>% head(10) %>%
  #     mutate(popup = str_c(str_c("Azienda:", nome, sep=" "),
  #                          str_c("fatturato nel 2020:", fatturato20, sep=" "), sep = "<br/>"))
  # 
  # })
  
  # #Making changes to the output leaflet map reflecting the cycle stations found above
  # icons = awesomeIcons(icon = "bicycle", library = "fa", squareMarker = TRUE, markerColor = "blue")
  # 
  # observe({
  #   proxy = tmapProxy("map", data =filteredData()) %>% clearMarkers()})
  # #   
  #   proxy %>%
  #     clearMarkers() %>% 
  #     addAwesomeMarkers(icon = icons, popup = ~popup) %>% 
  #     addMarkers(lng = input_pt()[, "X"], input_pt()[, "Y"], label = "Your Location")
  #   
  # })

# Run the application
shinyApp(ui = ui, server = server)  
