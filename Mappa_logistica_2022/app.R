pkgs = c("sf", "shiny", "spData", "leaflet", "tidyverse", "spDataLarge", "units", "tmap", "dplyr")
invisible(lapply(pkgs, library, character.only = TRUE))



# import Data
logistica_2022=readr::read_csv("../GDLOG classifica 2022__NGEOM.csv", na = "N.D.", 
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


selection= c("nome"= "RAGIONE SOCIALE", "comune"="SEDE", "provincia"="PROV", "fatturato20"="FATTURATO ‘20", 
             "fatturato19"="FATTURATO ’19", "utile20"="UTILE ‘20", "utile19"="UTILE ’19", 
             "ros20"= "ROS ‘20", "ros19"="ROS ‘19", "roe20"= "ROE ‘20", "roe19"= "ROE ‘19" )

ds = logistica_2022 %>%
  select(-`POS. '22`, -`POS. '21` ) %>% #-`...1`
  select(selection, everything()) %>% 
  mutate(fatturato20= numeric_conversion(fatturato20),
         fatturato19= numeric_conversion(fatturato19),
         utile20 = numeric_conversion(utile20),
         utile19 = numeric_conversion(utile19),
         ros20= numeric_conversion2(ros20),
         ros19= numeric_conversion2(ros19),
         roe20= numeric_conversion2(roe20),
         roe19= numeric_conversion2(roe19))


ds= ds %>% 
  mutate(variazione_fatturato= fatturato20 - fatturato19,
         variazione_utile= utile20 - utile19) %>%   
  mutate(variazione_fatturato_percentuale = round(100* variazione_fatturato/fatturato20, 2), 
         variazione_utile_percentuale = round((100*variazione_utile/utile20), 2),)

geo_ds= st_as_sf(ds, coords = c("longitude", "latitude"), crs = "EPSG:4326")
geo_ds= st_point_on_surface(geo_ds)


# Aggregazione per provincia

limiti_amministrativi_prov = read_sf("../Limiti01012020_g/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")

limiti_amministrativi_prov = limiti_amministrativi_prov %>% 
  select(-COD_RIP, -COD_UTS,-COD_PROV,-COD_CM,-DEN_CM,-TIPO_UTS,-SHAPE_AREA, -SHAPE_LEN, "Provincia"=DEN_UTS, -DEN_PROV) # in DEN_PROV ci sono alune osservazioni mancanti. Si è scelto di prendere DEN_UTS e rinominarlo come "Provincia"


aggregare =function(data){
  data= 
    ds %>%
    group_by(Provincia) %>% 
    summarise(fatturato20= sum(fatturato20),
              fatturato19= sum(fatturato19, na.rm = T),
              utile20 = sum(utile20, na.rm = T),
              utile19 = sum(utile19, na.rm = T),
              variazione_fatturato_percentuale = mean(variazione_fatturato_percentuale), #fare una media?
              variazione_utile_percentuale = mean(100*sum(variazione_utile, na.rm = T)/sum(utile20), 2),
              roe20= mean(roe20, na.rm= T),
              ros20= mean(ros20, na.rm = T)) %>% 
    left_join(., limiti_amministrativi_prov, by = "Provincia") #%>% 
  
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
    dplyr::filter(fatturato20 >= input$slider_fatturato,
                  variazione_fatturato_percentuale >= input$slider_var_fatt,
                  utile20 >= input$slider_utile) %>%
    tm_shape(.)+
    tm_fill(input$radio,style = "jenks", n= 7,  popup.vars = popup) +
    #tm_fill(popup.vars = popup)+
    tm_borders()
  
  
  return(mappa_provincia)
}


# Regione


limiti_amministrativi_reg = read_sf("../Limiti01012020_g/Reg01012020_g/Reg01012020_g_WGS84.shp")
limiti_amministrativi_reg= limiti_amministrativi_reg %>% 
  select(-COD_RIP,-SHAPE_AREA, -SHAPE_LEN)



fatturato_regione = fatturato_provincia %>% 
  select(-geometry)  %>% # rimuovo geometry delle province  
  group_by(COD_REG) %>% 
  summarise(fatturato20= sum(fatturato20),
            fatturato19= sum(fatturato19, na.rm = T),
            utile20 = sum(utile20, na.rm = T),
            utile19 = sum(utile19, na.rm = T),
            variazione_fatturato_percentuale = sum(variazione_fatturato_percentuale, na.rm = T), #fare una media?
            variazione_utile_percentuale = sum(variazione_utile_percentuale, na.rm = T),
            roe20= mean(roe20, na.rm= T),
            ros20= mean(ros20, na.rm = T)) %>% 
  left_join(.,limiti_amministrativi_reg , by = "COD_REG")

geo_fatturato_reg= st_transform(st_as_sf(fatturato_regione %>% select(DEN_REG, everything()))) #reorder??
geo_limiti_amministrativi_reg= st_transform(limiti_amministrativi_reg %>% select(DEN_REG, everything()))



# ui

ui = fluidPage(
  div(
    style = "display:flex; align-items:flex-start",
    div(
    wellPanel(style= "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
              radioButtons("radio_vis", h3("Visualizza per:"),
                           choices = list("regioni", "province", "località"), 
                           selected = "località"),
              br(),
              radioButtons("radio", h3("Tipo di aggregazione"),
                                     choices = list("fatturato20", "utile20", "roe20", "ros20" ), 
                                     selected = "fatturato20"),
              br(),
              uiOutput("slider_fatturato"),
              br(),
              uiOutput("slider_utile"),
              br(),
              uiOutput("slider_var_fatt"),
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
  

# f= function(datacolumn){renderUI({
#   maxfatt <- max(datacolumn)
#   minfatt <- min(datacolumn)
#   
#   sliderInput("slider_fatturato", h3("Filtra per fatturato:") , 
#               min   = minfatt, 
#               max   = maxfatt,
#               value = minfatt)
# })
#   }

server <- function(input, output){
  
  # output$slider_fatturato <- renderUI({if(input$radio_vis == "località"){f(geo_ds$fatturato20)}
  #   else if (input$radio_vis == "province") {f(l_amm$fatturato20)}
  #   else if (input$radio_vis == "regioni") {f(geo_fatturato_reg$fatturato20)}
  # })

#slider_fatturato  
  output$slider_fatturato <- renderUI({
    if(input$radio_vis == "località"){
      max_fatt <- max(na.omit(geo_ds$fatturato20))
      min_fatt <- min(na.omit(geo_ds$fatturato20))
    
    sliderInput("slider_fatturato", h3("Filtra per fatturato:") , 
                min   = min_fatt, 
                max   = max_fatt,
                value = min_fatt)}
    
    else if (input$radio_vis == "province") {
      max_fatt <- max(l_amm$fatturato20 )
      min_fatt <- min(l_amm$fatturato20 )
      
      sliderInput("slider_fatturato", h3("Filtra per fatturato:") , 
                  min   = min_fatt, 
                  max   = max_fatt,
                  value = min_fatt)}
    
    else if (input$radio_vis == "regioni") {
      max_fatt <- max(geo_fatturato_reg$fatturato20 )
      min_fatt <- min(geo_fatturato_reg$fatturato20)
      
      sliderInput("slider_fatturato", h3("Filtra per fatturato:") , 
                  min   = min_fatt, 
                  max   = max_fatt,
                  value = min_fatt)}
  })
  
#slider_utile
  output$slider_utile <- renderUI({if(input$radio_vis == "località"){
    max_utile <- max(na.omit(geo_ds$utile20))
    min_utile <- min(na.omit(geo_ds$utile20))
    
    sliderInput("slider_utile", h3("Filtra per utile:") , 
                min   = min_utile, 
                max   = max_utile,
                value = min_utile)}
    
    else if (input$radio_vis == "province") {
      max_utile <- max(l_amm$utile20 )
      min_utile <- min(l_amm$utile20 )
      
      sliderInput("slider_utile", h3("Filtra per utile:") , 
                  min   = min_utile, 
                  max   = max_utile,
                  value = min_utile)}
    
    else if (input$radio_vis == "regioni") {
      max_utile <- max(geo_fatturato_reg$utile20 )
      min_utile <- min(geo_fatturato_reg$utile20)
      
      sliderInput("slider_utile", h3("Filtra per utile:") , 
                  min   = min_utile, 
                  max   = max_utile,
                  value = min_utile)}
  })
  
#slider_var_fatt  
  output$slider_var_fatt <- renderUI({if(input$radio_vis == "località"){
    max_v_fatt <- max(na.omit(geo_ds$variazione_fatturato_percentuale))
    min_v_fatt <- min(na.omit(geo_ds$variazione_fatturato_percentuale))
    
    sliderInput("slider_var_fatt", h3("Filtra per variazione % fatturato:") , 
                min   = min_v_fatt, 
                max   = max_v_fatt,
                value = min_v_fatt)}
    
    else if (input$radio_vis == "province") {
      max_v_fatt <- max(na.omit(l_amm$variazione_fatturato_percentuale ))
      min_v_fatt <- min(na.omit(l_amm$variazione_fatturato_percentuale ))
      
      sliderInput("slider_var_fatt", h3("Filtra per variazione % fatturato:") , 
                  min   = min_v_fatt, 
                  max   = max_v_fatt,
                  value = min_v_fatt)}
    
    else if (input$radio_vis == "regioni") {
      max_v_fatt <- max(na.omit(geo_fatturato_reg$variazione_fatturato_percentuale ))
      min_v_fatt <- min(na.omit(geo_fatturato_reg$variazione_fatturato_percentuale))
      
      sliderInput("slider_var_fatt", h3("Filtra per variazione % fatturato:") , 
                  min   = min_v_fatt, 
                  max   = max_v_fatt,
                  value = min_v_fatt)}
  })
  

# output$map
  
  output$map <- renderTmap({
    tm_basemap(leaflet::providers$CartoDB.DarkMatter)+

#Mappa per province
    #Mappa per località  
    if (input$radio_vis == "località") {
    popup = c("fatturato20", "variazione_fatturato_percentuale", "roe20", "comune")
    
    #tm_shape(geo_limiti_amministrativi)+
      #tm_polygons(alpha = 0, border.alpha = 0)+
      #tmap_options(check.and.fix = TRUE)+
      geo_ds %>% dplyr::filter(fatturato20 >= input$slider_fatturato,
                               variazione_fatturato_percentuale >= input$slider_var_fatt,
                               utile20 >= input$slider_utile) %>%
      tm_shape(.)+

      tm_symbols(jitter = 0.05,
                 col = input$radio ,
                 size = input$radio ,
                 style= "jenks",
                 n=7,
                 popup.vars = popup)}
    else if(input$radio_vis == "province") {
      popup= c("fatturato20","variazione_fatturato_percentuale", "Provincia")
        tm_shape(geo_limiti_amministrativi) +
        tmap_options(check.and.fix = TRUE) +
        tm_polygons(alpha = 0.6, border.alpha = 0.6) +
        l_amm %>% 
        dplyr::filter(fatturato20 >= input$slider_fatturato,
                      variazione_fatturato_percentuale >= input$slider_var_fatt,
                      utile20 >= input$slider_utile) %>%
        tm_shape(.)+
        tm_fill(input$radio,style = "jenks", n= 7,  popup.vars = popup, showNA = T ) +
        #tm_fill(popup.vars = popup)+
        tm_borders()
      
      
      

#Mappa per regioni
        
    }else if (input$radio_vis == "regioni") {
      popup= c("fatturato20","variazione_fatturato_percentuale", "roe20", "DEN_REG")
      tm_shape(geo_limiti_amministrativi_reg)+
        tm_polygons(alpha = 0.6, border.alpha = 0.6)+
        geo_fatturato_reg %>% 
        dplyr::filter(fatturato20 >= input$slider_fatturato,
                      variazione_fatturato_percentuale >= input$slider_var_fatt,
                      utile20 >= input$slider_utile) %>%
        tm_shape(.)+
        tm_fill(input$radio , style= "jenks", n=7, popup.vars = popup, showNA = T)+
        tm_borders()+
        tm_legend()
    }
})

#outputmap
  #output$map <- renderTmap({reactive_map()})
  
}




# Run the application
shinyApp(ui = ui, server = server)  
