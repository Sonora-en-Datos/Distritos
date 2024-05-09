
library(sf)
library(tidyverse)
library(rcartocolor)
library(MetBrewer)
library(htmlwidgets)
library(leaflet.extras)
library(leafem)
library(leaflet)



capa_federal <- st_read("shape/DISTRITO_FEDERAL.shp", quiet = TRUE) %>% 
  st_transform('+proj=longlat +datum=WGS84')


color_federal = met.brewer("Cross", n=8)

federal <- sample(color_federal,length(color_federal),replace=FALSE)

  diputadosfederales <- read_csv("datos/diputadosfederales.csv")

capa_federal <- capa_federal %>% 
  left_join(diputadosfederales, by="DISTRITO") %>% 
  mutate(fill_color= case_when(DISTRITO==1 ~ federal[[1]],
                               DISTRITO==2 ~ federal[[2]],
                               DISTRITO==3 ~ federal[[3]],
                               DISTRITO==4 ~ federal[[4]],
                               DISTRITO==5 ~ federal[[5]],
                               DISTRITO==6 ~ federal[[6]],
                               DISTRITO==7 ~ federal[[7]]
                               )) %>% 
  janitor::clean_names()

pop_fed <- paste0(
  "<b>", "DISTRITO FEDERAL", as.character(capa_federal$distrito), "</b>",     "<br></p>",
  "<b>FUERZA Y CORAZÓN POR SONORA: </b><br>",capa_federal$fuerza_y_corazon_por_mexico, "<br></p>",
  "<b>SIGAMOS HACIENDO HISTORIA: </b><br>",capa_federal$sigamos_haciendo_historia, "<br></p>",
  "<b>", "MOVIMIENTO CIUDADANO   ", "</b><br>",   capa_federal$movimiento_ciudadano,"<br></p>"
)  %>% lapply(htmltools::HTML)


capa_local <- st_read("shape/DISTRITO_LOCAL.shp", quiet = TRUE) %>% 
st_transform('+proj=longlat +datum=WGS84') 


color_local = met.brewer("Cross", n=22)

local <- sample(color_local,length(color_local),replace=FALSE)

diputadoslocales <- read_csv("datos/diputadoslocales.csv")

capa_local <- capa_local %>% 
  left_join(diputadoslocales, by="DISTRITO_L") %>% 
  mutate(fill_color= case_when(DISTRITO_L==1 ~ local[[1]],
                               DISTRITO_L==2 ~ local[[2]],
                               DISTRITO_L==3 ~ local[[3]],
                               DISTRITO_L==4 ~ local[[4]],
                               DISTRITO_L==5 ~ local[[5]],
                               DISTRITO_L==6 ~ local[[6]],
                               DISTRITO_L==7 ~ local[[7]],
                               DISTRITO_L==8 ~ local[[8]],
                               DISTRITO_L==9 ~ local[[9]],
                               DISTRITO_L==10 ~ local[[10]],
                               DISTRITO_L==11 ~ local[[11]],
                               DISTRITO_L==12 ~ local[[12]],
                               DISTRITO_L==13 ~ local[[13]],
                               DISTRITO_L==14 ~ local[[14]],
                               DISTRITO_L==15 ~ local[[15]],
                               DISTRITO_L==16 ~ local[[16]],
                               DISTRITO_L==17 ~ local[[17]],
                               DISTRITO_L==18 ~ local[[18]],
                               DISTRITO_L==19 ~ local[[19]],
                               DISTRITO_L==20 ~ local[[20]],
                               DISTRITO_L==21 ~ local[[21]]
  )) %>% 
  janitor::clean_names() 


popup <- paste0(
  "<b>", "DISTRITO ", as.character(capa_local$distrito_local), "</b>",     "<br></p>",
  "<b>", if_else(!is.na(capa_local$fuerza_y_cora_zo_n_por_sonora),paste0("FUERZA Y CORAZÓN POR SONORA: </b><br>",capa_local$fuerza_y_cora_zo_n_por_sonora, "<br></p>"),
                 if_else(!is.na(capa_local$pan_pri_prd),paste0("PAN, PRI, PRD: </b><br>",capa_local$pan_pri_prd, "<br></p>"),
                         paste0("PAN:</b><br>",capa_local$pan, "<br></p>
                        <b>PRI:</b><br>",capa_local$pri, "<br></p>
                        <b>PRD:</b><br>",capa_local$prd, "<br></p>"))),
  "<b>", if_else(is.na(capa_local$morena),paste0("MORENA, PT, PVEM,NAS, PES: </b><br>",capa_local$morena_pt_pvem_nas_pes, "<br></p>"),
                 paste0("MORENA:</b><br>",capa_local$morena, "<br></p>")),
  "<b>", "MOVIMIENTO CIUDADANO   ", "</b><br>",   capa_local$movimiento_ciudadano,"<br></p>",
  "<b>", "PARTIDO SONORENSE", "</b><br>",   capa_local$partido_sonorense,  "<br></p>"
)  %>% lapply(htmltools::HTML)

mapa_distritos <- leaflet(capa_local) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data= capa_local,
              stroke= TRUE,
              weight=0.5,                   
              opacity=1,
              fillColor = capa_local$fill_color,
              color= capa_local$fill_color,
              fillOpacity = 0.1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1,
                                                  bringToFront = FALSE,
                                                  #fillColor=capa_local$fill_color,
                                                  fillOpacity = 0.3),
              group = 'DISTRITOS LOCALES',
#              label=label,
              popup = popup,
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              # labelOptions = labelOptions(noHide = F, direction = "auto", offset = c(12, 0), opacity=0.9,
              #                             style = list(
              #                               "color" = "black",
              #                               "font-family" = "Arial",
              #                               "font-style" = "regular",
              #                               "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
              #                               "font-size" = "14px",
              #                               "border-color" = "rgba(0,0,0,0.5)"
#                                          ))
) %>% 
  addPolygons(data= capa_federal,
              stroke= TRUE,
              weight=0.5,                   
              opacity=1,
              fillColor = capa_federal$fill_color,
              color= capa_federal$fill_color,
              fillOpacity = 0.1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1,
                                                  bringToFront = FALSE,
                                                  #fillColor=capa_federal$fill_color,
                                                  fillOpacity = 0.3),
              group = 'DISTRITOS FEDERALES',
              #              label=label,
              popup = pop_fed,
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              # labelOptions = labelOptions(noHide = F, direction = "auto", offset = c(12, 0), opacity=0.9,
              #                             style = list(
              #                               "color" = "black",
              #                               "font-family" = "Arial",
              #                               "font-style" = "regular",
              #                               "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
              #                               "font-size" = "14px",
              #                               "border-color" = "rgba(0,0,0,0.5)"
              #                                          ))
  ) %>% 
  # addSearchFeatures(targetGroups= 'DI', options = searchFeaturesOptions(zoom=13, openPopup=TRUE,moveToLocation=TRUE,
  #                                                                           firstTipSubmit = TRUE,
  #                                                                           autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>% 
  addLogo("https://www.luisarmandomoreno.com/wp-content/uploads/2022/05/SEDfesp.png",
          src= "remote", position = "topright", url = "https://www.sonoraendatos.com/",
          width = 100,
          height = 90) %>% 
  addLayersControl(
    baseGroups = c("DISTRITOS LOCALES", "DISTRITOS FEDERALES"),
    position = c("topleft"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% 
  leaflet.extras::addSearchOSM()

mapa_distritos
saveWidget(mapa_distritos,"docs/index.html",  title= "Distritos 2023 - Sonora en Datos",selfcontained = F, libdir = "lib")

