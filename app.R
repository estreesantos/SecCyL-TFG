# app.R

library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(robustbase)

datos <- readRDS("datosTFG.rds")
finalsec <- datos$finalsec
finalsec_df <- datos$finalsec_df
datos_scaled <- datos$datos_scaled
vars_base <- datos$vars_base
vars_tabla <- datos$vars_tabla

porcien_vars <- c("t4", "t4_2", "t4_3", "t5", "t9",
                  "t10", "t11", "t12", "t16")

finalsec <- finalsec %>%
  mutate(across(all_of(porcien_vars), ~ .x * 100))

finalsec <- st_transform(finalsec, crs = 4326)
finalsec <- st_make_valid(finalsec)
finalsec <- finalsec[st_is_valid(finalsec), ]  # quitar geometrías no válidas

avila <- finalsec %>% filter(cod_mun == "05019")
burgos <- finalsec %>% filter(cod_mun == "09059")
leon <- finalsec %>% filter(cod_mun == "24089")
palencia <- finalsec %>% filter(cod_mun == "34120")
salamanca <- finalsec %>% filter(cod_mun == "37274")
segovia <- finalsec %>% filter(cod_mun == "40194")
soria <- finalsec %>% filter(cod_mun == "42173")
valladolid <- finalsec %>% filter(cod_mun == "47186")
zamora <- finalsec %>% filter(cod_mun == "49275")


mcd_obj <- covMcd(datos_scaled,alpha=0.75)

mu_robusta  <- mcd_obj$center
cov_robusta <- mcd_obj$cov


buscar_mas_parecida_maha_global <- function(
    cusec_ref,
    datos_df,
    datos_scaled,
    vars_tabla,
    cov_robusta,
    session = shiny::getDefaultReactiveDomain()
) {
  ref_idx <- which(datos_df$CUSEC == cusec_ref)
  if (!length(ref_idx)) {
    showNotification("CUSEC no encontrado.", type = "error", session = session)
    return(NULL)
  }
  
  # Municipio/provincia
  cod_mun_ref <- datos_df$cod_mun[ref_idx]
  cpro_ref    <- datos_df$CPRO[ref_idx]
  
  # Subconjunto (municipio o provincia)
  subset_idx <- which(datos_df$cod_mun == cod_mun_ref)
  if (length(subset_idx) <= 1) {
    showNotification(
      "Solo hay una sección en el municipio, se va a buscar en la provincia.",
      type = "warning", session = session
    )
    subset_idx <- which(datos_df$CPRO == cpro_ref)
  }
  
  subs_df       <- datos_df[subset_idx, ]
  scaled_subset <- datos_scaled[subset_idx, vars_tabla, drop = FALSE]
  
  # sección de referencia
  ref_in_sub <- which(subs_df$CUSEC == cusec_ref)
  seccion_ref <- scaled_subset[ref_in_sub, , drop = FALSE]
  
  dist_mcd <- apply(
    scaled_subset, 1,
    function(x) mahalanobis(x, center = as.numeric(seccion_ref), cov = cov_robusta)
  )
  
  out <- data.frame(CUSEC = subs_df$CUSEC, distancia = dist_mcd)
  out <- out[out$CUSEC != cusec_ref, ]
  out[order(out$distancia), ][1, ]
}


# UI
ui <- dashboardPage(
  skin = "red",  
  
  dashboardHeader(title = "SecCyL"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("globe-europe")),
      menuItem("Sección Similar CyL", tabName = "busqueda", icon = icon("search-location")),
      menuItem("Capitales de provincia", icon = icon("building"),
               menuSubItem("Ávila", tabName = "capital_avila", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Burgos", tabName = "capital_burgos", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("León", tabName = "capital_leon", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Palencia", tabName = "capital_palencia", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Salamanca", tabName = "capital_salamanca", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Segovia", tabName = "capital_segovia", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Soria", tabName = "capital_soria", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Valladolid", tabName = "capital_valladolid", icon = icon("circle", class = "text-danger fa-2xs")),
               menuSubItem("Zamora", tabName = "capital_zamora", icon = icon("circle", class = "text-danger fa-2xs"))
      )
      
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: white !important;
      }
    "))
    ),
    tabItems(
      # 1. Inicio
      tabItem(tabName = "inicio",
              fluidRow(
                column(6, align = "left",
                       img(src = "logo_app.png", height = "160px", style = "margin: 20px;")
                ),
                column(6, align = "right",
                       h1("Buscador de Secciones Censales de Castilla y León", 
                          style = "color:#b52b27; font-weight:bold; margin: 40px 20px 20px 0;")
                )
              ),
              fluidRow(
                column(12, h2("Definiciones de las variables utilizadas", style = "color:#b52b27; font-weight: bold;"))
              ),
              
              fluidRow(
                column(6,
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t1 - Número total de personas en la sección"),
                           p("Cuenta total de residentes en la sección censal.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t4 - % de población menor de 16 años"),
                           p("Proporción de personas con menos de 16 años sobre el total de población.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t4_3 - % de población con más de 64 años"),
                           p("Proporción de personas de 65 años o más sobre el total de población.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t5 - % de población extranjera"),
                           p("Proporción de personas con nacionalidad extranjera sobre el total de población.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t10 - Tasa de paro"),
                           p("Porcentaje de población parada sobre la población activa.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t12 - Tasa de actividad"),
                           p("Proporción de población activa sobre la población de 16 y más años.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t24 - Tipo de entorno"),
                           p("Variable categórica que diferencia capital, municipio grande o pequeño.")
                       )
                ),
                
                column(6,
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t4_2 - % de población entre 16 y 64 años"),
                           p("Proporción de personas entre 16 y 64 años sobre el total de población.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t9 - % con estudios superiores"),
                           p("Porcentaje de población con estudios superiores sobre la población de 16 y más años.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t11 - Tasa de empleo"),
                           p("Porcentaje de población ocupada sobre la población de 16 y más años.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t16 - % de estudiantes"),
                           p("Porcentaje de población estudiante sobre la población de 16 y más años.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("t23 - Tamaño medio del hogar"),
                           p("Número medio de personas por hogar en la sección censal.")
                       ),
                       box(width = 12, status = "danger", solidHeader = FALSE,
                           h4("Renta - Renta media neta"),
                           p("Renta media neta por unidad de consumo de la sección censal.")
                       )
                )
              )
      )
      ,
      
      # 2. Sección Similar
      tabItem(tabName = "busqueda",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_busqueda", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 320, style = "z-index:500;",
                                div(style = "border:2px solid #d9534f; border-radius:8px; padding:10px; background-color:white;",
                                    selectInput("provincia", "Provincia (CPRO):", choices = c(
                                      "05 - Ávila" = "05", "09 - Burgos" = "09", "24 - León" = "24",
                                      "34 - Palencia" = "34", "37 - Salamanca" = "37", "40 - Segovia" = "40",
                                      "42 - Soria" = "42", "47 - Valladolid" = "47", "49 - Zamora" = "49")),
                                    uiOutput("municipio_ui"),
                                    uiOutput("seccion_ui"))))
      )
      ,
      
      tabItem(tabName = "indicadores_mapas",
              fluidRow(
                box(title = "Selecciona indicador y provincia", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("provincia_mapa", "Provincia:", choices = sort(unique(finalsec$CPRO))),
                    selectInput("indicador_mapa", "Indicador:",
                                choices = c("Renta media" = "Renta",
                                            "% Paro" = "t10",
                                            "% Empleo" = "t11"))
                ),
                box(title = "Mapa del indicador", width = 8, status = "primary",
                    withSpinner(leafletOutput("mapa_indicador", height = 600), 
                                type = 1, color = "darkred")
              )
      )),
      
      tabItem(tabName = "capital_avila",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_avila", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_avila", "Selecciona sección censal:",
                                            choices = sort(avila$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_burgos",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_burgos", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_burgos", "Selecciona sección censal:",
                                            choices = sort(burgos$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_leon",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_leon", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_leon", "Selecciona sección censal:", choices = sort(leon$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_palencia",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_palencia", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_palencia", "Selecciona sección censal:",
                                            choices = sort(palencia$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_salamanca",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_salamanca", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_salamanca", "Selecciona sección censal:",
                                            choices = sort(salamanca$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_segovia",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_segovia", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_segovia", "Selecciona sección censal:",
                                            choices = sort(segovia$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_soria",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_soria", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_soria", "Selecciona sección censal:",
                                            choices = sort(soria$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_valladolid",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_valladolid", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_valladolid", "Selecciona sección censal:",
                                            choices = sort(valladolid$CUSEC))))
      )
      ,
      tabItem(tabName = "capital_zamora",
              div(style = "position:relative;",
                  withSpinner(leafletOutput("mapa_zamora", height = 600), 
                              type = 1, color = "darkred"),
                  absolutePanel(top = 20, right = 20, draggable = TRUE, width = 300, style = "z-index:500;",
                                selectInput("cusec_zamora", "Selecciona sección censal:",
                                            choices = sort(zamora$CUSEC))))
      )
    
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # selector de municipio en función de la provincia
  output$municipio_ui <- renderUI({
    req(input$provincia)
    id_notif <-showNotification("Cargando municipios...", duration = NULL, type = "message")
    
    # CMUN y NMUN (nombre) y los ordenamos
    municipios_filtrados <- finalsec %>%
      filter(CPRO == input$provincia) %>%
      distinct(CMUN, NMUN) %>%
      arrange(CMUN)
    
    removeNotification(id_notif)  
    
    choices <- setNames(
      municipios_filtrados$CMUN,
      paste0(municipios_filtrados$CMUN, " - ", municipios_filtrados$NMUN)
    )
    
    selectInput("municipio", "Municipio (CMUN):", choices = choices)
  })
  
  
  # selector de sección en función del municipio
  output$seccion_ui <- renderUI({
    req(input$provincia, input$municipio)
    
    id_notif <- showNotification("Cargando secciones...", duration = NULL, type = "message")
    
    secciones_filtradas <- finalsec %>%
      filter(CPRO == input$provincia, CMUN == input$municipio)
    
    removeNotification(id_notif)
    
    selectInput("seccion_ref", "CUSEC:", choices = sort(secciones_filtradas$CUSEC))
  })
  
  observeEvent(input$seccion_ref, {
    req(input$seccion_ref)
    
    # sección seleccionada
    secc <- finalsec %>% filter(CUSEC == input$seccion_ref)
    req(nrow(secc) > 0)
    
    # calcular sección más parecida
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$seccion_ref,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta,
      session = session
    )
    
    if (is.character(resultado)) {
      showNotification(resultado, type = "error")
      return(NULL)
    }
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    output$mapa_busqueda <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = finalsec, fillColor = "gray90", color = "white", weight = 0.3) %>%
        addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                    label = popup_secc) %>%
        addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                    label = popup_similar)
    })
  })
  
  output$mapa_avila <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = avila, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_avila, {
    req(input$cusec_avila)
    
    secc <- avila %>% filter(CUSEC == input$cusec_avila)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_avila,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta,
      session = session
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_avila") %>%
      clearShapes() %>%
      addPolygons(data = avila, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  
  output$mapa_burgos <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = burgos, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_burgos, {
    req(input$cusec_burgos)
    
    secc <- burgos %>% filter(CUSEC == input$cusec_burgos)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_burgos,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta,
      session = session
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_burgos") %>%
      clearShapes() %>%
      addPolygons(data = burgos, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  
  output$mapa_leon <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = leon, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_leon, {
    req(input$cusec_leon)
    
    secc <- leon %>% filter(CUSEC == input$cusec_leon)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_leon,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_leon") %>%
      clearShapes() %>%
      addPolygons(data = leon, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  
  output$mapa_palencia <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = palencia, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_palencia, {
    req(input$cusec_palencia)
    
    secc <- palencia %>% filter(CUSEC == input$cusec_palencia)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_palencia,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_palencia") %>%
      clearShapes() %>%
      addPolygons(data = palencia, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  
  output$mapa_salamanca <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = salamanca, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_salamanca, {
    req(input$cusec_salamanca)
    
    secc <- salamanca %>% filter(CUSEC == input$cusec_salamanca)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_salamanca,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_salamanca") %>%
      clearShapes() %>%
      addPolygons(data = salamanca, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  output$mapa_segovia <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = segovia, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_segovia, {
    req(input$cusec_segovia)
    
    secc <- segovia %>% filter(CUSEC == input$cusec_segovia)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_segovia,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_segovia") %>%
      clearShapes() %>%
      addPolygons(data = segovia, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  output$mapa_soria <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = soria, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_soria, {
    req(input$cusec_soria)
    
    secc <- soria %>% filter(CUSEC == input$cusec_soria)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_soria,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_soria") %>%
      clearShapes() %>%
      addPolygons(data = soria, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  output$mapa_valladolid <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = valladolid, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_valladolid, {
    req(input$cusec_valladolid)
    
    secc <- valladolid %>% filter(CUSEC == input$cusec_valladolid)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_valladolid,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_valladolid") %>%
      clearShapes() %>%
      addPolygons(data = valladolid, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  output$mapa_zamora <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = zamora, fillColor = "gray90", color = "white", weight = 0.5)
  })
  
  observeEvent(input$cusec_zamora, {
    req(input$cusec_zamora)
    
    secc <- zamora %>% filter(CUSEC == input$cusec_zamora)
    
    resultado <- buscar_mas_parecida_maha_global(
      cusec_ref = input$cusec_zamora,
      datos_df = finalsec_df,
      datos_scaled = datos_scaled,
      vars_tabla = vars_tabla,
      cov_robusta = cov_robusta
    )
    
    similar_cusec <- resultado$CUSEC
    secc_similar <- finalsec %>% filter(CUSEC == similar_cusec)
    
    popup_secc <- lapply(paste0(
      "<strong>Seleccionada:</strong> ", secc$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc$t23, 1)
    ), htmltools::HTML)
    
    popup_similar <- lapply(paste0(
      "<strong>Más parecida:</strong> ", secc_similar$CUSEC, "<br/>",
      "<strong>Renta media/ud de consumo:</strong> ", round(secc_similar$Renta, 2), " €<br/>",
      "<strong>% Paro:</strong> ", round(secc_similar$t10, 1), " %<br/>",
      "<strong>% Extranjeros:</strong> ", round(secc_similar$t5, 1), " %<br/>",
      "<strong>Nº medio personas por hogar:</strong> ", round(secc_similar$t23, 1)
    ), htmltools::HTML)
    
    leafletProxy("mapa_zamora") %>%
      clearShapes() %>%
      addPolygons(data = zamora, fillColor = "gray90", color = "white", weight = 0.5) %>%
      addPolygons(data = secc, fillColor = "darkred", color = "black", weight = 2,
                  label = popup_secc) %>%
      addPolygons(data = secc_similar, fillColor = "blue", color = "black", weight = 2,
                  label = popup_similar)
  })
  
  
}


shinyApp(ui, server)