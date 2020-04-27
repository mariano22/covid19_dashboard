library("htmltools")

addLabelt <- function(data) {
  if (dim(data)[1]==0) return(data)
  data$label <- paste0(
    '<b>', data$LOCATION, '</b><br>
    <table style="width:120px;">
    <tr><td>Confirmed:</td><td align="right">', data$CONFIRMADOS, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)

  return(data)
}

map_data_at_datet <- function(date_argument) {
    global_time_series_melt %>%
    filter(date == date_argument & CONFIRMADOS>0) %>%
    backend_filter_location_by_level(3) %>%
    addLabelt()
}

mapt <- leaflet() %>%
  setMaxBounds(-180, -90, 180, 90) %>%
  setView( 	-60.698839780023796, -31.6244477760916, zoom = 6) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
  addLayersControl(
    baseGroups    = c("Light", "Satellite"),
    overlayGroups = c("Confirmed")
  ) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([20, 0], 2); }"))) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-map-marker", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }")))

observe({
  req(input$timeSlider, input$overview_map_santa_fe_zoom)
  zoomLevel               <- input$overview_map_santa_fe_zoom
  data                    <- map_data_at_datet(input$timeSlider)
  k <- 50
  leafletProxy("overview_map_santa_fe") %>%
    clearMarkers() %>%
    addCircleMarkers(
      lng          = data$LONG,
      lat          = data$LAT,
      radius       = log(data$CONFIRMADOS^(zoomLevel / 2)+k),
      stroke       = FALSE,
      fillOpacity  = 0.5,
      label        = data$label,
      labelOptions = labelOptions(textsize = 15),
      group        = "Confirmed"
    )
})

output$overview_map_santa_fe <- renderLeaflet(mapt)
