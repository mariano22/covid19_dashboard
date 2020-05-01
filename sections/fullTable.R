getFullTableData <- function(groupBy) {
    padding_left <- max(str_length(global_time_series_melt$CONFIRMADOS_DIFF), na.rm = TRUE)

    global_time_series_melt %>%
    filter(date == current_date) %>%
    backend_filter_location_by_level(1) %>%
    mutate(CONFIRMADOS_PER100K = round(CONFIRMADOS_PER100K,2)) %>%
    mutate(MUERTOS_PER100K = round(MUERTOS_PER100K,2)) %>%
    mutate(ACTIVOS_PER100K = round(ACTIVOS_PER100K,2)) %>%
    .[,c("LOCATION",
         "CONFIRMADOS",
         "CONFIRMADOS_DIFF",
         "CONFIRMADOS_PER100K",
         "RECUPERADOS",
         "RECUPERADOS_DIFF",
         "MUERTOS",
         "MUERTOS_PER100K",
         "MUERTOS_DIFF",
         "ACTIVOS",
         "ACTIVOS_DIFF",
         "ACTIVOS_PER100K",
         "CONFIRMADOS_DIFF_RATIO",
         "RECUPERADOS_DIFF_RATIO",
         "MUERTOS_DIFF_RATIO",
         "ACTIVOS_DIFF_RATIO")] %>%
     mutate(
       "CONFIRMADOS_DIFF_PER" = CONFIRMADOS_DIFF_RATIO * 100,
       "RECUPERADOS_DIFF_PER" = RECUPERADOS_DIFF_RATIO * 100,
       "MUERTOS_DIFF_PER"  = MUERTOS_DIFF_RATIO * 100,
       "ACTIVOS_DIFF_PER"    = ACTIVOS_DIFF_RATIO * 100
     ) %>%
     select(-CONFIRMADOS_DIFF_RATIO, -RECUPERADOS_DIFF_RATIO, -MUERTOS_DIFF_RATIO,
            -ACTIVOS_DIFF_RATIO) %>%
     mutate_at(vars(contains('_newPer')), list(~na_if(., Inf))) %>%
     mutate_at(vars(contains('_newPer')), list(~na_if(., 0))) %>%
     mutate(
       CONFIRMADOS_DIFF = str_c(str_pad(CONFIRMADOS_DIFF, width = padding_left, side = "left", pad = "0"), "|",
         CONFIRMADOS_DIFF, if_else(!is.na(CONFIRMADOS_DIFF_PER), sprintf(" (%+.2f %%)", CONFIRMADOS_DIFF_PER), "")),
       RECUPERADOS_DIFF = str_c(str_pad(RECUPERADOS_DIFF, width = padding_left, side = "left", pad = "0"), "|",
         RECUPERADOS_DIFF, if_else(!is.na(RECUPERADOS_DIFF_PER), sprintf(" (%+.2f %%)", RECUPERADOS_DIFF_PER), "")),
       MUERTOS_DIFF  = str_c(str_pad(MUERTOS_DIFF, width = padding_left, side = "left", pad = "0"), "|",
         MUERTOS_DIFF, if_else(!is.na(MUERTOS_DIFF_PER), sprintf(" (%+.2f %%)", MUERTOS_DIFF_PER), "")),
       ACTIVOS_DIFF    = str_c(str_pad(ACTIVOS_DIFF, width = padding_left, side = "left", pad = "0"), "|",
         ACTIVOS_DIFF, if_else(!is.na(ACTIVOS_DIFF_PER), sprintf(" (%+.2f %%)", ACTIVOS_DIFF_PER), ""))
     ) %>%
    as.data.frame()
}

output$fullTable <- renderDataTable({
  data       <- getFullTableData()
  columNames <- c(
    "Provincia",
    "Total Confirmados",
    "Nuevos Confirmados",
    "Total Confirmados <br>(cada 100K)",
    "Total Recuperados",
    "Nuevos Recuperados",
    "Total Fallecidos",
    "Total Fallecidos <br>(cada 100k)",
    "Nuevos Fallecidos",
    "Total Activos",
    "Nuevos Activos",
    "Total Activos <br>(cada 100k)")
  datatable(
    data,
    rownames  = FALSE,
    colnames  = columNames,
    escape    = FALSE,
    selection = "none",
    options   = list(
      pageLength     = -1,
      order          = list(9, "desc"),
      scrollX        = TRUE,
      scrollY        = "calc(100vh - 250px)",
      scrollCollapse = TRUE,
      dom            = "ft",
      server         = FALSE,
      columnDefs     = list(
        list(
          targets = c(2, 5, 8, 10),
          render  = JS(
            "function(data, type, row, meta) {
              if (data != null) {
                split = data.split('|')
                if (type == 'display') {
                  return split[1];
                } else {
                  return split[0];
                }
              }
            }"
          )
        ),
        list(className = 'dt-right', targets = 1:ncol(data) - 1),
        list(width = '100px', targets = 0),
        list(visible = FALSE, targets = 11:15)
      )
    )
  ) %>%
    formatStyle(
      columns    = "LOCATION",
      fontWeight = "bold"
    ) %>%
    formatStyle(
      columns         = "CONFIRMADOS_DIFF",
      valueColumns    = "CONFIRMADOS_DIFF_PER",
      backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
      color           = styleInterval(75, c("#000000", "#FFFFFF"))
    ) %>%
    formatStyle(
      columns         = "MUERTOS_DIFF",
      valueColumns    = "MUERTOS_DIFF_PER",
      backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
      color           = styleInterval(75, c("#000000", "#FFFFFF"))
    ) %>%
    formatStyle(
      columns         = "ACTIVOS_DIFF",
      valueColumns    = "ACTIVOS_DIFF_PER",
      backgroundColor = styleInterval(c(-33, -20, -10, 10, 20, 33, 50, 75), c("#66B066", "#99CA99", "#CCE4CC", "NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
      color           = styleInterval(75, c("#000000", "#FFFFFF"))
    ) %>%
    formatStyle(
      columns         = "RECUPERADOS_DIFF",
      valueColumns    = "RECUPERADOS_DIFF_PER",
      backgroundColor = styleInterval(c(10, 20, 33), c("NULL", "#CCE4CC", "#99CA99", "#66B066"))
    )
})
