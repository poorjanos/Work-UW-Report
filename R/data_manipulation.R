gen_aggregate <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way
  t_grouped <- df %>%
    group_by_(...) %>%
    summarize(
      ÁTLAG = mean(ERK_SZERZ, na.rm = TRUE),
      MEDIÁN = median(ERK_SZERZ, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTATÓ",
      value_col = "ÉRTÉK",
      gather_cols = c("ÁTLAG", "MEDIÁN", "VOLUMEN")
    ) %>%
    ungroup() %>%
    mutate(DIMENZIÓ := case_when(
      .$MUTATÓ == "VOLUMEN" ~ "VOLUMEN [db]",
      TRUE ~ "ÁTFUTÁS [mnap]"
    ))
  return(t_grouped)
}


gen_aggregate_cost <- function(df, df_wdays, ...){
  # Returns aggregated cost computations fit for plotting in a faceted way
  t_grouped <- df %>%
    filter(UW_TIP == "Manuális kötvényesítés") %>%
    group_by_(...) %>%
    summarise(
      SUM_EMBERNAP = sum(FELDOLG_IDO_PERC) / 60 / 7,
      VOLUMEN = length(VONALKOD)
    ) %>%
    left_join(df_wdays %>% select(IDOSZAK, MNAP), by = "IDOSZAK") %>%
    mutate(
      FTE := SUM_EMBERNAP / MNAP,
      FAJL_FTE := FTE / VOLUMEN * 1000
    ) %>%
    ungroup() %>%
    gather_(
      key_col = "MUTATÓ",
      value_col = "ÉRTÉK",
      gather_cols = c("FTE", "FAJL_FTE", "VOLUMEN")
    ) %>%
    mutate(MUTATÓ := case_when(.$MUTATÓ == "FTE" ~ "Erõforrásigény [FTE]",
                              .$MUTATÓ == "FAJL_FTE" ~ "Hatékonyság [fajlagos FTE]",
                              TRUE ~ "Volumen [db]"))
}