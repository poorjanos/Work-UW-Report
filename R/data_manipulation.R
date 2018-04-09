readQuery <-
  # Read sql file
  function(file)
    paste(readLines(file, warn = FALSE), collapse = "\n")


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


gen_aggregate_sales <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way (sales)
  t_grouped <- df %>%
    group_by_(...) %>%
    summarize(
      ALIR_SZERZ_ÁTLAG = mean(ALIR_SZERZ_NNAP, na.rm = TRUE),
      ALIR_SZERZ_MEDIÁN = median(ALIR_SZERZ_NNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_ÁTLAG = mean(SZERZ_DIJKONYV_NNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_MEDIÁN = median(SZERZ_DIJKONYV_NNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_ÁTLAG = mean(SZERZ_DIJBEFIZ_NNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_MEDIÁN = median(SZERZ_DIJBEFIZ_NNAP, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTATÓ",
      value_col = "ÉRTÉK",
      gather_cols = c(
        "ALIR_SZERZ_ÁTLAG", "ALIR_SZERZ_MEDIÁN", "SZERZ_DIJKONYV_ÁTLAG",
        "SZERZ_DIJKONYV_MEDIÁN", "SZERZ_DIJBEFIZ_ÁTLAG", "SZERZ_DIJBEFIZ_MEDIÁN",
        "VOLUMEN"
      )
    ) %>%
    ungroup() %>%
    mutate(DIMENZIÓ := case_when(
      .$MUTATÓ %in% c("ALIR_SZERZ_ÁTLAG", "ALIR_SZERZ_MEDIÁN") ~ "Aláír-kötv [nnap]",
      .$MUTATÓ %in% c("SZERZ_DIJKONYV_ÁTLAG", "SZERZ_DIJKONYV_MEDIÁN") ~ "Kötv-díjkönyv [nnap]",
      .$MUTATÓ %in% c("SZERZ_DIJBEFIZ_ÁTLAG", "SZERZ_DIJBEFIZ_MEDIÁN") ~ "TÁJ: Kötv-díjbefiz [nnap]",
      TRUE ~ "TÁJ: Volumen [db]"
    )) %>%
    mutate(MUTATÓ := case_when(
      stringr::str_detect(.$MUTATÓ, "ÁTLAG") ~ "ÁTLAG",
      stringr::str_detect(.$MUTATÓ, "MEDIÁN") ~ "MEDIÁN",
      TRUE ~ .$MUTATÓ
    ))
  return(t_grouped)
}
