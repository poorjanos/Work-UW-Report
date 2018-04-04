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
