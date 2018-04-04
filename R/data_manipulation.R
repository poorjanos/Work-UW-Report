gen_aggregate <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way
  t_grouped <- df %>% group_by_(...) %>%
    summarize(
      �TLAG = mean(ERK_SZERZ, na.rm = TRUE),
      MEDI�N = median(ERK_SZERZ, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTAT�",
      value_col =  "�RT�K",
      gather_cols = c("�TLAG", "MEDI�N", "VOLUMEN")
    ) %>%
    ungroup() %>%
    mutate(DIMENZI�  := case_when(.$MUTAT�  == 'VOLUMEN' ~ 'VOLUMEN [db]',
                                  TRUE ~ '�TFUT�S [mnap]'))
  return(t_grouped)
}