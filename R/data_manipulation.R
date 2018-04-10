# Funcs for Ops report ------------------------------------------------------------------

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


gen_plot_ops <- function(df, group = "."){
  # Plot with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = ÉRTÉK)) +
    facet_grid(paste("DIMENZIÓ ~", group), scales = "free", space = "fixed") +
    geom_line(
      data = subset(df, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_bar(data = subset(df, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
    labs(
      y = "Értékek",
      x = "Idõszak",
      colour = "Mutató"
    ) +
    theme(
      legend.position = c(0.1, 0.85),
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text.y = element_text(size = 10) 
    )
  
  # Adjust facet sizes manually
  # Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
  g <- ggplot_gtable(ggplot_build(p))
  g$heights[8] <- 0.5 * g$heights[8]
  grid.draw(g)
}


gen_plot_ops_cost <- function(df, group = "."){
  # Plot with facets passed as arguments
  ggplot(df, aes(x = IDOSZAK, y = ÉRTÉK)) +
    facet_grid(paste("MUTATÓ ~", group), scales = "free", labeller = label_wrap_gen(width=20)) +
    geom_line(
      data = filter(df, MUTATÓ == "Erõforrásigény [FTE]"),
      aes(group = 1),
      colour = "#619CFF"
    ) +
    geom_point(
      data = filter(df, MUTATÓ == "Erõforrásigény [FTE]"),
      aes(group = 1),
      colour = "#619CFF"
    ) +
    geom_line(
      data = filter(df, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
      aes(group = 1),
      colour = "#F8766D"
    ) +
    geom_point(
      data = filter(df, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
      aes(group = 1),
      colour = "#F8766D"
    ) +
    geom_bar(data = filter(df, MUTATÓ == "Volumen [db]"), stat = "identity") +
    labs(
      y = "Értékek",
      x = "Idõszak",
      colour = "Mutató"
    ) +
    theme(
      legend.position = c(0.1, 0.9),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
}



# Funcs for sales report ----------------------------------------------------------------
gen_aggregate_sales <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way (sales)
  t_grouped <- df %>%
    group_by_(...) %>%
    summarize(
      ALIR_SZERZ_ATLAG = mean(ALIR_SZERZ_MNAP, na.rm = TRUE),
      ALIR_SZERZ_MEDIAN = median(ALIR_SZERZ_MNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_ATLAG = mean(SZERZ_DIJBEFIZ_MNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_MEDIAN = median(SZERZ_DIJBEFIZ_MNAP, na.rm = TRUE),
      DIJBEFIZ_DIJKONYV_ATLAG = mean(DIJBEFIZ_DIJKONYV_MNAP, na.rm = TRUE),
      DIJBEFIZ_DIJKONYV_MEDIAN = median(DIJBEFIZ_DIJKONYV_MNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_ATLAG = mean(SZERZ_DIJKONYV_MNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_MEDIAN = median(SZERZ_DIJKONYV_MNAP, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTATÓ",
      value_col = "ÉRTÉK",
      gather_cols = c(
        "ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN", "SZERZ_DIJBEFIZ_ATLAG",
        "SZERZ_DIJBEFIZ_MEDIAN", "DIJBEFIZ_DIJKONYV_ATLAG", "DIJBEFIZ_DIJKONYV_MEDIAN",
        "SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN", "VOLUMEN"
      )
    ) %>%
    ungroup() %>%
    mutate(ÉRTÉK := case_when(.$ÉRTÉK < 0 & .$ÉRTÉK > -1 ~ 0,
                              TRUE ~ .$ÉRTÉK),
      DIMENZIÓ := case_when(
      .$MUTATÓ %in% c("ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN") ~ "1. Aláír-kötv [mnap]",
      .$MUTATÓ %in% c("SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN") ~ "2. Kötv-díjbefiz [mnap]",
      .$MUTATÓ %in% c("DIJBEFIZ_DIJKONYV_ATLAG", "DIJBEFIZ_DIJKONYV_MEDIAN") ~ "3. Dijbefiz-díjköny [mnap]",
      .$MUTATÓ %in% c("SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN") ~ "TÁJ: Kötv-díjkönyv [mnap]",
      TRUE ~ "TÁJ: Volumen [db]"
    )) %>%
    mutate(MUTATÓ := case_when(
      stringr::str_detect(.$MUTATÓ, "ATLAG") ~ "ÁTLAG",
      stringr::str_detect(.$MUTATÓ, "MEDIAN") ~ "MEDIÁN",
      TRUE ~ .$MUTATÓ
    ))
  return(t_grouped)
}


gen_aggregate_sales_tied <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way (sales)
  t_grouped <- df %>%
    group_by_(...) %>%
    summarize(
      ALIR_SZERZ_ATLAG = mean(ALIR_SZERZ_MNAP, na.rm = TRUE),
      ALIR_SZERZ_MEDIAN = median(ALIR_SZERZ_MNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_ATLAG = mean(SZERZ_DIJBEFIZ_MNAP, na.rm = TRUE),
      SZERZ_DIJBEFIZ_MEDIAN = median(SZERZ_DIJBEFIZ_MNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_ATLAG = mean(SZERZ_DIJKONYV_MNAP, na.rm = TRUE),
      SZERZ_DIJKONYV_MEDIAN = median(SZERZ_DIJKONYV_MNAP, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTATÓ",
      value_col = "ÉRTÉK",
      gather_cols = c(
        "ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN", "SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN",
        "SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN", "VOLUMEN"
      )
    ) %>%
    ungroup() %>%
    mutate(ÉRTÉK := case_when(.$ÉRTÉK < 0 & .$ÉRTÉK > -1 ~ 0,
                              TRUE ~ .$ÉRTÉK),
            DIMENZIÓ := case_when(
              .$MUTATÓ %in% c("ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN") ~ "1. Aláír-kötv [mnap]",
              .$MUTATÓ %in% c("SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN") ~ "TÁJ: Kötv-díjbefiz [mnap]",
              .$MUTATÓ %in% c("SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN") ~ "2. Kötv-díjkönyv [mnap]",
              TRUE ~ "TÁJ: Volumen [db]"
            )) %>%
    mutate(MUTATÓ := case_when(
      stringr::str_detect(.$MUTATÓ, "ATLAG") ~ "ÁTLAG",
      stringr::str_detect(.$MUTATÓ, "MEDIAN") ~ "MEDIÁN",
      TRUE ~ .$MUTATÓ
    ))
  return(t_grouped)
}


gen_plot_sales <- function(df, group = "."){
  # Plot  with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = ÉRTÉK)) +
    facet_grid(paste("DIMENZIÓ ~", group), scales = "free", space = "fixed", labeller = label_wrap_gen(width=15)) +
    geom_line(
      data = subset(df, DIMENZIÓ == "1. Aláír-kötv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "1. Aláír-kötv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_line(
      data = subset(df, DIMENZIÓ == "2. Kötv-díjbefiz [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "2. Kötv-díjbefiz [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_line(
      data = subset(df, DIMENZIÓ == "3. Dijbefiz-díjköny [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "3. Dijbefiz-díjköny [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_line(
      data = subset(df, DIMENZIÓ == "TÁJ: Kötv-díjkönyv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "TÁJ: Kötv-díjkönyv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_bar(data = subset(df, DIMENZIÓ == "TÁJ: Volumen [db]"), stat = "identity") +
    labs(
      y = "Értékek",
      x = "Idõszak",
      colour = "Mutató"
    ) +
    theme(
      #legend.position = c(0.1, 0.85),
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),
      strip.text.y = element_text(size = 10) 
    )
  
  # Adjust facet sizes manually
  # Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
  # Define grid rows for modification
  if (group == "."){
    h1 = 12
    h2 = 14
  } else {
    h1 = 13
    h2 = 15
  }
  
  g <- ggplot_gtable(ggplot_build(p))
  g$heights[h1] <- 0.7 * g$heights[h1]
  g$heights[h2] <- 0.7 * g$heights[h2]
  grid.draw(g)
}


gen_plot_sales_tied <- function(df, group = "."){
  # Plot  with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = ÉRTÉK)) +
    facet_grid(paste("DIMENZIÓ ~", group), scales = "free", space = "fixed", labeller = label_wrap_gen(width=15)) +
    geom_line(
      data = subset(df, DIMENZIÓ == "1. Aláír-kötv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "1. Aláír-kötv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_line(
      data = subset(df, DIMENZIÓ == "2. Kötv-díjkönyv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "2. Kötv-díjkönyv [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_line(
      data = subset(df, DIMENZIÓ == "TÁJ: Kötv-díjbefiz [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_point(
      data = subset(df, DIMENZIÓ == "TÁJ: Kötv-díjbefiz [mnap]"),
      aes(group = MUTATÓ, colour = MUTATÓ)
    ) +
    geom_bar(data = subset(df, DIMENZIÓ == "TÁJ: Volumen [db]"), stat = "identity") +
    labs(
      y = "Értékek",
      x = "Idõszak",
      colour = "Mutató"
    ) +
    theme(
      #legend.position = c(0.1, 0.85),
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),
      strip.text.y = element_text(size = 8) 
    )
  
  # Adjust facet sizes manually
  # Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
  # Define grid rows for modification
  if (group == "."){
    h1 = 10
    h2 = 12
  } else {
    h1 = 11
    h2 = 13
  }
  
  g <- ggplot_gtable(ggplot_build(p))
  g$heights[h1] <- 0.6 * g$heights[h1]
  g$heights[h2] <- 0.6 * g$heights[h2]
  grid.draw(g)
}