# Funcs for Ops report ------------------------------------------------------------------

gen_aggregate <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way
  t_grouped <- df %>%
    filter(ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100) %>% # clear atdolgozas and outliers
    group_by_(...) %>%
    summarize(
      �TLAG = mean(ERK_SZERZ, na.rm = TRUE),
      MEDI�N = median(ERK_SZERZ, na.rm = TRUE),
      VOLUMEN = length(VONALKOD)
    ) %>%
    gather_(
      key_col = "MUTAT�",
      value_col = "�RT�K",
      gather_cols = c("�TLAG", "MEDI�N", "VOLUMEN")
    ) %>%
    ungroup() %>%
    mutate(DIMENZI� := case_when(
      .$MUTAT� == "VOLUMEN" ~ "VOLUMEN [db]",
      TRUE ~ "�TFUT�S [mnap]"
    ))
  return(t_grouped)
}


gen_aggregate_cost <- function(df, df_wdays, ...){
  # Returns aggregated cost computations fit for plotting in a faceted way
  t_grouped <- df %>%
    filter(UW_TIP == "Manu�lis k�tv�nyes�t�s") %>%
    filter(ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100) %>% # clear atdolgozas and outliers
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
      key_col = "MUTAT�",
      value_col = "�RT�K",
      gather_cols = c("FTE", "FAJL_FTE", "VOLUMEN")
    ) %>%
    mutate(MUTAT� := case_when(.$MUTAT� == "FTE" ~ "Er�forr�sig�ny [FTE]",
                              .$MUTAT� == "FAJL_FTE" ~ "Hat�konys�g [fajlagos FTE]",
                              TRUE ~ "Volumen [db]"))
}


gen_plot_ops <- function(df, group = "."){
  # Plot with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = �RT�K)) +
    facet_grid(paste("DIMENZI� ~", group), scales = "free", space = "fixed") +
    geom_line(
      data = subset(df, DIMENZI� == "�TFUT�S [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point_interactive(
      data = subset(df, DIMENZI� == "�TFUT�S [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�, tooltip = round(�RT�K, 2))
    ) +
    geom_bar_interactive(data = subset(df, DIMENZI� == "VOLUMEN [db]"), stat = "identity",
                         aes(tooltip = �RT�K)) +
    labs(
      y = "�rt�kek",
      x = "Id�szak",
      colour = "Mutat�"
    ) +
    theme(
      legend.position = c(0.1, 0.85),
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10)
    )
  
  # Adjust facet sizes manually
  # Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
  
  # Determine which grid row to modify depending on faceting structure
  idx <- ifelse(group == ".", 8, 9) 
  
  g <- ggplot_gtable(ggplot_build(p))
  g$heights[idx] <- 0.5 * g$heights[idx]
  ggiraph(code = grid.draw(g), width_svg = 10)
}


gen_plot_ops_cost <- function(df, group = "."){
  # Plot with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = �RT�K)) +
    facet_grid(paste("MUTAT� ~", group), scales = "free", labeller = label_wrap_gen(width=20)) +
    geom_line(
      data = filter(df, MUTAT� == "Er�forr�sig�ny [FTE]"),
      aes(group = 1),
      colour = "#619CFF"
    ) +
    geom_point_interactive(
      data = filter(df, MUTAT� == "Er�forr�sig�ny [FTE]"),
      aes(group = 1, tooltip = round(�RT�K, 2)),
      colour = "#619CFF"
    ) +
    geom_line(
      data = filter(df, MUTAT� == "Hat�konys�g [fajlagos FTE]"),
      aes(group = 1),
      colour = "#F8766D"
    ) +
    geom_point_interactive(
      data = filter(df, MUTAT� == "Hat�konys�g [fajlagos FTE]"),
      aes(group = 1, tooltip = round(�RT�K, 2)),
      colour = "#F8766D"
    ) +
    geom_bar_interactive(data = filter(df, MUTAT� == "Volumen [db]"), stat = "identity",
                         aes(tooltip = �RT�K)) +
    labs(
      y = "�rt�kek",
      x = "Id�szak",
      colour = "Mutat�"
    ) +
    theme(
      legend.position = c(0.1, 0.9),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  
  ggiraph(code = print(p), width_svg = 10)
}



# Funcs for sales report ----------------------------------------------------------------
gen_aggregate_sales <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way (sales)
  t_grouped <- df %>%
    filter(ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100) %>% # clear atdolgozas and outliers
    filter(SZERZ_DIJKONYV_MNAP < 90) %>% # clear deferred payments
    filter(ALIR_DIJBEFIZ_NNAP > 0) %>%  # clear erroneous payment dates
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
      key_col = "MUTAT�",
      value_col = "�RT�K",
      gather_cols = c(
        "ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN", "SZERZ_DIJBEFIZ_ATLAG",
        "SZERZ_DIJBEFIZ_MEDIAN", "DIJBEFIZ_DIJKONYV_ATLAG", "DIJBEFIZ_DIJKONYV_MEDIAN",
        "SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN", "VOLUMEN"
      )
    ) %>%
    ungroup() %>%
    mutate(�RT�K := case_when(.$�RT�K < 0 & .$�RT�K > -1 ~ 0,
                              TRUE ~ .$�RT�K),
      DIMENZI� := case_when(
      .$MUTAT� %in% c("ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN") ~ "1. Al��r-k�tv [mnap]",
      .$MUTAT� %in% c("SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN") ~ "2. K�tv-d�jbefiz [mnap]",
      .$MUTAT� %in% c("DIJBEFIZ_DIJKONYV_ATLAG", "DIJBEFIZ_DIJKONYV_MEDIAN") ~ "3. Dijbefiz-d�jk�ny [mnap]",
      .$MUTAT� %in% c("SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN") ~ "T�J: K�tv-d�jk�nyv [mnap]",
      TRUE ~ "T�J: Volumen [db]"
    )) %>%
    mutate(MUTAT� := case_when(
      stringr::str_detect(.$MUTAT�, "ATLAG") ~ "�TLAG",
      stringr::str_detect(.$MUTAT�, "MEDIAN") ~ "MEDI�N",
      TRUE ~ .$MUTAT�
    ))
  return(t_grouped)
}


gen_aggregate_sales_tied <- function(df, ...) {
  # Returns aggregated df fit for plotting in a faceted way (sales)
  t_grouped <- df %>%
    filter(ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100) %>% # clear atdolgozas and outliers
    filter(SZERZ_DIJKONYV_MNAP < 90) %>% # clear deferred payments
    filter(ALIR_DIJBEFIZ_NNAP > 0) %>%  # clear erroneous payment dates
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
      key_col = "MUTAT�",
      value_col = "�RT�K",
      gather_cols = c(
        "ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN", "SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN",
        "SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN", "VOLUMEN"
      )
    ) %>%
    ungroup() %>%
    mutate(�RT�K := case_when(.$�RT�K < 0 & .$�RT�K > -1 ~ 0,
                              TRUE ~ .$�RT�K),
            DIMENZI� := case_when(
              .$MUTAT� %in% c("ALIR_SZERZ_ATLAG", "ALIR_SZERZ_MEDIAN") ~ "1. Al��r-k�tv [mnap]",
              .$MUTAT� %in% c("SZERZ_DIJBEFIZ_ATLAG", "SZERZ_DIJBEFIZ_MEDIAN") ~ "T�J: K�tv-d�jbefiz [mnap]",
              .$MUTAT� %in% c("SZERZ_DIJKONYV_ATLAG", "SZERZ_DIJKONYV_MEDIAN") ~ "2. K�tv-d�jk�nyv [mnap]",
              TRUE ~ "T�J: Volumen [db]"
            )) %>%
    mutate(MUTAT� := case_when(
      stringr::str_detect(.$MUTAT�, "ATLAG") ~ "�TLAG",
      stringr::str_detect(.$MUTAT�, "MEDIAN") ~ "MEDI�N",
      TRUE ~ .$MUTAT�
    ))
  return(t_grouped)
}


gen_plot_sales <- function(df, group = "."){
  # Plot  with facets passed as arguments
  p <- ggplot(df, aes(x = IDOSZAK, y = �RT�K)) +
    facet_grid(paste("DIMENZI� ~", group), scales = "free", space = "fixed", labeller = label_wrap_gen(width=15)) +
    geom_line(
      data = subset(df, DIMENZI� == "1. Al��r-k�tv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point(
      data = subset(df, DIMENZI� == "1. Al��r-k�tv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_line(
      data = subset(df, DIMENZI� == "2. K�tv-d�jbefiz [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point(
      data = subset(df, DIMENZI� == "2. K�tv-d�jbefiz [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_line(
      data = subset(df, DIMENZI� == "3. Dijbefiz-d�jk�ny [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point(
      data = subset(df, DIMENZI� == "3. Dijbefiz-d�jk�ny [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_line(
      data = subset(df, DIMENZI� == "T�J: K�tv-d�jk�nyv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point(
      data = subset(df, DIMENZI� == "T�J: K�tv-d�jk�nyv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_bar(data = subset(df, DIMENZI� == "T�J: Volumen [db]"), stat = "identity") +
    labs(
      y = "�rt�kek",
      x = "Id�szak",
      colour = "Mutat�"
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
  p <- ggplot(df, aes(x = IDOSZAK, y = �RT�K)) +
    facet_grid(paste("DIMENZI� ~", group), scales = "free", space = "fixed", labeller = label_wrap_gen(width=15)) +
    geom_line(
      data = subset(df, DIMENZI� == "1. Al��r-k�tv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point_interactive(
      data = subset(df, DIMENZI� == "1. Al��r-k�tv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�, tooltip = round(�RT�K, 2))
    ) +
    geom_line(
      data = subset(df, DIMENZI� == "2. K�tv-d�jk�nyv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point_interactive(
      data = subset(df, DIMENZI� == "2. K�tv-d�jk�nyv [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�, tooltip = round(�RT�K, 2))
    ) +
    geom_line(
      data = subset(df, DIMENZI� == "T�J: K�tv-d�jbefiz [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�)
    ) +
    geom_point_interactive(
      data = subset(df, DIMENZI� == "T�J: K�tv-d�jbefiz [mnap]"),
      aes(group = MUTAT�, colour = MUTAT�, tooltip = round(�RT�K, 2))
    ) +
    geom_bar_interactive(data = subset(df, DIMENZI� == "T�J: Volumen [db]"), stat = "identity",
                         aes(tooltip = �RT�K)) +
    labs(
      y = "�rt�kek",
      x = "Id�szak",
      colour = "Mutat�"
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
  ggiraph(code = grid.draw(g), width_svg = 8)
}