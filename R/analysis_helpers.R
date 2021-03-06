# Plot alir_dijbefiz
ggplot(
  t_uw %>% filter(
    ERTCSAT == "H�l�zat",
    ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100, # clear atdolgozas and outliers
    SZERZ_DIJKONYV_MNAP < 90, # clear deferred payments
    ALIR_DIJBEFIZ_NNAP > 0  # clear erroneous payment dates
  ),
  aes(ALIR_DIJBEFIZ_MNAP)
) +
  geom_histogram(bins = 200) +
  theme(strip.text.y = element_text(angle = 0)) + 
  facet_wrap(~IDOSZAK) +
  coord_cartesian(xlim = c(1, 90)) + 
  ggtitle("Al��r�st�l d�jbefizet�sig eloszl�s")


# Plot alir_dijbefiz by prod
ggplot(
  t_uw %>% filter(
    ERTCSAT == "H�l�zat",
    ALIR_SZERZ_NNAP < 90 | ERK_SZERZ < 100, # clear atdolgozas and outliers
    SZERZ_DIJKONYV_MNAP < 90, # clear deferred payments
    ALIR_DIJBEFIZ_NNAP > 0  # clear erroneous payment dates
  ),
  aes(ALIR_DIJBEFIZ_MNAP)
) +
  geom_histogram(bins = 200) +
  theme(strip.text.y = element_text(angle = 0)) + 
  facet_grid(IDOSZAK ~ TERMCSOP) +
  coord_cartesian(xlim = c(1, 75)) + 
  ggtitle("Al��r�st�l d�jbefizet�sig eloszl�s")

ggsave(here::here("Reports", "ALIR_DIJBEFIZ_eloszl�s.png"),
       width = 14,
       height = 7,
       dpi = 500
)
