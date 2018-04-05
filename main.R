# Load required libs
library(config)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)

# Create dirs (dir.create() does not crash when dir already exists)
dir.create(here::here("Data"), showWarnings = FALSE)
dir.create(here::here("Reports"), showWarnings = FALSE)
dir.create(here::here("SQL"), showWarnings = FALSE)

# Import helper functions
source(here::here("R", "data_manipulation.R"))

# Import helper data
t_mnap <- read.csv(here::here("Data", "Mnap.csv"), sep = ";", stringsAsFactors = FALSE)
t_mnap$IDOSZAK <- paste0(substr(t_mnap$HONAP, 1, 4), "/", substr(t_mnap$HONAP, 6, 7))
            

#########################################################################################
# Data Extraction #######################################################################
#########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_60")
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Get credentials
kontakt <-
  config::get("kontakt", file = "C:\\Users\\PoorJ\\Projects\\config.yml")

# Create connection driver
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")

# Open connection: kontakt---------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = kontakt$server,
    user = kontakt$uid,
    password = kontakt$pwd
  )

# Fetch data
query_uw_data <- "select * from t_uw_history where elutdat is null and stornodat is null"
t_uw <- dbGetQuery(jdbcConnection, query_uw_data)

# Close db connection: kontakt
dbDisconnect(jdbcConnection)


#########################################################################################
# Data Transformation ###################################################################
#########################################################################################

# Recode time period
t_uw$IDOSZAK <-
  paste0(substr(t_uw$IDOSZAK, 1, 4), "/", substr((t_uw$IDOSZAK), 6, 7))

# Recode product line
t_uw <- t_uw %>%
  mutate(TERMCSOP = case_when(
    .$TERMCSOP == "Vagyon" ~ "Lakás",
    TRUE ~ .$TERMCSOP
  ))

# Recode missing in processing time
t_uw[is.na(t_uw$FELDOLG_IDO_PERC), "FELDOLG_IDO_PERC"] <- 0

# Create binary uw type var
t_uw <- t_uw %>% mutate(UW_TIP = case_when(
  .$KIMENET == "Sikeres kpm" ~ "Automatikus kötvényesítés",
  TRUE ~ "Manuális kötvényesítés"
))

# Recode process flow
t_uw <- t_uw %>% mutate(FELDOLG_AG = case_when(
  .$FELDOLG_AG == "Happy flow" ~ "Happy flow",
  TRUE ~ "Hibaág"
))

#########################################################################################
# Data Aggregation ######################################################################
#########################################################################################


# Page1 ---------------------------------------------------------------------------------
# Total lead time with total volumes

# Gen data
t_page1 <- gen_aggregate(t_uw, "IDOSZAK")

# Plot
plot1 <- ggplot(t_page1, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ ., scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page1, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.85),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt1 <- ggplot_gtable(ggplot_build(plot1))
gt1$heights[8] <- 0.5 * gt1$heights[8]
grid.draw(gt1)


# Page2 ---------------------------------------------------------------------------------
# Full process (automted and manual together)


# Page2_1 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by UW type

# Gen data
t_page2_1 <- gen_aggregate(t_uw, "IDOSZAK", "UW_TIP")

# Plot
plot2_1 <- ggplot(t_page2_1, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ UW_TIP, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page2_1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page2_1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page2_1, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.85),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2_1 <- ggplot_gtable(ggplot_build(plot2_2))
gt2_1$heights[9] <- 0.5 * gt2_1$heights[9]
grid.draw(gt2_1)


# Page2_2 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by product line

# Gen data
t_page2_2 <- gen_aggregate(t_uw, "IDOSZAK", "TERMCSOP")

# Plot
plot2_2 <- ggplot(t_page2_2, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ TERMCSOP, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page2_2, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page2_2, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page2_2, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2_2 <- ggplot_gtable(ggplot_build(plot2_2))
gt2_2$heights[9] <- 0.5 * gt2_2$heights[9]
grid.draw(gt2_2)


# Page2_3 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by process flow

# Gen data
t_page2_3 <- gen_aggregate(t_uw, "IDOSZAK", "FELDOLG_AG")

# Plot
plot2_3 <- ggplot(t_page2_3, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ FELDOLG_AG, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page2_3, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page2_3, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page2_3, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2_3 <- ggplot_gtable(ggplot_build(plot2_3))
gt2_3$heights[9] <- 0.5 * gt2_3$heights[9]
grid.draw(gt2_3)


# Page2_4 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by process flow & product

# Gen data
t_page2_4 <- gen_aggregate(t_uw, "IDOSZAK", "UW_TIP", "TERMCSOP")

# Plot
plot2_4_1 <- ggplot(t_page2_4 %>% filter(UW_TIP == 'Automatikus kötvényesítés'), aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ TERMCSOP, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page2_4 %>% filter(UW_TIP == 'Automatikus kötvényesítés'), DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page2_4 %>% filter(UW_TIP == 'Automatikus kötvényesítés'), DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page2_4 %>% filter(UW_TIP == 'Automatikus kötvényesítés'), DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.9, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  ggtitle("Automatikus kötvényesítés: termékcsoportok")

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2_4_1 <- ggplot_gtable(ggplot_build(plot2_4_1))
gt2_4_1$heights[9] <- 0.5 * gt2_4_1$heights[9]
grid.draw(gt2_4_1)


# Plot
plot2_4_2 <- ggplot(t_page2_4 %>% filter(UW_TIP != 'Automatikus kötvényesítés'), aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ TERMCSOP, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page2_4 %>% filter(UW_TIP != 'Automatikus kötvényesítés'), DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page2_4 %>% filter(UW_TIP != 'Automatikus kötvényesítés'), DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page2_4 %>% filter(UW_TIP != 'Automatikus kötvényesítés'), DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  ggtitle("Manuális kötvényesítés: termékcsoportok")

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2_4_2 <- ggplot_gtable(ggplot_build(plot2_4_2))
gt2_4_2$heights[9] <- 0.5 * gt2_4_2$heights[9]
grid.draw(gt2_4_2)


# Page3 ---------------------------------------------------------------------------------
# Manual process

# Page3_1 ---------------------------------------------------------------------------------
# Manual process broken down by process flow

# Gen data
t_page3_1 <- gen_aggregate(t_uw %>% filter(UW_TIP == 'Manuális kötvényesítés'), "IDOSZAK", "FELDOLG_AG")

# Plot
plot3_1 <- ggplot(t_page3_1, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ FELDOLG_AG, scales = "free", space = "fixed") +
  geom_line(
    data = subset(t_page3_1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page3_1, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page3_1, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt3_1 <- ggplot_gtable(ggplot_build(plot3_1))
gt3_1$heights[9] <- 0.5 * gt3_1$heights[9]
grid.draw(gt3_1)



# Page3_2 ---------------------------------------------------------------------------------
# Manual process broken down by process flow & product line

# Gen data
t_page3_2 <- gen_aggregate(t_uw %>% filter(UW_TIP == 'Manuális kötvényesítés'),
                           "IDOSZAK", "FELDOLG_AG", "TERMCSOP") %>% 
              mutate(SZEGMENS = paste0(FELDOLG_AG, '::', TERMCSOP))

# Plot
plot3_2 <- ggplot(t_page3_2, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(DIMENZIÓ ~ SZEGMENS, scales = "free", space = "fixed", labeller = label_wrap_gen(width=20)) +
  geom_line(
    data = subset(t_page3_2, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_point(
    data = subset(t_page3_2, DIMENZIÓ == "ÁTFUTÁS [mnap]"),
    aes(group = MUTATÓ, colour = MUTATÓ)
  ) +
  geom_bar(data = subset(t_page3_2, DIMENZIÓ == "VOLUMEN [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt3_2 <- ggplot_gtable(ggplot_build(plot3_2))
gt3_2$heights[9] <- 0.5 * gt3_2$heights[9]
grid.draw(gt3_2)


# Page3_3 ---------------------------------------------------------------------------------
# Manual process costs and efficiency

# Gen data
t_page3_3 <- t_uw %>%
  filter(UW_TIP == "Manuális kötvényesítés") %>%
  group_by(IDOSZAK) %>%
  summarise(
    SUM_EMBERNAP = sum(FELDOLG_IDO_PERC) / 60 / 7,
    VOLUMEN = length(VONALKOD)
  ) %>%
  left_join(t_mnap %>% select(IDOSZAK, MNAP), by = "IDOSZAK") %>%
  mutate(
    FTE = SUM_EMBERNAP / MNAP,
    FAJL_FTE = FTE / VOLUMEN * 1000
  ) %>% 
  ungroup() %>% 
  select(IDOSZAK, FTE, FAJL_FTE, VOLUMEN) %>% 
  gather(MUTATÓ, ÉRTÉK, -IDOSZAK) %>% 
  mutate(MUTATÓ = case_when(.$MUTATÓ == "FTE" ~ "Erõforrásigény [FTE]",
                            .$MUTATÓ == "FAJL_FTE" ~ "Hatékonyság [fajlagos FTE]",
                            TRUE ~ "Volumen [db]"))


