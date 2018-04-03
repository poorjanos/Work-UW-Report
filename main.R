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
  config::get("kontakt" , file = "C:\\Users\\PoorJ\\Projects\\config.yml")

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
t_uw <-  t_uw %>%
  mutate(TERMCSOP = case_when(.$TERMCSOP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$TERMCSOP))

# Recode missing in processing time
t_uw[is.na(t_uw$FELDOLG_IDO_PERC), "FELDOLG_IDO_PERC"] <- 0

# Create binary uw type var
t_uw <- t_uw %>% mutate(UW_TIP = case_when(.$KIMENET == 'Sikeres kpm' ~ 'Auto-UW',
                                           TRUE ~ 'Manual-UW'))


#########################################################################################
# Data Aggregation ######################################################################
#########################################################################################


# Page1 ---------------------------------------------------------------------------------
# Total lead time with total volumes

# Gen data
t_page1 <- t_uw %>% group_by(IDOSZAK) %>%
  summarize(
    ÁTLAG = mean(ERK_SZERZ, na.rm = TRUE),
    MEDIÁN = median(ERK_SZERZ, na.rm = TRUE),
    VOLUMEN = length(VONALKOD)
  ) %>%
  gather(MUTATÓ,  ÉRTÉK,-IDOSZAK) %>%
  mutate(DIMENZIÓ  = case_when(.$MUTATÓ  == 'VOLUMEN' ~ 'VOLUMEN [db]',
                               TRUE ~ 'ÁTFUTÁS [mnap]'))

# Plot
plot1 <- ggplot(t_page1, aes(x = IDOSZAK, y =   ÉRTÉK)) +
  facet_grid(DIMENZIÓ  ~ ., scales = "free", space = "fixed") +
  geom_line(data = subset(t_page1, DIMENZIÓ   == "ÁTFUTÁS [mnap]"),
            aes(group = MUTATÓ, colour = MUTATÓ)) +
  geom_point(data = subset(t_page1, DIMENZIÓ   == "ÁTFUTÁS [mnap]"),
            aes(group = MUTATÓ, colour = MUTATÓ)) +
  geom_bar(data = subset(t_page1, DIMENZIÓ   == "VOLUMEN [db]"), stat = "identity") +
  labs(y = "Értékek",
       x = "Idõszak",
       colour = "Mutató") +
  theme(legend.position = c(0.1, 0.85),
        axis.text.x = element_text(angle = 90))

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt1 = ggplot_gtable(ggplot_build(plot1))
gt1$heights[8] = 0.5*gt1$heights[8]
grid.draw(gt1)


# Page2 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by UW type

# Gen data
t_page2 <- t_uw %>% group_by(IDOSZAK, UW_TIP) %>%
  summarize(
    ÁTLAG = mean(ERK_SZERZ, na.rm = TRUE),
    MEDIÁN = median(ERK_SZERZ, na.rm = TRUE),
    VOLUMEN = length(VONALKOD)
  ) %>%
  gather(MUTATÓ,  ÉRTÉK, -IDOSZAK, -UW_TIP) %>%
  ungroup() %>% 
  mutate(DIMENZIÓ  = case_when(.$MUTATÓ  == 'VOLUMEN' ~ 'VOLUMEN [db]',
                               TRUE ~ 'ÁTFUTÁS [mnap]'))

# Plot
plot2 <- ggplot(t_page2, aes(x = IDOSZAK, y =   ÉRTÉK)) +
  facet_grid(DIMENZIÓ  ~ UW_TIP, scales = "free", space = "fixed") +
  geom_line(data = subset(t_page2, DIMENZIÓ   == "ÁTFUTÁS [mnap]"),
            aes(group = MUTATÓ, colour = MUTATÓ)) +
  geom_point(data = subset(t_page2, DIMENZIÓ   == "ÁTFUTÁS [mnap]"),
             aes(group = MUTATÓ, colour = MUTATÓ)) +
  geom_bar(data = subset(t_page2, DIMENZIÓ   == "VOLUMEN [db]"), stat = "identity") +
  labs(y = "Értékek",
       x = "Idõszak",
       colour = "Mutató") +
  theme(legend.position = c(0.1, 0.85),
        axis.text.x = element_text(angle = 90))

# Adjust facet sizes manually
# Can view grid layout with gtable_show_layout(gt) to see which grid object to resize
gt2 = ggplot_gtable(ggplot_build(plot2))
gt2$heights[9] = 0.5*gt2$heights[9]
grid.draw(gt2)
