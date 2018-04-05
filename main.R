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
write.csv(t_page1, here::here("Data", "t_page1.csv"), row.names = FALSE)


# Page2 ---------------------------------------------------------------------------------
# Full process (automted and manual together)


# Page2_1 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by UW type

# Gen data
t_page2_1 <- gen_aggregate(t_uw, "IDOSZAK", "UW_TIP")
write.csv(t_page2_1, here::here("Data", "t_page2_1.csv"), row.names = FALSE)


# Page2_2 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by product line

# Gen data
t_page2_2 <- gen_aggregate(t_uw, "IDOSZAK", "TERMCSOP")
write.csv(t_page2_2, here::here("Data", "t_page2_2.csv"), row.names = FALSE)


# Page2_3 ---------------------------------------------------------------------------------
# Total lead time with total volumes broken down by process flow

# Gen data
t_page2_3 <- gen_aggregate(t_uw, "IDOSZAK", "FELDOLG_AG")
write.csv(t_page2_3, here::here("Data", "t_page2_3.csv"), row.names = FALSE)



# Page3 ---------------------------------------------------------------------------------
# Manual process

# Page3_1 ---------------------------------------------------------------------------------
# Manual process broken down by process flow

# Gen data
t_page3_1 <- gen_aggregate(t_uw %>% filter(UW_TIP == 'Manuális kötvényesítés'), "IDOSZAK", "FELDOLG_AG")
write.csv(t_page3_1, here::here("Data", "t_page3_1.csv"), row.names = FALSE)



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
t_page3_3 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK")

# Plot
ggplot(t_page3_3, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(MUTATÓ~., scales = "free") +
  geom_line(
    data = filter(t_page3_3, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_point(
    data = filter(t_page3_3, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_line(
    data = filter(t_page3_3, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_point(
    data = filter(t_page3_3, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_bar(data = filter(t_page3_3, MUTATÓ == "Volumen [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
  

# Page3_4 ---------------------------------------------------------------------------------
# Manual process costs and efficiency by product line

# Gen data
t_page3_4 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK", "TERMCSOP")

# Plot
ggplot(t_page3_4, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(MUTATÓ~TERMCSOP, scales = "free") +
  geom_line(
    data = filter(t_page3_4, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_point(
    data = filter(t_page3_4, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_line(
    data = filter(t_page3_4, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_point(
    data = filter(t_page3_4, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_bar(data = filter(t_page3_4, MUTATÓ == "Volumen [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


# Page3_5 ---------------------------------------------------------------------------------
# Manual process costs and efficiency by product line & process flow

# Gen data
t_page3_5 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK", "TERMCSOP", "FELDOLG_AG") %>% 
                mutate(SZEGMENS = paste0(FELDOLG_AG, '::', TERMCSOP))

# Plot
ggplot(t_page3_5, aes(x = IDOSZAK, y = ÉRTÉK)) +
  facet_grid(MUTATÓ~SZEGMENS, scales = "free") +
  geom_line(
    data = filter(t_page3_5, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_point(
    data = filter(t_page3_5, MUTATÓ == "Erõforrásigény [FTE]"),
    aes(group = 1),
    colour = "#619CFF"
  ) +
  geom_line(
    data = filter(t_page3_5, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_point(
    data = filter(t_page3_5, MUTATÓ == "Hatékonyság [fajlagos FTE]"),
    aes(group = 1),
    colour = "#F8766D"
  ) +
  geom_bar(data = filter(t_page3_5, MUTATÓ == "Volumen [db]"), stat = "identity") +
  labs(
    y = "Értékek",
    x = "Idõszak",
    colour = "Mutató"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
