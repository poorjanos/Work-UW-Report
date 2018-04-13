# Load required libs
library(config)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(stringr)

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
query_uw_data <- "select * from t_uw_history_r"
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

# Create sales channel var
# Direkt includes premium as well (A, DS, W, N, E, PR, PS)
t_uw <- t_uw %>%
        mutate(ERTCSAT = case_when(.$ERTCSAT %in% c("O", "U", "DU") ~ "Hálózat",
                                   .$ERTCSAT %in% c("B", "SB", "I", "C") ~ "Alkusz",
                                   TRUE ~ "Direkt"))


#########################################################################################
# Data Aggregation ######################################################################
#########################################################################################


# AFC Report ---------------------------------------------------------------------------

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
write.csv(t_page3_2, here::here("Data", "t_page3_2.csv"), row.names = FALSE)


# Page3_3 ---------------------------------------------------------------------------------
# Manual process costs and efficiency

# Gen data
t_page3_3 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK")
write.csv(t_page3_3, here::here("Data", "t_page3_3.csv"), row.names = FALSE)


# Page3_4 ---------------------------------------------------------------------------------
# Manual process costs and efficiency by product line

# Gen data
t_page3_4 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK", "TERMCSOP")
write.csv(t_page3_4, here::here("Data", "t_page3_4.csv"), row.names = FALSE)


# Page3_5 ---------------------------------------------------------------------------------
# Manual process costs and efficiency by product line & process flow

# Gen data
t_page3_5 <- gen_aggregate_cost(t_uw, t_mnap, "IDOSZAK", "TERMCSOP", "FELDOLG_AG") %>% 
                mutate(SZEGMENS = paste0(FELDOLG_AG, '::', TERMCSOP))
write.csv(t_page3_5, here::here("Data", "t_page3_5.csv"), row.names = FALSE)




# Sales Report ----------------------------------------------------------------------------

# Create new prod line var to separate code 127* to label
t_uw <- t_uw %>%
  mutate(TERMCSOP_SALES = case_when(
    .$TERMCSOP == "Lakás" & stringr::str_detect(.$MODKOD, "127")  ~ "Baleset",
    TRUE ~ .$TERMCSOP
  ))

# Create flag for outliers
t_uw <- t_uw %>% mutate(OUTLIER = case_when(SZERZ_DIJKONYV_MNAP > 90 ~ 1,
                          TRUE ~ 0))

# All channels ----------------------------------------------------------------------------
# Page 1 ----------------------------------------------------------------------------------
# Total lead times
s_page1 <- gen_aggregate_sales(t_uw %>% filter(stringr::str_detect(IDOSZAK, "2017")), "IDOSZAK")
gen_plot_sales(s_page1)


# Page 2------------------------------------------------------------------------------------
# Total lead time with total volumes broken down by product line
s_page2 <- gen_aggregate_sales(t_uw %>% filter(stringr::str_detect(IDOSZAK, "2017")), "IDOSZAK", "TERMCSOP_SALES")
gen_plot_sales(s_page2, "TERMCSOP_SALES")

# Total lead time with total volumes broken down by sales channel
s_page3 <- gen_aggregate_sales(t_uw %>% filter(stringr::str_detect(IDOSZAK, "2017")), "IDOSZAK", "ERTCSAT")
gen_plot_sales(s_page3, "ERTCSAT")

# Total lead time with total volumes broken down by sales channel
s_page4 <- gen_aggregate_sales(t_uw %>% filter(stringr::str_detect(IDOSZAK, "2017")),
                               "IDOSZAK", "ERTCSAT", "TERMCSOP_SALES") %>% 
            mutate(SZEGMENS = paste0(ERTCSAT, '::', TERMCSOP_SALES))
gen_plot_sales(s_page4, "SZEGMENS")


# Channel: tied agents ----------------------------------------------------------------------
# Page 3 ------------------------------------------------------------------------------------
# Total lead times
unique_periods <- sort(unique(t_uw$IDOSZAK))
t1 <- unique_periods[length(unique_periods)]
t2 <- unique_periods[length(unique_periods) - 1]


s_page5 <- gen_aggregate_sales_tied(t_uw %>%
  filter(ERTCSAT == "Hálózat", OUTLIER == 0), "IDOSZAK") %>%
  filter(!(!DIMENZIÓ %in% c("1. Aláír-kötv [mnap]", "TÁJ: Volumen [db]")
  & (stringr::str_detect(IDOSZAK, t1) | stringr::str_detect(IDOSZAK, t2))))

write.csv(s_page5, here::here("Data", "s_page5.csv"), row.names = FALSE)


# Total lead time with total volumes broken down by product line
s_page6 <- gen_aggregate_sales_tied(t_uw %>% filter(ERTCSAT == "Hálózat", OUTLIER == 0), "IDOSZAK", "TERMCSOP_SALES") %>%
  filter(!(!DIMENZIÓ %in% c("1. Aláír-kötv [mnap]", "TÁJ: Volumen [db]")
           & (stringr::str_detect(IDOSZAK, t1) | stringr::str_detect(IDOSZAK, t2))))

write.csv(s_page6, here::here("Data", "s_page6.csv"), row.names = FALSE)


