#Allowed year combinations in data
vt.mig_trend_year_combn <- df.db_mig_data %>%
  select(PREV_YEAR, CURR_YEAR) %>%
  distinct %>% 
  arrange(PREV_YEAR, CURR_YEAR)

#We always want to show all the data for trends
vt.mig_trend_param_year_1_min <- min(df.db_mig_data$PREV_YEAR)
vt.mig_trend_param_year_2_max <- max(df.db_mig_data$CURR_YEAR)

df.mig_trend_years_to_rank <- vt.mig_trend_year_combn %>%
  filter(
    PREV_YEAR >= vt.mig_trend_param_year_1_min,
    CURR_YEAR <= vt.mig_trend_param_year_2_max
  ) %>%
  mutate(YEAR_GAP = CURR_YEAR - PREV_YEAR) %>%
  group_by(PREV_YEAR) %>%
  filter(YEAR_GAP == min(YEAR_GAP)) 

df.init_mig_trend_base_data = df.db_mig_data %>% 
  inner_join(df.mig_trend_years_to_rank %>% 
               select(PREV_YEAR, CURR_YEAR),
             by = c("PREV_YEAR", "CURR_YEAR"))

#Direction of Flow
vt.init_mig_trend_param_flow_select <- c("Net inflow", "Inflow", "Outflow")

vt.init_mig_trend_param_flow <- vt.init_mig_trend_param_flow_select[1]

df.mig_trend_param_flow_map <- data_frame(NAME = vt.init_mig_trend_param_flow_select,
                                          VALUE = c('net', "in", 'out'))

#Mapping tbl for flow_type
df.mig_trend_flow_type_mapping <-  data_frame(FLOW_TYPE = c('tot', 2:4),
                                              FLOW_TYPE_LABEL = c("Total population change",
                                                                  "Migration within NZ",
                                                                  "International migration",
                                                                  "Natural increase"))

#Type of population change for Map
vt.init_mig_trend_param_flow_type_select <- df.mig_trend_flow_type_mapping$FLOW_TYPE_LABEL
vt.init_mig_trend_param_flow_type <- vt.init_mig_trend_param_flow_type_select[1]

#Type of population change for movement between locations
vt.init_mig_trend_param_flow_type_st_select <- df.mig_trend_flow_type_mapping %>% 
  filter(FLOW_TYPE %in% 2:3) %>% 
  .[["FLOW_TYPE_LABEL"]]

vt.init_mig_trend_param_flow_type_st <- vt.init_mig_trend_param_flow_type_st_select[1]

vt.init_mig_trend_param_chart_type_select <- c("Line", "Bar")
vt.init_mig_trend_param_chart_type <- vt.init_mig_trend_param_chart_type_select[1]

#Default levels for the movement between locations
vt.init_mig_trend_param_source_select <- list(`Total NZ` = "Total NZ",
                                              `Territorial Authorities` = df.db_mig_data %>% 
                                                select(TA, TA_LEVEL) %>% 
                                                distinct %>% 
                                                filter(TA_LEVEL) %>%
                                                arrange(TA) %>% 
                                                .[["TA"]])

vt.init_mig_trend_param_source <- "Auckland"
vt.init_mig_trend_param_target <- "Christchurch City"

#Labels for population change demo breakdown selection
vt.init_mig_trend_param_demo_select <- df.db_mig_data %>% 
  mutate(FLOW_DEMOGRAPHIC_LABEL = ifelse(is.na(FLOW_DEMOGRAPHIC),
                                         "Total",
                                         as.character(FLOW_DEMOGRAPHIC))) %>% 
  .[["FLOW_DEMOGRAPHIC_LABEL"]] %>% 
  unique()

vt.init_mig_trend_param_demo <- vt.init_mig_trend_param_demo_select[1]

#Labels for population demo breakdown
vt.init_mig_trend_param_popn_demo_select <- c("Total",
                                         df.db_mig_data %>% 
                                           filter(FLOW_TYPE == "tot") %>% 
                                           mutate(FLOW_DEMOGRAPHIC_LABEL = ifelse(grepl("Age", FLOW_DEMOGRAPHIC),
                                                                                  "Age",
                                                                                  as.character(FLOW_DEMOGRAPHIC))) %>% 
                                           .[["FLOW_DEMOGRAPHIC_LABEL"]] %>% 
                                           unique())

vt.init_mig_trend_param_popn_demo <- vt.init_mig_trend_param_demo_select[1]


#TA code to TA mapping
df.mig_ta_code_mapping <- df.db_mig_data %>% 
  filter(FLOW_DEMOGRAPHIC == "TA",
         as.numeric(FLOW_DEMOGRAPHIC_VALUE) == TA_CODE
  ) %>% 
  select(TA, FLOW_DEMOGRAPHIC_VALUE) %>% 
  distinct

#Colouring for highcharts
vt.mig_trends_hc_colouring <- c(
  df.treasury_color_refresh %>% filter(colour == "Treasury blue") %>% .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Blue 3", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Blue 4") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 3") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Green 3", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Secondary", degree == 3) %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Secondary", degree == 4) %>%  .[['code']]
)

#Name the panels
#These also are used in the download handler
vt.mig_trend_panel_1 <- "Population trends"
vt.mig_trend_panel_2 <- "Population change by type of change"
vt.mig_trend_panel_3 <- "Population flows between TAs and for specific countries"

vt.mig_trend_source_loc_name <- "Location(s) of interest 1"
vt.mig_trend_target_loc_name <- "Location(s) of interest 2"
vt.mig_trend_target_international_loc_name <- "Country of origin"
vt.mig_trend_param_demo_label <- "Characteristic of interest"

#The country names to ID mapping
df.region_country_mapping_table

vt.init_mig_trends_param_country_select <- list(`Total International` = "Total International",
                                                `Country of origin` = sort(df.region_country_mapping_table$COUNTRY))
vt.init_mig_trends_param_country <- "Australia"
df.mig_trends_world_country_mapping <- df.region_country_mapping_table

#Generate total popn metrics
df.mig_trends_total_metric_names <- df.db_mig_data %>%
  filter(FLOW_DIR == 'in',
         (FLOW_TYPE == 2) |
           (FLOW_DEMOGRAPHIC %in% c("Country", "Visa") &
           FLOW_TYPE == 3)) %>% 
  select(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, METRIC) %>%
  distinct %>%
  inner_join(df.metric_mapping_table,
             by = "METRIC") %>% 
  mutate(TOTAL_METRIC_NAME = str_replace(METRIC_NAME, "Inflow - WithinNZ|Inflow - International", "Total -")) %>% 
  select(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE, TOTAL_METRIC_NAME) %>%
  filter(!is.na(FLOW_DEMOGRAPHIC) & !is.na(FLOW_DEMOGRAPHIC_VALUE)) %>% 
  group_by(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>% 
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(data_frame(FLOW_DEMOGRAPHIC = NA,
                       FLOW_DEMOGRAPHIC_VALUE = NA,
                       TOTAL_METRIC_NAME = "Total Population Change"))%>% 
  mutate(FDV_LABEL = str_extract(TOTAL_METRIC_NAME, "(?<=- )(.)*"))

#Mapping tbl for flow_type
df.mig_trend_flow_type_mapping <- data_frame(FLOW_TYPE = c('tot', 2:4),
                                       FLOW_TYPE_LABEL = c("Total population change",
                                                           "Migration within NZ",
                                                           "International migration",
                                                           "Natural increase"))


#Used for determining which csv column mapping to use for download functionality
vt.mig_trend_param_flow_dir_in = c("net", "in")

vt.mig_trend_tot_ft_label = df.mig_trend_flow_type_mapping %>% 
  filter(FLOW_TYPE == "tot") %>% 
  .[["FLOW_TYPE_LABEL"]]

#For the % calculation in the population graph
df.mig_trends_year_popn = df.db_mig_data %>% 
  filter(TA_CODE == 999) %>% 
  inner_join(df.mig_all_years_to_rank,
             by = c("PREV_YEAR", "CURR_YEAR")) %>% 
  mutate(MIN_YEAR = PREV_YEAR == min(PREV_YEAR)) %>% 
  mutate(YEAR = CURR_YEAR) %>% 
  mutate(POPULATION = TOTAL_CURR) %>% 
  bind_rows(df.db_mig_data %>% 
              filter(TA_CODE == 999,
                     PREV_YEAR == min(PREV_YEAR)) %>% 
              inner_join(df.mig_all_years_to_rank,
                         by = c("PREV_YEAR", "CURR_YEAR")) %>% 
              mutate(YEAR = PREV_YEAR,
                     POPULATION = TOTAL_PREV)) %>% 
  select(YEAR, POPULATION) %>% 
  distinct()
