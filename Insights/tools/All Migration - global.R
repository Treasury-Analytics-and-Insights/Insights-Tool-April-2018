#global script for combined National/ta migration panel

#Allowed year combinations in data
vt.mig_all_year_combn <- df.db_mig_data %>%
  select(PREV_YEAR, CURR_YEAR) %>%
  distinct %>% 
  arrange(PREV_YEAR, CURR_YEAR)

#Year Inputs
vt.init_mig_all_param_year_1_select <- df.db_mig_data %>% 
  select(PREV_YEAR) %>% 
  distinct() %>% 
  arrange(PREV_YEAR) %>% 
  .[["PREV_YEAR"]]

vt.init_mig_all_param_year_1 <- vt.init_mig_all_param_year_1_select[length(vt.init_mig_all_param_year_1_select)]

vt.init_mig_all_param_year_2_select <- df.db_mig_data %>% 
  select(CURR_YEAR) %>% 
  distinct() %>% 
  arrange(CURR_YEAR) %>% 
  .[["CURR_YEAR"]]

vt.init_mig_all_param_year_2 <- vt.init_mig_all_param_year_2_select[length(vt.init_mig_all_param_year_2_select)]

vt.init_mig_all_year_min <- min(vt.init_mig_all_param_year_1_select)

vt.init_mig_all_year_max <- max(vt.init_mig_all_param_year_2_select)

vt.init_mig_all_year_range <- c(max(vt.init_mig_all_param_year_1_select),
                                max(vt.init_mig_all_param_year_2_select))

#TA or national Input
vt.init_mig_all_agg_level_select <- c("Total NZ", "Territorial authority")
vt.init_mig_all_agg_level <- vt.init_mig_all_agg_level_select[1]


#TA Select
vt.init_mig_all_param_ta_select <- df.db_mig_data %>% 
  filter(TA_LEVEL) %>% 
  select(TA) %>% 
  distinct() %>% 
  arrange(TA) %>% 
  .[["TA"]]

vt.init_mig_all_param_ta <- "Auckland"

vt.init_mig_all_param_flow_select <- c("Net inflow", "Inflow", "Outflow")

vt.init_mig_all_param_flow <- vt.init_mig_all_param_flow_select[1]

df.mig_all_param_flow_map <- data_frame(NAME = vt.init_mig_all_param_flow_select,
                                        VALUE = c('net', "in", 'out'))

#Initial data
df.init_mig_all_base_data <- df.db_mig_data %>% 
  filter(
    PREV_YEAR %in% vt.init_mig_all_param_year_1,
    CURR_YEAR %in% vt.init_mig_all_param_year_2
  )

#Render heat map for migration numbers or for as a percentage of the popn.
vt.init_mig_all_param_map_perc_select <- c(F, T)
vt.init_mig_all_param_map_perc <- vt.init_mig_all_param_map_perc_select[1]

#Mapping tbl for flow_type
df.mig_flow_type_mapping <- data_frame(FLOW_TYPE = c('tot', 2:4),
                                       FLOW_TYPE_LABEL = c("Total population change",
                                                           "Migration within NZ",
                                                           "International migration",
                                                           "Natural increase"))

vt.mig_flow_type_tot_label = df.mig_flow_type_mapping %>% 
  filter(FLOW_TYPE == 'tot') %>% 
  .[["FLOW_TYPE_LABEL"]]

#Mapping tbl for flow_dir for natural change
df.mig_natural_change_flow_dir_mapping <- data_frame(FLOW_DIR = vt.init_mig_all_param_flow_select,
                                                     FLOW_DIR_VALUE = c('net', "in", 'out'),
                                                     FLOW_DIR_LABEL = c("Natural increase",
                                                                        "Births",
                                                                        "Deaths"))

#Type of migration for Map
vt.init_mig_all_parm_flow_type_select <- df.mig_flow_type_mapping$FLOW_TYPE_LABEL
vt.init_mig_all_parm_flow_type <- vt.init_mig_all_parm_flow_type_select[1]

#Label test for map dir
vt.mig_all_map_label_test <- df.mig_natural_change_flow_dir_mapping %>%  
  filter(FLOW_DIR_VALUE %in% c("net", "in")) %>% 
  .[["FLOW_DIR"]]

#Colouring of the selected TA
vt.mig_all_map_selected_fill <- "#808080"
vt.mig_all_map_selected_outline = "#FFFFFF"

#Only have domestic migration from other TAs, no other types
vt.init_mig_all_parm_flow_type_ta_select <- df.mig_flow_type_mapping %>% 
  filter(!(FLOW_TYPE %in% c(3:4, 'tot'))) %>% 
  .[["FLOW_TYPE_LABEL"]]
vt.init_mig_all_parm_flow_type_ta <- vt.init_mig_all_parm_flow_type_ta_select[1]

#Initial mapping data

spldf.init_mig_all_map <- spldf.nz_ta_migration
spldf.init_mig_all_map@data <- spldf.init_mig_all_map@data %>% 
  mutate(code = as.numeric(code)) %>% 
  left_join(df.init_mig_all_base_data %>% 
              filter(FLOW_DIR == df.mig_all_param_flow_map %>% 
                       filter(NAME == vt.init_mig_all_param_flow) %>% 
                       .[["VALUE"]],
                     FLOW_TYPE %in% 2:5,
                     is.na(FLOW_DEMOGRAPHIC),
                     TA_LEVEL) %>% 
              mutate(FLOW_TYPE = 'tot',
                     FLOW_DIR = 'net') %>% 
              group_by(TA, TA_CODE, TOTAL_PREV, TOTAL_CURR, PREV_YEAR, CURR_YEAR, FLOW_DIR, FLOW_TYPE) %>% 
              summarise(VALUE = sum(VALUE)),
            by = c('code' = 'TA_CODE'))

#Initial international mapping data
spldf.init_mig_all_world_map <- spldf.world_country

#Countries to use
vt.mig_all_world_map_countries <- df.db_mig_data %>% 
  filter(FLOW_TYPE == 3,
         FLOW_DEMOGRAPHIC == "Country",
         FLOW_DEMOGRAPHIC_VALUE != "ZZ") %>% 
  select(FLOW_DEMOGRAPHIC_VALUE) %>% 
  distinct %>% 
  .[["FLOW_DEMOGRAPHIC_VALUE"]]


spldf.init_mig_all_world_map@data <- spldf.init_mig_all_world_map@data %>%
  inner_join(df.db_mig_data %>% 
               filter(TA_CODE == 999,
                      FLOW_DIR == df.mig_all_param_flow_map %>% 
                        filter(NAME == vt.init_mig_all_param_flow) %>% 
                        .[["VALUE"]],
                      FLOW_DEMOGRAPHIC == "Country",
                      FLOW_TYPE == 3,
                      PREV_YEAR == vt.init_mig_all_param_year_1,
                      CURR_YEAR == vt.init_mig_all_param_year_2),
             by = c("ID" = "FLOW_DEMOGRAPHIC_VALUE"))

#Groupings to display in our chord diagram
vt.mig_all_chord_grouping <- df.db_mig_data %>% 
  filter(
    TA_LEVEL
  ) %>% 
  select(TA, TA_CODE) %>% 
  distinct %>% 
  mutate(TA_CODE_GROUP = case_when(as.numeric(.$TA_CODE) %in% c(76, 16, 23, 71) ~ .$TA,
                                   as.numeric(.$TA_CODE) %in% c(47, 46, 45, 44) ~ "Greater Wellington",
                                   as.numeric(.$TA_CODE) %in% c(60, 62, 63) ~ "Greater Christchurch",
                                   as.numeric(.$TA_CODE) <= 50 ~ "Other North Island",
                                   T ~ "Other South Island"))

#North to south ordering
vt.mig_all_chord_levels <- c("Auckland", 
                             "Tauranga City", 
                             "Hamilton City",
                             "Greater Wellington",
                             "Other North Island",
                             "Greater Christchurch",
                             "Dunedin City",
                             "Other South Island")

#Ordering for National chord diagram
vt.mig_all_chord_order <- c("Auckland",
                            "Tauranga City",
                            "Hamilton City",
                            "Greater Wellington",
                            "Other North Island",
                            "Greater Christchurch",
                            "Dunedin City",
                            "Other South Island")

#Total prev populations to join in TA level percentage chord diagram
df.mig_all_chord_ta_popn <- df.db_mig_data %>% 
  select(TA, TA_CODE, TOTAL_PREV, PREV_YEAR, CURR_YEAR) %>% 
  distinct

#Colouring for chord diagram
vt.mig_all_chord_colouring <- c(
  df.treasury_color_refresh %>% filter(colour == "Treasury blue") %>% .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Blue 3", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Blue 4") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 3") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Green 3", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Core") %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Secondary", degree == 3) %>%  .[['code']],
  df.treasury_color_refresh %>% filter(colour == "Supporting 4", palette == "Secondary", degree == 4) %>%  .[['code']]
)

#Colour definitions
vt.mig_all_col_red <- "#fc0000"
vt.mig_all_col_green <- '#2efc00' 

vt.mig_all_col_purple1_d3 <- df.treasury_color %>% 
  filter(colour == "purple1") %>% 
  filter(degree == 3) %>% 
  .[['code']]

vt.mig_all_col_purple1_d2 <- df.treasury_color %>% 
  filter(colour == "purple1") %>% 
  filter(degree == 2) %>% 
  .[['code']]

vt.mig_all_col_purple1_d1 <- df.treasury_color %>% 
  filter(colour == "purple1") %>% 
  filter(degree == 1) %>% 
  .[['code']]

#Initialise bump chart data
df.init_mig_all_bump <- df.db_mig_data %>% 
  filter(
    PREV_YEAR %in% vt.init_mig_all_param_year_1, 
    CURR_YEAR %in% vt.init_mig_all_param_year_2,
    TA %in% vt.init_mig_all_param_ta,
    FLOW_DEMOGRAPHIC == "TA"
  ) %>% 
  mutate(VALUE_RANK = rank(-VALUE, ties.method = "min"))

#Mapping table for chord diagram
#from ta chr codes and other names to final names
df.mig_all_chord_mapping <- spldf.nz_ta_migration@data %>%
  bind_rows(data_frame(
    ID = c("Other North Island", "Other South Island"),
    code = c("oni", "osi")
  ))

#Find steps to take (of minimum size between first and last year)
#Used in bumps chart

#We always want to show all the data for this chart
vt.mig_all_param_year_1_min <- min(df.db_mig_data$PREV_YEAR)
vt.mig_all_param_year_2_max <- max(df.db_mig_data$CURR_YEAR)

df.mig_all_years_to_rank <- vt.mig_all_year_combn %>%
  filter(
    PREV_YEAR >= vt.mig_all_param_year_1_min,
    CURR_YEAR <= vt.mig_all_param_year_2_max
  ) %>%
  mutate(YEAR_GAP = CURR_YEAR - PREV_YEAR) %>%
  group_by(PREV_YEAR) %>%
  filter(YEAR_GAP == min(YEAR_GAP)) 

#Different metrics for filtering bump chart
vt.init_mig_all_bumps_param_metric_select <- df.db_mig_data %>% 
  select(FLOW_DEMOGRAPHIC) %>% 
  distinct() %>% 
  #Relabel
  mutate(FLOW_DEMOGRAPHIC = ifelse(is.na(FLOW_DEMOGRAPHIC),
                                   "All",
                                   FLOW_DEMOGRAPHIC)) %>% 
  .[["FLOW_DEMOGRAPHIC"]]

vt.init_mig_all_bumps_param_metric <- "TA"

#New colour palette colours to use
vt.mig_all_col_new_treasury_blue <-  df.treasury_color_refresh %>% 
  filter(
    palette == "Core",
    colour == "Treasury blue"
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_blue_3 <-  df.treasury_color_refresh %>% 
  filter(
    palette == "Core",
    colour == "Blue 3"
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_blue_4 <-  df.treasury_color_refresh %>% 
  filter(
    palette == "Core",
    colour == "Blue 3"
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_blue_5_d4 <-  df.treasury_color_refresh %>% 
  filter(
    palette == "Secondary",
    colour == "Blue 5",
    degree == 4
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_green_3_d1 <- df.treasury_color_refresh %>% 
  filter(
    palette == "Secondary",
    colour == "Green 3",
    degree == 1
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_green_3_d3 <- df.treasury_color_refresh %>% 
  filter(
    palette == "Secondary",
    colour == "Green 3",
    degree == 3
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_grey <- df.treasury_color_refresh %>% 
  filter(
    palette == "Publication",
    colour == "Lt Grey"
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_orange_d1 <- df.treasury_color_refresh %>% 
  filter(
    palette == "Secondary",
    colour == "Supporting 4",
    degree == 1
  ) %>% 
  .[["code"]]

vt.mig_all_col_new_orange_d3 <- df.treasury_color_refresh %>% 
  filter(
    palette == "Secondary",
    colour == "Supporting 4",
    degree == 3
  ) %>% 
  .[["code"]]

#Colours to use for different flow types
vt.mig_all_col_in <- vt.mig_all_col_new_blue_4

vt.mig_all_col_out <- vt.mig_all_col_new_orange_d1

df.mig_all_flow_type_col_mapping <- data_frame(FLOW_TYPE = c('tot', 2:4),
                                               COL_IN = c(df.treasury_color_refresh %>% 
                                                            filter(colour == "Supporting 4",
                                                                   palette == "Core") %>% 
                                                            .[['code']],
                                                          df.treasury_color_refresh %>% 
                                                            filter(colour == "Green 3",
                                                                   palette == "Core") %>% 
                                                            .[['code']],
                                                          df.treasury_color_refresh %>% 
                                                            filter(colour == "Supporting 3",
                                                                   palette == "Core") %>% 
                                                            .[['code']],
                                                          df.treasury_color_refresh %>% 
                                                            filter(colour == "Supporting 4",
                                                                   palette == "Secondary",
                                                                   degree == 3) %>% 
                                                            .[['code']]),
                                               COL_OUT = c(df.treasury_color_refresh %>% 
                                                             filter(colour == "Blue 5",
                                                                    palette == "Secondary",
                                                                    degree == 5) %>% 
                                                             .[['code']],
                                                           df.treasury_color_refresh %>% 
                                                             filter(colour == "Blue 4",
                                                                    palette == "Publication") %>% 
                                                             .[['code']],
                                                           df.treasury_color_refresh %>% 
                                                             filter(colour == "Blue 3",
                                                                    palette == "Core") %>% 
                                                             .[['code']],
                                                           df.treasury_color_refresh %>% 
                                                             filter(colour == "Treasury blue",
                                                                    palette == "Core") %>% 
                                                             .[['code']]))

#New shades of colours not in the palette, added for the map 
#Reverted at the moment but, I will leave it below in case Keith wants to try editing this again later.
df.mig_all_flow_type_col_map_mapping <- df.mig_all_flow_type_col_mapping

# df.mig_all_flow_type_col_map_mapping <- data_frame(FLOW_TYPE = c('tot', 2:4),
#                                                COL_IN = c(rgb(201,127,13, maxColorValue = 255),
#                                                           rgb(77,126,63, maxColorValue = 255),
#                                                           rgb(102,119,26, maxColorValue = 255),
#                                                           rgb(203,94,30, maxColorValue = 255)),
#                                                COL_OUT = c(rgb(63,64,58, maxColorValue = 255),
#                                                            rgb(0,98,129, maxColorValue = 255),
#                                                            rgb(0,94,113, maxColorValue = 255),
#                                                            rgb(0,32,91, maxColorValue = 255)))

vt.mig_all_col_neutral <- '#000000'
vt.mig_all_col_neutral_map <- '#ffffff'

#aggregation table for movers graph
df.mig_all_fdv_mapping_table <- df.db_mig_data %>% 
  filter(FLOW_DEMOGRAPHIC %in%  c("Previous Age",
                                  "Sex",
                                  "Country",
                                  "Ethnicity",
                                  "Visa")) %>% 
  select(FLOW_DEMOGRAPHIC, FLOW_DEMOGRAPHIC_VALUE) %>%
  distinct %>% 
  mutate(FDV_AGG = case_when(.$FLOW_DEMOGRAPHIC_VALUE %in% c("00 - 04",
                                                             "05 - 09",
                                                             "10 - 14") ~ "Aged 0 - 14",
                             .$FLOW_DEMOGRAPHIC_VALUE %in% c("15 - 19",
                                                             "20 - 24") ~ "Aged 15 - 24",
                             .$FLOW_DEMOGRAPHIC_VALUE %in% c("25 - 29",
                                                             "30 - 34",
                                                             "35 - 39",
                                                             "40 - 44") ~ "Aged 25 - 44",
                             .$FLOW_DEMOGRAPHIC_VALUE %in% c("45 - 49",
                                                             "50 - 54",
                                                             "55 - 59",
                                                             "60 - 64") ~ "Aged 45 - 64",
                             .$FLOW_DEMOGRAPHIC == "Previous Age" ~ "Aged 65+",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 1 ~ "European",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 2 ~ "Maori",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 3 ~ "Pacific",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 4 ~ "Asian",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 5 ~ "MELAA",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 6 ~ "Other",
                             .$FLOW_DEMOGRAPHIC == "Ethnicity" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 7 ~ "Unknown",
                             .$FLOW_DEMOGRAPHIC == "Country" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "NZ" ~ "New Zealand",
                             .$FLOW_DEMOGRAPHIC == "Country" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "AA" ~ "All Other",
                             .$FLOW_DEMOGRAPHIC == "Sex" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 1 ~ "Male",
                             .$FLOW_DEMOGRAPHIC == "Sex" &
                               .$FLOW_DEMOGRAPHIC_VALUE == 2 ~ "Female",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "AUS" ~ "Australian citizen",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "NA" ~ "Not applicable",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "OTH" ~ "Other visas",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "RES" ~ "Residents visas",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "STU" ~ "International students",
                             .$FLOW_DEMOGRAPHIC == "Visa" &
                               .$FLOW_DEMOGRAPHIC_VALUE == "WRK" ~ "Temporary workers")) %>% 
  filter(!is.na(FDV_AGG))

#Get image dimensions for value box, use this to scale width
vt.mig_all_img_icons <- c("www/img/icons/tsy-icons_male-female.png",
                          "www/img/icons/tsy-icons_globe.png",
                          "www/img/icons/tsy-icons_bird.png",
                          "www/img/icons/tsy-icons_pattern.png")

df.mig_all_img_dim <- data_frame(x_length = sapply(vt.mig_all_img_icons,
                                                   function(x){dim(readPNG(x))[2]}),
                                 y_length = sapply(vt.mig_all_img_icons,
                                                   function(x){dim(readPNG(x))[1]}),
                                 icon = c('male-female', 'globe', 'bird', 'pattern')) %>% 
  mutate(ratio_scale = y_length / x_length,
         ratio_scale = ratio_scale/min(ratio_scale),
         height_perc = paste0(formatC(50 * 1/ratio_scale,
                                      format = 'f',
                                      digits = 1),
                              "%"))

#Country name to code mapping
df.mig_all_world_country_mapping <- spldf.world_country@data 

df.mig_all_world_country_vb_mapping <- df.mig_all_world_country_mapping %>% 
  mutate(code = ifelse(ID %in% c("US", "GB", "PH", "NL"), 
                       paste('the', code),
                       code))

#Arrange with single country regions last, required for highchart
df.region_country_mapping_table <- df.region_country_mapping_table %>% 
  group_by(REGION) %>%
  mutate(n_countries = n_distinct(COUNTRY)) %>%
  ungroup %>% 
  arrange(desc(n_countries)) %>% 
  select(-n_countries)

#Arrange Region/country levels
df.region_country_mapping_table <- df.region_country_mapping_table %>% 
  mutate(REGION = factor(REGION,
                         levels = c("New Zealand",
                                    "Oceania",
                                    "South-East and North-East Asia",
                                    "Southern and Central Asia",
                                    "North-West Europe",
                                    "Southern and Eastern Europe",
                                    "The Americas",
                                    "Sub-Saharan Africa",
                                    "North Africa and the Middle East",
                                    "Other"))) %>% 
  mutate(IS_REGION = is.na(as.numeric(CODE))) %>% 
  arrange(REGION, desc(IS_REGION), COUNTRY) %>% 
  select(-IS_REGION) %>% 
  mutate(REGION = as.character(REGION))

#Levels for arranging the highchart DF
vt.mig_all_region_levels <- unique(df.region_country_mapping_table$REGION)

vt.mig_all_country_levels <- unique(df.region_country_mapping_table$COUNTRY)

#Add line breaks to make things look nice
df.region_country_mapping_table_formatted <- df.region_country_mapping_table %>% 
  mutate(REGION = case_when(.$REGION == "Southern and Central Asia" ~  "Southern and <br>Central Asia",
                            .$REGION == "Southern and Eastern Europe" ~  "Southern and <br>Eastern Europe",
                            .$REGION == "Sub-Saharan Africa" ~ "Sub-Saharan <br>Africa",
                            T ~ .$REGION))

#Grouped country/regions for highchart
list.mig_all_country_region_map <- lapply(df.region_country_mapping_table_formatted %>% 
                                            filter(CODE != "NZ") %>% 
                                            .[["REGION"]] %>% 
                                            unique,
                                          function(x){
                                            
                                            vt.sub_cat_length <- df.region_country_mapping_table_formatted %>% 
                                              filter(REGION == x) %>% 
                                              .[["COUNTRY"]] %>% 
                                              length()
                                            
                                            #If the number of subcategories is 1 you need to only list the name attribute
                                            #If there are at least three categories you have enough room to split the name.
                                            if(vt.sub_cat_length > 3){
                                              
                                              tmp.cat <- df.region_country_mapping_table_formatted %>% 
                                                filter(REGION == x) %>% 
                                                .[["COUNTRY"]] %>% 
                                                gsub("(Other)(.*)", "Other", .)
                                              
                                              tmp.list_value <- list(name =  gsub(" ", " <br>", x),
                                                                     categories = tmp.cat)
                                              
                                            } 
                                            
                                            if(vt.sub_cat_length %in% 2:3){
                                              
                                              tmp.cat <- df.region_country_mapping_table_formatted %>% 
                                                filter(REGION == x) %>% 
                                                .[["COUNTRY"]] %>% 
                                                gsub("(Other)(.*)", "Other", .)
                                              
                                              tmp.list_value <- list(name =  x,
                                                                     categories = tmp.cat)
                                              
                                            } 
                                            
                                            if(vt.sub_cat_length == 1){
                                              
                                              tmp.name <- gsub("North Africa and the Middle East", "North Africa / Middle East", x)
                                              
                                              tmp.list_value <- list(name = tmp.name)
                                              
                                            }
                                            
                                            return(tmp.list_value)
                                            
                                          })

#Used for determining which csv column mapping to use for download functionality
vt.mig_all_param_flow_dir_in = c("net", "in")

#Title of map tab is also used for map invalidation the first time it is displayed.
vt.mig_all_map_tab_title = "Population change map"
