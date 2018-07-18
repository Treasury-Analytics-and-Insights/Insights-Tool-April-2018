#### Section 01: Setup ----
## libraries control
require(checkpoint)
## comment on the your own checkpoint eval when finished
# dir.lib_checkpoint <- "/srv/shiny-server/R" # server
# dir.lib_checkpoint <- "D:/lib" # local
dir.lib_checkpoint <- "C:/Users/mcleodk" # local
# dir.lib_checkpoint <- "C:/R/project_checkpoints" # local
vt.checkpoint_date <- "2018-02-01"
# vt.r_version <- "3.3.1" # server
vt.r_version <- "3.4.0" # local
# vt.r_version <- "3.3.3" # local

checkpoint(snapshotDate = vt.checkpoint_date,
           R.version = vt.r_version,
           checkpointLocation = dir.lib_checkpoint,
           scanForPackages = TRUE)

### libraries
## data managements
require(dplyr)
require(readr)
require(tidyr)
require(lubridate)
require(stringr)
require(glue)
require(scales)
require(tidyverse)

## dashboard framework
require(shiny)
require(shinydashboard)
require(shinyBS)
require(shinyjs)

## charts
require(leaflet)
require(DT)
require(highcharter)
require(VennDiagram)
require(treemap)
require(circlize)

## TYO plotly charts
require(plotly)
require(scales)

## spatial
require(sp)
require(rgdal)

## image processing
require(png)

#Checkpoint directory to check that non CRAN packages are installed in
dir.check <- list.dirs(file.path(dir.lib_checkpoint, 
                                 ".checkpoint",
                                 vt.checkpoint_date),
                       recursive = F) %>% 
  # go down two more levels
  list.dirs(recursive = F) %>% 
  list.dirs(recursive = F)

#Find the directory for the version we're using
dir.check <- dir.check[which(grepl(vt.r_version, dir.check))]

# #To install non-CRAN package
library(devtools)
withr::with_libpaths(new = dir.check,
                     code = install_local(path = "./data/chorddiag-master"))

#Check that we do detect the non-CRAN packages in the checkpoint directory
vt.install_check <- installed.packages(dir.check) %>% 
  as.data.frame() %>% 
  as.tbl() %>% 
  filter(Package %in% "chorddiag") %>% 
  nrow()

stopifnot(vt.install_check == 1)

#NOTE: Chorddiag is not on CRAN
require(chorddiag)

rm(list = ls()); gc()

### directories
dir.input <- "data"
dir.src <- "scripts"
dir.tools <- "tools"
dir.cyar_circles <- "data/venn diagrams"

### utility functions
source(file.path(dir.src, "utility functions.R"))
source(file.path(dir.src, "treasury style.R"))

### dashboard components
source(file.path(dir.tools, "treasury-styles.R"))
source(file.path(dir.tools, "intro-pages.R"))
source(file.path(dir.tools, "SFY - ui.R"))
source(file.path(dir.tools, "TYO - ui.R"))
source(file.path(dir.tools, "CYAR - ui.R"))
source(file.path(dir.tools, "All Migration - ui.R"))
source(file.path(dir.tools, "Migration Trends - ui.R"))

### data 
load(file.path(dir.input, "CYAR Dashboard Data - SFY.rda"))
load(file.path(dir.input, "CYAR Dashboard Data - TYO.rda"))
load(file.path(dir.input, "CYAR Dashboard Data - CYAR.rda"))
load(file.path(dir.input, "Insights Dashboard Data - Migration.rda"))
load(file.path(dir.input, "CYAR Dashboard Data - Shapefiles.rda"))
df.meta_control <- read_csv(file.path(dir.input, "meta - control.csv"))
df.treasury_color <- read_csv(file.path(dir.input, "Treasury Colour Patterns.csv"))
df.treasury_color_refresh <- read_csv(file.path(dir.input, "Treasury Colour Patterns - Visual Identity Refresh.csv"))

#Renaming tables for csv downloads
df.mig_all_map_national_rename_absolute = read_csv(file.path(dir.input, "col_rename_national_absolute.csv"))
df.mig_all_map_national_rename_percentage = read_csv(file.path(dir.input, "col_rename_national_percentage.csv"))

df.mig_all_map_ta_rename_in_absolute = read_csv(file.path(dir.input, "col_rename_ta_in_absolute.csv"))
df.mig_all_map_ta_rename_in_percentage = read_csv(file.path(dir.input, "col_rename_ta_in_percentage.csv"))

df.mig_all_map_ta_rename_out_absolute = read_csv(file.path(dir.input, "col_rename_ta_out_absolute.csv"))
df.mig_all_map_ta_rename_out_percentage = read_csv(file.path(dir.input, "col_rename_ta_out_percentage.csv"))

df.mig_trend_locations_in_absolute =  read_csv(file.path(dir.input, "col_rename_locations_in_absolute.csv"))
df.mig_trend_locations_out_absolute =  read_csv(file.path(dir.input, "col_rename_locations_out_absolute.csv"))
df.mig_trend_locations_in_percentage =  read_csv(file.path(dir.input, "col_rename_locations_in_percentage.csv"))
df.mig_trend_locations_out_percentage =  read_csv(file.path(dir.input, "col_rename_locations_out_percentage.csv"))

df.mig_trend_population_rename_absolute = read_csv(file.path(dir.input, "population_col_rename_absolute.csv"))
df.mig_trend_population_rename_percentage = read_csv(file.path(dir.input, "population_col_rename_percentage.csv"))

df.mig_trend_loi_rename_absolute = read_csv(file.path(dir.input, "col_rename_loi_absolute.csv"))
df.mig_trend_loi_rename_percentage = read_csv(file.path(dir.input, "col_rename_loi_percentage.csv"))


### initial reactive values ----
source(file.path(dir.tools, "SFY - global.R"))
source(file.path(dir.tools, "TYO - global.R"))
source(file.path(dir.tools, "CYAR - global.R"))
source(file.path(dir.tools, "All Migration - global.R"))
source(file.path(dir.tools, "Migration Trends - global.R"))