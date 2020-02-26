#Mapping libraries ###############################################################
{library(circular)
  library(tidyverse)
  library(ggplot2)
  library(jpeg)
  library(ggmap)
  library(rworldmap)
  library(maps)
  library(lubridate)
  library(dplyr)}

#Make sure working on UTC. MUST DO EVERY TIME. ################################### 
Sys.setenv(TZ = "UTC")

#Install & Load MOTUS & Razimuth #################################################
{install.packages("remotes")
  library(remotes)
  install_github("MotusWTS/motus")
  install_github("MotusWTS/motusData")
  install_github("cppeck/razimuth")
  library(motus)
  library(motusData)}

# set proj.num to 176 for sample data
proj.num <- 176 #Sample dataset = 176

# - download and load detection data from the sample project and save in data folder
# - login and password for sample data is "motus.sample"
# - if you are accessing already-downloaded data, use new = FALSE; if you don't
#   want to update your data, also set update = FALSE
sql.motus <- tagme(proj.num, new = TRUE, update = TRUE, dir = "./")

# access the "alltags" table within the SQLite file
tbl.alltags <- tbl(sql.motus, "alltags")

# convert "tbl.alltags" to a flat dataframe and change numeric time to a datetime object
df.alltags <- tbl.alltags %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

# reads in your file "df.alltags.rds" saved in the data folder
df.alltags.saved <- readRDS("./df_alltags.rds")
