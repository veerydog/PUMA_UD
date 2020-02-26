#########################################
# Filtering and Processin PUMA eBird Data
# Katie Bird
# 13 November 2019
#########################################

######   Step 1: Install Libraries   ####
{#install.packages(c("auk", raster", "tidyvserse", "sf", "rnaturalearth", "rnaturalearthhires", "tigris", "tigris", "viridisLite", "leaflet"))
    library(auk)
    library(raster)
    library(tidyverse)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthhires)
    library(tigris)
    library(rgeos)
    library(viridisLite)
    library(leaflet)
    }
#########################################

######    Step 2: Set up paths      #####

# path to ebird data txt file
ebd_dir <- "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/"

# ebd
f <- file.path(ebd_dir, "ebd_US_purmar_relSep-2019.txt")
 
# define the paths to ebd files
f_in_ebd <- file.path(ebd_dir, "ebd_US_purmar_relSep-2019.txt")

# create an object referencing this file
auk_ebd(file = f_in_ebd)

#Now let's apply the filters we want.
ebd_filters <- auk_ebd(f_in_ebd) %>%
    auk_state(c("US-DE", "US-NJ", "US-MD")) %>%
    auk_date(c("*-07-01", "*-09-30")) %>%
    auk_complete()
ebd_filters

#Now extract the filtered data to another file.
f_out_ebd <- "ebd_puma_de-nj-md_fallmig.txt"
ebd_filtered <- auk_filter(ebd_filters, file = f_out_ebd, overwrite = TRUE)

ebd <- read_ebd(f_out_ebd)
glimpse(ebd)

ebd$observation_count <- as.numeric(ebd$observation_count)
roosts <- subset(ebd, observation_count>100)
roosts_NJ <- subset(roosts, roosts$state == "New Jersey")
roosts_MD <- subset(roosts, roosts$state == "Maryland")
roosts_DE <- subset(roosts, roosts$state == "Delaware")

write.csv(roosts_DE, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_de.csv", row.names = FALSE)

write.csv(roosts_MD, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_md.csv", row.names = FALSE)

write.csv(roosts_NJ, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_nj.csv", row.names = FALSE)

#########################################
###          Plot a Static Map        ###
###      Don't run this as a loop!    ###
#########################################
{#Now that the data is in R, we can make a map of it.
    # convert ebd to spatial object
    ebd_sf <- roosts %>% 
        select(common_name, latitude, longitude) %>% 
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    # get state boundaries using rnaturalearth package
    states <- ne_states(iso_a2 = c("US", "CA"), returnclass = "sf")
    de <- filter(states, postal == "DE") %>% 
        st_geometry()
    # map
    par(mar = c(0, 0, 0, 0), bg = "skyblue")
    # set plot extent
    plot(de, col = NA)
    # add state boundaries
    plot(states %>% st_geometry(), col = "grey40", border = "white", add = TRUE)
    plot(de, col = "grey20", border = "white", add = TRUE)
    # ebird data
    plot(ebd_sf %>% filter(common_name == "Purple Martin"), 
         col = "#4daf4a99", pch = 19, cex = 0.75, add = TRUE)
    }

##################################################################    
######    Make an interactive map of roosts for MD, NJ, DE.    ###
##################################################################
{
#if(!require(leaflet)) install_github("rstudio/leaflet")
#library(leaflet)
} #hidden if needed

# Plot a default web map (brackets display the result)
m_roosts <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(data = roosts, lng = roosts$longitude, lat=roosts$latitude, label = ~observation_date)

m_roosts  # Print the map
    
####################################################################
# Roosts within NJ, segrated by large and small
# large is > 1000
# small is < 1000 but > 100

getColor <- function(roosts_NJ) 
    sapply(roosts_NJ$observation_count, function(observation_count) 
        if(observation_count >= 1000) {"green"
        } else if(observation_count>=101 & observation_count<=999) {
            "orange"})

icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(roosts_NJ)
)

leaflet(roosts_NJ) %>% addTiles() %>%
    addAwesomeMarkers(roosts_NJ$longitude, roosts_NJ$latitude, icon=icons, label=~as.character(observation_count))
    
####################################################################

getColor <- function(roosts_MD) 
    sapply(roosts_MD$observation_count, function(observation_count) 
        if(observation_count >= 1000) {"green"
        } else if(observation_count>=101 & observation_count<=999) {
            "orange"})

icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(roosts_MD)
)

leaflet(roosts_MD) %>% addTiles() %>%
    addAwesomeMarkers(roosts_MD$longitude, roosts_MD$latitude, icon=icons, label=~as.character(observation_count))

####################################################################

getColor <- function(roosts_DE) 
    sapply(roosts_DE$observation_count, function(observation_count) 
        if(observation_count >= 1000) {"green"
        } else if(observation_count>=101 & observation_count<=999) {
            "orange"})

icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(roosts_DE)
)

leaflet(roosts_DE) %>% addTiles() %>%
    addAwesomeMarkers(roosts_DE$longitude, roosts_DE$latitude, icon=icons, label=~as.character(observation_count))

###################################################################################

#Now let's apply the filters we want.
ebd_filters <- auk_ebd(f_in_ebd) %>%
    auk_state(c("US-VA", "US-NC", "US-SC")) %>%
    auk_date(c("*-07-01", "*-09-30")) %>%
    auk_complete()
ebd_filters

#Now extract the filtered data to another file.
f_out_ebd <- "ebd_puma_va-nc-sc_fallmig.txt"
ebd_filtered <- auk_filter(ebd_filters, file = f_out_ebd, overwrite = TRUE)

ebd <- read_ebd(f_out_ebd)
glimpse(ebd)

ebd$observation_count <- as.numeric(ebd$observation_count)
roosts <- subset(ebd, observation_count>100)
roosts_VA <- subset(roosts, roosts$state == "Virginia")
roosts_NC <- subset(roosts, roosts$state == "North Carolina")
roosts_SC <- subset(roosts, roosts$state == "South Carolina")

write.csv(roosts_VA, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_va.csv", row.names = FALSE)

write.csv(roosts_NC, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_nc.csv", row.names = FALSE)

write.csv(roosts_SC, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_sc.csv", row.names = FALSE)

######################################################################################

#Now let's apply the filters we want.
ebd_filters <- auk_ebd(f_in_ebd) %>%
    auk_state(c("US-PA", "US-NY", "US-CT")) %>%
    auk_date(c("*-07-01", "*-09-30")) %>%
    auk_complete()
ebd_filters

#Now extract the filtered data to another file.
f_out_ebd <- "ebd_puma_pa_ny_ct_fallmig.txt"
ebd_filtered <- auk_filter(ebd_filters, file = f_out_ebd, overwrite = TRUE)

ebd <- read_ebd(f_out_ebd)
glimpse(ebd)

ebd$observation_count <- as.numeric(ebd$observation_count)
roosts <- subset(ebd, observation_count>100)
roosts_PA <- subset(roosts, roosts$state == "Pennsylvania")
roosts_NY <- subset(roosts, roosts$state == "New York")
roosts_CT <- subset(roosts, roosts$state == "Connecticut")

write.csv(roosts_PA, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_pa.csv", row.names = FALSE)

write.csv(roosts_NY, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_ny.csv", row.names = FALSE)

write.csv(roosts_CT, "C:/Users/radar/Desktop/Projects/PurpleMartin_Project/ebird/roosts_ct.csv", row.names = FALSE)
