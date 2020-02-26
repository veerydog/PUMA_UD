#MOTUS Chapter 4: Tag & Receiver Deplpoyments
#Notes taken 24 Feb 2020 by KMB

#STEP 1: Load packages and set time zone
{library(tidyverse)
library(motus)
library(lubridate)

# Set the system environment time zone to UTC (to ensure that you are always working in UTC)
Sys.setenv(TZ = "UTC")}
getwd()

#Step 2: Load .motus file
proj.num <- 262
sql.motus <- tagme(proj.num, update = TRUE, dir = "./")

#Step 3: Assess Tag Deployments and Tag Metadata
metadata(sql.motus, projectIDs = proj.num) #download complete metadata
tbl.tags <- tbl(sql.motus, "tags") 
df.tags <- tbl.tags %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame()
nrow(df.tags) # number of registered tags in the database

###^ Error here. My numbers aren't matching with the tutorial##########

tbl.tagDeps <- tbl(sql.motus, "tagDeps") 

df.tagDeps <- tbl.tagDeps %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame() %>% # once in df format, can format dates with lubridate
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01")) 

anti_join(df.tags, df.tagDeps, by = "tagID") 

### Load base map files
na.lakes <- map_data(map = "lakes")
na.lakes <- mutate(na.lakes, long = long - 360)

# Include all of the Americas to begin
na.map <- map_data(map = "world2")
na.map <- filter(na.map, region %in% c("Canada", "USA"))

na.map <- mutate(na.map, long = long- 360)

# Others countries in the Americas that you may want to plot, depending on your
# location: "Mexico", "lakes","Belize", "Costa Rica", "Panama", "Guatemala",
# "Honduras", "Nicaragua", "El Salvador", "Colombia", "Venezuela", "Ecuador",
# "Peru", "Brazil", "Guyana","Suriname", "Bolivia", "French Guiana", "Jamaica",
# "Cuba", "Haiti", "Dominican Republic", "The Bahamas", "Turks and Caicos
# Islands", "Puerto Rico", "British Virgin Islands", "Montserrat", "Dominica",
# "Saint Lucia", "Barbados", "Grenada", "Trinidad and Tobago", "Chile",
# "Argentina", "Uruguay"

# set limits to map based on locations of detections, ensuring they include the
# deployment locations
xmin <- -100 #min(df.tagDeps$longitude, na.rm = TRUE) - 5
xmax <- max(df.tagDeps$longitude, na.rm = TRUE) + 5
ymin <- min(df.tagDeps$latitude, na.rm = TRUE) - 5
ymax <- max(df.tagDeps$latitude, na.rm = TRUE) + 5

# map using ggplot
ggplot(data = na.lakes, aes(x = long, y = lat)) + 
  geom_polygon(data = na.map, aes(long, lat, group = group), 
               colour = "grey", fill="grey98") + 
  geom_polygon(aes(group = group), colour = "grey", fill = "white") +
  coord_map(projection = "mercator", 
            xlim = c(xmin, xmax), 
            ylim = c(ymin, ymax)) +
  labs(x = "", y = "") + 
  theme_bw() + 
  geom_point(data = filter(df.tagDeps, projectID == 176), 
             aes(longitude, latitude), size = 2, shape = 1, colour = "red")
