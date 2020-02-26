#Mapping libraries ###############################################################
{library(circular)
library(tidyverse)
library(ggplot2)
library(jpeg)
library(ggmap)
library(rworldmap)
library(maps)
library(lubridate)
library(dplyr)
library(RSQLite)}

#Make sure working on UTC. MUST DO EVERY TIME. ################################### 
Sys.setenv(TZ = "UTC")

#Install & Load MOTUS & Razimuth #################################################
{#install.packages("remotes")
library(remotes)
#install_github("MotusWTS/motus")
#install_github("MotusWTS/motusData")
#install_github("cppeck/razimuth")
library(motus)
library(motusData)}

#################################################################################
###            Downloading and Accessing MOTUS Data w/ Examples               ###
#################################################################################

getwd() #check working directory and set a new one if needed with setwd()
setwd("C:/Users/radar/Desktop/Projects/PurpleMartin_Project/PUMAfun/Triangulation_Code/")
proj.num <- 310 #specify project number. Sampleset = 176. PUMA_UD = 310
sql.motus <- tagme(projRecv = proj.num, new = TRUE, update = TRUE) #settings for first time downloading the data
#^this will create a .motus file and a SQL object in wd

#If download is interrupted use this to pick up from where it left off:
#sql.motus <-tagme(proj.num, update = TRUE)

# specify the filepath where your .motus file is saved, and the file name.
  file.name <- dbConnect(SQLite(), "./project-310.motus") 

# get a list of tables in the .motus file specified above.
  dbListTables(file.name)

# get a list of variables in the "species" table in the .motus file.
  dbListFields(file.name, "species")
  
# this retrieves the "alltags" table from the "sql.motus" SQLite file we read in earlier
  tbl.alltags <- tbl(sql.motus, "alltags") # virtual table
  
  str(tbl.alltags) #the underlying structure of these tables is a list of 2
  
# Another way to access the components of the underlying dataframe:
  tbl.alltags %>% 
    collect() %>%
    names() # list the variable names in the table
  
# To convert tables into typical flat format, which can be further manipulated
# or used to generate a RDS file of data for archiving or export:
  df.alltags <- tbl.alltags %>% 
    collect() %>% 
    as.data.frame() #Now we can look at some metrics of this file:
  
  names(df.alltags)     # field names
  str(df.alltags)       # structure of your data fields
  head(df.alltags)      # prints the first 6 rows of your df to the console
  summary(df.alltags)   # summary of each column in your df
  df.alltags <- tbl.alltags %>% 
    collect() %>% 
    as.data.frame() %>%     # for all fields in the df (data frame)
    mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))#Updates time stamp format
  
  # the tz = "UTC" is not necessary here, provided you have set your system time to UTC
  # ... but it serves as a useful reminder!
  
# to grab a subset of variables, in this case a unique list of Motus tag IDs at
# each receiver and antenna.
  df.alltagsSub <- tbl.alltags %>%
    select(recv, port, motusTagID) %>%
    distinct() %>% 
    collect() %>% 
    as.data.frame() 
  
# filter to include only motusTagIDs 16011, 23316
  df.alltagsSub <- tbl.alltags %>%
    filter(motusTagID %in% c(16011, 23316)) %>% 
    collect() %>% 
    as.data.frame() %>%    
    mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))  
  
# filter to only Red Knot (using speciesID)
  df.4670 <- tbl.alltags %>%
    filter(speciesID == 4670) %>%  
    collect() %>% 
    as.data.frame() %>%    
    mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))  
  
# filter to only Red Knot (using English name)
  df.redKnot <- tbl.alltags %>%
    filter(speciesEN == "Red Knot") %>%   
    collect() %>% 
    as.data.frame() %>%    
    mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))  
  
# use dplyr to summarize virtual data table before converting to flat file. This example
# finds the number of diff detections for each tag @ each receiver:
  df.detectSum <- tbl.alltags %>% 
    count(motusTagID, recv) %>%
    collect() %>%
    as.data.frame()
  
#To export flat dataframe to RDS file ***preserves time stamps***
  saveRDS(df.alltags, "./df_alltags.rds")
#To export as CSV file *** does NOT preserve time stamps***
  write.csv(df.alltags, "./df_alltags.csv")
  
  
  
  
#################################################################################
df.vanish <- vanishBearing %>% #reads in sample dataset for vanishingbearings
  mutate(recvSiteName = reorder(recvSiteName, recvLat),
         motusTagID = as.factor(as.character(motusTagID))) %>% # order sites by latitude
  arrange(ts) # arrange by ts

ggplot(data = df.vanish, aes(x = ts, y = recvLat, colour = as.factor(recvSiteName))) + 
  geom_point(pch = 21) +
  facet_wrap(~ motusTagID, scales = "free", ncol = 1) +
  theme_bw()
ggplot(data = filter(df.vanish, motusTagID == 16823, ts > "2015-05-30 00:00:00"), 
       aes(x = ts, y = sig, colour = as.factor(port))) + 
  theme_bw() + 
  geom_point(pch = 21) +
  facet_grid(recvSiteName ~ .)
ggplot(data = filter(df.vanish, motusTagID == 16823), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point() +
  facet_grid(recvSiteName ~ .)



#How to logout:
motusLogout()



---
  title: "razimuth: user guide"
author:
  date:
  output:
  rmarkdown::html_vignette:
  toc: true
toc_depth: 1
vignette: >
  %\VignetteIndexEntry{razimuth: user guide}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}
---
  
  <style>
  img {
    max-width: 100%;
    max-height: 100%;
  }
</style>
  
  ## Installation
  Prior to installation, ensure a compiler (`Rtools` for Windows, `gfortran` for macOS) has been associated with R. To install the package, execute the following via the command line

```
R CMD INSTALL razimuth_0.1.0.tar.gz
```
If a dependency is not available, install package and then rerun the above command.

## Example dataset
To begin, use the package's `sim_atd()` function to simulate an example azimuthal telemetry dataset.

```{r, echo = FALSE}
library(razimuth)
```
```{r}
set.seed(3141)
grouse <- sim_atd(n_loc = 100, sq_km = 4, n_azimuth = 3, dist_vec = c(200,300), 
                  kappa_sim = 50, prior_r = 1000, plot = FALSE)$sim_df
head(x = grouse, n = 3)
```

`grouse` represents a typical azimuthal telemetry dataset with the likely exception of the `prior_r` column. In this framework, the quantity `prior_r` is defined as the maximum distance (in meters) between the observer and the transmitter prior to taking the azimuth. This quantity will likely be derived from a combination of situational factors such as user experience level and/or landscape topography. In cases where this information was not obtained during data collection, the user must still specify an upper bound for each azimuth.  

The function `convert_atm()` is used to convert the data frame into a list object necessary for subsequent plotting and model fitting.

```{r}
grouse_atm <- convert_atm(df = grouse)
```

The function `visualize_atm()` can be used to explore the geometry of each individual relocation attempt.

```{r, out.width = '80%', fig.align = 'center', fig.height = 7, fig.width = 7}
visualize_atm(atm_df = grouse_atm, obs_id = 93, add_prior = T)
```

The `atm_mcmc()` function is used to fit the Azimuthal Telemetry Model (ATM) using Markov Chain Monte Carlo. 

```{r, hide = T}
atm_fit <- atm_mcmc(atm_df = grouse_atm, n_mcmc = 11000, n_burn = 1000)
```

The resulting object `atm_fit` contains the following model output:

* `mu_ls` - list of lists corresponding to each relocation attempt. Each list contains information about the relocation: identifier (`pid`), date (`date`), posterior mode (`pmode`), and a matrix containing the posterior draws (`pdraws`).
* `kappa_ls` - list containing information concerning the concentration parameter kappa: acceptance rate (`acceptance`), posterior draws (`pdraws`), and tuning parameter values from the adaptive tuning performed during burn in (`tuning`).
* `pmode_mat` - matrix containing the posterior modes for each relocation.

For plotting model output associated with a particular relocation, the helper function `which_pid()` is used to identify the list within `mu_ls` or row number of `pmode_mat` associated with the specified relocation identifier (`obs_id`). To plot posterior credible isopleths, use the `p_isopleth()` function.
```{r, out.width = '80%', fig.align = 'center', fig.height = 7, fig.width = 7}
id_tmp <- which_pid(atm_df = grouse_atm, obs_id = 20)
visualize_atm(atm_df = grouse_atm, obs_id = id_tmp, add_prior = TRUE)
p_isopleth(df = atm_fit$mu_ls[[id_tmp]]$pdraws, prob_lvls = c(0.5,0.9), range_extend = 0,
           kde_n = 50, col_vec = c(4,4))
points(matrix(atm_fit$pmode[id_tmp, 2:3], ncol = 2), pch = 21, bg = 4)
legend("topleft", c("Posterior Mode"), pch = 21, pt.bg = 4, bty = "n")
```

Plotting transmitter location posterior draws using a color scale such as `viridis` (from the `viridis` R package) helps to visualize 2-dimensional mixing. 

```{r, out.width = '80%', fig.align = 'center', fig.height = 7, fig.width = 7}
visualize_atm(atm_df = grouse_atm, obs_id = id_tmp, add_prior = T, add_legend = F)
points(atm_fit$mu_ls[[id_tmp]]$pdraws, pch = 20,
       col = viridis::viridis(n = nrow(atm_fit$mu_ls[[id_tmp]]$pdraws)))
```

To assess convergence of the concentration parameter kappa, the `plot_kappa()` provides the following diagnostics per the `item` argument: traceplot (`traceplot`), posterior kernel density estimate (`density`), tuning parameter update trace (`tuning`), and running mean plot (`run_mean`).

```{r, out.width = '80%', fig.align = 'center', fig.height = 7, fig.width = 10}
plot_kappa(atm_obj = atm_fit$kappa_ls, item = "traceplot")
plot_kappa(atm_obj = atm_fit$kappa_ls, item = "run_mean")
```











