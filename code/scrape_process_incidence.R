#_________________________________________________________________________
# ____ Script for scraping casualty Incidents from airwars.org _____
# ________________________________________________________________________

source("~/repos/airwars_scraping_project/code/helpful_functions.R")

pacman::p_load(rvest, lubridate, RSQLite, DBI, text, geosphere, xml2,
               furrr, parallel,
               tictoc, glue, jsonlite, tidyverse, data.table)


# ________________________ Build Metadata table  ________________________

# scrape metadata information to build a dataframe
airwars_website <- read_html("https://airwars.org/conflict/israel-and-gaza-2023/")

# store needed information, and process
airwars_meta <- scrape_metadata_fun(airwars_website) |> 
  arrange()

# convert each row, incident, into a list
airwars_meta_list <- airwars_meta |> 
  mutate(id= row_number()) |> 
  group_by(id) |> 
  group_split()

# change date string to date variable
airwars_meta <- airwars_meta |> 
  mutate(Incident_Date = mdy(Incident_Date))


#____________________________ END _________________________________


# ________________________ Read in incident URLs ________________________


folder_path <- "~/repos/airwars_scraping_project/database/webpages/"

# 1. download URL webpages
## 1st, if URL is not already saved, download and save to github folder
map(airwars_meta_list, function(x){
  # read web pages, write each to github
  read_html_write_folder_fun(x, folder_path)
})

## 2nd, read into a list available URL pages saved in the github folder
incident_content <- read_save_url_fun(folder_path)





# 2. scrape needed content from the web papers & build tables

## scrape incident assessment
airwars_incidents <- map_dfr(incident_content, function(x){ 
  # parse out summary of event
  pull_assessment_fun(x) |> 
    as_tibble() |> 
    mutate(sentiment_score_fun(assessment),
           attack_location = attack_location_fun(assessment),
           pull_coords_fun(x),
           find_location_coord_fun(attack_location),
           pull_incident_fun(x) 
    )
}) |> 
  # we need to extract number killed by men, women, children
  mutate(children_killed = as.integer(
    str_extract(Civilian_type, "(\\d)+(?= child)")),
    women_killed = as.integer(
      str_extract(Civilian_type, "(\\d)+(?= wom)")),
    men_killed = as.integer(
      str_extract(Civilian_type, "(\\d)+(?= m)"))) |>
  relocate(Civilian_type, .after=last_col())




# 3. process casualty daily deaths from https://data.techforpalestine.org/docs/casualties-daily/
daily_casualties <- read_csv(
  "~/repos/airwars_scraping_project/raw_data/casualties_daily_gaza.csv") |> 
  select(
    report_date, ext_killed, ext_killed_cum, 
    ext_killed_children_cum, ext_killed_women_cum) |> 
  # we need to create a lag count for children and women
  mutate(ext_killed_children = 
           ext_killed_children_cum - lag(ext_killed_children_cum),
         ext_killed_women = 
           ext_killed_women_cum - lag(ext_killed_women_cum)) |> 
  # convert first rows to 0.
  mutate_at(c(6:7), ~replace_na(., 0)) |> 
  rename(Incident_Date = report_date)



#____________________________ END _________________________________



# ___________________ # write results to database table  _______________________

# connect to database
mydb <- dbConnect(SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

dbWriteTable(mydb, "airwars_meta", airwars_meta, overwrite=TRUE)
dbWriteTable(mydb, "airwars_incidents", airwars_incidents, overwrite=TRUE)
dbWriteTable(mydb, "daily_casualties", daily_casualties, overwrite=TRUE)


# print tables in database
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________
