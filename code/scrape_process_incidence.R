#_________________________________________________________________________
# ____ Script for scraping casualty Incidents from airwars.org _____
# ________________________________________________________________________

source("~/repos/airwars_scraping_project/code/helpful_functions.R")

pacman::p_load(rvest, lubridate, RSQLite, DBI, text, geosphere,
               glue, jsonlite, tidyverse, data.table)


# ________________________ Build Metadata table  ________________________

# scrape metadata information to build a dataframe
airwars_website <- read_html("https://airwars.org/conflict/israel-and-gaza-2023/")

# store needed information, and process
airwars_meta <- scrape_metadata_fun(airwars_website)

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

# 1. save each web page in a list
incident_content <- map(airwars_meta_list, function(x){ 
  
  url_content = list(Incident_id = x[[2]],
                     read_html(x[[3]])
  )
})

# write webpages out to repo
folder_path <- "~/repos/airwars_scraping_project/database/webpages/"

map(incident_content, function(x){
  
  file_name = str_c(x[[1]], ".html")

 xml2::write_html(x[[2]], str_c(folder_path, file_name))
})





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


# process Gaza conflict data from https://acleddata.com/data-export-tool/
daily_conflict <- read_csv(
  "~/repos/airwars_scraping_project/raw_data/2023-10-02-2024-11-01-Palestine_acled.csv") |> 
  mutate(event_date = dmy(event_date)) |> 
  rename(Incident_Date = event_date, 
         lat = latitude,
         long=longitude) |> 
  arrange(Incident_Date) |> 
  # we want to use the OSM API to get boundary boxes of targeted places
  mutate(map2_dfr(lat, long, function(x, y){
    
    # hit the OSM API
    osm_coord = fromJSON(
      glue(
        "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={x}&lon={y}")
    )
    # skip coordinates that do not resolve
    if(osm_coord[[1]] == "Unable to geocode") {
      tibble(xmin=NA, xmax=NA, ymin=NA, ymax=NA)
    } else {
      # keep coordinates that resolve
      osm_coord$boundingbox |> 
        as_tibble() |> 
        data.table::transpose() |> 
        rename(xmin=1, xmax=2, ymin=3, ymax=4) 
    }
  })
  ) 


#____________________________ END _________________________________



# ___________________ # write results to database table  _______________________

# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

dbWriteTable(mydb, "airwars_meta", airwars_meta, overwrite=TRUE)
dbWriteTable(mydb, "airwars_assessment", airwars_assessment, overwrite=TRUE)
dbWriteTable(mydb, "airwars_coord", airwars_coord, overwrite=TRUE)
dbWriteTable(mydb, "airwars_incidents", airwars_incidents, overwrite=TRUE)
dbWriteTable(mydb, "daily_casualties", daily_casualties, overwrite=TRUE)
dbWriteTable(mydb, "daily_conflict", daily_conflict, overwrite=TRUE)


# print tables in database
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________
