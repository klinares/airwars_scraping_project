#_________________________________________________________________________
# ____ Script for scraping casualty Incidents from airwars.org _____
# ________________________________________________________________________

source("~/repos/airwars_scraping_project/code/helpful_functions.R")

pacman::p_load(rvest, lubridate, RSQLite, DBI, text,
               glue, jsonlite, tidyverse, data.table)


# ________________________ Build Metadata table  ________________________

# scrape metadata information to build a dataframe
airwars_website <- read_html("https://airwars.org/conflict/israel-and-gaza-2023/")

# store needed information, and process
airwars_meta <- tibble(
  # scrape date of incident
  Incident_Date = html_nodes(
    airwars_website, xpath = 
      '//*[contains(concat( " ", @class, " " ), concat( " ", "incidentpreview__date", " " ))]//h1') |>  
    html_text2(),
  # scrape airwars unique id of incident
  Incident_id = html_nodes(
    airwars_website, xpath = 
                             
      '//*[contains(concat( " ", @class, " " ), concat( " ", "meta-block", " " ))]//span') |> 
    html_text2() 
) |> 
  # we need to reformat the variables and build out the url
  mutate(Incident_id = str_to_lower(Incident_id),
         Incident_Date = str_to_lower(Incident_Date),
         Incident_Date = str_remove(Incident_Date, ","),
         Incident_Date = str_replace_all(Incident_Date, " ", "-"),
         link = glue(
           "https://airwars.org/civilian-casualties/{Incident_id}-{Incident_Date}/")) 


# convert each row, incident, into a list
airwars_meta_list <- airwars_meta |> 
  mutate(id= row_number()) |> 
  group_by(id) |> 
  group_split()



#____________________________ END _________________________________


# ________________________ Read in incident URLs ________________________

# save each web page in a list
incident_content <- map(airwars_meta_list, function(x){ 
  
  url_content = list(Incident_id = x[[2]],
                     read_html(x[[3]])
  )
})


# scrape needed content from the web papers & build tables

## scrape incident assessment
airwars_assessment <- map_dfr(incident_content, function(x){ 
  # parse out summary of event
  pull_assessment_fun(x) |> 
    as_tibble() |> 
    mutate(sentiment_score_fun(assessment))
}) 



# scrape geolocation  data
## find location type when coordinates are present
airwars_coord <- map_dfr(incident_content, function(x){ 
  # parse out summary of event
  pull_coords_fun(x)
}) 



airwars_incidents <- map_dfr(incident_content, function(x){ 
  # parse out summary of event
  pull_incident_fun(x)
}) |> 
  # we need to extract number killed by men, women, children
  mutate(children_killed = as.integer(
    str_extract(Civilian_type, "(\\d)+(?= child)")),
    women_killed = as.integer(
      str_extract(Civilian_type, "(\\d)+(?= wom)")),
    men_killed = as.integer(
      str_extract(Civilian_type, "(\\d)+(?= m)"))) |>
  relocate(Civilian_type, .after=last_col())

#____________________________ END _________________________________


# ___________________ # write results to database table  _______________________

# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

dbWriteTable(mydb, "airwars_meta", airwars_meta, overwrite=TRUE)
dbWriteTable(mydb, "airwars_assessment", airwars_assessment, overwrite=TRUE)
dbWriteTable(mydb, "airwars_coord", airwars_coord, overwrite=TRUE)
dbWriteTable(mydb, "airwars_incidents", airwars_incidents, overwrite=TRUE)

# print tables in database
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________
