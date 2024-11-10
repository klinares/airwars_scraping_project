#_________________________________________________________________________
# ____ Script for scraping Casualty Incidents from airwars.org _____
# ________________________________________________________________________

pacman::p_load(rvest, glue, lubridate, RSQLite, DBI, furrr, dbplyr, parallel, tidyverse)


# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")


# create date vector w/ most recent date in airwars 
# The webpage only presents 30 events at a time.
dates <- seq(as_date("2023-10-01"), as_date(today()), by="day") 


print(str_c("Scrapped data as of ", Sys.Date()))


plan(multisession, workers = detectCores())
### Loop through dates in the airwars archive page, parse Event days & IDs
airwars_new <- future_map_dfr(dates, function(x){
  
  url = read_html(glue(
    "https://airwars.org/civilian-casualties/?end_date={x}&start_date={x}&country=the-gaza-strip")) 
  
  description = html_nodes(url, xpath = 
               '//*[contains(concat( " ", @class, " " ), concat( " ", "meta-block", " " ))]//span | //h1//*[contains(concat( " ", @class, " " ), concat( " ", "incidentpreview__date", " " ))]//h4')
  
  tibble(
    Incident_Date = html_nodes(url, xpath = '//*[(@id = "posts")]//h1') |>   html_text2(),
    Incident_id = html_nodes(url, xpath = 
                                '//*[contains(concat( " ", @class, " " ), concat( " ", "meta-block", " " ))]//span') |> 
      html_text2() 
  )

}) |> 
# we need to reformat the variables and build out the url
  mutate(Incident_id = str_to_lower(Incident_id),
         Incident_Date = str_to_lower(Incident_Date),
         Incident_Date = str_remove(Incident_Date, ","),
         Incident_Date = str_replace_all(Incident_Date, " ", "-"),
         link = glue(
           "https://airwars.org/civilian-casualties/{Incident_id}-{Incident_Date}/")) 


# we can now append the new rows to the database
dbWriteTable(mydb, "airwars_meta", airwars_new, overwrite=TRUE)


dbDisconnect(mydb)
# ____________________________________ END _____________________________________
