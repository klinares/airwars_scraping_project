#_________________________________________________________________________
# ____ Script for scraping Casualty Incidents from airwars.org _____
# ________________________________________________________________________

pacman::p_load(rvest, glue, lubridate, furrr, parallel, tidyverse)


### We will need a list of dates from the beginning of the war.
# The webpage only presents 30 events at a time.
# The start of the war was on October 7 2023
dates <- seq(ymd("2023-10-07"), as_date(today()), by="day") 

print(str_c("Scrapped data as of ", Sys.Date()))


plan(multisession, workers = detectCores())
### Loop through dates in the airwars archive page, parse Event days & IDs
airwars <- future_map_dfr(dates, function(x){
  
  link = glue("https://airwars.org/civilian-casualties/?end_date={x}&start_date={x}&country=the-gaza-strip")
  
  read_html(glue("https://airwars.org/civilian-casualties/?end_date={x}&start_date={x}&country=the-gaza-strip")) |> # pass the link
    html_elements(".incidentpreview__header") |> # keep what is needed
    html_text2() |> # parse information 
    as_tibble()

})

# Finally, we 
airwars <- airwars |> 
  # parse what is needed for 2nd step
  mutate(value = str_remove(value, "Incident date\n"), # strips un-needed text
         value = str_replace(value, "\nIncident Code\n", "_")) |> # uses a marker to separate in the next line
  separate(value, into=c('Incident_Date', 'Incident_id'), sep="_") |> # splits string into 2 new columns
  
  # we need to reformat the variables
  mutate(Incident_id = str_to_lower(Incident_id),
         Incident_Date = str_to_lower(Incident_Date),
         Incident_Date = str_remove(Incident_Date, ","),
         Incident_Date = str_replace_all(Incident_Date, " ", "-"),
         id = row_number()
         )


# there are some events where there incidence date does not match their URL & we have to go correct
airwars <- airwars |> 
  mutate(Incident_Date = ifelse(Incident_id == "ispt0394", "october-20-2023", Incident_Date),
         # after this correction we want to create the URLs
         link = glue("https://airwars.org/civilian-casualties/{Incident_id}-{Incident_Date}/")
  )
  # after this correction we want to create the URLs

# save out airwars metadata
write_csv(airwars, "~/repos/airwars_scraping_project/data/airwars_meta.csv")

# ____________________________________ END _____________________________________
