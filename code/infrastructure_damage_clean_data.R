#_________________________________________________________________________
# __________ Script for reading infrastructure damage from _______________
#____ https://data.techforpalestine.org/docs/infrastructure-damaged/ _____
# ________________________________________________________________________

library(jsonlite)
library(DBI)
library(tidyverse)

# read in nested json from url
infrastructure_damage_list <- fromJSON("https://data.techforpalestine.org/api/v3/infrastructure-damaged.json") |> 
  as_tibble()

# keep what is needed, damage or destroyed


building_types <- infrastructure_damage_list |>
  select(-1, -places_of_worship) |> 
  colnames() |> 
  as.vector()

building_types

# parse out destoryed buildings
infrastructure_damage <- map_dfr(building_types, function(x){
  infrastructure_damage_list |> 
    select(report_date, all_of(x)) |> 
    unnest_wider(x) |>
    select(report_date, ext_destroyed) |> 
    mutate(building_type=x)
})


# separate places of worship
places_of_worship <- infrastructure_damage_list |> 
  select(report_date, places_of_worship) |> 
  unnest_wider(places_of_worship) |> 
  pivot_longer(-report_date) |> 
  filter(str_detect(name, "ext_mosques_destroyed|ext_churches_destroyed")) |> 
  group_by(report_date) |> 
  reframe(ext_destroyed = sum(value)) |> 
  mutate(building_type = "place_of_worship")


# combine dataframes into one table

infrastructure_damage <- infrastructure_damage |> 
  full_join(places_of_worship)
  

# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

dbWriteTable(mydb, "infrastructure_damage", infrastructure_damage)

# we can now load the table into the database
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________
