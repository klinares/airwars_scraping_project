#_________________________________________________________________________
#         ________ Clean daily casualty dataset from __________
#            _____https://data.techforpalestine.org/ _____
# ________________________________________________________________________


pacman::p_load(RSQLite, DBI, tidyverse)

casualties <- read_csv(
  "~/repos/airwars_scraping_project/raw_data/casualties_daily_gaza.csv") |> 
  select(
    report_date, ext_killed, ext_killed_cum, 
    ext_killed_children_cum, ext_killed_women_cum) |> 
  # we need to create a lag count for children and women
  mutate(ext_killed_children = 
           ext_killed_children_cum - lag(ext_killed_children_cum),
         ext_killed_women = 
           ext_killed_women_cum - lag(ext_killed_women_cum))
           
# write to database

# connect to database
mydb <- dbConnect(RSQLite::SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

dbWriteTable(mydb, "daily_casualties", casualties)

# we can now load the table into the database
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________


