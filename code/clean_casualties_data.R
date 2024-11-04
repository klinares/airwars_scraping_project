#_________________________________________________________________________
#     ____ Clean casualty fields from airwars_events dataset  _____
# ________________________________________________________________________


pacman::p_load(RSQLite, DBI, tidyverse)

# fields contain either ranges (2-5), or several counts(1 child, 3 women, 1 man)
## we need to separate each one of these into their own field, & overwrite the table

# connect to database
mydb <- dbConnect(RSQLite::SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

airwars_events <- tbl(mydb, "airwars_events") |> 
  as_tibble()

airwars_events <- airwars_events |> 
  # clean up casualty column
  mutate(civilians_reported_killed = 
           na_if(`Civilians reported killed`, "Unknown"),
         casualty_estimate = ifelse(str_detect(
           civilians_reported_killed, "–"), "range", "absolute") 
         ) |> 
  # separate estimates with ranges
  separate(civilians_reported_killed, c("min_killed", "max_killed")) |> 
  mutate(max_killed = ifelse(is.na(max_killed), min_killed, max_killed),
         # combine kill ranges
         min_killed = as.integer(min_killed), 
         max_killed = as.integer(max_killed)) |> 
  mutate(killed = (min_killed + max_killed) / 2,
         killed = as.integer(killed)) 

# we need to extract number killed by men, women, children
airwars_events <- airwars_events |> 
  mutate(children_killed = as.integer(
    str_extract(Civilian_type, "(\\d)+(?= child)")),
         women_killed = as.integer(
           str_extract(Civilian_type, "(\\d)+(?= wom)")),
         men_killed = as.integer(
           str_extract(Civilian_type, "(\\d)+(?= m)"))) |> 
  relocate(Civilian_type, .after=last_col())


# write the table back to the database
dbWriteTable(mydb, "airwars_events", airwars_events,  overwrite=TRUE)

