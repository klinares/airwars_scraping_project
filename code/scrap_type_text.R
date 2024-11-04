#_________________________________________________________________________
# ____ Script for scraping Cassualty Incidents from airwars.org _____
# ________________________________________________________________________

pacman::p_load(lubridate, RSQLite, DBI, furrr, dbplyr, ggmap,
               parallel, tidyverse, data.table)



# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

airwars_assessment <- tbl(mydb, "airwars_assessment") |> 
  as_tibble()

# enrich coordinates with type of location


airwars_coord_new <- 
  airwars_coord |> 
  drop_na() |> 
   mutate(
     type_location = map2(lat, long, function(x, y){
       
        fromJSON(
        glue(
          "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={x}&lon={y}")
      )$type
  }),
  type_location = as.character(type_location)
  ) 
  




register_google(key = "AIzaSyDCAezNPnkewEmZoy9u1Xwzo-nxMntG3lU", write = TRUE)

get_googlemap(center = "Gaza Strip", maptype = "hybrid", zoom=11) |> 
  ggmap() + 
  stat_density2d(data = airwars_coord, aes(x = long, y = lat, 
                                           fill = ..level.., alpha = ..level..), 
                 size = 1, bins = 20, geom = 'polygon') +
  scale_alpha(range = c(.02, .2), guide = FALSE) +
  scale_fill_gradient(low = "black", high = "red")


