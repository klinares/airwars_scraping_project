#_________________________________________________________________________
# ____ Script for scraping Cassualty Incidents from airwars.org _____
# ________________________________________________________________________

pacman::p_load(rvest, lubridate, RSQLite, DBI, dbplyr, 
               glue, jsonlite, tidyverse, data.table)


# connect to database
mydb <- dbConnect(RSQLite::SQLite(), "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

airwars_meta <- tbl(mydb, "airwars_new") |> 
  as_tibble() |> 
  mutate(id= row_number()) |> 
  group_by(id) |> 
  group_split()
  
# ________________________________________________________________________
# scrap incident assessment

airwars_assessment <- map_dfr(airwars_meta, function(x){ 
  
  html = read_html(x[[3]])
  
  tibble(
    Incident_id = x[[2]],
    # parse out summary of event
    assessment = html_nodes(html, xpath = 
                                      '//*[contains(concat( " ", @class, " " ), concat( " ", "summary", " " ))]//p') |> 
      html_text2() |> toString()
  )
  })
  
# write results to database table
dbWriteTable(mydb, "airwars_assessment", airwars_assessment, 
             append=TRUE, overwrite=FALSE)

# __________________________________________________________________________


# scrap geolocation  data
## find location type when coordinates are present

airwars_coord <- map_dfr(airwars_meta, function(x){ 
  
  html = read_html(x[[3]])
  
  tibble(
    Incident_id = x[[2]],
    if(html |> html_nodes(".geolocation-notes") |> length() == 0 ){
          tibble(lat=NA, long=NA, type_location = NA)

        } else{
          html |> html_nodes(".geolocation-notes") |> html_text2() |>
            str_extract_all("\\d+\\.\\d+") |>
            unlist() |>
            bind_cols() |>
            data.table::transpose() |>
            rename(lat=1, long=2) |> 
            # locate the type of location usign coordinates
            mutate(lat = as.numeric(lat),
                   long = as.numeric(long),
                   type_location = map2(lat, long, function(x, y){
                     
                     fromJSON(
                       glue(
                         "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={x}&lon={y}")
                     )$type
                   }),
                   type_location = as.character(type_location))
        } |> 
      mutate(across(where(is.list), ~ as.character(.x)))
      ) |> 
    select(Incident_id, lat, long, type_location)
  })

# write results to database table
dbWriteTable(mydb, "airwars_coord", airwars_coord, 
             append=TRUE, overwrite=FALSE)

# ______________________________________________________________________


# scrap incident data

airwars_events <- map(airwars_meta, function(x){
    
    html = read_html(x[[3]])
    
    # read in summary fields
    html_meta = html |> html_nodes(".meta-list") 
    
    
    summary_dat = 
      
      # meta data
      tibble(Incident_Date = x[[1]],
             Incident_id = x[[2]]) |> 
      
      # add new columns
      add_column(
        
        html_meta[[1]] |> html_children() |> html_text2() |> 
          append(
            html_meta[[2]] |> html_children() |> html_text2()
          ) |> 
          as_tibble() |> 
          separate(value, into=c('Incident_field', 'Incident_result', 'Incident_notes'), sep="\n") |> 
          # move row 6 from field to result
          mutate(var_with_casualty_type = ifelse(is.na(Incident_result), TRUE, FALSE),
                 Incident_result = ifelse(var_with_casualty_type == TRUE, Incident_field, Incident_result),
                 Incident_field = ifelse(var_with_casualty_type == TRUE, "Civilian_type", Incident_field)) |> 
          select(-Incident_notes, -var_with_casualty_type)|> 
          pivot_wider(names_from = Incident_field, values_from = Incident_result) 
      ) |> 
      mutate(across(where(is.list), ~ as.character(.x)))
    
    return(summary_dat)
    
  }) |> 
  bind_rows() 


# write results to database table
dbWriteTable(mydb, "airwars_events", airwars_events, 
             append=TRUE,  overwrite=FALSE)

# remvove airwars_new
dbRemoveTable(mydb, "airwars_new")
dbListTables(mydb)

dbDisconnect(mydb)
# _____________________________ END __________________________
