#_________________________________________________________________________
# ____ Script for scraping Cassualty Incidents from airwars.org _____
# ________________________________________________________________________

pacman::p_load(rvest, lubridate, arrow, furrr, writexl, parallel, tidyverse, data.table)

# read in metadata need for creating queries
airwars <- read.csv("~/repos/airwars_scraping_project/data/airwars_meta.csv") |> 
  arrange(id) |> 
  group_by(id) |> 
  group_split()
  

plan(multisession, workers = detectCores())

airwars_events <- future_map(airwars, function(x){
    
    
    message(str_c("Scarping incidence number ", x[[3]], 
                  " \nevent ID ", x[[2]], 
                  " that occured on ", x[[1]], 
                  "\nand url link is . . . ", x[[4]]))
    
    html = read_html(x[[4]])
    
    # keep what is needed
    
    # parse out summary of event
    Airwars_assessment = html |> html_elements(".summary") |> html_text2() |> toString()
    
    # read in summary fields
    html_meta = html |> html_nodes(".meta-list") 
    
    
    summary_dat = 
      
      #meta data
      tibble(Airwars_assessment = str_remove_all(Airwars_assessment, "\n"),
             Incidence_date = x[[1]],
             Incidence_id = x[[2]],
             Incidence_url = x[[4]]) |> 
      
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
      
      # add geodata
      add_column(
        
        if(html |> html_nodes(".geolocation-notes") |> length() == 0 ){
          tibble(lat=NA, lon=NA)
          
        } else{
          html |> html_nodes(".geolocation-notes") |> html_text2() |> 
            str_extract_all("\\d+\\.\\d+") |>
            unlist() |> 
            bind_cols() |> 
            data.table::transpose() |> 
            rename(lat=1, long=2)
        }
      ) |> 
      # convert list variables to character
      mutate(across(where(is.list), ~ as.character(.x)))
    
    return(summary_dat)
    
  }) |> 
  bind_rows() |> 
  # move text column to last
  relocate(Airwars_assessment, .after=last_col())

# save out data
write_parquet(airwars_events, "~/repos/airwars_scraping_project/data/airwars_incidence.parquet")
write_csv(airwars_events, "~/repos/airwars_scraping_project/data/airwars_incidence.csv")
# _____________________________ END __________________________
