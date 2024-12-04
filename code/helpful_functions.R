#_______________________________________________________________________________
# ___ Functions for scraping incidents from airwars & process into database ____
# ______________________________________________________________________________

options(scipen=999,
        future.globals.onReference = "error",
        future.globals.onReference = "warning")




# this code chunk scrapes the necessary metadata for each incident
## & creates a URL for scraping the actually page later. 
### If the structure of the website changes, modify this chunk!
scrape_metadata_fun <- function(website){
  
  tibble(
    # scrape date of incident
    Incident_Date = html_nodes(
      website, xpath = 
        '//*[contains(concat( " ", @class, " " ), concat( " ", "incidentpreview__date", " " ))]//h1') |>  
      html_text2(),
    # scrape airwars unique id of incident
    Incident_id = html_nodes(
      website, xpath = 
        
        '//*[contains(concat( " ", @class, " " ), concat( " ", "meta-block", " " ))]//span') |> 
      html_text2() 
  ) |> 
    # we need to reformat the variables and build out the URL
    mutate(Incident_id = str_to_lower(Incident_id),
           Incident_Date = str_to_lower(Incident_Date),
           Incident_Date = str_remove(Incident_Date, ","),
           Incident_Date = str_replace_all(Incident_Date, " ", "-"),
           link = glue(
             "https://airwars.org/civilian-casualties/{Incident_id}-{Incident_Date}/"))
}



# Download each web URL & save to a local folder, than push to github
read_html_write_folder_fun <- function(meta_list, save_path){
    
    print(str_c("Reading URL file", " . . . for ", meta_list[[2]], 
                "event occured on ", meta_list[[1]]) )
  
    write_html(read_html(meta_list[[3]]), # html content
               str_c(save_path, 
                     str_c(meta_list[[2]], # id number
                           ".html"))
    )
}
  



# Read in URL web pages from local folder to begin parsing
read_url_fun <- function(file_path_name){
  file_names = list.files(file_path_name)
  
  incident_content_test <- map(file_names, function(x){
    
    file_path = str_c(file_path_name, x)
    file_name = str_remove(x, ".html")
    
    dat_list = list(file_name, read_html(file_path)
    ) 
    return(dat_list)
  })
}



# 1st parse pass,
# Parse assessment description text
pull_assessment_fun <- function(web_content){
  
  tibble(
    # attach incident id number
    Incident_id = web_content[[1]],
    assessment = html_nodes(
      web_content[[2]], 
      xpath = 
        '//*[contains(concat( " ", @class, " " ), concat( " ", "summary", " " ))]//p') |> 
      html_text2() |> toString()
  ) 
}



# Use an LLM to calculate sentiment scores
sentiment_score_fun <- function(incident_assessment){

  # shorten descriptions to confirm to the model limitation
  description = str_trunc(incident_assessment, 1600) 
  
  # use huggingface model to detect emotional tone of summary
  mod = 
    textClassify(description,
                 model = "j-hartmann/emotion-english-distilroberta-base", 
                 return_all_scores = TRUE, 
                 return_incorrect_results = TRUE,
                 function_to_apply = "softmax",
                 tokenizer_parallelism = TRUE,
                 device = "gpu") |> # change to "cpu" if a gpu is not configured 
    pivot_wider(names_from = label_x, values_from=score_x)
  
}


# 2nd parse pass
# reverse coordinates from nominatim 
pull_coords_fun <- function(web_content){

 # read coordinates
  geo_coords = html_nodes(
    web_content[[2]], 
    xpath = 
      '//*[contains(concat( " ", @class, " " ), concat( " ", "lat-lng", " " ))]')

  # if coordinates are available, parse them out, query to nominatim, 
  # return location type and boundary box
  if(length(geo_coords) > 0){
    dat = geo_coords |> 
      html_text2() |>
      as_tibble() |> 
      separate(value, into=c("incident_lat", "incident_long"), sep=", ") |> 
      slice(1)
    
    # hit the nominatim api
    nominatim_request =
      fromJSON(glue(
          "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={dat$incident_lat}&lon={dat$incident_long}")
      )
    
    dat = dat |> 
      add_column(
      target_type = nominatim_request$type,
      target_address_type  = nominatim_request$addresstype,
      nominatim_request$boundingbox |> 
        as_tibble() |> 
        data.table::transpose() |> 
        rename(lat_min=1, lat_max=2, long_min=3, long_max=4)
    )

  # if there are no coordinates, create the new variables w/ NA
  } else{
    dat = tibble(incident_lat=NA, incident_long=NA,
                 target_type = NA,  target_address_type = NA,  
                 lat_min=NA, lat_max=NA, long_min=NA, long_max=NA)
  }
  
  # convert bbox diagonal distance
  dat = dat |> 
    # use geosphere to calculate distance in meters
    mutate(lat_min = as.numeric(lat_min),
           lat_max = as.numeric(lat_max),
           long_min = as.numeric(long_min),
           long_max = as.numeric(long_max))
  
  return(dat)
}






# parse incident data from Airwars web page
pull_incident_fun <- function(web_content){
  
  # read in summary fields
  html_meta = web_content[[2]] |> html_nodes(".meta-list") 
  
  dat = html_meta[[1]] |> html_children() |> html_text2() |> 
    append(
      html_meta[[2]] |> html_children() |> html_text2()
    ) |> 
    as_tibble() |> 
    separate(value, into=c('Incident_field', 'Incident_result', 'Incident_notes'), sep="\n") |> 
    # move row 6 from field to result
    mutate(var_with_casualty_type = ifelse(is.na(Incident_result), TRUE, FALSE),
           Incident_result = ifelse(var_with_casualty_type == TRUE, Incident_field, Incident_result),
           Incident_field = ifelse(var_with_casualty_type == TRUE, "Civilian_type", Incident_field),) |> 
    select(-Incident_notes, -var_with_casualty_type)|> 
    pivot_wider(names_from = Incident_field, values_from = Incident_result) |> 
    mutate(across(where(is.list), ~ as.character(.x)),
           civilians_reported_killed = 
             na_if(`Civilians reported killed`, "Unknown"),
           casualty_estimate = ifelse(str_detect(
             civilians_reported_killed, "–"), "range", "absolute") 
           ) |> 
    separate(civilians_reported_killed, c("min_killed", "max_killed")) |> 
    mutate(max_killed = ifelse(is.na(max_killed), min_killed, max_killed),
           # combine kill ranges
           min_killed = as.integer(min_killed), 
           max_killed = as.integer(max_killed)) |> 
    mutate(killed = (min_killed + max_killed) / 2,
            killed = as.integer(killed)) |> 
    # # add location metadata
    mutate(location_meta =
             html_nodes(web_content[[2]],
                        xpath=
                          '//*[contains(concat( " ", @class, " " ), concat( " ", "location", " " ))]') |>
             html_text2() |>
             # remove Arabic characters, keep English translation
             str_replace_all( "[^a-zA-Z0-9]|Location", " ") |>
             str_trim()
            )
  
  return(dat)
}




#____________________________ END _________________________________
