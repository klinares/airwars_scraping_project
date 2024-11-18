#_______________________________________________________________________________
# ___ Functions for scraping incidents from airwars & process into database ____
# ______________________________________________________________________________

options(scipen=999)


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
    # we need to reformat the variables and build out the url
    mutate(Incident_id = str_to_lower(Incident_id),
           Incident_Date = str_to_lower(Incident_Date),
           Incident_Date = str_remove(Incident_Date, ","),
           Incident_Date = str_replace_all(Incident_Date, " ", "-"),
           link = glue(
             "https://airwars.org/civilian-casualties/{Incident_id}-{Incident_Date}/")) 
  
}






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




sentiment_score_fun <- function(incident_assessment){
  
  description = str_trunc(incident_assessment, 1600) 
  
  # use huggingface model to detect emotional tone of summary
  mod = 
    textClassify(description,
                 model = "j-hartmann/emotion-english-distilroberta-base", 
                 return_all_scores = TRUE, 
                 return_incorrect_results = TRUE,
                 function_to_apply = "softmax",
                 tokenizer_parallelism = TRUE,
                 device = "gpu") |> 
    pivot_wider(names_from = label_x, values_from=score_x)
  
}



attack_location_fun <- function(incident_assessment){
  
  description = incident_assessment
  
  # use huggingface model to detect emotional tone of summary
  mod = 
    textQA(question = "Where was the location of the attack?",
           context = description,
                 model = "deepset/roberta-base-squad2", 
                 device = "gpu")$answer
}




# reverse coordinates
pull_coords_fun <- function(web_content){
  
  geo_coords = html_nodes(
    web_content[[2]], 
    xpath = 
      '//*[contains(concat( " ", @class, " " ), concat( " ", "lat-lng", " " ))]')
  
  if(length(geo_coords) > 0){
    dat = geo_coords |> 
      html_text2() |>
      as_tibble() |> 
      separate(value, into=c("incident_lat", "incident_long"), sep=", ") |> 
      slice(1)

  } else{
    dat = tibble(incident_lat=NA, incident_long=NA)
  }
  
  return(dat)
}




# find location from text search 
find_location_coord_fun <- function(location_found){
  
  # clean the string before creating URL
  location_attack = str_replace_all(location_found, "’", "%60")
  location_attack = str_remove_all(location_found, "\\(|Education|Kinda|\\)")
  location_attack = str_replace_all(location_found, " ", "+")
  
  
  nominatim_request = tryCatch(
    fromJSON(
      glue(
        "https://nominatim.openstreetmap.org/search?addressdetails=1&q={location_attack}&format=jsonv2&limit=1")
      ), 
    error = function(e) {return(NA)} )
  
  if(length(nominatim_request) < 2){
    
    dat = tibble(lat = NA, long = NA, 
                 lat_min=NA, lat_max=NA, long_min=NA, long_max=NA)
  } else{
      
      dat = tibble(
        lat = nominatim_request$lat[[1]],
        long = nominatim_request$lon[[1]],
      ) |> 
        add_column(
          nominatim_request$boundingbox[[1]] |> 
            as_tibble() |> 
            data.table::transpose() |> 
            rename(lat_min=1, lat_max=2, long_min=3, long_max=4)
        )
  } 
  
  # convert bbox diagonal distance
  dat = dat |> 
    # use geosphere to calculate distance in meters
    mutate(lat_min = as.numeric(lat_min),
           lat_max = as.numeric(lat_max),
           long_min = as.numeric(long_min),
           long_max = as.numeric(long_max),
           bbox_dist = as.vector(distm(
             c(long_min, lat_min), c(long_max, lat_max), 
             fun = distHaversine)) )
  
  return(dat)
}






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