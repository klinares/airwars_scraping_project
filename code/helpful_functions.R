#_______________________________________________________________________________
# ___ Functions for scraping incidents from airwars & process into database ____
# ______________________________________________________________________________

pull_assessment_fun <- function(web_content){
  
  
  tibble(
    # attach incident id number
    Incident_id = web_content[[1]],
    assessment = html_nodes(web_content[[2]], xpath = 
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




pull_coords_fun <- function(web_content){
  
  
  
  if(web_content[[2]] |> html_nodes(".geolocation-notes") |> length() == 0 ){
    tibble(lat=NA, long=NA, type_location = NA)
    
  } else{
    web_content[[2]] |> html_nodes(".geolocation-notes") |> html_text2() |>
      str_extract_all("\\d+\\.\\d+") |>
      unlist() |>
      bind_cols() |>
      data.table::transpose() |>
      rename(lat=1, long=2) |> 
      # locate the type of location using coordinates
      mutate(lat = as.numeric(lat),
             long = as.numeric(long),
             type_location = map2(lat, long, function(x, y){
               
               # hit the OSM API
               fromJSON(
                 glue(
                   "https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat={x}&lon={y}")
               )$type
             }),
             type_location = as.character(type_location))
  } |> 
    mutate(across(where(is.list), ~ as.character(.x)),
           # attach incident id number
           Incident_id = web_content[[1]]) |> 
    select(Incident_id, lat, long, type_location) |> 
    drop_na()
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
           # attach incident id number
           Incident_id = web_content[[1]]) |> 
    relocate(Incident_id) |> 
    mutate(civilians_reported_killed = 
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
           killed = as.integer(killed)) 
  
  return(dat)
}


#____________________________ END _________________________________