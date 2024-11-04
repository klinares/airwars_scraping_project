
pacman::p_load(DBI, reticulate, tidytext, furrr, parallel, huggingfaceR, dbplyr, RSQLite, tidyverse)


# connect to database
mydb <- dbConnect(SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

airwars_assessment <- tbl(mydb, "airwars_assessment") |> 
  as_tibble() 


# pipein the model
SamLowe <- hf_load_pipeline(
  model_id = "SamLowe/roberta-base-go_emotions", 
  task = "text-classification", return_all_scores=TRUE, 
   truncation=TRUE)


# we want to get responses for these questions
airwars_assessment_new <-
  airwars_assessment |> 
  group_split(Incident_id) |> 
  map_dfr(function(x){
    
    print(x[1])
    print(x[2])
    # mod = SamLowe(x[[2]]) |> 
    #   bind_rows() |> 
    #   as_tibble() |> 
    #   mutate(Incident_id = x)
    }) 
  
