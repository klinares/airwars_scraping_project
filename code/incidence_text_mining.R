
pacman::p_load(DBI, text, RSQLite, furrr, parallel, tidyverse)
# first time installing folllow directions from https://r-text.org/
#textrpp_initialize()

# connect to database
mydb <- dbConnect(SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

# read in data
airwars_assessment <- tbl(mydb, "airwars_assessment") |> 
  as_tibble() |> 
  # descriptions are long, need to truncate 
  mutate(assessment = str_trunc(assessment, 1600)) 

plan(multisession, workers = detectCores())

airwars_assessment_new <- airwars_assessment |> 
  group_split(Incident_id) |> 
  future_map_dfr(function(x){
    
    # use huggingface model to detect emotional tone of summary
    mod = 
      textClassify(x[[2]],
                   model = "j-hartmann/emotion-english-distilroberta-base", 
                   return_all_scores = TRUE, 
                   return_incorrect_results = TRUE,
                   function_to_apply = "softmax",
                   tokenizer_parallelism = TRUE,
                   device = "gpu") |> 
      mutate(Incident_id = x[[1]])
        }) 
  
# save table to database
# create table with new events
dbWriteTable(mydb, "airwars_text_emotion", airwars_assessment_new, overwrite=TRUE)

# check if the table saved
dbListTables(mydb)


dbDisconnect(mydb)
