
pacman::p_load(DBI, reticulate, tidytext, huggingfaceR, dbplyr, RSQLite, tidyverse)


# connect to database
mydb <- dbConnect(SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)

airwars_assessment <- tbl(mydb, "airwars_assessment") |> 
  as_tibble() 


# pipein the model
roberta <- hf_load_pipeline(
  model_id = "j-hartmann/emotion-english-roberta-large", 
  task = "text-classification", return_all_scores=TRUE, 
   truncation=TRUE)


# we want to get responses for these questions
airwars_assessment_new <-
  airwars_assessment |> 
  group_split(Incident_id) |> 
  map_dfr(function(x){
    mod = roberta(x[[2]]) |>
      bind_rows() |>
      as_tibble() |>
      mutate(Incident_id = x[[1]])
    }) 
  

# save table to database
# create table with new events
dbWriteTable(mydb, "airwars_text_emotion", airwars_assessment_new, overwrite=TRUE)

# check if the table saved
dbListTables(mydb)


dbDisconnect(mydb)
