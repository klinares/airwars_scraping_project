
pacman::p_load(DBI, tidytext, dbplyr, RSQLite, tidyverse)


# connect to database
mydb <- dbConnect(SQLite(), 
                  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")

# we can now load the table into the database
dbListTables(mydb)


airwars_meta <- tbl(mydb, "airwars_meta") |> 
  as_tibble() 


airwars_text_emotion <- tbl(mydb, "airwars_text_emotion") |> 
  as_tibble() |> 
  left_join(airwars_meta |> select(Incident_Date, Incident_id)) |> 
  mutate(Incident_Date = lubridate::mdy(Incident_Date) ) |> 
  # average by day
  group_by(Incident_Date, label_x) |> 
  reframe(score_x = mean(score_x))

# types of emotions
airwars_text_emotion |> distinct(label_x) 

airwars_text_emotion |> 
  ggplot(aes(x=Incident_Date, y=score_x)) +
  geom_area(fill = "gray", alpha = 0.9) +
  geom_smooth(se = FALSE) +
  facet_wrap(~label_x)
