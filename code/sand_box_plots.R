
pacman::p_load(lubridate, RSQLite, knitr, DBI, ggthemes, zoo, hrbrthemes,
              janitor,  jsonlite, viridis, tidyverse)



# connect to database
mydb <- dbConnect(
  SQLite(), 
  "~/repos/airwars_scraping_project/database/airwars_db.sqlite")


# read in data tables

airwars_meta <- tbl(mydb, "airwars_meta") |> 
  as_tibble() |> 
  # convert Incident_Date to date format
  mutate(Incident_Date = as_date(Incident_Date)) |> 
  arrange(Incident_Date)


airwars_incidents <- tbl(mydb, "airwars_incidents") |> 
  as_tibble() |> 
  clean_names() |> 
  # convert NA to 0
  mutate(killed = ifelse(is.na(killed), 0, killed)) |> 
  rename(Incident_id = incident_id) |> 
  # add dates
  left_join(airwars_meta |> select(Incident_id, Incident_Date))

  
daily_casualties <- tbl(mydb, "daily_casualties") |> 
  as_tibble()
# _________________________ PLOTS ________________________________





airwars_incidents |> 
  select(Incident_Date, impact, men_killed, women_killed, children_killed) |> 
  pivot_longer(-c("Incident_Date", "impact")) |> 
  mutate(value = replace_na(value, 0)) |> 
  filter(!is.na(impact)) |> 
  ggplot(aes(x=Incident_Date, y=value, color=name)) +
  geom_point() + 
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()

  







# plot kills
killed_by_day <- airwars_incidents |> 
  group_by(Incident_Date) |> 
  reframe(killed = sum(killed)) 
  
killed_by_day |> 
  ggplot(aes(x=Incident_Date, y=killed)) +
  geom_col(alpha = 1/10) + 
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()
  

# plot cumulative casualty incidents by gender and adult/children
airwars_incidents |> 
  select(Incident_Date, men_killed, women_killed, children_killed) |> 
  pivot_longer(-Incident_Date) |> 
  mutate(Casualty = ifelse(is.na(value), 0, value),
         casualty_type = case_when(
           name == "men_killed" ~ "Men",
           name == "women_killed" ~ "Women",
           name == "children_killed" ~ "Children"),
         name = factor(casualty_type, 
                       levels=c("Children", 
                                "Women", 
                                "Men" )) ) |> 
  group_by(Incident_Date, casualty_type) |> 
  reframe(Casualty = sum(Casualty)) |> 
  group_by(casualty_type) |> 
  mutate(Casualty_cum = cumsum(Casualty)) |> 
  ggplot(aes(x=Incident_Date, y=Casualty_cum, fill=casualty_type)) +
  geom_area(position = "stack") +
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()
  
  
  
# plot by proportion
airwars_incidents |> 
  select(Incident_Date, men_killed, women_killed, children_killed) |> 
  pivot_longer(-Incident_Date) |> 
  mutate(Casualty = ifelse(is.na(value), 0, value),
         casualty_type = case_when(
           name == "men_killed" ~ "Men",
           name == "women_killed" ~ "Women",
           name == "children_killed" ~ "Children"),
         name = factor(casualty_type, 
                       levels=c("Children", 
                                "Women", 
                                "Men" )) ) |> 
  group_by(Incident_Date, casualty_type) |> 
  reframe(Casualty = sum(Casualty)) |> 
  group_by(Incident_Date) |> 
  mutate(prop = Casualty/sum(Casualty)) |> 
  ungroup() |> 
  ggplot(aes(x=Incident_Date, y=prop, fill=casualty_type)) +
  geom_area(position = "stack") +
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()

  
  





daily_casualties |> 
  mutate(ext_killed_men_cum = ext_killed_cum - 
           (ext_killed_children_cum + ext_killed_women_cum),
         Incident_Date = lubridate::as_date(Incident_Date)) |> 
  select(Incident_Date, ext_killed_men_cum, 
         ext_killed_women_cum, ext_killed_children_cum) |> 
  pivot_longer(-Incident_Date) |> 
  mutate(cumulative_casualties = case_when(
    name == "ext_killed_men_cum" ~ "Men",
    name == "ext_killed_women_cum" ~ "Women",
    name == "ext_killed_children_cum" ~ "Children"
  ),
  name = factor(cumulative_casualties, 
                levels=c("Children", 
                         "Women", 
                         "Men" ))) |> 
  ggplot(aes(x=Incident_Date, y=value, fill=cumulative_casualties)) +
  geom_area(position = "stack") +
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()


# by proportion
daily_casualties |> 
  mutate(ext_killed_men_cum = ext_killed_cum - 
           (ext_killed_children_cum + ext_killed_women_cum),
         Incident_Date = lubridate::as_date(Incident_Date)) |> 
  select(Incident_Date, ext_killed_men_cum, 
         ext_killed_women_cum, ext_killed_children_cum) |> 
  pivot_longer(-Incident_Date) |> 
  mutate(cumulative_casualties = case_when(
    name == "ext_killed_men_cum" ~ "Men",
    name == "ext_killed_women_cum" ~ "Women",
    name == "ext_killed_children_cum" ~ "Children"
  ),
  name = factor(cumulative_casualties, 
                levels=c("Children", 
                         "Women", 
                         "Men" ))) |> 
  group_by(Incident_Date) |> 
  mutate(prop = value/sum(value)) |> 
  ggplot(aes(x=Incident_Date, y=prop, fill=cumulative_casualties)) +
  geom_area(position = "stack") +
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="Cumulative Casualties")) +
  scale_fill_viridis_d(option="E" ,alpha=.75, begin=.20) +
  theme_hc()









# how well did huggingface do in finding locations
airwars_incidents |> 
  mutate(incident_lat = as.numeric(incident_lat),
         incident_long = as.numeric(incident_long),
         lat_min = as.numeric(lat_min),
         lat_max = as.numeric(lat_max),
         long_min = as.numeric(long_min),
         long_max = as.numeric(long_max),
         inside_bbox = ifelse(
           between(incident_lat, lat_min, lat_max) &
           between(incident_long, long_min, long_max), 
           "inside", "outside" )) |> 
  count(inside_bbox)








# __________________ testing

#brm_data <- 
  killed_by_day |> 
  ggplot() +
  geom_point(aes(x=Incident_Date, y=killed))
  

