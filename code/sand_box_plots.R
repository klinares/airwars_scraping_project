
pacman::p_load(lubridate, RSQLite, knitr, DBI, ggthemes, zoo, hrbrthemes, 
               geosphere, ggdark, janitor,  jsonlite, viridis, tidyverse)

options(scipen=999)

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
  as_tibble() |> 
  mutate(Incident_Date = lubridate::as_date(Incident_Date)) |> 
  mutate(data_source = "MoH") 



# combine daily casualties with airwars
incidents_cum <- airwars_incidents |> 
  select(Incident_Date, killed, children_killed, women_killed) |> 
  mutate(across(where(is.integer),~ replace_na(.x, 0))) |> 
  group_by(Incident_Date) |> 
  reframe(killed = sum(killed), children_killed = sum(children_killed), 
                       women_killed = sum(women_killed)) |> 
  mutate(Total = cumsum(killed),
         Children = cumsum(children_killed),
         Women = cumsum(women_killed)) |> 
  select(Incident_Date, Total, Children, Women) |> 
  pivot_longer(-c("Incident_Date")) |> 
  mutate(name = fct_reorder(name, value, .fun = max),
         data_source = "Airwars") |> 
  full_join(daily_casualties) |> 
  arrange(Incident_Date)



# demographics in Gaza
# source https://www.cia.gov/the-world-factbook/countries/gaza-strip/#people-and-society
Total <- 2141643
Children = .45 * Total
Men = (Total - Children) * .51
Women = Total - (Children + Men)

pop_data <- rbind(Total, Children, Men, Women) |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(name=1, pop=2) |> 
  mutate(pop = as.integer(pop))



# _________________________ PLOTS ________________________________


# plot MoH statistics

# plot incidents across time
incidents_cum |> 
  ggplot(aes(x=Incident_Date, y=value, fill=name)) +
  geom_area(position = "dodge") +
  facet_wrap(~data_source, scales = "free") +
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month") +
  ylab("Casualties") +
  xlab("") +
  guides(fill=guide_legend(title="")) +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.70, end =.90, begin=.15) +
  dark_theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size=16),
        legend.position="top") 


# total percent kill
incidents_cum |> 
  group_by(data_source) |> 
  filter(Incident_Date == "2024-08-28") |> 
  pivot_wider(names_from = name, values_from=value) |> 
  mutate(Children = Children/Total,
         Women = Women/Total) |> 
  select(Incident_Date, Children, Women) |> 
  pivot_longer(-c("Incident_Date", "data_source")) |> 
  ggplot(aes(x=data_source, y=value, fill=name, label=value)) +
  geom_bar(position="stack", stat="identity") +
  #facet_wrap(~data_source, scales = "free") +
  ylab("Casualties") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="")) +
  geom_text(aes(label = paste0(round(100*value, 0),"%"),y=value), vjust=.5,
            size = 6, color="darkred",position = position_stack(vjust = 0.5))  +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.75, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(legend.position="top",
        text = element_text(size = 16)) 



# estimate monthly casualty rate
incidents_cum |> 
  filter(data_source == "Airwars") |> 
  mutate(Incident_month = as.yearmon(Incident_Date)) |> 
  pivot_wider(names_from=name, values_from=value) |> 
  # create a variable for males
  mutate(Men = Total - (Children + Women)) |> 
  select(Incident_month, Children, Women, Men) |> 
  pivot_longer(-Incident_month) |> 
  group_by(Incident_month, name) |> 
  reframe(monthly_casualties = max(value)) |> 
  left_join(pop_data) |> 
  # create casualty rate
  # (total casualties / total population) x 100
  mutate(casualty_rate = (monthly_casualties/pop) * 100,
         name = fct_reorder(name, casualty_rate, .fun = min)) |> 
  ggplot(aes(x=Incident_month, y=casualty_rate, fill=name)) +
  geom_col(position="dodge", stat="identity") +
  geom_smooth(aes(color=name), method="loess", se=FALSE,
              show.legend = FALSE) +
  ylab("Casualty Rate") +
  xlab("") +
  guides(fill=guide_legend(title="")) +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  scale_color_viridis_d(option="cividis", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size=14),
        legend.position="top") 



# calculate overall casualty rate 

# estimate casualty rate
incidents_cum |> 
  group_by(data_source, name) |> 
  # create casualty rate
  # (total casualties / total population) x 100
  reframe(casualty_sum = max(value) ) |> 
  left_join(pop_data) |> 
  mutate(casualty_rate = (casualty_sum / pop) * 100,
         name = factor(name, 
                       levels=c("Children", 
                                "Women", 
                                "Total" ))) |> 
  ggplot(aes(x=name, y=casualty_rate, fill=name)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~data_source, scale="free") +
  ylab("Casualty Rate") +
  xlab("") +
  guides(fill=guide_legend(title="")) +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(legend.position="top",
        text = element_text(size = 16)) 





# ______________________________ other features of attacks _________________________

# Are children and women disproportionately killed by strike_type
# subset incidents where at least 1 child was killed


airwars_incidents_coord <- airwars_incidents |>
  filter(!is.na(incident_lat), # filter data with 
    !is.na(strike_type)#, strike_type != "Airstrike"
  ) |> 
  mutate(incident_lat = as.integer(incident_lat),
         incident_long = as.integer(incident_long),
    children_killed = ifelse(is.na(children_killed), 0, children_killed),
         women_killed = ifelse(is.na(women_killed), 0, women_killed),
         
         # clean up strike type string
         strike_type = str_remove_all(strike_type, 
                                      "Airstrike and/or|Airstrike,|Artillery,"),
         strike_type = str_trim(strike_type),
         strike_type = ifelse(strike_type == "Counter-Terrorism Action (Ground)", 
                              "Ground operation", 
                              ifelse(strike_type=="Naval bombardment", "Artillery",
                                     strike_type)),
         
         # clean up target_type
         target_type = str_replace_all(target_type, "secondary|tertiary|college", "school"),
         target_type = str_replace(target_type, "pharmacy", "hospital"),
         target_type = ifelse(civilian_infrastructure=="IDP or refugee camp", 
                              "IDP or refugee camp", target_type),
         target_type = ifelse(target_type %in% 
                                c("residential", "school", "hospital", "place_of_worship",
                                  "IDP or refugee camp"),
                              target_type, "other"),
         
         # clean civilian infrastructure string
         civilian_infrastructure =
           str_remove(civilian_infrastructure, "\\,.*"),
         civilian_infrastructure = ifelse(
           civilian_infrastructure %in% c("Residential building", "IDP or refugee camp",
                                          "Religious Institution", "Healthcare facility", "School"),
           civilian_infrastructure, "other")
         ) 


dispro_killed_fun <- function(dat, var_name){
  
  dat |> 
    group_by_at(var_name) |> 
    reframe(
      # sum strike types
      type_count = n(),
      # count child deaths by strike type
      children_women_killed = sum(children_killed + women_killed),
      # count total killed by strike type
      killed_count = sum(killed)
    ) |> 
    mutate(
      # compute strike type occurrences by overall events
      type_wt = type_count/sum(type_count),
      # compute proportion of children killed by strike types
      childern_women_killed_prop = children_women_killed/killed_count
    ) |> 
    # weight death rate by proportion of strike type
    mutate(child_death_rate_weighted = childern_women_killed_prop*type_wt)
}


dispro_killed_fun(airwars_incidents_coord, "strike_type")
dispro_killed_fun(airwars_incidents_coord, "target_type") 






# examining geographic data



# compute boundary box area size
airwars_incidents_coord <- airwars_incidents_coord |> 
  rowwise() |> 
  mutate(bbox_area = distm(c(long_min, lat_min), c(long_max, lat_min), fun=distHaversine) +
              distm(c(long_max, lat_min), c(long_max, lat_max), fun=distHaversine) +
              distm(c(long_max, lat_max), c(long_min, lat_max), fun=distHaversine) +
              distm(c(long_min, lat_max), c(long_min, lat_min), fun=distHaversine),
         bbox_area = as.numeric(bbox_area)
  ) |> ungroup()







# sentiment analysis
airwars_incidents |> 
  select(anger:surprise, children_killed, women_killed, killed) |> 
  mutate(children_killed = replace_na(children_killed, 0),
         women_killed = replace_na(women_killed, 0),
         men_killed = killed - (children_killed + women_killed)
  ) |> 
  select(-killed) |> 
  pivot_longer(-c("anger", "disgust", "fear", "joy", "neutral", "sadness", "surprise")) |> 
  #filter(value > 0) |> 
  ggplot(aes(x=value, y=sadness , color= name)) +
  geom_point(size=2) +
  guides(color=guide_legend(title="")) +
  scale_color_viridis_d(option="turbo", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(text = element_text(size = 16))





library(factoextra)




acs_il_t_kmeans_dat  <- airwars_incidents_coord |> 
  select(killed, anger:surprise, incident_lat, incident_long) |> 
  drop_na() 


fviz_nbclust(acs_il_t_kmeans_dat, #data set we want to use
             kmeans, #cluster method
             #method for estimating the optimal number of clusters
             method = "wss", 
             k.max = 20, iter.max=200)

km_1 <- kmeans(acs_il_t_kmeans_dat, 4, nstart = 20)
km_1


# add cluster to the dataset
acs_il_t_kmeans_dat <- acs_il_t_kmeans_dat |> 
  # add the cluster assignment
  mutate(cluster = factor(km_1$cluster)) |> 
  # we do not have to do much processign to join b/c
  ## missing values were on the variables we used for clustering
  ### so they are accounted for before the join
  left_join(airwars_incidents_coord)


acs_il_t_kmeans_dat |> 
  group_by(cluster) |> 
  reframe(mean(children_killed), mean(women_killed)) |> 
  pivot_longer(-cluster) |> 
  ggplot(aes(x=cluster, y=value, fill=cluster)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  facet_wrap(~name, scales = "free") +
  theme_hc() +
  ylab("") +
  scale_fill_viridis_d(option="rocket", 
                       direction = -1, end=.90, alpha=.75) 





acs_il_t_kmeans_dat |> 
  mutate(school = ifelse(target_type == "school", 1, 0),
         hospital = ifelse(target_type == "hospital", 1, 0),
         place_of_worship = ifelse(target_type == "place_of_worship", 1, 0),
         refugee_camp = ifelse(str_detect(target_type, "refugee camp"), 1, 0),
         Airstrike = ifelse(strike_type == "Airstrike", 1, 0),
         Artillery = ifelse(strike_type=="Artillery", 1, 0)
  ) |> 
  group_by(cluster) |> 
  reframe(mean(sadness), mean(fear), mean(disgust), mean(anger), 
          mean(killed), mean(children_killed), mean(women_killed),
          mean(school), mean(hospital), mean(place_of_worship),
          mean(refugee_camp), mean(Airstrike), mean(Artillery)) |> 
  pivot_longer(-cluster) |> 
  ggplot(aes(x=cluster, y=value, fill=cluster)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  facet_wrap(~name, scales = "free") +
  theme_hc() +
  ylab("") +
  scale_fill_viridis_d(option="rocket", 
                       direction = -1, end=.90, alpha=.75) 






