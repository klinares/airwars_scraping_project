
pacman::p_load(lubridate, RSQLite, DBI, zoo, ggthemes, 
               factoextra, gridExtra, ggsci, ggmap, geosphere,
               ggdark, janitor,  jsonlite, viridis, tidyverse)

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
  mutate(killed = ifelse(is.na(killed), 0, killed),
         injured = str_remove(civilians_reported_injured, ".*–"),
         injured = ifelse(is.na(injured), 0, injured),
         injured = as.numeric(injured),
         incident_lat = as.numeric(incident_lat),
         incident_long = as.numeric(incident_long),
         children_killed = ifelse(is.na(children_killed), 0, children_killed),
         women_killed = ifelse(is.na(women_killed), 0, women_killed),
         men_killed = killed - (children_killed + women_killed),
         men_killed = ifelse(is.na(men_killed), 0, men_killed)) |> 
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
Children = .47 * Total
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
  filter(Incident_Date == max(Incident_Date)) |> 
  pivot_wider(names_from = name, values_from=value) |> 
  mutate(Children = Children/Total,
         Women = Women/Total) |> 
  select(Incident_Date, Children, Women) |> 
  pivot_longer(-c("Incident_Date", "data_source")) |> 
  ggplot(aes(x=data_source, y=value, fill=name, label=value)) +
  geom_bar(position="stack", stat="identity") +
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
  #filter(data_source == "Airwars") |> 
  mutate(Incident_month = as.yearmon(Incident_Date)) |> 
  pivot_wider(names_from=name, values_from=value) |> 
  mutate(Children = Children /Total,
         Women = Women/Total) |> 
  rename(Men=Total) |> 
  # create a variable for males
  #mutate(Men = Total - (Children + Women)) |> 
  select(data_source, Incident_month, Children, Women, Men) |> 
  pivot_longer(-c("Incident_month", "data_source")) |> 
  group_by(data_source, Incident_month, name) |> 
  reframe(casualty = max(value)) |> 
  left_join(pop_data) |> 
  # create casualty rate
  # (total casualties / total population) x 100
  group_by(data_source, Incident_month) |> 
  mutate(adjusted_casualty = casualty* (pop/sum(pop))) |> 
  filter(name!= "Men") |> 
  select(-pop) |> 
  pivot_longer(-c("data_source", "Incident_month", "name"), names_to = "estimate") |> 
  mutate(estimate = relevel(
    factor(estimate), ref="casualty") ) |> 
  ggplot(aes(x=Incident_month, y=value, fill=name)) +
  geom_bar(position="stack", stat="identity") +
  ylab("Casualty Rate") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(rows=vars(data_source), cols=vars(estimate)) +
  guides(fill=guide_legend(title="")) +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  scale_color_viridis_d(option="cividis", direction = -1,
                        alpha=.80, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size=12),
        legend.position="top") 



# calculate overall casualty rate 

# estimate casualty rate
incidents_cum |> 
  group_by(data_source) |> 
  filter(Incident_Date == max(Incident_Date)) |> 
  pivot_wider(names_from=name, values_from=value) |> 
  mutate(Men = Total - (Children + Women)) |> 
  select(-Total, -Incident_Date) |> 
  pivot_longer(-data_source) |> 
  left_join(pop_data) |> 
  # calculate casualty rate normalized by per 1000
  mutate(casualty_rate = value/sum(value),
         pop_prop = pop/sum(pop),
         casualty_rate_adj =  (casualty_rate * pop_prop) * 1000,
         casualty_rate_adj = casualty_rate_adj / sum(casualty_rate_adj),
         name = fct_reorder(name, casualty_rate_adj, .fun=min)
  ) |> 
  ungroup() |> 
  select(data_source, name, casualty_rate, casualty_rate_adj) |> 
  reshape2::melt(id=c("data_source", "name")) |> 
  mutate(name = factor(name, 
                       levels=c("Children", 
                                "Women", 
                                "Men" ))) |> 
  ggplot(aes(x=data_source, y=value, fill=name)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = round(value, 2)*100), position = position_fill(vjust=.5), size=4)  +
  facet_wrap(~variable) +
  #coord_flip() +
  ylab("") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="", reverse = TRUE)) +
  scale_fill_viridis_d(option="cividis", direction = -1,
                       alpha=.80, end =.70, begin=.15) +
  dark_theme_linedraw() +
  theme(legend.position="top",
        text = element_text(size = 14)) 






# ______________________________ other features of attacks _________________________




# sentiment analysis plot
airwars_incidents |> 
  select(Incident_Date, anger:surprise) |> 
  group_by(Incident_Date) |> 
  reframe(across(where(is.double), ~ mean(.x))) |> 
  pivot_longer(-Incident_Date) |> 
  mutate(name = fct_reorder(name, value, .fun=max)) |> 
  ggplot(aes(x=Incident_Date, y=value , fill=name)) +
  geom_area(stat = "smooth", method = "loess", position="stack") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month") +
  ylab("Rating") +
  xlab("") +
  guides(fill=guide_legend(title="")) +
  scale_fill_viridis_d(option="mako", direction = -1,
                       alpha=.99, end =.99, begin=.35)  +
  dark_theme_linedraw() +
  theme(text = element_text(size = 14))

# next assign each incident highest emotion
airwars_incidents <- airwars_incidents |> 
  select(Incident_id, anger:surprise) |> 
  pivot_longer(-Incident_id) |> 
  group_by(Incident_id) |> 
  filter(value == max(value)) |> 
  ungroup() |> 
  select(Incident_id, name) |> 
  rename(highest_emo = name) |> 
  right_join(airwars_incidents)



# average kills by top 4 emotions
top_4_emo <- c("disgust", "anger", "fear", "sadness")

# pivot data to prepare for plotting
highest_emo_dat <- airwars_incidents |> 
  filter(highest_emo %in% top_4_emo) |> 
  select(highest_emo, killed, injured, 
         children_killed, women_killed, men_killed) |> 
  pivot_longer(-highest_emo) 

means <- highest_emo_dat |> 
  group_by(highest_emo, name) |> 
  reframe(value = mean(value))

# jitter plot with means of incidents by emotion
highest_emo_dat |> 
  ggplot(aes(x=highest_emo, y=value, color=highest_emo)) + 
  geom_jitter(width=0.3, alpha=.7, size=.5) +
  facet_wrap(~name, scale="free") +
  stat_summary(fun="mean", color="dodgerblue", alpha=.8, size=.2) +
  geom_text(data = means, show.legend = FALSE,
            aes(label = round(value, 1)),
            vjust = -10, hjust = .5) +
  ylab("Average Incident") +
  xlab("") +
  scale_color_viridis_d(option="mako", direction = -1,
                        alpha=.99, end =.99, begin=.35) +
  guides(color=guide_legend(title="")) +
  dark_theme_linedraw() +
  theme(size=14, legend.position="top", axis.text.x=element_blank()) 



# examining geographic data through hierarchical clusters


# drop NA, select variables of interest and scale
hclust_data <- airwars_incidents |> 
  select( incident_lat, incident_long,
          injured, killed, all_of(top_4_emo)) |> 
  drop_na() |> 
  mutate_all(scale)

# create distance matrix
hclust_d <- dist(hclust_data)

# conduct clustering
hc_ward <- hclust(hclust_d, method = "ward.D2")

dend_plot_fun <- function(mod, k_num) {
  
  fviz_dend(mod, k=k_num, 
            lwd = 0.8, , k_colors = "tron", rect = TRUE, rect_border = "tron", 
            rect_fill = TRUE, show_labels = FALSE, cex = 0.75,  # label size
            #c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
            color_labels_by_k = TRUE,  # color labels by groups
            ggtheme = dark_theme_linedraw(), 
            main = str_c("Cluster Dendrogram k = ", k_num)
  )
}


plot_list <- map(2:10, function(x){
  dend_plot_fun(hc_ward, x)
}) 


# plot dendogram for each cluster 
do.call("grid.arrange", 
        c(plot_list, ncol =
            floor(sqrt( length(plot_list)) ) 
        )
)

# examine cluster assignment proportion
## choose k-clusters to compare
map(3:7, function(x){
  factor(cutree(hc_ward, x)) |> 
    as_tibble() |> 
    rename(cluster=1) |> 
    count(cluster) |> 
    mutate(prop = n/sum(n))
}) 



# write the cluster assignment back to the dataframe, prepare for plotting
airwars_incidents_coord <- airwars_incidents |> 
  filter(!is.na(incident_lat)) |> 
  mutate(cluster = factor(cutree(hc_ward, 3))) |> 
  mutate(
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
    # make qualitative variables
    school = ifelse(target_type == "school", 1, 0),
    hospital = ifelse(target_type == "hospital", 1, 0),
    place_of_worship = ifelse(target_type == "place_of_worship", 1, 0),
    refugee_camp = ifelse(str_detect(target_type, "refugee camp"), 1, 0),
    Airstrike = ifelse(strike_type == "Airstrike", 1, 0),
    Artillery = ifelse(strike_type=="Artillery", 1, 0)
  ) 


airwars_incidents_coord |> group_by(cluster) |> 
  reframe(
    Injured = sum(injured),
    Killed = sum(killed),
    Children = sum(children_killed)/sum(killed), 
    Women = sum(women_killed)/sum(killed), 
    Men = sum(men_killed)/sum(killed),
    Sadness = mean(sadness, na.rm = TRUE), 
    Fear = mean(fear, na.rm = TRUE), 
    Disgust = mean(disgust, na.rm = TRUE), 
    Anger = mean(anger, na.rm = TRUE), 
    School = sum(school), 
    Hospital = sum(hospital), 
    Place_of_Workship = sum(place_of_worship),
    Refugee_Camp = sum(refugee_camp), 
  ) |> 
  mutate(Injured = Injured / sum(Injured), 
         Killed = Killed/sum(Killed),
         School = School/sum(School), 
         Hospital = Hospital/sum(Hospital), 
         Place_of_Workship = Place_of_Workship/sum(Place_of_Workship),
         Refugee_Camp = Refugee_Camp/sum(Refugee_Camp)) |> 
  pivot_longer(-cluster) |>
  mutate(name = str_replace_all(name, "_", " "),
         name=factor(name, levels=c("Injured", "Killed", "Children","Women","Men",
                                    "Sadness", "Fear","Anger",  "Disgust", 
                                    "School", "Hospital", 
                                    "Place of Workship", "Refugee Camp"))) |> 
  ggplot(aes(x=cluster, y=value, fill=cluster)) +
  geom_bar(stat="identity", width=.5, position = "stack") +
  facet_wrap(~name, scales = "free") +
  theme_hc() +
  ylab("") +
  xlab("") +
  guides(fill=guide_legend(title="Clusters")) +
  scale_fill_tron() +
  dark_theme_linedraw() +
  theme(legend.position="top",
        text = element_text(size = 12)) 






# plot the clusters on a map
cs_key <- read_csv("~/repos/api-keys.csv") |> 
  filter(key_id == "Google_key") |> 
  pull(key)


register_google(key = cs_key, write = TRUE)


gaza_map <- get_googlemap(center = c(lon = 34.3900, lat = 31.4100),  #"Gaza Strip", 
                          maptype = "hybrid",  #size = c(800, 650),
                          zoom=11) 


gaza_map |> 
  ggmap() + 
  stat_density2d(data = airwars_incidents_coord,
                 aes(x = incident_long, y = incident_lat,
                     fill = cluster, alpha = ..level..),
                 size = 1, bins = 20,
                 geom = 'polygon') +
  scale_alpha(range = c(.01, .40), guide = FALSE) +
  geom_point(aes(x = incident_long, y = incident_lat , 
                 color=cluster), size=.2,
             data=airwars_incidents_coord) +
  scale_color_tron(alpha=.75) + 
  scale_fill_tron() +
  dark_theme_linedraw() +
  theme(legend.position="top",
        text = element_text(size = 16))







