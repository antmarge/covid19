# plot covid data from nytimes
#install.packages("remotes")
#remotes::install_github("moodymudskipper/safejoin")

require("cowplot")
library("dplyr")
library(safejoin)
library(ggplot2)
library(ggrepel)


HOME = "~/Documents/projects/covid19/"
setwd(HOME)

date = "2020-03-27"

# Data files
nyt_county_file = paste0(HOME,"data/covid-19-data/us-counties.csv")
nyt_state_file = paste0(HOME,"data/covid-19-data/us-states.csv")
state_abrev_file = paste0(HOME,"data/state_abrev.csv")
city_county_file = paste0(HOME, "data/us_cities_states_counties.csv")
sah_file = paste0(HOME, "data/stay_at_home_dates.txt")

cities_of_interest_file <- paste0(HOME, "data/cities_of_interest.txt")

# Parameters
case_threshold = 10
cbbPalette <- c("#000000",  "darkred","#E69F00", "#56B4E9", "#009E73",  
                "dodgerblue","#0072B2", "#D55E00", "#CC79A7","#999999","#F0E442")


# Read data
nyt_dat <- read.csv(nyt_county_file, 
                    header = T,
                    stringsAsFactors = F)
sah_dat <- read.table(sah_file, 
                      header = T,
                      sep = "\t",
                      stringsAsFactors = F)

capitalizeString <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

# Read in cities of interest
interest_dat <- read.table(cities_of_interest_file, 
                           header = T, sep = "\t",
                           stringsAsFactors = F)

# Get county info for cities of interest

city_county_dat <- read.csv(city_county_file, 
                            header = T, 
                            stringsAsFactors = F) %>%
  mutate(County = sapply(tolower(County), capitalizeString)) %>%
  # Special case NYC is considered county in NYT data
  mutate(City = ifelse(City.alias == "New York City" & 
                         State.full == "New York", "New York City", City)) %>%
  mutate(County = ifelse(City.alias == "New York City" & 
                           State.full == "New York", "New York City", County)) %>%
  select(-City.alias) %>%
  unique() %>%
  # Add shelter in place dates
  left_join(sah_dat, by = c("County" = "county", "State.full" = "state"))
  # UPDATE: add a secondary join for state shelter in place dates if county not available
  

interest_full_dat <- interest_dat %>%
  left_join(city_county_dat , by = c("county" = "County", "state" = "State.full")) %>% 
  rename("state_abrev" = "State.short", "county" = "County") %>%
  # Be careful - SF returns two counties, don't think it's actually part of San Mateo
  # UPDATE: cleaner way to do this?
  group_by(city) %>%
    filter(row_number() == 1) %>%
  ungroup()

# Join interest list with NYT data and filter for threshold to plot data
dat_clean <- nyt_dat %>%
  left_join(city_county_dat %>%
              select(County, State.full, State.short) %>% unique(), 
            by =  c("county" = "County", "state" = "State.full")) %>%
  #inner_join(interest_full_dat, by = c("state", "county")) %>% 
  mutate(region_label = paste0(county, " (", State.short, ")")) %>%
  group_by(region_label) %>%
  arrange(county, date) %>% 
  mutate(new_cases = cases - lag(cases, n = 7L)) %>%
  filter(cases >= case_threshold) %>%
  mutate(days_since_threshold = row_number()) %>% 
  ungroup() %>%
  mutate(region_label = paste0(county, ", ", State.short) )


dat_clean_sah <- dat_clean %>%
  #filter(date <= stay_at_home_date) %>%
  group_by(region_label) %>%
  arrange(date) %>%
  mutate(diff_days = days_since_threshold - lag(days_since_threshold, n = 5L),
         diff_growth = cases - lag(cases,n = 7L),
         rate_percent = (diff_growth / diff_days)/cases * 100) 


# Doubling model
days_to_double = c(2,3,4,7)
model_dat <- data.frame()
for (d in days_to_double){
  x = seq(from = 1, to = max(dat_clean_sah$days_since_threshold), by = 1)
  t = length(dat_clean_sah$days_since_threshold)
  N_0 = case_threshold
  N_x = N_0 * 2^(x/d) 
  model_dat <- rbind(model_dat, data.frame(days_since_threshold = x, cases = N_x, days_to_double = d))
}

  
ggplot(dat_clean_sah, #%>%
         #group_by(region_label) %>%
         #filter(days_since_threshold == min(days_since_threshold) +1 |
        #        days_since_threshold == max(days_since_threshold) |
        #        date == stay_at_home_date) %>%
        # ungroup(),
       aes(x = days_since_threshold, y = cases, color = region_label)) +
  geom_text_repel(data = dat_clean_sah %>%
                    group_by(region_label) %>%
                    filter(date == stay_at_home_date) %>%
                    ungroup(),
                  aes(label = region_label),
                  box.padding = 1,
                  fontface = "bold",
                  segment.color = "transparent",
                  point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  geom_line(data = model_dat, 
            inherit.aes = F,
            aes(x = days_since_threshold, y = cases, 
                group = days_to_double),
            linetype = "dashed",
            color = "lightgray"
  ) +
  geom_text_repel(data = model_dat %>%
                    filter(days_since_threshold == 25), 
            inherit.aes = F,
            aes(x = days_since_threshold, y = cases, 
                group = days_to_double,
                label = paste0(days_to_double, " days to double")),
            fontface = "bold.italic",
            size = 5,
            color = "gray"
  ) +
  #geom_point() +
  scale_y_log10(labels = comma) +
  geom_line(size = 1) +
  geom_point(data = dat_clean_sah %>%
               group_by(region_label) %>%
               filter(date == stay_at_home_date) %>%
               ungroup(),
             aes(x = days_since_threshold, y = cases, color = region_label),
             pch = 22, size = 5, stroke = 2) +

  guides(color = F) +
  scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(title = "Confirmed COVID19 Cases through 27 March 2020",
       subtitle = "Select regions of interest (county-level data)",
       caption = "Sources: Case data from The New York Times\nDates of Stay-at-home orders from various published sources",
       x = paste0("Days since ", case_threshold, "th case"),
       y = "Cummulative Cases")

  
###############ALL ########################

ggplot(dat_clean,
       aes(x = days_since_threshold, y = cases, color = region_label)) +
  geom_text_repel(data = dat_clean_sah %>%
                    group_by(region_label) %>%
                    filter(date == max(date)) %>%
                    ungroup(),
                  aes(label = region_label),
                  box.padding = 1,
                  fontface = "bold",
                  segment.color = "transparent",
                  point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  geom_line(data = model_dat, 
            inherit.aes = F,
            aes(x = days_since_threshold, y = cases, 
                group = days_to_double),
            linetype = "dashed",
            color = "lightgray"
  ) +
  geom_text_repel(data = model_dat %>%
                    filter(days_since_threshold == 25), 
                  inherit.aes = F,
                  aes(x = days_since_threshold, y = cases, 
                      group = days_to_double,
                      label = paste0(days_to_double, " days to double")),
                  fontface = "bold.italic",
                  size = 5,
                  color = "gray"
  ) +
  #geom_point() +
  scale_y_log10(labels = comma) +
  geom_line(size = 1) +
  geom_point(data = dat_clean_sah %>%
               group_by(region_label) %>%
               filter(date == stay_at_home_date) %>%
               ungroup(),
             aes(x = days_since_threshold, y = cases, color = region_label),
             pch = 22, size = 5, stroke = 2) +
  
  guides(color = F) +
  #scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(title = "Confirmed COVID19 Cases through 27 March 2020",
       subtitle = "Select regions of interest (county-level data)",
       caption = "Sources: Case data from The New York Times\nDates of Stay-at-home orders from various published sources",
       x = paste0("Days since ", case_threshold, "th case"),
       y = "Cummulative Cases")



############## Plot new cases vs existing cases ##################


ggplot(dat_clean,
       aes(x = cases, y = new_cases, color = region_label)) +
  geom_text_repel(data = dat_clean %>%
                    group_by(region_label) %>%
                    filter(date ==max(date)) %>%
                    ungroup(),
                  aes(label = region_label),
                  box.padding = 1,
                  fontface = "bold",
                  segment.color = "transparent",
                  point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +

  scale_y_log10(labels = comma) +
  scale_x_log10(labels = comma) +
  
  geom_line(size = 1) +
  #geom_point(data = dat_clean %>%
  #             group_by(region_label) %>%
  #             filter(date == stay_at_home_date) %>%
  #             ungroup(),
  #           aes(x = cases, y = new_cases, color = region_label),
  #           pch = 22, size = 5, stroke = 2) +
  geom_point(data = dat_clean %>%
               group_by(region_label) %>%
               filter(date == max(date)) %>%
               ungroup(),
             aes(x = cases, y = new_cases, color = region_label),
             size = 3, stroke = 2) +
  
  guides(color = F) +
 # scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(title = "Confirmed COVID19 Cases through 27 March 2020",
       subtitle = "Select regions of interest (county-level data)",
       caption = "Sources: Case data from The New York Times\nDates of Stay-at-home orders from various published sources"#,
       #x = paste0("Days since ", case_threshold, "th case"),
       #y = "Cummulative Cases"
       )




#####################

ggplot(data = dat_clean,
         aes(x = days_since_threshold, y = cases, color = county)) +
    geom_line(size = 1) +
    #geom_point() +
    geom_point(data = dat_clean %>%
                 filter(as.character(date) == as.character(sip_date)),
               aes(x = days_since_threshold, y = cases),
               pch = 22,
               size = 3, stroke = 2) +
  geom_text_repel(data = dat_clean %>%
                    group_by(county) %>%
                    filter(
                      row_number() == n()) %>%
                    ungroup(), 
                  aes(label = county),
                  box.padding = 1,
                  size = 6,
                  fontface = "bold",
                  #segment.color = "transparent",
                  point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  scale_color_manual(values = cbbPalette) +
    scale_y_log10() +
    guides(color = F) +
  labs(title = "Confirmed COVID19 Cases through 26 March 2020",
       subtitle = "Select regions of interest (county-level data)",
       caption = "Sources: Case data from The New York Times\nDates of Stay-at-home orders from various published sources",
       x = paste0("Days since ", case_threshold, "th case"),
       y = "Cummulative Cases")


  

##################### STATE DATA ####################################

  
nyt_state_dat <- read.csv(nyt_state_file,
                            header = T,
                            stringsAsFactors = F) %>% 
  group_by(state) %>%
  
  arrange(state, date) %>% 
  mutate(new_cases = cases - lag(cases, n = 4L)) %>%
  #filter(cases >= case_threshold) %>%
  #filter(cases >= 10 & new_cases >= 10) %>% 
  arrange(state, date) %>%
  filter(! is.na(new_cases))
  
new_vs_exist_plot <- ggplot(nyt_state_dat,
         aes(x = cases, y = new_cases, group = state)) +

  
  scale_y_log10(labels = comma, limits = c(10,50000)) +
  scale_x_log10(labels = comma, limits = c(10,50000)) +
  
  geom_line(size = 1, color = "gray") +
  geom_point(data = nyt_state_dat %>%
               group_by(state) %>%
               filter(date == max(date)) %>%
               ungroup(),
             aes(x = cases, y = new_cases, color = state),
             size = 2, stroke = 2, color = "red") +
    geom_text_repel(data = nyt_state_dat %>%
                      group_by(state) %>%
                      filter(date == max(date)) %>%
                      ungroup() %>%
                      arrange(desc(cases)) %>% 
                      filter(row_number() <= 10 | row_number() > n() -10),
                    aes(label = state),
                    box.padding = 1,
                    fontface = "bold",
                    #segment.color = "transparent",
                    #point.padding = 1,
                    arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  guides(color = F) +
  #scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(
       subtitle = "No deviations from exponential in U.S. states :(",
       caption = "Source: Case data from The New York Times",
       x = "Total Cases",
       y = "New Cases (in past 7 days)"
  ) 

  


case_threshold = 100

nyt_state_dat <- read.csv(nyt_state_file,
                          header = T,
                          stringsAsFactors = F) %>% 
  group_by(state) %>%
  
  arrange(state, date) %>% 
  filter(cases >= case_threshold) %>%
  mutate(days_since_threshold = row_number()) %>% 
  ungroup()


days_to_double = c(2,3,4,7)
model_dat <- data.frame()
for (d in days_to_double){
  x = seq(from = 1, to = max(nyt_state_dat$days_since_threshold), by = 1)
  t = length(dat_clean_sah$days_since_threshold)
  N_0 = case_threshold
  N_x = N_0 * 2^(x/d) 
  model_dat <- rbind(model_dat, data.frame(days_since_threshold = x, cases = N_x, days_to_double = d))
}


  
  
old_plot <- 
  ggplot(nyt_state_dat, 
         aes(x = days_since_threshold, 
             y = cases, group = state)) +
  scale_y_log10(labels = comma) +
  #scale_x_log10(labels = comma, limits = c(10,50000)) +
  
  geom_line(size = 1, color = "gray") +
  geom_point(data = nyt_state_dat %>%
               group_by(state) %>%
               filter(date == max(date)) %>%
               ungroup(),
             size = 2, stroke = 2, color = "red") +
  geom_text_repel(data = nyt_state_dat %>%
                    group_by(state) %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    arrange(desc(cases)) %>% 
                    filter(row_number() <= 20),
                  aes(label = state),
                  box.padding = 1,
                  fontface = "bold",
                  #segment.color = "transparent",
                  #point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  geom_line(data = model_dat, inherit.aes = F,
            aes( x = days_since_threshold, y = cases, group = days_to_double),
            linetype = "dashed", 
            color = "darkgray") +
  geom_text_repel(data = model_dat %>%
                    filter(days_since_threshold == 20), 
                  inherit.aes = F,
                  aes(x = days_since_threshold, y = cases, 
                      group = days_to_double,
                      label = paste0(days_to_double, " days to double")),
                  fontface = "bold.italic",
                  size = 5,
                  color = "gray"
  ) +
  guides(color = F) +
  #scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(
       subtitle = "Cummulative cases in U.S. states since 100th case",
       y = "Total Cases",
       caption = "",
       x = paste0("Days since ", case_threshold, "th case")
  ) 



ggarrange(old_plot, new_vs_exist_plot,
          labels = c("A", "B"))
  




nyt_state_dat <- read.csv(nyt_state_file,
                          header = T,
                          stringsAsFactors = F) %>% 
  group_by(state) %>%
  
  arrange(state, date) %>% 
  mutate(new_cases = cases - lag(cases, n = 4L)) %>%
  #filter(cases >= case_threshold) %>%
  #filter(cases >= 10 & new_cases >= 10) %>% 
  arrange(state, date) %>%
  group_by(state) %>%
  
  arrange(state, date) %>% 
  filter(cases >= case_threshold) %>%
  mutate(days_since_threshold = row_number()) %>% 
  ungroup()


ggplot(nyt_state_dat,
       aes(x = days_since_threshold, y = new_cases, group = state)) +
  
  
  scale_y_log10(labels = comma, limits = c(10,50000)) +
  scale_x_log10(labels = comma, limits = c(10,50000)) +
  
  geom_line(size = 1, color = "gray") +
  geom_point(data = nyt_state_dat %>%
               group_by(state) %>%
               filter(date == max(date)) %>%
               ungroup(),
             aes(x = days_since_threshold, y = new_cases, color = state),
             size = 2, stroke = 2, color = "red") +
  geom_text_repel(data = nyt_state_dat %>%
                    group_by(state) %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    arrange(desc(cases)) %>% 
                    filter(row_number() <= 10 | row_number() > n() -10),
                  aes(label = state),
                  box.padding = 1,
                  fontface = "bold",
                  #segment.color = "transparent",
                  #point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  guides(color = F) +
  #scale_color_manual(values = rep(cbbPalette, times = 2)) +
  labs(
    subtitle = "No deviations from exponential in U.S. states :(",
    caption = "Source: Case data from The New York Times",
    x = "Total Cases",
    y = "New Cases (in past 7 days)"
  ) 












  ############################
  
  
  
  counties_of_interest = c("Santa Clara, CA", "Tippecanoe, IN", 
                           "Marion (Indianapolis)", "Harris (Houston)", 
                           "New York City", 
                            "Cook (Chicago)", "Hennepin (Minneapolis)")
  
  case_threshold = 10
  
  cbbPalette <- c("#999999","#56B4E9", "#009E73",  "#0072B2", "#D55E00","#CC79A7","#000000")

  
  dat_clean <- dat %>% 
    mutate(county = as.character(county)) %>%
    
    mutate(county = ifelse(county == "Tippecanoe" & state == "Indiana", "Tippecanoe, IN", county)) %>%
    mutate(county = ifelse(county == "Wayne" & state == "Michigan", "Wayne (Detroit)", county)) %>%
    
    mutate(county = ifelse(county == "Cook" & state == "Illinois", "Cook (Chicago)", county)) %>%
    mutate(county = ifelse(county == "Hennepin" & state == "Minnesota", "Hennepin (Minneapolis)", county)) %>%
    mutate(county = ifelse(county == "Marion" & state == "Indiana", "Marion (Indianapolis)", county)) %>%
    mutate(county = ifelse(county == "Santa Clara" & state == "California", "Santa Clara, CA", county)) %>%
    mutate(county = ifelse(county == "Harris" & state == "Texas", "Harris (Houston)", county)) %>%
    
    mutate(county = ifelse(county == "Suffolk" & state == "Massachusetts", "Suffolk (Boston)", county)) %>%
    
    filter(county %in% counties_of_interest) %>%
    group_by(county) %>%
    arrange(date) %>%
    mutate(cumm_cases = cumsum(cases),
           cumm_deaths = cumsum(deaths)) %>%
    filter(cumm_cases >= case_threshold) %>%
    mutate(days_since_threshold = row_number()) %>%
    ungroup() %>%
    mutate(county = as.character(county)) #%>%
    #mutate(county = ifelse(county == "Cook", "Cook (Chicago)", county),
    #       county = ifelse(county == "Marion", "Marion (Indianapolis)", county),
    #       county = ifelse(county == "Suffolk", "Suffolk (Boston)", county))
  
  ggplot(data = dat_clean,
         aes(x = days_since_threshold, y = log(cumm_cases), color = county)) +
    geom_line(size = 1) +
    geom_point() +
    geom_label_repel(data = dat_clean %>%
                       group_by(county) %>%
                       filter(row_number() == n()) %>%
                       ungroup(), 
                     aes(label = county),
                     box.padding = 1,
                     fontface = "bold",
                     segment.color = "lightgray",
                     arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
    scale_color_manual(values = cbbPalette) +
    guides(color = F) +
    labs(title = "COVID19 Cases",
         subtitle = "My favorite counties with NYC for reference ",
         caption = "Source: NY Times Data on Github 2020-03-27",
         x = paste0("Days since ", case_threshold, "th case"),
         y = "Log(Cummulative Cases)")
  
  


