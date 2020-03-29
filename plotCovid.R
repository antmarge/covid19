# plot covid data from nytimes
setwd("~/Documents/covid/")

require("cowplot")

dat <- read.csv("covid-19-data/us-counties.csv", header = T)

date = "2020-03-27"

head(dat)



sip_order <- data.frame(
  county = c("New York City", "Santa Clara, CA", 
             "San Francisco", "Cook, IL (Chicago)", 
             "Marion, IN (Indianapolis)", "Wayne, MI (Detroit)",
             "Harris, TX (Houston)", "Hennepin, MN (Minneapolis)", 
             "King, WA (Seattle)", "Suffolk, MA (Boston)",
             "Cuyahoga, OH (Cleveland)"),
  sip_date = c("2020-03-20","2020-03-17", 
               "2020-03-17","2020-03-20",
               "2020-03-25","2020-03-24",
               "2020-03-24","2020-03-28",
               "2020-03-23","2020-03-24",
               "2020-03-23")
)

counties_of_interest = c("Santa Clara, CA", 
                         "San Francisco", 
                         #"Philadelphia",
                         "Tippecanoe, IN",  
                         "Cuyahoga, OH (Cleveland)",
                         "Harris, TX (Houston)", 
                         "New York City", 
                         "Wayne, MI (Detroit)",
                         "Suffolk, MA (Boston)",  
                         "Marion, IN (Indianapolis)",
                          "Cook, IL (Chicago)", 
                         "Hennepin, MN (Minneapolis)", 
                         "King, WA (Seattle)")

ggplot(sip_order, aes(x = sip_date, y = county)) +
  geom_point()



case_threshold = 10

cbbPalette <- c("#000000",  "darkred","#E69F00", "#56B4E9", "#009E73",  
                "dodgerblue","#0072B2", "#D55E00", "#CC79A7","#999999","#F0E442")


dat_clean <- dat %>% 
  mutate(county = as.character(county)) %>%
  mutate(county = ifelse(county == "King" & state == "Washington", "King, WA (Seattle)", county)) %>%
  mutate(county = ifelse(county == "Cuyahoga" & state == "Ohio", "Cuyahoga, OH (Cleveland)", county)) %>%
  mutate(county = ifelse(county == "Wayne" & state == "Michigan", "Wayne, MI (Detroit)", county)) %>%
  mutate(county = ifelse(county == "Suffolk" & state == "Massachusetts", "Suffolk, MA (Boston)", county)) %>%
  mutate(county = ifelse(county == "Cook" & state == "Illinois", "Cook, IL (Chicago)", county)) %>%
  mutate(county = ifelse(county == "Hennepin" & state == "Minnesota", "Hennepin, MN (Minneapolis)", county)) %>%
  mutate(county = ifelse(county == "Marion" & state == "Indiana", "Marion, IN (Indianapolis)", county)) %>%
  mutate(county = ifelse(county == "Santa Clara" & state == "California", "Santa Clara, CA", county)) %>%
  mutate(county = ifelse(county == "Harris" & state == "Texas", "Harris, TX (Houston)", county)) %>%
  mutate(county = ifelse(county == "Suffolk" & state == "Massachusetts", "Suffolk, MA (Boston)", county)) %>%
  mutate(county = ifelse(county == "Marion" & state == "Indiana", "Marion, IN (Indianapolis)", county)) %>%
  mutate(county = ifelse(county == "Tippecanoe" & state == "Indiana", "Tippecanoe, IN", county)) %>%
  left_join(sip_order, by = "county") %>% 
  
  
  filter(county %in% counties_of_interest) %>% 
  group_by(county) %>%
  arrange(county, date) %>% 
  filter(cases >= case_threshold) %>%
  mutate(days_since_threshold = row_number()) %>% 
  ungroup() %>%
  mutate(county = as.character(county))
  
  
ggplot(dat_clean %>%
         group_by(county) %>%
         filter(as.character(date) == as.character(sip_date)) %>%
         ungroup()) +
  scale_y_log10() +
  geom_text_repel(aes(label = county),
                  box.padding = 1,
                  fontface = "bold",
                  #segment.color = "transparent",
                  point.padding = 1,
                  arrow = arrow(length = unit(0.02, "npc"),type = "open")) +
  geom_point( pch = 22, stroke = 2, size = 3) +
  scale_color_manual(values = cbbPalette) 
  




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
                    filter(row_number() == n()) %>%
                    ungroup(), 
                  aes(label = county),
                  box.padding = 1,
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
  
  