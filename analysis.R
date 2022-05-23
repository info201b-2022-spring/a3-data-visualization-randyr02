library("tidyverse")
library("plotly")
library("ggplot2")
library("usmap")

#county-level 
incarcerations <- read.csv("https://github.com/vera-institute/incarceration_trends/blob/master/incarceration_trends.csv?raw=true")

#What is the average value of my variable across all the counties (in the current year)?
#Where is my variable the highest / lowest?
#How much has my variable change over the last N years?


#Variable Questions

# Adding Location (County, State) Column for Easier Location

incarcerations <- incarcerations %>% mutate(location = paste0(county_name, ", ", state))

# Urban County in most recent year with the largest minority to white ratio in jail (more minorities compared to white) = District of Columia, DC

most_recent_year <- incarcerations %>% filter(year == max(year)) %>% filter(urbanicity == "urban") %>% 
                          mutate(minority_ratio = (total_jail_pop - white_jail_pop)/ white_jail_pop) %>% 
                          filter(minority_ratio != Inf)

urban_max_ratio <- most_recent_year[most_recent_year$minority_ratio == max(most_recent_year$minority_ratio, inf.rm = TRUE), "location"] #District of Columbia, DC

# Percentage of BlackPop 15-64 in Jail by Year

urban_black_ratio <- incarcerations %>% filter(urbanicity != "urban" ) %>% group_by(year) %>% filter(is.na(black_jail_pop) == FALSE) %>% 
  filter(is.na(black_pop_15to64) == FALSE) %>%
  summarize(year = unique(year), black_jail_pop = sum(black_jail_pop), 
            black_pop_15to64 = sum(black_pop_15to64), 
            black_ratio = black_jail_pop/black_pop_15to64) %>% mutate(ratio_change = black_ratio - lag(black_ratio))

#Year with Greatest Increase and Decrease from the previous year in Change of Percentage of BlackPop 15-64 in Jail
# Increase = 1995
# Decrease = 2015

max_black_ratio <- urban_black_ratio %>% filter(ratio_change == max(ratio_change, na.rm = TRUE)) %>% pull(year) # 1992
min_black_ratio <- urban_black_ratio %>% filter(ratio_change == min(ratio_change, na.rm = TRUE)) %>% pull(year) # 2015

# Average change in jail population over time = 934,150 people per year 

yearly_jail_pop_rate <- incarcerations %>% group_by(year) %>% filter(is.na(total_jail_pop_rate) == FALSE) %>% 
  summarize(year = unique(year), total_jail_pop_rate = sum(total_jail_pop_rate))
                                          
avg_yearly_jail_pop_rate <- mean(yearly_jail_pop_rate$total_jail_pop_rate) 



# Which region in most recent year has the highest percentage of minority jail to population jail = Northeast

jail_2018 <- incarcerations %>% filter(year==max(year))

regions_2018 <- jail_2018 %>% filter(is.na(total_jail_pop) == FALSE) %>% filter(is.na(white_jail_pop) == FALSE) %>% group_by(region) %>%
  summarize(region = unique(region), total_jail_pop = sum(total_jail_pop), white_jail_pop = sum(white_jail_pop), minority_ratio = (total_jail_pop - white_jail_pop)/total_jail_pop, .groups = 'drop') %>%
  filter(minority_ratio == max(minority_ratio)) %>% pull(region)

# Which county in most recent year has the highest percentage of whites jail to population jail = Mobile County, AL

county_2018 <- jail_2018 %>% filter(urbanicity != "rural") %>% filter(is.na(total_jail_pop) == FALSE) %>% filter(is.na(white_jail_pop) == FALSE) %>% mutate(white_ratio = white_jail_pop/total_jail_pop) %>%
                              filter(white_ratio == max(white_ratio, na.rm = TRUE)) %>% pull(location) 
                              

#Charts
#BlackPop in Jail Over years, density of color based on new blackpop per year 
# A chart that shows trends over time for a variable of your choice

region_names <- unique(incarcerations$region)
print(region_names)

get_bar <- function(df, region_select) {
  trends <- df %>% filter(is.na(black_jail_pop) == FALSE) %>% 
    filter(region == region_select) %>%
    filter(year >= 2000 & year <= 2010) %>%
    group_by(year) %>% 
    filter(is.na(black_jail_pop_rate) == FALSE) %>%
    summarize(black_jail_pop = sum(black_jail_pop), black_jail_pop_rate = mean(black_jail_pop_rate))
  
  
  bar <- ggplot(date = trends) +
    geom_col(mapping = aes(x = trends$year, y = trends$black_jail_pop,
                            fill = trends$black_jail_pop_rate)) +
            labs(x = "Year",
                 y = "Total Black Jail Population",
                 fill = "Average New Black Jail Population",
                 title = paste0("Black Jail Population over Time in the ", region_select)) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_fill_gradient(low = "white", high = "black")
ggplotly(bar)
  
}

trend1_bar <- get_bar(incarcerations, "Northeast")
trend2_bar <- get_bar(incarcerations, "Midwest")

# Total Population, Minority Ratio = y, Color = Region, in 2018
# A chart that compares two variables to one another


get_scatter <- function(df) {
 
  minorities <- df %>% filter(year == max(year)) %>% 
    filter(is.na(total_jail_pop) == FALSE) %>%
    filter(is.na(white_jail_pop) == FALSE) %>%
    mutate(minority_ratio = (total_jail_pop - white_jail_pop)/ white_jail_pop) %>% 
    filter(minority_ratio != Inf)
  
  scatter <- ggplot(data = minorities) +
    geom_point(mapping = aes(x = minority_ratio, y = total_jail_pop,
                             color = region,
                             text = paste("County, State:", location ))) +
    labs(x = "Minorities to White Jail Population",
         y = "Total Jail Population",
         color = "Region")
  ggplotly(scatter)
}

scatter_plot <- get_scatter(incarcerations)

# Jail Ratio (Jail Pop/ Total Pop) in each state in 2018
#A map that shows how your measure of interest varies geographically

  
get_map <- function(df) {
  state_2018 <- df %>% group_by(state) %>%
    filter(is.na(total_jail_pop) == FALSE) %>%
    filter(is.na(total_pop) == FALSE) %>%
    summarize(total_jail_pop = sum(total_jail_pop), total_pop = sum(total_pop),state = unique(state)) %>%
    mutate(jail_ratio = total_jail_pop / total_pop)
  
  
  plot_usmap(data = state_2018, 
             regions = "state", 
             values = "jail_ratio",
             color = "black",
             exclude = c("DC")) +
    scale_fill_continuous(low = "white", high="blue",
                          name = "Jail Population to State Population",
                          label = scales :: comma) +
    
    labs(title = "Jail Population Ratio by State") +
    theme(legend.position = "right")
  
}


state_map <- get_map(jail_2018)












