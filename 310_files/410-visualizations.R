# Capstone Step 3 Visualizations
library(tidyverse)
library(ggrepel)
library(ggthemes)
svi <- read.csv("C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410\\SVI2022_NEWYORK_tract.csv")

# Comparing socioeconomic factors potentially associated with not owning a vehicle of similar counties to Livingston

# Ranking each county on their public transit
# Unideal <- Lewis, Ontario, Schoharie, Tioga, Wyoming
# Sufficient <- Dutchess, Livingston, Maddison, Wayne
# Good <- Nassau, Ulster

# Cleaning DataFrame to only have values we need
svi2 <- svi |> 
  select(COUNTY, E_TOTPOP, E_NOVEH, E_UNEMP, E_NOHSDP, E_UNINSUR,
         E_AGE65, E_AGE17, E_DISABL, E_MINRTY, E_GROUPQ) |> 
  filter(COUNTY %in% c('Dutchess County', 'Lewis County', 'Livingston County', 'Madison County', 
                    'Nassau County', 'Ontario County',
                    'Schoharie County', 'Tioga County', 
                    'Ulster County', 'Wayne County', 'Wyoming County'),
         E_TOTPOP >= 0,
         E_NOVEH >= 0,
         E_UNEMP >= 0, 
         E_NOHSDP >= 0, 
         E_UNINSUR >= 0, 
         E_AGE65 >= 0, 
         E_AGE17 >= 0, 
         E_DISABL >= 0,
         E_MINRTY >= 0,
         E_GROUPQ >= 0) |> 
  group_by(COUNTY) |> 
  summarize(C_TOTPOP = sum(E_TOTPOP),
            C_NOVEH = sum(E_NOVEH),
            C_UNEMP = sum(E_UNEMP),
            C_NOHSDP = sum(E_NOHSDP),
            C_UNINSUR = sum(E_UNINSUR),
            C_AGE65 = sum(E_AGE65),
            C_AGE17 = sum(E_AGE17),
            C_DISABL = sum(E_DISABL),
            C_MINRTY = sum(E_MINRTY)) |> 
  mutate(RATING = c("Sufficient","Unideal","Sufficient","Sufficient",
                    "Good","Unideal","Unideal","Unideal","Good","Sufficient","Unideal"))
  
svi3 <- svi2 |> 
  mutate(NOVEH = C_NOVEH/C_TOTPOP,
         UNEMP = C_UNEMP/C_TOTPOP,
         NOHSDP = C_NOHSDP/C_TOTPOP,
         UNINSUR = C_UNINSUR/C_TOTPOP,
         AGE65 = C_AGE65/C_TOTPOP,
         AGE17 = C_AGE17/C_TOTPOP,
         DISABL = C_DISABL/C_TOTPOP,
         MINRTY = C_MINRTY/C_TOTPOP)



# Data for map
# Counties included in analysis
county_map <- socviz::county_map
  
county_data <- socviz::county_data

county <- county_map |> 
  left_join(county_data)

county <- county |> 
  filter(name %in% c('Dutchess County', 'Lewis County', 'Livingston County', 'Madison County', 
                     'Nassau County', 'Ontario County',
                     'Schoharie County', 'Tioga County', 
                     'Ulster County', 'Wayne County', 'Wyoming County'),
         state == "NY")

# All NY counties
state <- county_map |> 
  left_join(county_data) |> 
  filter(state == "NY")

#Map with highlighted counties
map <- ggplot()+
  geom_polygon(data = state,
               mapping = aes(x = long,
                             y = lat,
                             group = group),
               color = "black",
               fill = "grey")+
  geom_polygon(data = county,
               mapping = aes(x = long,
                             y = lat,
                             group = group),
               color = "black",
               fill = "steelblue")+
  coord_equal()+ 
  theme_map()
  map
# ggsave("county_map.jpeg",
#        plot = map, 
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410")


# Bar Chart for County Populations
populations <- ggplot(data = svi2,
       mapping = aes(x = fct_reorder(COUNTY,
                                     C_TOTPOP),
                     y = C_TOTPOP))+
  geom_col(fill = 'steelblue4')+
  geom_text(aes(label = C_TOTPOP),
            size = 2.5,
            vjust = -.3)+
  labs(x = "",
       y = "County Population",
       title = "Population of Each County",
       subtitle = "Year: 2022")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'))
populations
# ggsave("county_populations.jpeg",
#        plot = populations,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")


# Make DataFrame longer to incorporate all factors
svi3_long <- svi3 |> 
  select(COUNTY, NOVEH, UNEMP, NOHSDP,
         UNINSUR, AGE65, AGE17,
         DISABL, MINRTY) |> 
  pivot_longer(cols = c('NOVEH', 'UNEMP', 'NOHSDP',
                        'UNINSUR', 'AGE65', 'AGE17',
                        'DISABL', 'MINRTY'),
               names_to = "PERC_TYPE",
               values_to = "PERC")  


# County Percentages of Vulnerability Factors (by rating)
# Unideal
unideal <- ggplot()+
  geom_line(data = svi3_long |> filter(COUNTY %in% c('Lewis County', 'Schoharie County', 'Tioga County', 
                                                     'Wyoming County')),
            mapping = aes(x = PERC_TYPE,
                          y = PERC,
                          group = COUNTY,
                          color = COUNTY),
            linewidth = 1.5)+
  geom_point(data = svi3_long |> filter(COUNTY %in% c('Lewis County', 'Ontario County', 'Schoharie County', 'Tioga County', 
                                                      'Wyoming County')),
             mapping = aes(x = PERC_TYPE,
                           y = PERC))+
  scale_color_viridis_d()+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Unideal' Public Transportation Services")+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))
unideal
# ggsave("unideal_counties.jpeg",
#        plot = unideal,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")

# Sufficient
sufficient <- ggplot(data = svi3_long |> filter(COUNTY %in% c('Dutchess County', 'Livingston County', 'Madison County', 'Wayne County')),
       mapping = aes(x = PERC_TYPE,
                     y = PERC,
                     group = COUNTY))+
  geom_line(aes(color = COUNTY),
            linewidth = 1.5)+
  geom_point()+
  scale_color_tableau()+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Sufficient' Public Transportation Services")+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))

# ggsave("sufficient_counties.jpeg",
#        plot = sufficient,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")


# Good
good <- ggplot(data = svi3_long |> filter(COUNTY %in% c('Ontario County','Nassau County', 'Ulster County')),
       mapping = aes(x = PERC_TYPE,
                     y = PERC,
                     group = COUNTY))+
  geom_line(aes(color = COUNTY),
            linewidth = 1.5)+
  geom_point()+
  scale_color_manual(values = c('grey30', 'darkgreen', 'purple4'))+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Good' Public Transportation Services")+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))
good
# ggsave("good_counties.jpeg",
#        plot = good,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")

#avg % for each variable
#unideal
avg_perc_unideal <- svi3_long |> 
  filter(COUNTY %in% c('Lewis County', 'Schoharie County', 'Tioga County', 
                       'Wyoming County')) |> 
  group_by(PERC_TYPE) |> 
  summarize(AVG_PERC = mean(PERC)) 


avg_u <- ggplot(data = avg_perc_unideal,
                mapping = aes(x = PERC_TYPE,
                              y = AVG_PERC))+
  geom_line(aes(group = 1),
            color = 'red',
            linewidth = 1.5)+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Average Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Unideal' Public Transportation Services")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))
avg_u
# ggsave("avg_unideal_counties.jpeg",
#        plot = avg_u,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")

#sufficient
avg_perc_sufficient <- svi3_long |> 
  filter(COUNTY %in% c('Dutchess County', 'Livingston County', 'Madison County', 'Wayne County')) |> 
  group_by(PERC_TYPE) |> 
  summarize(AVG_PERC = mean(PERC))


avg_s <- ggplot(data = avg_perc_sufficient,
                mapping = aes(x = PERC_TYPE,
                              y = AVG_PERC))+
  geom_line(aes(group = 1),
            color = 'red',
            linewidth = 1.5)+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Average Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Sufficient' Public Transportation Services")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))
avg_s
# ggsave("avg_sufficient_counties.jpeg",
#        plot = avg_s,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")

#good
avg_perc_good <- svi3_long |> 
  filter(COUNTY %in% c('Ontario County','Nassau County','Ulster County')) |> 
  group_by(PERC_TYPE) |> 
  summarize(AVG_PERC = mean(PERC))


avg_g <- ggplot(data = avg_perc_good,
                mapping = aes(x = PERC_TYPE,
                              y = AVG_PERC))+
  geom_line(aes(group = 1),
            color = 'red',
            linewidth = 1.5)+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Average Social Vulnerability Percentages',
       subtitle = "Counties Classified Under 'Good' Public Transportation Services")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'))
avg_g
# ggsave("avg_good_counties.jpeg",
#        plot = avg_g,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")


avg_perc_unideal <- avg_perc_unideal %>% mutate(Condition = "Unideal")
avg_perc_sufficient <- avg_perc_sufficient %>% mutate(Condition = "Sufficient")
avg_perc_good <- avg_perc_good %>% mutate(Condition = "Good")

avg_perc_all <- bind_rows(avg_perc_unideal,
                          avg_perc_sufficient,
                          avg_perc_good)

final <- ggplot(data = avg_perc_all,
       mapping = aes(x = PERC_TYPE,
                     y = AVG_PERC,
                     group = Condition,
                     color = Condition))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c('Unideal' = 'darkblue',
                                'Sufficient' = 'blue',
                                'Good' = 'dodgerblue'))+
  scale_y_continuous(labels = scales::percent_format( ))+
  labs(x = 'Social Vulnerability Factor',
       y = 'Percentage',
       title = 'Average Social Vulnerability Percentages')+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = 'bold'),
        title = element_text(face = 'bold'),
        legend.position = 'top')
final
# ggsave("avg_factor_pct.jpeg",
#        plot = final,
#        device = "jpeg",
#        path = "C:\\Users\\peter\\OneDrive\\School_2024_25\\Spring25\\410",
#        width = 8,
#        height = 5,
#        units = "in")
