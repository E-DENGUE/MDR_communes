library(tidyverse)
library(viridis)
library(plotly)
library(pheatmap)
a1 <- readRDS('./Data/CONFIDENTIAL/updated_commune_data_with_new_boundaries.rds')

a2 <- a1 %>%
  filter(Year<=2021) %>%
  group_by(Year, District,Communes) %>%
  summarize(dengue=sum(Dengue)) %>%
  ungroup() %>%
  arrange(District, Communes, Year) %>%
  group_by(Communes) %>%
  mutate(dengue_mean=mean(dengue),
         commune2= paste(District, Communes, sep='_') 
         )
  
grav <- a1 %>%
  group_by(Communes, Year) %>%
  mutate( wgt = Dengue/sum(Dengue),
          week_wgt = wgt*week) %>%
  summarize(center_of_gravity = sum(week_wgt))

a3 <- a2 %>%
  ungroup() %>%
  group_by(District, Communes) %>%
  summarize(dengue_mean=mean(dengue), dengue_sd=sd(dengue), dengue_min=min(dengue)) %>%
  mutate(inv_var=1/dengue_sd^2)

hist(a3$dengue_mean)

ggplot(a3) +
  geom_point(aes(x=District, y=dengue_mean, size=inv_var))

ggplot(a3) +
  geom_point(aes(y=sqrt(inv_var), x=dengue_mean))

ggplot(a3) +
  geom_point(aes(y=dengue_min, x=dengue_mean))

a2 %>%
  filter(dengue_mean>=50) %>%
ggplot( aes(x = Year, y = Communes, fill = sqrt(dengue))) +
  geom_tile(color = NA) +  # Create tiles for heatmap
  scale_fill_viridis_c(option = "viridis") +  # Use viridis color scale
  facet_wrap(~District, scales = "free_y", ncol = 1) +  # Group by district
  theme_minimal() +
  labs(title = "Case Counts by Year and Commune",
       x = "Year", 
       y = "Commune",
       fill = "Case Counts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df <- a2 %>%
  filter(dengue_mean>=25 & Year<=2021) %>%
  mutate(commune_district = factor(paste(District, Communes, sep = " - "), 
                                   levels = unique(paste(District, Communes, sep = " - "))))

# Plot the heatmap
p1 <- ggplot(df, aes(x = Year, y = commune_district, fill = sqrt(dengue))) +
  geom_tile(color = NA, size = 0.1) +  # Thin borders around tiles
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Sqrt case Counts by Year and Commune (Grouped by District)",
       x = "Year", 
       y = "Commune",
       fill = "Case Counts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(vjust = 0.5),
        panel.grid.major.y = element_blank())  # Remove horizontal gridlines

ggplotly(p1)


# Create heatmap with hierarchical clustering using pheatmap
df_wide <- a2 %>%
  filter(dengue_mean>=25) %>%
  spread(key = Year, value = dengue, fill = 0) %>% # Spread year across columns
  ungroup()

case_matrix <- as.matrix(df_wide %>% select(-Communes, -District, -dengue_mean, -commune2))

rownames(case_matrix) <- df_wide$Communes  # Set commune names as rownames

pheatmap(case_matrix, 
         cluster_rows = TRUE, 
         cluster_cols = FALSE,  # If you want to cluster years as well, set to TRUE
         color = viridis::viridis(100), 
         display_numbers = F,  # Optional: shows the case counts on the tiles
         main = "Hierarchically Clustered Case Counts")


#What proportion of cases in the district are covered in each commune

b1 <- a2 %>%
  arrange(District, Year, -dengue) %>%
  group_by(District, Year) %>%
  mutate(prop = dengue/sum(dengue),
         cum_prop = cumsum(prop),
         rank=row_number(),
         proportion_cases_top3 = sum(dengue*(rank<3)) /sum(dengue)
         )%>%
  ungroup() %>%
  group_by(District) 
  
prop_commune <- reshape2::dcast(b1,District+Communes~Year, value.var='prop')  


prop_range <- b1 %>%
  ungroup() %>%
  group_by(District, Communes) %>%
  summarize(min_prop=min(prop),
            max_prop=max(prop),
            median_prop=median(prop),
            mean_cases=mean(dengue),
            mean_rank= mean(rank)
            )%>%
  mutate(prop_range=max_prop-min_prop,
         optum= min_prop*max_prop*(median_prop>=0.15))%>%
  ungroup()%>%
arrange(-optum)

ggplot(prop_range, aes(y=min_prop, x=max_prop))+
  geom_point()

write.csv(prop_range,'./Data/CONFIDENTIAL/proportion_cases_district.csv')
