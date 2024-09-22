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
