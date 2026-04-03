library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(extrafont)
remotes::install_github("R-CoderDotCom/ridgeline@main")

library(ridgeline)


loadfonts(device = "win")
tuesdata <- tidytuesdayR::tt_load(2026, week = 10)

absolute <- tuesdata$absolute_judgements
pairwise <- tuesdata$pairwise_comparisons
metadata <- tuesdata$respondent_metadata


  
df <- absolute %>% 
  left_join(metadata, by = "response_id")

df<- subset(df, select = -c(timestamp))

colnames(con) <- gsub(" ", "_", colnames(con))

con <- df %>% 
  group_by(term, country_of_residence) %>% 
  summarize(mean = mean(probability), 
            min = min(probability),
            max = max(probability),
            median = median(probability),
            mode = mode(probability))

# Does optimism vary by age?

ggplot(df, aes(x = probability, fill = age_band)) +
  geom_density() +
  facet_wrap(~age_band) +
  labs(title = "Probability Distributions by Age Group",
       subtitle = "Shifts to the right indicate higher 'optimism' in interpretation",
       x = "Probability (%)") +
  theme_dark()


# Differenciate by country (heat map is a bad idea when u have too many countries)

ggplot(con, aes(x = term, y = country_of_residence, fill = mean)) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1)

ggplot(con, aes(x = country_of_residence,
                y = mean, 
                group = term,
                color = term)) +
  coord_polar() +
  facet_wrap(~term) 

ggplot(con, aes(x = reorder(country_of_residence, mean), y = mean)) +
  geom_segment(aes(xend = country_of_residence, yend = 0), color = "grey") +
  geom_point(color = "orange", size = 2) +
  coord_flip() + # Makes country names readable on the Y-axis
  facet_wrap(~term, scales = "free_y") + # You might want to filter to just 4 terms at a time
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) # Shrink text to fit

con %>% filter(class = c()

  

