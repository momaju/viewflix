library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales)
library("RColorBrewer")
theme_set(theme_minimal())



# lendo os dados ----------------------------------------------------------

season_one <- read_csv("Files/CONTENT_INTERACTION/ViewingActivity.csv")


# Cleaning Variable Names -------------------------------------------------


season_one_clean <- season_one %>% 
  clean_names() %>% 
  mutate(profile_name = str_to_title(profile_name))
  #select(profile_name, start_time, duration, title)
  

season_one_clean %>% 
  count(country)

season_one_clean %>% 
  count(profile_name)

season_one_clean %>% 
  arrange(desc(duration))



view_duration <- season_one_clean %>% 
  select(profile_name, start_time, duration, title) %>% 
  mutate(ano = year(start_time)) %>% 
  group_by(profile_name) %>%
  summarize(total = sum((duration))) %>% 
  mutate(profile_name = fct_reorder(profile_name, total)) %>% 
  mutate(total = seconds_to_period(total))

view_duration %>% 
  ggplot(aes(profile_name,total, fill = profile_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total), vjust = -0.4, 
            color = "#0040FF", size=4.0, fontface = "bold") +
  scale_fill_manual(values = c("#FFFF00","#FFCC00","#FF9900", 
                               "#FF6600", "#FF0000")) +
  scale_y_time(breaks = date_breaks('200 hour'),
               labels = label_date(format = "%H:%M:%S")) +
  labs(title = "Total de Horas de Visualização por Perfil",
       subtitle = "Entre 2012 e 2022",
       x = "Nome do Perfil",
       y = "Total de Horas",
       caption = "Fonte: Netflix") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "red", 
                                    face = "italic"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 15))
  


# stacked barplot ---------------------------------------------------------

view_duration_by_year <- season_one_clean %>% 
  select(profile_name, start_time, duration, title) %>% 
  mutate(ano = as.factor(year(start_time))) %>% 
  group_by(profile_name, ano) %>%
  summarize(total = sum((duration))) %>% 
  mutate(profile_name = fct_reorder(profile_name, total)) %>% 
  mutate(total = seconds_to_period(total))


view_duration_by_year %>% 
  ggplot(aes(profile_name,total, fill = ano)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = ano), vjust = +0.50,
            hjust = -0.1,
            color = "#0040FF", size=3.0, fontface = "bold",
            position = position_dodge(0.9), angle = 90) +
  scale_fill_brewer(palette = "Set3") +
  #scale_fill_manual(values = c("#FFFF00","#FFCC00","#FF9900", 
                               #"#FF6600", "#FF0000")) +
  scale_y_time(breaks = date_breaks('200 hour'),
               labels = label_date(format = "%H:%M:%S")) +
  labs(title = "Total de Horas de Visualização por Perfil",
       subtitle = "Entre 2012 e 2022",
       x = "Nome do Perfil",
       y = "Total de Horas",
       caption = "Fonte: Netflix") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "red", 
                                    face = "italic"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 15))




# geom_col ----------------------------------------------------------------

view_duration %>%
  mutate(profile_name = str_to_title(profile_name)) %>% 
  group_by(profile_name) %>% 
  summarize(total = sum((duration))) %>% 
  mutate(total = seconds_to_period(total)) %>% 
  ggplot(aes(profile_name,Total, fill = profile_name)) +
  geom_col() +
  geom_text(aes(label = total), vjust = -0.3, 
            color = "#0040FF", size=3.0) +
  scale_fill_manual(values = c("#FFFF00","#FFCC00","#FF9900", 
                               "#FF6600", "#FF0000")) +
  scale_y_time(breaks = date_breaks('200 hour'),
               labels = label_date(format = "%H:%M:%S")) +
  labs(title = "Total de Horas de Visualização por Perfil",
       subtitle = "Entre 2012 e 2022",
       x = "Nome do Perfil",
       y = "Total de Horas",
       caption = "Fonte: Netflix") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "gray60", 
                                    face = "italic"))


