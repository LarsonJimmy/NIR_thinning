# NIR: plotting data
# author: Jimmy Larson
# created: 12.7.2020
# last edited: 12.7.2020

## load packages
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
#devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## load data----
carbs <- read_csv("data/HPLC_carbohydrates.csv")
## separate sample ID and date----
carbs %>%
  separate(ID, c("cultivar", "position"), " ") %>%
  mutate(date1 = case_when(date == 8042020 ~ mdy("08/04/2020"),
                           date == 7132020 ~ mdy("07-13-2020"),
                           date == 6022020 ~ mdy("06-02-2020"),
                           date == 5062020 ~ mdy("05-06-2020"),
                           date == 4282020 ~ mdy("04-28-2020"))) -> carbs
## calculate percent carbohydrates----
carbs %>%
  mutate(perSucrose = (sucrose/carb)*100,
         perGlucose = (glucose/carb)*100,
         perFructose = (fructose/carb)*100,
         perSorbitol = (sorbitol/carb)*100,
         perStarch = (starch/carb)*100) -> carbs
## facet labels----
labels <- c("Gala", "Red Delicious")
names(labels) <- c("Gala", "RedDelicious")
## line plot -----
### % carbohydrates
carbs %>%
  gather('perSucrose', 'perGlucose', 'perFructose', 'perSorbitol', 'perStarch',
          key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) -> carbsLong
ggplot(carbsLong, aes(x = date1, y = avgCarbohydrate, color = carbohydrate)) +
  geom_point()+
  geom_line(aes(linetype = position)) +
  facet_wrap(~cultivar,
             labeller = labeller(cultivar = labels)) +
  scale_color_brewer(palette = "Dark2", labels = c("Fructose", "Glucose", "Sorbitol", "Starch", "Sucrose")) +
  scale_linetype_discrete(labels = c("King", "Lateral")) +
  labs(y = "% Carbohydrate",
       x = "Date",
       color = "Carbohydrate",
       linetype = "Fruitlet Position")+
  theme_jl
#ggsave("per_carbs.png", width = 6, height = 4)
### number carbohydrates
carbs %>%
  gather('sucrose', 'glucose', 'fructose', 'sorbitol', 'starch',
         key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) %>%
  ggplot(aes(x = date1, y = avgCarbohydrate, color = carbohydrate)) +
  geom_point()+
  geom_line(aes(linetype = position)) +
  facet_wrap(~cultivar,
             labeller = labeller(cultivar = labels)) +
  scale_color_brewer(palette = "Dark2", labels = c("Fructose", "Glucose", "Sorbitol", "Starch", "Sucrose")) +
  scale_linetype_discrete(labels = c("King", "Lateral")) +
  labs(y = "Carbohydrate (mg/g dry weight)",
       x = "Date",
       color = "Carbohydrate",
       linetype = "Fruitlet Position")+
  theme_jl
#ggsave("total_carbs.png", width = 6, height = 4)
## stacked barplot----
ggplot(carbsLong, aes(x = as.factor(date1), y = avgCarbohydrate, fill = carbohydrate))+
  geom_bar(position="fill", stat="identity")+
  facet_grid(cultivar ~ position) +
  scale_fill_brewer(palette = "Dark2", labels = c("Fructose", "Glucose", "Sorbitol", "Starch", "Sucrose")) +
  labs(y = "% Carbohydrate",
       x = "Date",
       fill = "Carbohydrate")+
  theme_jl
## stream graph----
### gala king % carb
carbsLong %>%
  filter(cultivar == "Gala" & position == "King") -> stream_per_carbs
streamgraph(stream_per_carbs, key = "carbohydrate", value = "avgCarbohydrate", date = "date1") %>%
  sg_fill_brewer("Dark2") %>%
  sg_legend(TRUE, "Carbohydrate:")
### gala king total carbs
carbs %>%
  gather('sucrose', 'glucose', 'fructose', 'sorbitol', 'starch',
         key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) %>%
  filter(cultivar == "Gala" & position == "King") -> stream_carbs
galaKing_stream <- streamgraph(stream_carbs, key = "carbohydrate", value = "avgCarbohydrate", date = "date1") %>%
  sg_fill_brewer("Dark2") %>%
  sg_legend(TRUE, "Carbohydrate:")
### gala lateral % carbs
carbsLong %>%
  filter(cultivar == "Gala" & position == "lateral") -> stream_per_carbs_lat
streamgraph(stream_per_carbs_lat, key = "carbohydrate", value = "avgCarbohydrate", date = "date1") %>%
  sg_fill_brewer("Dark2") %>%
  sg_legend(TRUE, "Carbohydrate:")
### gala lateral total carbs
carbs %>%
  gather('sucrose', 'glucose', 'fructose', 'sorbitol', 'starch',
         key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) %>%
  filter(cultivar == "Gala" & position == "lateral") -> stream_carbs_lat
galaKing_stream <- streamgraph(stream_carbs_lat, key = "carbohydrate", value = "avgCarbohydrate", date = "date1") %>%
  sg_fill_brewer("Dark2") %>%
  sg_legend(TRUE, "Carbohydrate:")

## thinning dates----
### individual carbs
carbs %>%
  gather('sucrose', 'glucose', 'fructose', 'sorbitol', 'starch',
         key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) %>%
  filter(date1 <= mdy("05-06-2020") & date1 >= mdy("04-28-2020")) %>%
  ggplot(aes(x = carbohydrate, y = avgCarbohydrate, fill = position))+
  geom_bar(position="dodge", stat="identity", color = "black")+
  geom_errorbar(aes(ymin = avgCarbohydrate - stdCarbohydrate, ymax = avgCarbohydrate + stdCarbohydrate),
                width = 0.2, position = position_dodge(0.9))+
  facet_grid(cultivar ~ date1,
             labeller = labeller(cultivar = labels)) +
  scale_fill_brewer(palette = "Set2", labels = c("King", "Lateral")) +
  labs(y = "Carbohydrate (mg/g dry weight)",
       x = "Carbohydrate",
       fill = "Fruitlet Position")+
  theme_jl
#ggsave("abscission_carbs.png", width = 10, height = 6)
### total carbs
carbs %>%
  mutate(total_carb = sucrose + glucose + fructose + sorbitol + starch) %>%
  select(cultivar, position, date1, total_carb) %>% 
  group_by(cultivar, position, date1, total_carb) %>%
  summarise(avgCarbohydrate = mean(total_carb))%>%
  filter(date1 <= mdy("05-06-2020") & date1 >= mdy("04-28-2020")) %>%
  ggplot(aes(x = position, y = avgCarbohydrate, fill = position))+
  geom_bar(position="dodge", stat="identity", color = "black")+
  facet_grid(cultivar ~ date1,
             labeller = labeller(cultivar = labels)) +
  scale_fill_brewer(palette = "Set2", labels = c("King", "Lateral")) +
  labs(y = "Carbohydrate (mg/g dry weight)",
       x = "Carbohydrate",
       fill = "Fruitlet Position")+
  theme_jl
#ggsave("total_carbs.png", width = 10, height = 10)
