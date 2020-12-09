# NIR: plotting data
# author: Jimmy Larson
# created: 12.7.2020
# last edited: 12.7.2020

## load packages
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
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
## plot % total carbohydrates----
carbs %>%
  gather('perSucrose', 'perGlucose', 'perFructose', 'perSorbitol', 'perStarch',
          key = "carbohydrate", value = "quantity") %>%
  select(cultivar, position, date1, carbohydrate, quantity) %>%
  group_by(cultivar, position, date1, carbohydrate) %>%
  summarise(avgCarbohydrate = mean(quantity),
            stdCarbohydrate = sd(quantity)) %>%
  ggplot(aes(x = date1, y = avgCarbohydrate, color = carbohydrate)) +
  geom_point()+
  geom_line(aes(linetype = position)) +
  facet_wrap(~cultivar) +
  theme_jl
