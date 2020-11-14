# NIR: plotting data
# author: Jimmy Larson
# created: 11.13.2020
# last edited: 11.13.2020

## load packages
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## load data----
redKingMay6 <- read_csv("data/sample_86.csv")
galaKingMay6 <- read_csv("data/sample_87.csv")
galaLateralMay6 <- read_csv("data/sample_88.csv")
redLateralMay6 <- read_csv("data/sample_89.csv")
redKingApril28 <- read_csv("data/sample_90.csv")
redLateralApril28 <- read_csv("data/sample_91.csv")
galaKingApril28 <- read_csv("data/sample_92.csv")
galaLateralApril28 <- read_csv("data/sample_93.csv")
## add label info----
### Red Delicious
galaKingMay6 %>%
  mutate(cultivar = "Gala",
         position = "king",
         date = "May 6") %>%
  select(wavelength, absorbance, cultivar, position, date) -> galaKingMay6
galaLateralMay6 %>%
  mutate(cultivar = "Gala",
         position = "lateral",
         date = "May 6") %>%
  select(wavelength, absorbance, cultivar, position, date) -> galaLateralMay6
galaKingApril28 %>%
  mutate(cultivar = "Gala",
         position = "king",
         date = "April 28") %>%
  select(wavelength, absorbance, cultivar, position, date) -> galaKingApril28
galaLateralApril28 %>%
  mutate(cultivar = "Gala",
         position = "lateral",
         date = "April 28") %>%
  select(wavelength, absorbance, cultivar, position, date) -> galaLateralApril28
### Gala
redKingMay6 %>%
  mutate(cultivar = "Red Delicious",
         position = "king",
         date = "May 6") %>%
  select(wavelength, absorbance, cultivar, position, date) -> redKingMay6
redLateralMay6 %>%
  mutate(cultivar = "Red Delicious",
         position = "lateral",
         date = "May 6") %>%
  select(wavelength, absorbance, cultivar, position, date) -> redLateralMay6
redKingApril28 %>%
  mutate(cultivar = "Red Delicious",
         position = "king",
         date = "April 28") %>%
  select(wavelength, absorbance, cultivar, position, date) -> redKingApril28
redLateralApril28 %>%
  mutate(cultivar = "Red Delicious",
         position = "lateral",
         date = "April 28") %>%
  select(wavelength, absorbance, cultivar, position, date) -> redLateralApril28
## bind data----
absorbance <- rbind(galaKingApril28, galaLateralApril28, galaKingMay6, galaLateralMay6,
                    redKingApril28, redLateralApril28, redKingMay6, redLateralMay6)
## plot data----
ggplot(absorbance, aes(x = wavelength, y = absorbance, color = position, linetype = date))+
  geom_line() +
  facet_wrap(~cultivar) +
  scale_color_brewer(palette = "Dark2", labels = c("King", "Lateral")) +
  labs(y = "Absorbance",
       x = "Wavelength",
       color = "Fruitlet Position",
       linetype = "Date")+
  theme_jl
ggsave("fruitlet_absorption.png")

absorbance %>%
  filter(wavelength > 3687) %>%
  ggplot(aes(x = wavelength, y = absorbance, color = position, linetype = date))+
  geom_line() +
  facet_wrap(~cultivar) +
  scale_color_brewer(palette = "Dark2", labels = c("King", "Lateral")) +
  labs(y = "Absorbance",
       x = "Wavelength",
       color = "Fruitlet Position",
       linetype = "Date")+
  theme_jl
