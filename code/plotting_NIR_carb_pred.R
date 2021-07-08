# NIR: HPLC and NIR pred values plotting
# author: Jimmy Larson
# created: 7.8.2021
# last edited: 7.8.2021

## load packages
library(tidyverse)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## read in data----
pred <- read_csv("data/carb_pred_values_whole_fruit.csv")
## plotting----
labels <- c("Fructose", "Glucose", "Sorbitol", "Starch", "Sucrose", "Total Soluble Sugar")
names(labels) <- c("fructose", "glucose", "sorbitol", "starch", "sucrose", "tss")
rSquared <- data.frame(carb = c("fructose", "glucose", "sorbitol", "starch", "sucrose", "tss"),
                       rSquare = c("0.8885", "0.6404", "0.9755", "0.8576", "0.8372", "0.7928"),
                       x = c(300, 102, 60, 180, 125, 600),
                       y = c(50, 50, 4, -5, -15, 210))
ggplot(pred, aes(x = HPLC, y = pred,)) +
  geom_point(aes( color = carb), alpha = 0.5) +
  geom_abline(slope = 1) +
  facet_wrap(~ carb,
             labeller = labeller(carb = labels),
             scales = "free") +
  geom_text(data = rSquared, mapping = aes(x = x, y = y, label = paste("r-squared = ", rSquare, sep = "")))+
  scale_color_brewer(palette = "Dark2", labels = c("Fructose", "Glucose", "Sorbitol", "Starch", "Sucrose", "Total Soluble Sugar")) +
  guides(color = FALSE) +
  labs(x = "HPLC Measured Value (mg/g dry weight)",
       y = "Predicted Value from NIR Spectra (mg/g dry weight)") +
  theme_jl
ggsave("carb_pred_plot.png",  width = 12, height = 8)  
