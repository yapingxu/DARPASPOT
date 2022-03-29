# decomp fluid FILP data#


library(knitr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggeffects)
library(lme4)
library(lmerTest)
library(car)
library(MASS)
library(devtools)
library(tidyverse)
library(readxl)

library(stringr)
# combine all .csv's in file
setwd("D:/Yaping Xu/SPOT/Analysis/Greenhouse/Brightness Data")
Data <- ldply(list.files(), read.csv, header=TRUE) %>% mutate_at(.vars = "Date", as.Date, format = "%m/%d/%Y")


########################
## Fill in the blanks ##
########################

excitation <- "465" #options: 405, 465, 525
emission <- "575"   #options: 525, 575, 680, 740
label <- paste(excitation, "nm excitation,", emission, "nm emission")

plot.data <- filter(Data,((Emmission == emission) & (Excitation == excitation) ))

treatment_df <- plot.data %>% 
  mutate(Treatment = case_when(
    str_detect(Plant.ID, "Control") == TRUE ~ "000 mL",
    str_detect(Plant.ID, "50mL") == TRUE ~ "050 mL",
    str_detect(Plant.ID, "100mL") == TRUE ~ "100 mL",
    str_detect(Plant.ID, "175mL") == TRUE ~ "175 mL",
    str_detect(Plant.ID, "Stnd 1") == TRUE ~ "Standard 1",
    str_detect(Plant.ID, "Stnd 2") == TRUE ~ "Standard 2"
  ))

average_df <- treatment_df %>% 
  group_by(Treatment, Day) %>%
  dplyr::summarise(average_mean = mean(Mean),
                   n = n(), 
                   standard_deviation = sd(Mean),
                   standard_high = average_mean + (2*standard_deviation),
                   standard_low = average_mean - (2*standard_deviation))

ggplot(subset(treatment_df, Treatment %in% c("000 mL", "050 mL", "100 mL", "175 mL")), aes(x=Day, y=Mean, color=Treatment)) +
  geom_point() +
  # polynomial interpolation: https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/#:~:text=Key%20R%20function%3A%20geom_smooth()%20for,smoothed%20conditional%20means%20%2F%20regression%20line.&text=color%20%2C%20size%20and%20linetype%20%3A%20Change,color%20of%20the%20confidence%20region.
  stat_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3), data = average_df %>% filter(Treatment %in% c("000 mL", "050 mL", "100 mL", "175 mL")), aes(x=Day, y=average_mean, color=Treatment)) +
  geom_line(data = average_df %>% filter(Treatment %in% c("000 mL", "050 mL", "100 mL", "175 mL")), aes(x=Day, y=standard_high, color=Treatment)) +
  geom_line(data = average_df %>% filter(Treatment %in% c("000 mL", "050 mL", "100 mL", "175 mL")), aes(x=Day, y=standard_low, color=Treatment)) +
  scale_color_manual(values = c("#4EA2A2", 
                                "#D4B95E", 
                                "#D28F33", 
                                "#751A33")) +
  ggtitle(label) +
  labs(x = "Days since treatment", y = "Pixel intensity") +
  facet_wrap(~Treatment) +
  theme_minimal() +
  theme(legend.position="none")




##

