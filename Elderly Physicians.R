library(OECD)
library(dplyr)
library(ggplot2)
library(ggsci)

### Load Data ####
datasets <- get_datasets()
HEALTH_REAC <- get_dataset("HEALTH_REAC")
HEALTH_REAC_STRUC <- get_data_structure("HEALTH_REAC")

### Set Variables ####
Countries <- c("AUT","BEL","DNK","IRL","NLD","FIN","SWE","ISR")
year <- "2018"

## Get OECD AVG for the year####
avg_data <- 
HEALTH_REAC %>% 
  filter(VAR %in% c("PAGGT75O","PAGGT65O")) %>% 
  filter(TIME_FORMAT == "P1Y") %>% 
  filter(obsTime == year) %>% 
  filter(UNIT == "PHYTOTNB") %>% 
  group_by(VAR) %>% 
  summarise(
    n = n(),
    avg = mean(obsValue)
  ) %>% 
  as.data.frame()

### Draw Plot ####
merge(x = HEALTH_REAC, y = as.data.frame(HEALTH_REAC_STRUC$COU), by.x = "COU", by.y = "id") %>%  
  filter(VAR %in% c("PAGGT75O","PAGGT65O")) %>% 
  filter(TIME_FORMAT == "P1Y") %>% 
  filter(obsTime == year) %>% 
  filter(UNIT == "PHYTOTNB") %>% 
  filter(COU %in% Countries) %>% 
  ggplot(aes(x = label,
             y = obsValue,
             fill = VAR
  )) + 
  geom_col() + 
  ylab("") +
  scale_y_continuous( breaks = seq(0,30,2.5), limits = c(0,30), expand = c(0, 0.1)) +
  xlab("Country") + 
  scale_fill_nejm(name = "%",
                  labels = c(paste0("% Aged 65-74 \n OECD Average - ",avg_data %>% filter(VAR == "PAGGT65O") %>% select(avg) %>% round(1)),
                             paste0("% Aged > 75 \n OECD Average - ",avg_data %>% filter(VAR == "PAGGT75O") %>% select(avg) %>% round(1)))
                  )+
  ggtitle("Percent of Elderly Physicians out of total physicians, OECD countries, 2018") + 
  theme_classic()
