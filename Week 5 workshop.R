#Setup Tidyverse
library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages('plotrix')
library(plotrix)
#Read In Data 
Table1 <- read_csv("https://3mmarand.github.io/BIO00058M-Data-science-2020/data-raw/Human-development-index.csv")
#Tidy data frame
Table2 <- Table1 %>% clean_names()

TidyTable <- Table2 %>%
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country))

#Filter data set to exclude countries which have missing observations
ENDtable <- filter(TidyTable, !is.na(value))
#Summarize data
#Mean, Standard dev, N= number of values in the mean and standard error
hdi_summary <- ENDtable %>% 
  group_by(country) %>% 
  summarise(mean_value = mean(value),
            n = length(value),
            Sd = sd(value),
            Se = std.error(value))
#Filter 10 Lowest HDi countries
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_value) < 11)

hdi_summary_low

#Plot graph from data 
hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_value)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_value - Se,
                    ymax = mean_value+ Se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()
#Build Pipeline in the data. 
read_csv("https://3mmarand.github.io/BIO00058M-Data-science-2020/data-raw/Human-development-index.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country)) %>% 
  filter(!is.na(value)) %>%
  group_by(country) %>% 
  summarise(mean_value = mean(value),
            n = length(value),
            Sd = sd(value),
            Se = std.error(value)) %>% 
  filter(rank(mean_value) < 11) %>%
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_value)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_value - Se,
                    ymax = mean_value + Se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#Task 2
#Read In data
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)
#Give column names
buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)


?scan





names(buoy44025)




