# Example 2 : Drill Down Example -------------------------------------------------------------
library(highcharter)
library(gapminder)
library(tidyverse)

data(gapminder, package = "gapminder")
glimpse(gapminder)

gp <- gapminder %>% 
  arrange(desc(year)) %>% 
  distinct(country, .keep_all = TRUE)

# Prep Drilldown
gppop <- gapminder %>%
  select(country, x = year, y = pop) %>%
  nest(-country) %>%
  mutate(data = map(data, list_parse)) %>%
  mutate(id = tolower(country)) %>%
  mutate(name = country) %>%
  select(country, id, data, name) %>%
  nest(-country) %>%
  mutate(data = map(data, list_parse))


t11 = unlist(gppop$data,recursive=FALSE)

gptot <- left_join(gp, gppop, by='country')

# Setup Level 1 dataframe
df <- gptot %>%
  select(country, continent, lifeExp, pop, gdpPercap) %>% mutate(drilldown = tolower(country))

hc <- hchart(df, "point", hcaes('lifeExp', 'gdpPercap', name = 'country', size = 'pop', group = 'continent')) %>% 
  hc_yAxis(type = "logarithmic") %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = t11
  )
hc
