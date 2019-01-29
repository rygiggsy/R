# Example 1 : Bubble Plot Example -------------------------------------------------------------

library(highcharter)
library(gapminder)
library(tidyverse)

data(gapminder, package = "gapminder")
glimpse(gapminder)

gp <- gapminder %>% 
  arrange(desc(year)) %>% 
  distinct(country, .keep_all = TRUE)

gppop <- gapminder %>% 
  select(country, x = year, y = pop) %>% 
  nest(-country) %>% 
  mutate(data = map(data, list_parse)) %>% 
  rename(ttdata = data)

gp2 <- gapminder %>% 
  group_by(country) %>% 
  do(ttdata = .$lifeExp)
gp2

gptot <- left_join(gp, gppop, by='country')

# Set Thousand Sep 
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

#---> Version 1 : 
hc1 <- hchart(gptot, "point", hcaes('lifeExp', 'gdpPercap', name = 'country', size = 'pop', group = 'continent')) %>% 
  hc_yAxis(type = "logarithmic") %>% 
  hc_tooltip(style=list(color = "#915f6d"), crosshairs = c(TRUE,TRUE), useHTML = TRUE, headerFormat = "",
             pointFormat = '<span style="color:{point.color}; font-size: 18px"> <b>&#9679 
             {point.continent}</b> </span> <br> 
             Country :  <b>{point.country}</b>  <br> 
             Size :  <b>{point.pop:,f}</b>  <br> 
             Life Expectancy :  <b>{point.lifeExp}</b>  <br> 
             GDP :  <b>${point.gdpPercap:,.2f}</b>  <br> ') %>% 
  hc_add_theme(hc_theme_google()) 
hc1

#---> Version 2
tt <- tooltip_table(c("Continent", "Country", "Size", "Life Expectancy", "GDP"), c("{point.continent}", "{point.country}", "{point.pop:,f}", "{point.lifeExp}", "{point.gdpPercap:,.2f}"))
hc2 <- hchart(gptot, "point", hcaes('lifeExp', 'gdpPercap', name = 'country', size = 'pop', group = 'continent')) %>% 
  hc_yAxis(type = "logarithmic") %>% 
  hc_tooltip(pointFormat = tt, useHTML = TRUE)
hc2
