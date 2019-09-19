library(tidyverse) #tell R that you want to use the tidyverse packages and functions

read_csv("data/gapminder.csv") #tidyverse function that reads data into R

gapminder <- read_csv("data/gapminder.csv")

nrow(gapminder) #to check how many rows the data has

ncol #to check how many columns the data has

colnames(gapminder) #to show the names of the columns

glimpse(gapminder) #to show a glimpse of the data

#chr = characters, text
#dbl = double = numbers 
#ord = ordered factor (numbers that have been ordered)
#int = intenger (whole numbers, not fractional or decimal numbers)

nrow(storms)
ncol(storms)
colnames(storms)
glimpse(storms)

select(gapminder, year, country, pop)
select(gapminder, 3, 1, 5)
select(gapminder, 3:5)
select(gapminder, -lifeExp, -pop)
select(gapminder, year:lifeExp)
select(gapminder, -(year:lifeExp))

just_population <- select(gapminder, year, country, pop)                                                                                                                                                                                                               
just_population

select(gapminder, country)
select(gapminder, country, year, pop, gdpPercap)
select(gapminder, 1, 3, 5, 6)
select(gapminder, -continent, -lifeExp)

select(gapminder, starts_with("co"))
select(gapminder, contains("e"))                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
select(gapminder, ends_with("p"))
select(gapminder, endsWith("p"))

select(gapminder, population = pop, country)
rename(gapminder, population = pop)

filter(gapminder, country == Australia)
filter(gapminder, country =="Australia")
filter(gapminder, year >=1997)

filter(gapminder, lifeExp >=80)
filter(gapminder, continent == "Europe")
filter(gapminder, lifeExp >=80, gdpPercap >=30.000)
filter(gapminder, lifeExp >= 80 | gdpPercap >= 30.000)
filter(gapminder, continent %in% c("Africa", "Asia", "Europe"))
