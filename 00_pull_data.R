library(rvest)
library(tidyverse)
library(stringr)

### State Population ----
# URL of the page
url <-
  "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
# Assuming it's the first table in this case.
table <- html_nodes(page, "table") %>% .[[1]]

# Convert the table to a dataframe
state_pop <- html_table(table, fill = TRUE)
state_pop <- state_pop[, 1:2]
names(state_pop) <- c('state', 'pop2023')
state_pop %>%
  filter(state != 'State or territory') %>%
  mutate(pop2023 = gsub(",", "", pop2023),
         pop2023 = as.numeric(pop2023))   %>%
  filter(!if_any(everything(), is.na)) ->
  state_pop

### Country Population ----
# URL of the page
url <-
  "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
# Assuming it's the first table in this case.
table <- html_nodes(page, "table") %>% .[[1]]
country_pop <- html_table(table, fill = TRUE)
country_pop %>%
  select(country = Location, pop2023 = Population)  %>%
  mutate(pop2023 = gsub(",", "", pop2023),
         pop2023 = as.numeric(pop2023))  ->
  country_pop


### State GDP ----
# URL of the page
url <-
  "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP"

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
# Assuming it's the first table in this case.
table <- html_nodes(page, "table") %>% .[[1]]
state_gdp <- html_table(table, fill = TRUE)
state_gdp <- state_gdp[, 1:2]
names(state_gdp) <- c('state', 'gdp2022_mill_usd')
state_gdp %>%
  mutate(state = as.character(state),
         # Convert to character if it's not already
         state = str_replace_all(state, " \\*", "")) %>%
  mutate(
    gdp2022_mill_usd = gsub(",", "", gdp2022_mill_usd),
    gdp2022_mill_usd = as.numeric(gdp2022_mill_usd)
  ) %>%
  filter(!if_any(everything(), is.na)) ->
  state_gdp



### Country GDP ----
# URL of the page
url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
table <- html_nodes(page, "table") %>% .[[3]]
country_gdp <- html_table(table, fill = TRUE)
country_gdp  <- country_gdp[, c(1, 3)][-1, ]
names(country_gdp) <- c('country', 'gdp2022_mill_usd')

country_gdp %>%
  mutate(
    gdp2022_mill_usd = gsub(",", "", gdp2022_mill_usd),
    gdp2022_mill_usd = as.numeric(gdp2022_mill_usd)
  )  ->
  country_gdp


# State Area -----

# URL of the page
url <-
  'https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area'

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
table <- html_nodes(page, "table") %>% .[[1]]
state_area <- html_table(table, fill = TRUE)
state_area <- state_area[, c(1, 2)][-1, ]
names(state_area) <- c('state', 'area_mi2')

state_area %>%
  mutate(area_mi2 = gsub(",", "", area_mi2),
         area_mi2 = as.numeric(area_mi2)) ->
  state_area




# Country Area -----

# URL of the page
url <-
  'https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area'

# Read HTML content from the URL
page <- read_html(url)

# Find the correct table. Usually, you'd use the selector or position of the table.
table <- html_nodes(page, "table") %>% .[[2]]
country_area <- html_table(table, fill = TRUE)
country_area <- country_area[, c(2, 3)]
names(country_area) <- c('country', 'area')

country_area %>%
  # Split the column into two
  mutate(area_mi2 = str_split_i(area, " ", 2))  %>%
  select(country, area_mi2) %>%
  mutate(area_mi2 = as.numeric(str_remove_all(area_mi2, "[,()]"))) ->
  country_area



# Bring Countries Together ----
country_area %>%
  inner_join(country_pop) %>%
  inner_join(country_gdp) ->
  country_data

country_data %>%
  write_csv('data/country_data.csv')

# Brings states together ---
state_area %>%
  inner_join(state_pop) %>%
  inner_join(state_gdp) ->
  state_data

state_data %>%
  write_csv('data/state_data.csv')


## test differences -----
country_data %>%
  arrange(pop2023) %>%
  filter(country == 'Ireland') %>%
  select(-country) ->
  ref

# diffs --------------------

state_data %>%
  mutate(
    area_diff = ((area_mi2 - ref$area_mi2[1]) / ref$area_mi2[1]),
    pop_diff = ((pop2023 - ref$pop2023[1]) / ref$pop2023[1]),
    gdp_diff = ((gdp2022_mill_usd - ref$gdp2022_mill_usd[1]) / ref$gdp2022_mill_usd[1]
    ),
    total_diff = abs(area_diff) + (abs(pop_diff) + abs(gdp_diff))
  ) %>%
  arrange(total_diff)  