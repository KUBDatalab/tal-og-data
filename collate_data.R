library(purrr)
library(lubridate)
library(tibble)  
library(plotly)
library(rmarkdown)
library(here)
library(stringr)
library(ggplot2)
library(plotly)
library(knitr)
library(tidyr)
library(dplyr)
library(httr) 
library(readr)
library(openssl)
library(zoo)


# Indlæser script med nyttige funktioner
source("funcs.R", encoding = "UTF8") # her samler vi nyttige funktioner

# get client secret ----
if(here::here() == "C:/Users/cbk/Documents/R_projekter/tal-og-data"){
  client_secret <- keyring::key_get("libcal")
}else{
  client_secret <- Sys.getenv("CLIENT_SECRET")
}

# get_token ----
token <- get_token(client_secret = client_secret)

# hent_kalendere ----
kalendere <- modify_url(
  url = "https://kubkalender.kb.dk",
  path = c("1.1", "calendars")
) %>% 
  GET(add_headers('Authorization' = paste("bearer", token))) %>% 
  content() %>% 
  as_tibble()


# get_calid ----
# Nu finder vi så det relevante calid vi skal bruge
kalendere <- kalendere %>%
  unnest_wider(calendars) %>%
  unnest_wider(url) %>%
  unnest_wider(owner, names_repair = "universal") %>%
  rename(kalender_navn = name...2,
         ejer_id = id,
         ejer_navn = name...6)

kalender_id <- kalendere %>%
  select(calid, kalender_navn, public) %>%
  filter(kalender_navn == wanted) %>% 
  .[["calid"]]

# hent_nye_events ----
# Hvis data for events for foregående år ligger i systemet allerede.
# Så skal vi have fat på det aktuelle år.
# Vi skriver til csv - så bliver den gamle og den nye csv-fil nemlig 
# behandlet ens.
i <- aktuelt_year
get_events(kalender_id, i) %>% 
  as_tibble() %>% 
  unnest_wider(events) %>%
  unnest_wider(url, names_sep="_") %>%
  unnest_wider(location, names_sep="_") %>%
  unnest_wider(campus, names_sep="_") %>%
  unnest_wider(owner, names_sep= "_") %>%
  unnest_wider(calendar, names_sep = "_") %>% 
  unnest_longer(category) %>% 
  unnest_wider(category, names_sep = "_") %>% 
  select(-future_dates, -seats, -geolocation) %>% 
  mutate(start = as_datetime(start),
         end = as_datetime(end)) %>% 
  select(-registration_cost) %>% 
  write_csv2("data/event_data/aktuelt_events.csv")
aktuelt_events <- read_csv2("data/event_data/aktuelt_events.csv")
