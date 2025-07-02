# Henter historiske events skal kun køre en gang.

library(tidyverse)
library(httr2)
library(httr)
library(here)


cal_id <- 6416L



if(here::here() == "C:/Users/cbk/Documents/R_projekter/tal-og-data"){
  client_secret <- keyring::key_get("libcal")
}else{
  client_secret <- Sys.getenv("CLIENT_SECRET")
}

# Get token
# returnerer et access-token. Tager client_secret som input.
get_token <- function(client_secret){
  token_endpoint <- "https://kubkalender.kb.dk/1.1/oauth/token"
  client_id <- "110"
  token <- POST(token_endpoint,
                body = list(grant_type = "client_credentials",
                            client_id = client_id,
                            client_secret = client_secret)) %>% 
    content() 
  token[["access_token"]]
  
}

# get_token ----
token <- get_token(client_secret = client_secret)




# Henter de 500 første events i year for kalenderen med id'et calid
# returnerer liste med events.
get_events <- function(calid, year){
  dato <- ymd(paste0(year, "-01-01"))
  dato <- as.character(dato)
  
  url <- modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events"),
    query = list(
      cal_id = calid,
      date = dato,
      days = 365,
      limit = 490
    )
  )
  GET(url, add_headers('Authorization' = paste("bearer", token))) %>% 
    content()
}

i <- 2025

data_2025 <- get_events(kalender_id, i)  %>% as_tibble %>% unnest_wider(events) %>% 
  mutate(attendance = NA)


i <- 2024

data_2024 <- get_events(kalender_id, i) %>% as_tibble %>% unnest_wider(events) %>% 
  mutate(attendance = NA)
  

# Af en eller anden årsag optræder kolonnen online_host_url ikke i 2023
i <- 2023
data_2023 <- get_events(kalender_id, i) %>% as_tibble %>% unnest_wider(events) %>% 
  mutate(attendance = NA, online_host_url = NA, audience = NA)

i <- 2022

data_2022 <- get_events(kalender_id, i) %>% as_tibble %>% unnest_wider(events) %>% 
  mutate(audience = NA)

i <- 2021

data_2021 <- get_events(kalender_id, i) %>% as_tibble %>% unnest_wider(events)  %>% 
  mutate(audience = NA) 

meta_data <- bind_rows(data_2021, data_2022, data_2023, data_2024, data_2025)

# 556
meta_data <- meta_data %>% 
  select(-c(attendance, future_dates)) %>% 
    filter(as_datetime(end) < now()) %>% 
  unnest_wider(url) %>% 
  unnest_wider(location, names_sep = "_") %>% 
  # select(-c(id, title, allday, start, end, description, public, admin, location_id,
  #           location_type, location_name)) %>% 
  unnest_wider(campus, names_sep = "_") %>% 
  # select(-c(campus_id, campus_name, campus_1, presenter, registration, registration_form_id,
  #           registration_series_linked, physical_seats_taken, online_seats, physical_seats,
  #           online_seats_taken, wait_list, color, featured_image, seats_taken, 
  #           registration_cost, setup_time, teardown_time, more_info, zoom_email,
  #           online_user_id, online_meeting_id, online_host_url, online_join_url,
  #           online_join_password, online_provider)) %>% 
  unnest_wider(owner, names_sep = "_") %>% 
#  select(-c(has_registration_opened, has_registration_closed))  %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest_wider(calendar, names_sep = "_") %>% 
  #select(-c(calendar_id, calendar_name, calendar_public, calendar_admin, geolocation, owner_id, owner_name)) %>% 
  unnest_wider(audience, names_sep = "_") %>% 
  unnest_wider(audience_1, names_sep = "_") %>% 
  unnest_wider(audience_2, names_sep = "_") %>% 
  unnest_wider(audience_3, names_sep = "_") %>% 
  mutate(seats = map(seats, ~ as.numeric(.x))) %>% 
  unnest(seats) %>% 
  unnest_longer(category) %>% 
  unnest_wider(category, names_sep = "_")

meta_data %>% filter(id == 4216763)

meta_data %>% write_csv2(file = "data-raw/kursus_metadata.csv")
  
  