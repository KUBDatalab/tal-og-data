library(tidyverse)
library(httr2)
library(httr)
library(here)
cal_id <- 6416L

# har pt 1749 rækker i metadata. Efter dagens åbne værksted, skal der gerne være
# mindst en mere. som er fra 2. juli
# find dog også ud af hvorfor id 4216763 fører til 10 rækker?


meta_data <- read_csv2("data-raw/kursus_metadata.csv")

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


# Henter de 500 første events i year for kalenderen med id'et calid, fra for
# en måned siden, og et år frem. 
# returnerer liste med events.
get_events <- function(calid, year){
  dato <- today() %m-% months(1)
  dato <- as.character(dato)
  
  url <- modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events"),
    query = list(
      cal_id = calid,
      date = dato,
      days = 365,
      limit = 500
    )
  )
  GET(url, add_headers('Authorization' = paste("bearer", token))) %>% 
    content()
}


i <- year(today())
data <- get_events(kalender_id, i) %>% 
  as_tibble() 


nye_meta_data <- data %>% unnest_wider(events) %>% 
  select(-c(future_dates)) %>% 
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
  unnest_wider(any_of("audience"), names_sep = "_")  %>% 
  unnest_wider(any_of("audience_1"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_2"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_3"), names_sep = "_") %>% 
  mutate(seats = map(seats, ~ as.numeric(.x))) %>% 
  unnest(seats) %>% 
  unnest_longer(category) %>% 
  unnest_wider(category, names_sep = "_") 



nye_meta_data <- nye_meta_data %>% filter(!(id %in% meta_data$id)) %>% 
bind_rows(nye_meta_data, meta_data) %>% 
  write_csv2("data-raw/kursus_metadata.csv")
