library(tidyverse)
library(httr2)
library(here)
wanted <- "KUB Datalab"



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

# hent_kalendere ----
kalendere <- modify_url(
  url = "https://kubkalender.kb.dk",
  path = c("1.1", "calendars")
) %>% 
  GET(add_headers('Authorization' = paste("bearer", token))) %>% 
  content() %>% 
  as_tibble()

# Henter de 500 første events i year for kalenderen med id'et calid
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

# get_calid ----
# Nu finder vi så det relevante calid vi skal bruge
kalendere <- kalendere %>%
  unnest_wider(calendars) %>%
  unnest_wider(url) %>%
  unnest_wider(owner, names_repair = "universal") %>%
  rename(kalender_navn = name...2,
         ejer_id = id,
         ejer_navn = name...7)

kalender_id <- kalendere %>%
  select(calid, kalender_navn, public) %>%
  filter(kalender_navn == wanted) %>% 
  .[["calid"]]

i <- year(today())
data <- get_events(kalender_id, i) %>% 
  as_tibble() 


data %>% unnest_wider(events) %>% view()
test_event <- 4299895
event_id <- 4299895

get_signup_details <- function(event_id){
  modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events", test_event, "registrations"),
    query = list(
      waitlist = 1,
      custom_answers = 1
      
    )
  ) %>% 
  GET(, add_headers('Authorization' = paste("bearer", token))) %>% 
    content(as = "text", type = "application/json") %>% 
    jsonlite::fromJSON() %>% 
    as_tibble()  %>% 
    pivot_longer(cols = registrants:waitlist, names_to = "tilm_type", values_to = "values") %>% 
    unnest_wider(values,
                 simplify = TRUE) %>% 
    unnest_longer(col = -2) %>% 
    unnest_wider(answers) %>% 
    
    unnest_longer(qid:answer)
  
}


get_signup_details(event_id)






