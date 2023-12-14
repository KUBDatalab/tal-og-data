## Nyttige funktioner til at servicere 

library(purrr)
library(lubridate)
library(tibble)  
library(rmarkdown)
library(here)
library(ggplot2)
library(plotly)
library(knitr)
library(tidyr)
library(dplyr)
library(httr)
library(readr)

# Get token
# returnerer et access-token. Tager client_secret som input.
get_token <- function(client_secret){
  token_endpoint <- "https://kubkalender.kb.dk/1.1/oauth/token"
  client_id <- "110"
  POST(token_endpoint,
                body = list(grant_type = "client_credentials",
                            client_id = client_id,
                            client_secret = client_secret)) %>% 
    content() %>% 
    .[["access_token"]]
  
}


# Henter de 500 første events i year for kalenderen med id'et calid
# returnerer liste med events.
get_events <- function(calid, year){
    dato <- today %m-% months(1)
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

# Henter detaljer om et givet event id
get_event_details <- function(id){
  url <- modify_url(
      url = "https://kubkalender.kb.dk",
      path = c("api", "1.1", "events", id, "registrations"),
      query = list(
        waitlist = 1
      )
  )
  GET(url, add_headers('Authorization' = paste("bearer", token))) %>% 
    content()
}

# Returnerer sandsynligheden for at der er tale om en mand, givet et fornavn
# Vi har antallet af kvinder og mænd med et givet fornavn.
# de er trukket fra Danmarks Statistik pr. 20210101
# For hvert fornavn har vi antallet af kvinder, antallet af mænd, og den andel
# af mænd der har det givne navn.
# der er 6 mænd der har navnet "Anne" og 44872 kvinder der har navnet.
# Har vi en kursist med fornavnet "Anne", er sandsynligheden for at der er tale
# om en mand, 0.000134
sex_distr <- read_csv2("data/sex_dist.csv")
get_sex_prob <- function(name){
  name <- toupper(name)
  name <- str_extract(name, pattern = "(.+?)\\b")
  if(!(name %in% sex_distr$Navn)){
    return(NA)
  }else{
    sex_distr[sex_distr$Navn == name,]$sex
  }
}
get_sex_prob <- Vectorize(get_sex_prob)

# henter detaljerede, men hashede og anonyme data på deltagere
# både dem der er tilmeldt og de der er på venteliste.
# for et givet event-id
get_registrants <- function(id){
  skabelon <- tibble(event_id=integer(0),
                     booking_id = integer(0),
                     registration_type = character(0),
                     email = character(0),
                     registered_date = character(0),
                     attendance = character(0),
                     liste = character(0),
                     køn = double(0))  
  
  detaljer <- get_event_details(id) %>% 
    as_tibble_col() %>% 
    unnest_wider("value") 
  
  registrants <- detaljer %>% select(event_id, registrants)
  waitlist <- detaljer %>% select(event_id, registrants = waitlist)
  
  if(is.na(registrants$registrants)){
    registrants <- skabelon
  } else{
    registrants <- registrants %>% 
      unnest_longer("registrants") %>% 
      unnest_wider("registrants") %>% 
      mutate(email=as.character(md5(email)),
             køn = get_sex_prob(first_name),
             liste = "deltager") %>% 
      select(event_id, booking_id, registration_type, email, registered_date,
             attendance, liste, køn)
  }
  if(is.na(waitlist$registrants)){
    waitlist <- skabelon
  } else {
    waitlist <- waitlist %>% 
      unnest_longer("registrants") %>% 
      unnest_wider("registrants") %>% 
      mutate(email=as.character(md5(email)),
             køn = get_sex_prob(first_name),
             liste = "venteliste") %>% 
      select(event_id, booking_id, registration_type, email, registered_date,
             attendance, liste, køn)
  }
  
  list(rbind(registrants,waitlist))
}
