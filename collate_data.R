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
# settings
wanted <- "KUB Datalab"
first_year <- 2018 # Det første år vi interesserer os for
aktuelt_year <- as.numeric(substr(Sys.Date(), 1, 4))

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



# samler events ------
# Opdaterer allerede gemte events for aktuelt år
data_filer <- dir("data/event_data/")
if(paste0("events_", aktuelt_year, ".csv") %in% data_filer){
  past_events <- read_csv2(file=paste0("data/event_data/events_", aktuelt_year, ".csv"))
  aktuelt_events <- aktuelt_events %>% mutate(online_join_password = as.character(online_join_password))  # udfordringer med inkonsistent numeric vs character password.
  past_events <- past_events %>% mutate(online_join_password = as.character(online_join_password))
  aktuelt_events <- bind_rows(aktuelt_events, past_events) %>% distinct()
}
aktuelt_events %>% 
  write_csv2(file=paste0("data/event_data/events_",i,".csv"))


# indlæs all_events ----
# indlæser alle events registreret for alle år
all_events <- list.files("data/event_data/", 
                         pattern = "events_\\d{4}\\.csv",
                         full.names = T) %>% 
  map_df(~read.csv2(.))



# liste over events der skal hentes detaljer for ----
# Nu trækker vi så de filer der skal gemmes. Først laver vi 
# en liste af alle de events vi godt vil have data på

alle_events <- all_events %>% 
  select(id, end) %>% 
  mutate(maaned = month(end),
         aar = year(end)) %>% 
  mutate(maaned = str_pad(maaned, 2, side="left", pad="0")) %>% 
  mutate(periode = str_c("event_details_", aar,"_", maaned, ".csv")) %>% 
  distinct()

# hent eksisterende event_data ----
# Nogen af dem er der allerede:

hvilke_er_der_allerede <- list.files("data/event_data/")

# hent current events ----
# Så er der nogen vi ikke ønsker at gemme filer på (endnu).
# Det er indeværende måned, og alle fremtidige måneder:
current_events <- alle_events %>% 
  filter(end >= as_datetime(floor_date(now(), 'month')))


# de events vi godt vil have gemt ----
# Det her er så de events vi godt vil have gemt - hvis ikke de allerede 
# er det:
old_events <- setdiff(alle_events, current_events)

# de events vi godt vil have gemt som ikke er gemt endnu ----
# Og her har vi dem vi faktisk vil have gemt - fordi de ikke er gemt
# allerede.
de_der_skal_gemmes <- old_events %>% 
  filter(!(periode %in% hvilke_er_der_allerede))


# gem details ----
# Dem looper vi så igennem, og får gemt data:
for(peri in unique(de_der_skal_gemmes$periode)){
  de_der_skal_gemmes %>% 
    filter(periode == peri) %>% 
    pull(id) %>% 
    map_dfr(., get_registrants) %>% 
    write_csv2(file = paste0("data/event_data/", peri))
}

# hent aktuelle details ----

# Nu mangler vi så at få hentet data på de aktuelle events.
# De der er i indeværende måned - og fremtidige.
# 
# for at få ensartet data indlæst - trækker vi detaljer på 
# current events, og gemmer som fil.
current_events %>% 
  pull(id) %>% 
  map_dfr(., get_registrants) %>% 
  write_csv2(file = "data/event_data/current_events_details.csv")

# liste over alle filer med detaljer ----
# Så henter vi listen over alle event detaljer filerne, og 
# tilføjer current_events.csv, som vi lige lavede:
filer <- list.files("data/event_data/",
                    pattern = "event_details_\\d{4}_\\d{2}\\.csv",
                    full.names = T)

filer <- c(filer, "data/event_data/current_events_details.csv")

# samler detaljer ----
# Og så indlæser vi alle eventsdetaljefilerne
all_events_details <- filer %>% 
  map_df(~read_csv2(., col_types = "ddccTccd"))


# skriver detaljer og indlæser ---- 
# Og gemmer den som fil - mest så vi efterfølgende kan trække den
# ud og arbejde med den lokalt.
write_csv2(all_events_details, file = "data/event_data/all_event_details.csv")

# rydder environment
rm(list=ls())

# Genindlæser relevant data
master_data <- read_csv2("data/event_data/all_event_details.csv")
all_events <- list.files("data/event_data/", 
                         pattern = "events_\\d{4}\\.csv",
                         full.names = T) %>% 
  map_df(~read.csv2(.))
