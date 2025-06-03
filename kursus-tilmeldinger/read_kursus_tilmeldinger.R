# Indlæser de tilmeldingsfiler vi gemmer. under generering af fillisterne kan
# det godt gøres lidt mere fikst. 
# 
# Ekstraherer hvad der skal ekstraheres i tre csv-filer. 
# tilmeldinger, med hashet mailadresse og bedste estimat på køn (her er det 
# faktisk et spektrum - sandsynligheden for at personen er en mand, fra 0 til 1).
# Presenter der indeholder oplysninger om hvem der underviste, for der kan være 
# mere end en.
# Og
# metadata, med kursusnavn, gruppe, antal pladser, dato etc.


# biblioteker -------------------------------------------------------------


library(tidyverse)
library(readxl)
library(digest)

# genererer fillister -------------------------------------------------
# bør gøres generisk

files <- dir("data-raw/kursus_tilmeldinger/", full.names = TRUE, recursive = TRUE)

filer <-   tibble(filename = files) %>% 
  mutate(year = str_extract(filename, "(?<=Tilmeldinger )(\\d{4})")) %>% 
  mutate(kursus_id = str_extract(filename, "(?<=_)(\\d*)(?=_)")) %>% 
  mutate(file_id = str_extract(filename, "(?<=_)(\\d*)(?=\\.csv)"))
  
# vi skal have oplysning om venteliste eller deltagerliste
# filer %>% filter(str_detect(filename, "lc_attendees|wait_list"))


# minikonferencen.... ------------------------------------------------
# minikonference er et snefnug...
# og skal derfor håndteres separat

skrammel <- filer %>% 
  filter(str_detect(filename, "minikonference"))



# så har vi dem der følger formen. Vi smider oplysninger om venteliste ind 
# med det samme.
tilmeldings_data <- filer %>% 
  filter(str_detect(filename, "lc_attendees|wait_list")) %>% 
  mutate(tilm_type = case_when(
    str_detect(filename, "lc_attendees") ~ "tilmeldt",
    str_detect(filename, "wait_list") ~ "venteliste",
    .default = NA_character_
  )) 

# funktion til at indlæse og splitte filerne i to efter den tomme linie
read_files <- function(path){
  lines <- readLines(path)
  empty_index <- which(nchar(trimws(lines)) == 0)
  if(length(empty_index) == 0) {
    print(path)
    stop("Filen indeholder ingen tomme linie!")
  }
  split_index <- empty_index[1]
  
  # Del filen op i to dele
  del1 <- lines[1:(split_index - 1)]
  del2 <- lines[(split_index + 1):length(lines)]
  return(list("meta" = del1, "data" = del2))
}

# Så mapper vi, og unnester.
tilmeldings_data <- tilmeldings_data %>% 
  mutate(noget = map(filename, read_files)) %>% 
  unnest_wider(noget)


# og deler op efter om det er metadata eller tilmeldinger.
metadata <- tilmeldings_data %>% 
  select(-data)

tilmeldings_data <- tilmeldings_data %>% 
  select(-meta)

# Vi har nu en tibble med tilmeldingerne. Den behandler vi videre
# bl.a. hasher vi mailadresserne som et forsøg på at identificere den studerende.
# fornavne er ladt tilbage - for jeg har ikke skrevet hvad der skal til for 
# at gætte på kønnet.

tilmeldings_data <- tilmeldings_data %>%
  mutate(data = map(data, ~ read_csv(paste(.x, collapse= "\n"), show_col_types = FALSE))) %>% 
  unnest_longer(data) %>% 
  unnest_wider(data) %>% 
  janitor::clean_names() %>% 
  mutate(kursus_id = str_extract(filename, "(?<=_)(\\d*)(?=_)"))




tilmeldings_data <- tilmeldings_data %>% 
  select(-last_name) %>%
  rowwise() %>% 
  mutate(pid = digest(email, algo = "md5")) %>% 
  ungroup() %>% 
  select(-email) %>% 
  rename(fornavn = first_name,
         faculty = "x1_faculty",
         niveau = x3_programme_level,
         erfaring = x4_level_of_experience_in_regards_to_course_subject,
         os = x5_which_operating_system_do_you_use,
         kontakt_tilladelse = x7_may_we_contact_you_in_regards_to_future_courses_and_events,
         gdpr = x8_i_hereby_consent_to_the_royal_danish_librarys_privacy_and_personal_data_protection_policy_http_www_kb_dk_en_kb_webstedet_cookiepolitik_html,
         kilde = x6_where_did_you_hear_about_this_event, 
         udd = x2_programme_line_of_study_if_not_applicable_please_write_none)


# Og så metadata

meta_ekstraher <- function(df){
  df[-1]
  
}

metadata <- metadata %>%
  mutate(meta = map(meta, meta_ekstraher)) %>% 
  mutate(meta = map(meta, ~ read_csv(paste(.x, collapse= "\n"), show_col_types = FALSE, col_names = FALSE)))

metadata <- metadata %>% 
  unnest_wider(meta) %>% 
  unnest_longer(c(X1, X2)) %>% 
  mutate(kursus_id = str_extract(filename, "(?<=_)(\\d*)(?=_)"))



# separat presenter tabel

presenter <- metadata %>% 
  filter(X1 == "Presenter") %>% 
  separate_longer_delim(X2, delim = ",") %>% 
  separate_longer_delim(X2, delim = "&") %>% 
  mutate(X2 = str_trim(X2)) %>% 
  select(filename, presenter = X2, kursus_id) 

# Og så fjerner vi Presenter fra metadata

metadata <- metadata %>% 
  filter(X1 != "Presenter")


# Vi skal også have pillet dato og tid ud separat.

date_time <- metadata %>% 
  filter(X1 %in% c("From", "To", "Date & Time")) %>% 
  mutate(dato = str_extract(X2, "\\d{2}/\\d{2}/\\d{4}")) %>% 
  mutate(X2 = str_remove(X2, "\\d{2}/\\d{2}/\\d{4}")) %>% 
  mutate(X2 = str_remove(X2, ","))


date_time <- bind_rows(
  date_time %>% 
    filter(X1 == "Date & Time") %>% 
    separate_wider_delim(cols = X2, names = c("From", "To"), delim = "-"),
  date_time %>% 
    filter(X1 != "Date & Time") %>% 
    pivot_wider(names_from = X1, values_from = X2)
)
date_time <- date_time %>%
  select(-X1) %>% 
  mutate(across(From:To, str_trim)) %>% 
  mutate(dato = dmy(dato)) %>% 
  rename(from = From)

# Og så metadata uden date_time

metadata <- metadata %>% 
  filter(!(X1 %in% c("From", "To", "Date & Time")))

metadata <- metadata %>% 
  pivot_wider(names_from = X1, values_from = X2) %>% 
  select(-"Anticipated Attendance") %>% 
  mutate(gruppe = case_when(str_detect(Event, "Python") ~ "python",
                          str_detect(Event, "NVivo") ~ "nvivo",
                          str_detect(Event, "Orange") ~ "orange",
                          str_detect(Event, "R") ~ "r",
                          .default  = NA)) %>% 
  mutate(online = str_detect(Location, "Online"))




metadata <- metadata %>% 
  distinct(kursus_id, .keep_all = TRUE)



date_time <- date_time %>% distinct(kursus_id, .keep_all = TRUE)



# Her skal vi have håndteret at der er dubletter. 


metadata <- metadata %>% 
  left_join(date_time) 

# Hm. Vi skal nok have harmoniseret lokationerne... Men stadig?


# lad os også få køn på tilmeldingerne.
# Husk at navnene i sex_dist.csv er med stort.
# og at vi har alle fornavne med i tilmeldinger.

koen <- read_csv2("data/sex_dist.csv")


tilmeldings_data <- tilmeldings_data %>%
  mutate(fornavn = word(fornavn, 1)) %>% 
  mutate(fornavn = toupper(fornavn)) %>% 
  left_join(koen, by=c("fornavn" = "Navn")) %>% 
  select(-c(fornavn, male, female)) %>% 
  mutate(sex = if_else(sex<0.5, "f", "m"))



write_csv2(tilmeldings_data, "data/tilmeldingsdata/tilmeldinger.csv")
write_csv2(presenter, "data/tilmeldingsdata/presenter.csv")
write_csv2(metadata, "data/tilmeldingsdata/metadata.csv")

