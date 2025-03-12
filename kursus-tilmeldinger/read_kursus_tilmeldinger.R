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


library(tidyverse)
library(readxl)
library(digest)

# genererer fillister

tilm_2024 <- dir("data-raw/kursus_tilmeldinger/Tilmeldinger 2024/", full.names = TRUE)
tilm_2025 <- dir("data-raw/kursus_tilmeldinger/Tilmeldinger 2025/", full.names = TRUE)

# samler dem i tibbler
tilm_2024 <- tibble(year = 2024, filename = tilm_2024)
tilm_2025 <- tibble(year = 2025, filename = tilm_2025)

# samler i en tibble og rydder op
tilmeldinger <- bind_rows(tilm_2024, tilm_2025)
rm(tilm_2024, tilm_2025)

# minikonference er et snefnug...
# og skal derfor håndteres separat

skrammel <- tilmeldinger %>% 
  filter(!str_detect(filename, "lc_attendees"))

# så har vi dem der følger formen
tilmeldinger <- tilmeldinger %>% 
  filter(str_detect(filename, "lc_attendees"))


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
tilmeldinger <- tilmeldinger %>% 
  mutate(noget = map(filename, read_files)) %>% 
  unnest_wider(noget)

# og deler op efter om det er metadata eller tilmeldinger.
metadata <- tilmeldinger %>% 
  select(-data)

tilmeldinger <- tilmeldinger %>% 
  select(-meta)

# Vi har nu en tibble med tilmeldingerne. Den behandler vi videre
# bl.a. hasher vi mailadresserne som et forsøg på at identificere den studerende.
# fornavne er ladt tilbage - for jeg har ikke skrevet hvad der skal til for 
# at gætte på kønnet.

tilmeldinger <- tilmeldinger %>%
  mutate(data = map(data, ~ read_csv(paste(.x, collapse= "\n"), show_col_types = FALSE))) %>% 
  unnest_longer(data) %>% 
  unnest_wider(data) %>% 
  janitor::clean_names()


tilmeldinger <- tilmeldinger %>% 
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
  unnest_longer(c(X1, X2))



# separat presenter tabel

presenter <- metadata %>% 
  filter(X1 == "Presenter") %>% 
  separate_longer_delim(X2, delim = ",") %>% 
  separate_longer_delim(X2, delim = "&") %>% 
  mutate(X2 = str_trim(X2)) %>% 
  select(filename, presenter = X2) 

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
  mutate(dato = dmy(dato))

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
  left_join(date_time)

# lad os også få køn på tilmeldingerne.
# Husk at navnene i sex_dist.csv er med stort.
# og at vi har alle fornavne med i tilmeldinger.

koen <- read_csv2("data/sex_dist.csv")


tilmeldinger <- tilmeldinger %>%
  mutate(fornavn = word(fornavn, 1)) %>% 
  mutate(fornavn = toupper(fornavn)) %>% 
  left_join(koen, by=c("fornavn" = "Navn")) %>% 
  select(-c(fornavn, male, female)) %>% 
  mutate(sex = if_else(sex<0.5, "f", "m"))


write_csv2(tilmeldinger, "data/tilmeldingsdata/tilmeldinger.csv")
write_csv2(presenter, "data/tilmeldingsdata/presenter.csv")
write_csv2(metadata, "data/tilmeldingsdata/metadata.csv")


