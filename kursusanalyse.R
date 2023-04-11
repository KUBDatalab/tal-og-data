# https://kbintern-my.sharepoint.com/:x:/r/personal/mufa_kb_dk/_layouts/15/Doc.aspx?sourcedoc=%7B547F647B-BAC1-41C6-BF6E-9CA226C09D7F%7D&file=KUB%20Datalab%20Undervisningsstatistik.xlsx&action=default&mobileredirect=true
library(tidyverse)
library(readxl)
data <- read_excel("../Documents/KUB Datalab Undervisningsstatistik.xlsx")
data
names(data)
view(data)

renamer <- function(t){
  t <- str_replace_all(t, " ", "")
  t <- str_replace_all(t, "\\.", "")
  str_remove_all(t, "/|:|,|\\(|\\)|\\?|-")
}

data <- data %>% 
  rename_with(renamer)



data <- data %>%  
  select(ID, Vælgdato,Uteam,Kursustitelevt,
         Kursustype,Fakultet,Antaldeltagere,Hvemvarundervisere,
         VarkursusindlejretaftaltmedKUellervardetetåbentkursusevtetforskerservicekursus) %>% 
  filter(Uteam == "KUB DATALAB") %>% 
  select(-Uteam) %>% 
  rename(aic = VarkursusindlejretaftaltmedKUellervardetetåbentkursusevtetforskerservicekursus) %>% 
  select(-Kursustype) 

view(data)

data %>% 
  filter(aic == "Aftalt med KU") %>% 
  mutate(aar = year(Vælgdato)) %>%
  group_by(Fakultet, aar) %>% 
  summarise(antal = sum(Antaldeltagere)) %>% 
  pivot_wider(names_from = aar, values_from = antal, values_fill = 0)


data %>% 
  mutate(aar = year(Vælgdato)) %>% 
  group_by(aic, aar) %>% 
  summarise(antal = sum(Antaldeltagere)) %>% 
  pivot_wider(names_from = aar, values_from = antal, values_fill = 0)


data %>% 
  mutate(aar = year(Vælgdato)) %>% 
  select(-c(Vælgdato, Kursustitelevt, Fakultet, aic)) %>% 
  mutate(underviser = str_split(Hvemvarundervisere, ",|;")) %>% 
  select(-Hvemvarundervisere) %>% 
  unnest(underviser) %>% 
  mutate(underviser = str_remove_all(underviser, "\\[")) %>%  
  mutate(underviser = str_remove_all(underviser, "\\]")) %>% 
  mutate(underviser = str_remove_all(underviser, '\\"')) %>% 
  mutate(underviser = str_remove_all(underviser, '\\)')) %>% 
  mutate(underviser = str_remove_all(underviser, '\\(')) %>% 
  mutate(underviser = str_remove_all(underviser, 'ekstern')) %>% 
  mutate(underviser = str_squish(underviser)) %>% 
  filter(underviser != "") %>% 
  group_by(underviser, aar) %>% 
  summarise(antal = sum(Antaldeltagere)) %>% 
  pivot_wider(names_from = aar, values_from = antal, values_fill = 0) %>% 
  arrange(desc(`2023`))



data %>% select(ID, Vælgdato, Antaldeltagere, Hvemvarundervisere) %>% 
  mutate(underviser = str_split(Hvemvarundervisere, ",|;")) %>% 
  select(-Hvemvarundervisere) %>% 
  unnest(underviser) %>% 
  mutate(underviser = str_remove_all(underviser, "\\[")) %>%  
  mutate(underviser = str_remove_all(underviser, "\\]")) %>% 
  mutate(underviser = str_remove_all(underviser, '\\"')) %>% 
  mutate(underviser = str_remove_all(underviser, '\\)')) %>% 
  mutate(underviser = str_remove_all(underviser, '\\(')) %>% 
  mutate(underviser = str_remove_all(underviser, 'ekstern')) %>% 
  mutate(underviser = str_squish(underviser)) %>% 
  filter(underviser != "") %>% 
  mutate(aar = year(Vælgdato)) %>% 
  group_by(underviser, aar) %>% 
  summarise(antal = sum(Antaldeltagere)) %>% 
  pivot_wider(names_from = aar, values_from = antal, values_fill = 0) %>% 
  View()
  
