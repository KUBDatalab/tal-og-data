## Nyttige funktioner til at servicere 

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
  dato <- paste0(year, "-01-01")
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