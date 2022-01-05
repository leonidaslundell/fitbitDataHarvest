#persistent activation code for generating access tokens
activationCode <- function(code, clientID){
  x <- httr::POST(url = "https://api.fitbit.com/oauth2/token", 
                  httr::add_headers("Authorization" = " Basic MjM4M0Q2OjdlYjdlZDQ5NGJjMWFkN2FjMGFlZGE0MWJlODk4Zjhj",
                                    "Content-Type" = "application/x-www-form-urlencoded"),
                  body = list("clientId" = clientID,
                              "grant_type" = "authorization_code",
                              "redirect_uri" = "https://localhost:1410/",
                              "expires_in" = "259200",
                              "code" = code),
                  encode = "form") |> 
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
  x[["refresh_token"]]
}

#use access code for generating an access token
accessToken <- function(refresh, clientID){
  x <- httr::POST(url = "https://api.fitbit.com/oauth2/token", 
             httr::add_headers("Authorization" = " Basic MjM4M0Q2OjdlYjdlZDQ5NGJjMWFkN2FjMGFlZGE0MWJlODk4Zjhj",
                               "Content-Type" = "application/x-www-form-urlencoded"),
             body = list("clientId" = clientID,
                         "grant_type" = "refresh_token",
                         "refresh_token" = refresh,
                         "expires_in" = "31560000"),
             encode = "form") |> 
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
  x[["access_token"]]
}

#get data for 
getData <- function(token, 
                    startDate, 
                    finishDate = "today"){
  url <- paste0("https://api.fitbit.com/1/user/-/activities/steps/date/",
                startDate,
                "/",
                finishDate,
                ".json")
  httr::GET(url = url, httr::add_headers("Authorization" = paste("Bearer", token))) |>
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
}

# refrToken <- activationCode("18f11e2596a6cf799115c7ba41073a6da8f11a0c")
# actiToken <- accessToken("e4e50c7a0ffd1a809426a4710ab99399f621560b08e7997322a69e115ca5bf56")
# getData(actiToken$access_token, startDate = "2021-12-29")
