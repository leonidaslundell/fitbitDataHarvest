#persistent activation code for generating access tokens
activationCode <- function(code, clientID, auth){
  x <- httr::POST(url = "https://api.fitbit.com/oauth2/token", 
                  httr::add_headers("Authorization" = auth,
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
accessToken <- function(refresh, clientID, auth){
  x <- httr::POST(url = "https://api.fitbit.com/oauth2/token", 
             httr::add_headers("Authorization" = auth,
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
