#persistent activation code for generating access tokens
activationCode <- function(code){
  httr::POST(url = "https://api.fitbit.com/oauth2/token", 
             httr::add_headers("Authorization" = " Basic MjM4M0Q2OjdlYjdlZDQ5NGJjMWFkN2FjMGFlZGE0MWJlODk4Zjhj",
                               "Content-Type" = "application/x-www-form-urlencoded"),
             body = list("clientId" = "2383D6",
                         "grant_type" = "authorization_code",
                         "redirect_uri" = "https://localhost:1410/",
                         "code" = code),
             encode = "form") |> 
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
}

#use access code for generating an access token
accessToken <- function(refresh){
  httr::POST(url = "https://api.fitbit.com/oauth2/token", 
             httr::add_headers("Authorization" = " Basic MjM4M0Q2OjdlYjdlZDQ5NGJjMWFkN2FjMGFlZGE0MWJlODk4Zjhj",
                               "Content-Type" = "application/x-www-form-urlencoded"),
             body = list("clientId" = "2383D6",
                         "grant_type" = "refresh_token",
                         "refresh_token" = refresh),
             encode = "form") |> 
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
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

refrToken <- activationCode("24fd274a42962b8db197a353709dcf5a5890d1d8")
actiToken <- accessToken(refrToken$refresh_token)
getData(actiToken$access_token, startDate = "2021-12-20")
