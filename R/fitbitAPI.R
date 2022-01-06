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
  
  activities <- lapply(c("steps", 
                         "heart", 
                         "minutesSedentary", 
                         "minutesLightlyActive", 
                         "minutesFairlyActive", 
                         "minutesVeryActive"), \(x){
             Sys.sleep(.2)
             url <- paste0("https://api.fitbit.com/1/user/-/activities/",
                           x,
                           "/date/",
                           startDate,
                           "/",
                           finishDate,
                           ".json")
             x <- httr::GET(url = url, httr::add_headers("Authorization" = paste("Bearer", token))) |>
               httr::content(as = "text") |>
               jsonlite::fromJSON(simplifyVector = TRUE)
             x[[1]]
           })
  
  names(activities) <- c("steps", 
                         "heart", 
                         "minutesSedentary", 
                         "minutesLightlyActive", 
                         "minutesFairlyActive", 
                         "minutesVeryActive")
  
  url <- paste0("https://api.fitbit.com/1.2/user/-/sleep/date/",
                startDate,
                "/",
                finishDate,
                ".json")
  activities$sleep <- httr::GET(url = url, httr::add_headers("Authorization" = paste("Bearer", token))) |>
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
  
  activities <- sapply(names(activities), \(act){
    if(!is.null(dim(activities[[act]]))){#sleep returns null sometimes
      colnames(activities[[act]])[-1] <- act
      activities[[act]]
    }else{
      activities[[act]] <- data.frame(dateTime = activities[[1]][,1])
    }
  })
  
  activities <- cbind(activities$heart, Reduce(, activities[-2]))
}
