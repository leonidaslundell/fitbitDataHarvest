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
                         "minutesVeryActive"), \(act){
             Sys.sleep(.2)
             url <- paste0("https://api.fitbit.com/1/user/-/activities/",
                           act,
                           "/date/",
                           startDate,
                           "/",
                           finishDate,
                           ".json")
             act <- httr::GET(url = url, httr::add_headers("Authorization" = paste("Bearer", token))) |>
               httr::content(as = "text") |>
               jsonlite::fromJSON(simplifyVector = TRUE)
             act[[1]]
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
  
  #heart data json response needs separate handlening
  heart <- activities$heart
  # keep <- heart
  # heart <- keep
  # names(heart$value$heartRateZones) <- heart$dateTime
  dateTime <- heart$dateTime
  heart <- lapply(c("Out of Range",
                    "Fat Burn",
                    "Cardio",
                    "Peak"), \(property){
                      propertyInt <- sapply(heart$value$heartRateZones, \(day){
                        x <- day[day$name == property, -3] 
                        colnames(x) <-  paste0(colnames(x)," ", property)
                        x
                      })
                      t(propertyInt)
                    })
  heart <- Reduce(cbind, heart)
  
  #recast the data for one dataframe
  activities <- activities[!grepl("heart", names(activities))]
  activities <- sapply(names(activities), \(act){
    if(!is.null(dim(activities[[act]]))){#sleep returns null sometimes
      colnames(activities[[act]])[-1] <- act
      activities[[act]]
    }else{
      activities[[act]] <- data.frame(dateTime = activities[[1]][,1])#ugly hack for sleep being null
    }
  })
  
  activities <- Reduce(cbind, activities)
  activities <- activities[,!grepl("dateTime", colnames(activities))]
  activities <- cbind(dateTime, heart, activities)
  activities
}
