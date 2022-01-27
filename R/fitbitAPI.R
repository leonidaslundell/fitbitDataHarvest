#https://dev.fitbit.com/apps/oauthinteractivetutorial

#persistent activation code for generating access tokens
activationCode <- function(code, clientID, auth){
  x <- httr::POST(url = "https://api.fitbit.com/oauth2/token", 
                  httr::add_headers("Authorization" = paste0(" Basic ", auth),
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
             httr::add_headers("Authorization" = paste0(" Basic ", auth),
                               "Content-Type" = "application/x-www-form-urlencoded"),
             body = list("clientId" = clientID,
                         "grant_type" = "refresh_token",
                         "refresh_token" = refresh,
                         "expires_in" = "31560000"),
             encode = "form") |> 
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
  x[c("access_token", "refresh_token")]
}

#get data for 
getData <- function(token, 
                    startDate, 
                    finishDate = NULL){
  if(is.null(finishDate))
    finishDate <- Sys.Date()
  
  if(is.null(token))#API returns different for NULL or invalid token
    token <- "Bad token"
  
  activities <- lapply(c("steps", 
                         "heart", 
                         "minutesSedentary", 
                         "minutesLightlyActive", 
                         "minutesFairlyActive", 
                         "minutesVeryActive",
                         "calories"), \(act){
             Sys.sleep(.3)
             url <- paste0("https://api.fitbit.com/1/user/-/activities/",
                           act,
                           "/date/",
                           startDate,
                           "/",
                           finishDate,
                           ".json")
             act <- httr::GET(url = url, 
                              httr::add_headers("Authorization" = paste("Bearer", token))) |>
               httr::content(as = "text") |>
               jsonlite::fromJSON(simplifyVector = TRUE)
             act[[1]]
           })
  
  names(activities) <- c("steps", 
                         "heart", 
                         "minutesSedentary", 
                         "minutesLightlyActive", 
                         "minutesFairlyActive", 
                         "minutesVeryActive",
                         "calories")
  
  url <- paste0("https://api.fitbit.com/1.2/user/-/sleep/date/",
                startDate,
                "/",
                finishDate,
                ".json")
  sleep <- httr::GET(url = url, httr::add_headers("Authorization" = paste("Bearer", token))) |>
    httr::content(as = "text") |>
    jsonlite::fromJSON(simplifyVector = TRUE)
  
  #sleep date is coming out inverted..
  sleep <- data.frame(dateTime = sleep$sleep$dateOfSleep, 
                      sapply(sleep$sleep$levels$summary, \(x) x$minutes))
  
  #handling no data from api
  if(!activities$steps |> dim() |> is.null()){
    #heart data json response needs separate handlening
    heart <- activities$heart
    dateTime <- heart$dateTime
    
    heart <- sapply(c("Out of Range",
                      "Fat Burn",
                      "Cardio",
                      "Peak"), \(property){
                        sapply(heart$value$heartRateZones, \(day){
                          if(is.null(day[day$name == property, "minutes"]))
                            return(NA)
                          else
                            day[day$name == property, "minutes"] 
                        })
                      })#if NULL then no time has been spent in either of these ranges
    
    colnames(heart) <- paste0("Min spent ", colnames(heart))
    
    #recast the data for one dataframe
    activities <- activities[!grepl("heart", names(activities))]
    activities <- lapply(names(activities), \(act){
      if(!is.null(dim(activities[[act]]))){#sleep returns null sometimes
        colnames(activities[[act]])[-1] <- act
        activities[[act]]
      }else{
        activities[[act]] <- data.frame(dateTime = activities[[1]][,1])#ugly hack for sleep being null
      }
    })
    
    activities <- Reduce(cbind, activities)
    activities <- activities[,!grepl("dateTime", colnames(activities))]
    
    #reformat sleep for merging with activities
    sleep <- sleep[match(dateTime, sleep$dateTime), -1]
    
    activities <- cbind(dateTime, heart, activities, sleep)
    activities[is.na(activities)] <- "No data"
  }else{
    activities <- data.frame()
  }
  
  return(activities)
}

#check the output shape 

integrityCheck <- function(df, token){
  comp <- matrix("error", ncol = 16, nrow = 1) |> as.data.frame()
  colnames(comp) <- c("dateTime",
                      "Min spent Out of Range",
                      "Min spent Fat Burn",
                      "Min spent Cardio",
                      "Min spent Peak",
                      "steps",
                      "minutesSedentary",
                      "minutesLightlyActive",
                      "minutesFairlyActive",
                      "minutesVeryActive",
                      "calories",
                      "deep",
                      "light",
                      "rem",
                      "wake",
                      "token")
  df$token <- token
  if(all(colnames(comp) %in% colnames(df))){
    df[,colnames(comp)]#it seems like the columns are reorganized for some specific users...
    return(df)
  }else{
    return(comp)
  }
}
