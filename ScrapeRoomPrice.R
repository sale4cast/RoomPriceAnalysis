source("Func.R")
source("Graph.R")
library(shiny)
library(curl)
library(chromote)
library(waiter)
library(pagedown)
library(tidyverse)
library(maps)
library(igraph)
library(tidygraph)
library(ggraph)
library(tibble)
library(stringr)
library(ggplot2)

# enabling 'enter' to submit hotel name
js <- '$(document).keyup(function(event) {
  if ($("#hotelName").is(":focus") && (event.key == "Enter")) {
    $("#hotelBtn").click();
  }
});'

ui <- fluidPage(
  use_waiter(),
  shiny::tags$head(shiny::tags$script(HTML(js))),
  textInput("hotelName", "Enter Hotel Name with Location"),
  dateInput("inputDate", "Select a Date:",min = Sys.Date()),
  actionButton("hotelBtn", "Find Hotel"),
  tableOutput("data"),
  span(textOutput("targetHotelRating"), style="color: blue;"),
  span(textOutput("targetHotelReview"), style="color: green;"),  
  span(textOutput("notFound"), style="color: red;"),
  
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=0.5")
  ),
  uiOutput("graph")
)

server <- function(input, output, session) {
  driver <- ChromoteSession$new()
  
  # function to check to see if the chromote session is active or not
  checkActive <- function(){
    if(!driver$is_active()){
      driver <- driver$respawn()
    }
  }
  
  checkActive()
  driver$view()
  
  #enabling the notification in the footer
  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
  }
  
  observeEvent(input$hotelBtn, {
    # Validate input
    req(input$hotelName)
    
    id <- notify("Opening browser in a hidden mode ...")
    on.exit(removeNotification(id), add = TRUE)
    
    output$notFound <- renderText("")
    output$graph <- renderUI({});
    
    # Show a loading spinner
    Waiter$new(html = spin_loaders(id=3, color = "black"), color = "#00000010")$show()
    
    output$data <- renderTable({})
    output$notFound <- renderText("")
    
    checkActive()
    driver$Page$navigate("https://www.google.com")
    notify("Navigating a browser http://.... on the device", id = id)
    Sys.sleep(2)
    
    # List of prepositions to remove
    prepositions <- c("on", "in", "at", "to", "of", "from")
    
    # Create a regular expression pattern for prepositions
    prepositionPattern <- paste(prepositions, collapse = "|")
    
    # Remove prepositions from the input hotel name.
    # EX: Input: hotelName = "the pullman hotel in riga old town", Output: hotelName = "the pullman hotel  riga old town"
    hotelName <- gsub(paste0("\\b(?:", prepositionPattern, ")\\b"), "", input$hotelName, ignore.case = TRUE)
    
    # Split the hotel name into words. 
    # Ex1: Input: hotelName, Output: splitHotelName = "the"     "pullman" "hotel"   ""        "riga"    "old"     "town"
    # Ex2: Input: hotelName = "pullman,riga old town" , Output: "pullman" "riga"    "old"     "town"   
    # Ex3: Input: hotelName = "pullman-riga old town" , Output: "pullman" "riga"    "old"     "town"   
    if(grepl("-|,", hotelName, ignore.case = TRUE))
      splitHotelName <- unlist(strsplit(hotelName, (" |,|-")))
    else
      splitHotelName <- unlist(strsplit(hotelName, " "))
    # Ex: Input: nzchar(splitHotelName), Output: TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    # Ex: Input: splitHotelName, Output: splitHotelName = "the"     "pullman" "hotel"   "riga"    "old"     "town" 
    splitHotelName <- splitHotelName[nzchar(splitHotelName)]
    
    city <- NULL
    YES <- 1
    lastCityIndex <- totOccurOfCity <- cityExistence <- 0
    # Find the last city name and it's index from splitHotelName. Ex: Input: splitHotelName, Output: city = "riga"
    # Note: If there is only one city then first and last city name are identical.
    for (i in 1:length(splitHotelName)) {
      if (tolower(splitHotelName[[i]]) %in% tolower(world.cities$name)) {
        city <- splitHotelName[[i]]
        cityExistence = 1
        lastCityIndex = i
        totOccurOfCity = totOccurOfCity + 1
      }
    }
    
    # Ex1: Input1: splitHotelName = "the"  "pullman" "hotel"   "riga"    "old"    "town"
    # Ex1: Output1: splitHotelName = "the"  "pullman" "hotel"   "riga"    "old"    "town" 
    # Ex2: Input2: "the pullman hotel in riga", Output2: splitHotelName = "the"  "pullman" "hotel"
    if (lastCityIndex != 0 && length(splitHotelName) == lastCityIndex) {
      indexBeforeLastCity <- max(which(splitHotelName != city))
      if (!is.na(indexBeforeLastCity)) {
        splitHotelName <- splitHotelName[1:indexBeforeLastCity]
      }
    }
    # Stop if the number of words from input exceeds 10
    if (length(splitHotelName) > 10) {
      output$notFound <- renderText("Hotel name should not exceed 10 words.")
      Waiter$new(html = spin_wave())$hide()
    }else if (is.null(city)) { # Stop if the city name is not found
      output$notFound <- renderText("Please enter the name of the hotel along with its city name!")
      Waiter$new(html = spin_wave())$hide()
    } else if (cityExistence == YES && length(splitHotelName) >= 1 && totOccurOfCity >= 1) {
      output$notFound <- renderText("")
      # Ex1: Input2: splitHotelName = "the pullman hotel in riga old town", Output: searchText = "the pullman hotel riga old town in riga"
      # Ex2: Input2: splitHotelName = "the pullman hotel in riga", Output: searchText = "the pullman hotel in riga"
      googleSearchText <- paste0(paste(unique(tolower(splitHotelName)), collapse = " "), " in ", city)
      splitHotelName <- tolower(splitHotelName)
      # Remove empty string and article which are seperated by space
      # Ex: Input: str <- "the pullman hotel in riga old town" Output: "pullman hotel in riga old town"
      hotelNameWithoutArticle <- gsub(paste0("\\b(a|an|the)\\b"), "", splitHotelName, ignore.case = TRUE)
      hotelNameWithoutArticle <- hotelNameWithoutArticle[nzchar(hotelNameWithoutArticle)]
      
      # Created four different pattern of hotel name and then take them in one sentence.
      # Ex1:  "the pullman hotel riga old town", "thepullmanhotelrigaoldtown", "pullman hotel riga old town", "pullmanhotelrigaoldtown"
      # Exep: "tallink hotel", "tallinkhotel", "tallink hotel", "tallinkhotel"
      hotelNameWithArticleAndSpace <- paste(splitHotelName, collapse = " ")
      hotelNameWithArticleWithoutSpace <- paste(splitHotelName, collapse = "")
      hotelNameWithoutArticleAndSpace <- paste(hotelNameWithoutArticle, collapse = " ")
      hotelNameWithoutArticleWithSpace <- paste(hotelNameWithoutArticle, collapse = "")
      hotelNamePattern <- c(input$hotelName, googleSearchText, paste(splitHotelName, collapse = " "), hotelNameWithArticleAndSpace, hotelNameWithArticleWithoutSpace, hotelNameWithoutArticleAndSpace, hotelNameWithoutArticleWithSpace)
      
      hotelNamePattern <- paste(unique(hotelNamePattern), collapse = "|")
      
      # ---------------- Plugin - Point ------------------------ #
      # checkActive() check the status of current session. If current session is inactive, then make it active. 
      checkHotelStarType <- hotelStarType <- hotelReviews <- hotelRating <- divSecFromGooglePage <- NULL
      checkActive()
      driver$Page$navigate("https://www.google.com/")
      Sys.sleep(2)
      checkActive()
      driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', googleSearchText,'"'))
      checkActive()
      driver$Runtime$evaluate('document.querySelector(".gNO89b").click()')
      Sys.sleep(3)
      # extract 3rd div of body of google page.
      # divSecFromGooglePage has character(0) value sometimes !!!!!!!!!!!
      divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("div.fQtNvd").innerText')$result$value))
      # Extract the whole HTML page text and convert it to lowercase
      googlePageText <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("body").innerText')$result$value))
      occurrenceFound <-  str_count(googlePageText, hotelNamePattern)
      checkActive()
      Sys.sleep(3)
      
      hotelStar <- c(2,3,4,5)
      if(occurrenceFound >= 1){
        checkActive()
        # Get splitHotelName by removing the city name Ex: splitHotelName = c("the", "pullman", "hotel", "Riga")
        # Output: splitHotelName = c("the", "pullman", "hotel")            
        splitHotelName <- splitHotelName[splitHotelName != city] ## Need to understand why we are removing city name!
        # Scanning top right corner of google page to look if there is any star type over there.
        checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span.E5BaQ").innerText')
        checkActive()
        if(is.null(checkHotelStarType$result$value)) 
          checkHotelStarType <- driver$Runtime$evaluate('document.querySelector(".YhemCb").innerText') ## ! why do we need .YhemCb class
        # Extracting hoter STAR '*' type and review number from top right corner of google page but not hotel rating.
        if(!is.null(checkHotelStarType) && !is.null(checkHotelStarType$result$value) && !is.na(parse_number(checkHotelStarType$result$value)) && parse_number(checkHotelStarType$result$value) %in% hotelStar)
        {
          hotelStarType <- parse_number(checkHotelStarType$result$value)
          checkActive()
          hotelRating <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span.Aq14fc").innerText')$result$value
          hotelRating <- parse_number(hotelRating)
          hotelReviews <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > a").innerText')
          hotelReviews <- parse_number(hotelReviews$result$value)
        }
        # Extracting hotel STAR * type, rating and review number from the 3rd div of body of google page.
        else if(!is.null(divSecFromGooglePage) && length(divSecFromGooglePage) > 0)
        {
          hotelStarType <- findHotelStarType(splitHotelName, divSecFromGooglePage, TRUE)     
          checkActive()
          hotelRating <- driver$Runtime$evaluate('document.querySelector("span.yi40Hd.YrbPuc").innerText')$result$value
          hotelRating <- parse_number(hotelRating)
          checkActive()
          hotelReviews <- driver$Runtime$evaluate('document.querySelector("span.RDApEe.YrbPuc").innerText')
          if(grepl("k", hotelReviews, ignore.case = TRUE))
            hotelReviews <- parse_number(hotelReviews$result$value) * 1000
          else
            hotelReviews <- parse_number(hotelReviews$result$value)
        }
        # Finding hoter STAR '*' type from body of google page. However, it is not possible to find review number and rating.
        # !!! How do we set the review and rating then !!!
        # Input: The Social Hub Eindhoven. Output: checkHotelStarType$result$type = "string", checkHotelStarType$result$value = "Hotel"
        else if(is.na(parse_number(checkHotelStarType$result$value)))
          hotelStarType <- findHotelStarType(splitHotelName, googlePageText, FALSE) 
      }   
      # In special case when checkHotelStarType is not null but hotelRating and hotelReviews are null. Example: The Social Hub Eindhoven
      if ((!is.null(checkHotelStarType$result$value)) && is.null(hotelRating) && is.null(hotelReviews)) {
        checkActive()
        hotelRating <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span.Aq14fc").innerText')$result$value
        hotelRating <- parse_number(hotelRating)
        checkActive()
        hotelReviews <- driver$Runtime$evaluate('document.querySelector("a[data-async-trigger=\'reviewDialog\']").innerText')
        hotelReviews <- parse_number(hotelReviews$result$value)
      }
      
      if(!is.null(hotelStarType)){
        searchTextForNeighborHotel <- paste0(hotelStarType, " star hotel in ", city)
        checkActive()
        driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', searchTextForNeighborHotel,'"'))
        
        checkActive()
        driver$Runtime$evaluate('document.querySelector("#tsf > div:nth-child(1) > div.A8SBwf > div.RNNXgb > button").click()')
        Sys.sleep(2)
        
        notify(paste0("Searching neighbor hotels which are ", hotelStarType," star type"), id = id)
        
        checkActive()
        # Click on the drop-down menu and check if it is exist by guestDropdownBtn$result$objectId 
        guestDropdownBtn <- driver$Runtime$evaluate('document.querySelector("div.R2w7Jd")')
        Sys.sleep(3)   
        if(is.null(guestDropdownBtn$result$objectId)){
          output$notFound <- renderText("Not found dropdown button!")
          Waiter$new(html = spin_wave())$hide()
          
        }else{
          checkActive()
          # Click to open the drop down-menu.
          driver$Runtime$evaluate('document.querySelector("div.R2w7Jd").click()')
          checkActive()
          # Click to select guest number 1 from the drop-down menu.
          driver$Runtime$evaluate('document.querySelector("div.JWXKNd").click()')
          notify("Clicking on the dropdown button to select a number of guest ...", id = id)        
          Sys.sleep(3)
          
          hotelRoomType <- c("Single", "Double", "Triple", "Family")
          # Click to open filter tab in the right side of the window.
          checkActive()
          driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div > div.wpMGDb.Vz4hIc.Vy4Vcc > div > button").click()')
          notify("Opening filter tab to filter neighbor hotels ...", id = id)
          Sys.sleep(3)
          
          # Click to select relevant review button from the filter tab.
          checkActive()
          driver$Runtime$evaluate('document.querySelector("input[value=\'13\']").click()')
          notify("Selecting hotels which has relevant reviews and ratings as yours ...", id = id)
          Sys.sleep(3)
          
          # Click to close filter tab.
          checkActive()
          driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div.dq93Ae.lqK3vd.cmKkVe.eXUIm > div.xkSIab > div > div.pD9FRb > div.i73Eid.I1vvIb > span > button").click()')
          Sys.sleep(3)
          userInputDate <- input$inputDate
          # Click to open date picker.
          checkActive()
          driver$Runtime$evaluate('document.querySelector("input[placeholder=\'Check-in\']").click()')
          notify("Opening date picker ...", id = id)
          Sys.sleep(3)
          # Click to select check-in date from date picker.
          checkActive()
          driver$Runtime$evaluate(paste0('document.querySelector("',paste0("div[data-iso='", userInputDate, "']"),'").click()'))
          notify("Selecting check-in date ...", id = id)
          Sys.sleep(3)
          
          # Click to select check-out date from date picker.
          checkActive()
          driver$Runtime$evaluate(paste0('document.querySelector("',paste0("div[data-iso='", userInputDate + 1, "']"),'").click()'))
          notify("Selecting check-out date ...", id = id)
          Sys.sleep(3)
          
          # Retrieve current date from date picker.
          checkInDate <- driver$Runtime$evaluate('document.querySelector(".HDland").getAttribute("data-iso")')$result$value
          
          # Click to submit check-in and check-out date.
          checkActive()
          driver$Runtime$evaluate('document.querySelector("#ow37 > div.fSZK0b.DzHPKc.iWO5td > div > div.g3VIld.pqCVeb.XkOXPd.yXO5Pd.k57Mic.J9Nfi.iWO5td > div.rSonUc > div > button.VfPpkd-LgbsSe.VfPpkd-LgbsSe-OWXEXe-k8QpJ.VfPpkd-LgbsSe-OWXEXe-dgl2Hf.nCP5yc.AjY5Oe.DuMIQc.LQeN7.z18xM.rtW97.Q74FEc.D9Ipie > div.VfPpkd-RLmnJb").click()')
          notify("Submit date picker...", id = id)
          Sys.sleep(3)
          
          # Collect hotel prices, ratings and number of reviews from the page
          roomPrices <- list()
          for(room in hotelRoomType){
            
            # Collecting room price of neighbor hotels from the page.
            notify(paste0("Scanning ",room, " room price of neighbor hotels ..."), id = id)
            Sys.sleep(2)
            checkActive()
            priceElement <- driver$Runtime$evaluate(
              'var elements = document.querySelectorAll(".K1smNd > c-wiz[jsrenderer=\'hAbFdb\'] .PwV1Ac");
                 var elementPrices = [];
                 elements.forEach(function(element) {
                   elementPrices.push(element.innerText);
                 });
                 elementPrices.join("@");'
            )
            
            splittedPriceElements <- unlist(strsplit(priceElement$result$value, "@"))
          
            neighborHotelRoomPrices <- lapply(splittedPriceElements, function(aElement){
              ## Great Deal\n$80    Deal 5%\n$90
              roomPrice <- unlist(strsplit(aElement,"\n"))
              if(length(roomPrice) > 1){
                return(roomPrice[[2]])
              }
              return (aElement)
            })
            
            # Collecting user rating of neighbor hotels from the page.
            notify("Scanning user rating of neighbor hotels ...", id = id)
            checkActive()
            ratingElement <- driver$Runtime$evaluate(
              'var elements = document.querySelectorAll(".K1smNd > c-wiz[jsrenderer=\'hAbFdb\'] .KFi5wf");
                 var elementRatings = [];
                 elements.forEach(function(element) {
                   elementRatings.push(element.innerText);
                 });
                 elementRatings.join("\\n");'
            )
            
            splittedRatingElements <- strsplit(ratingElement$result$value, "\n")
            neighborHotelRatings <- lapply(splittedRatingElements, function(aElement){
              return (aElement)
            })
            
            # Collecting user review number of neighbor hotels from the page.
            notify("Scanning user review numbers of neighbor hotels ...", id = id)
            checkActive()
            reviewElement <- driver$Runtime$evaluate(
              'var elements = document.querySelectorAll(".K1smNd > c-wiz[jsrenderer=\'hAbFdb\'] .jdzyld");
                 var elementReviews = [];
                 elements.forEach(function(element) {
                   elementReviews.push(element.innerText);
                 });
                 elementReviews.join("\\n");'
            )
            
            splittedReviewElements <- strsplit(reviewElement$result$value, "\n")
            
            neighborHotelReviews <- lapply(splittedReviewElements, function(aElement){
              return(gsub("[()]", "", (aElement)))
            })
            
            # Collecting neighbor hotels name from the page.
            notify("Scanning neighbor hotels name ...", id = id)
            checkActive()
            hotelNameElement <- driver$Runtime$evaluate(
              'var elements = document.querySelectorAll(".K1smNd > c-wiz[jsrenderer=\'hAbFdb\'] div.QT7m7 h2[jsaction = \'YcW9n:dDUAne;\']");
                 var elementHotelNames = [];
                 elements.forEach(function(element) {
                   elementHotelNames.push(element.innerText);
                 });
                 elementHotelNames.join("\\n");'
            )
            
            splittedHotelNameElements <- strsplit(hotelNameElement$result$value, "\n")
            
            neighborHotelNames <- lapply(splittedHotelNameElements, function(aElement){
              return(aElement)
            })
            
            roomPrices[[room]][["Prices"]] <- unlist(neighborHotelRoomPrices)
            #roomPrices[[room]][["Prices"]] <- unlist(neighborHotelRoomPrices)
            roomPrices[[room]][["Ratings"]] <- unlist(neighborHotelRatings)
            roomPrices[[room]][["Reviews"]] <- unlist(neighborHotelReviews)
            roomPrices[[room]][["HotelName"]] <- unlist(neighborHotelNames)
            
            if(room == "Family"){
              notify("Ploting the optimized price for each types of room ...", id = id)
              Sys.sleep(2)
              break
            }
            
            # Click on the '+' button to increase the guest number by 1. !by following Nr. 281 Line!
            checkActive()
            driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.tbDMNe > div.YFfNHd > div > div > div > div.cQnuXe.k0gFV > div").click()')
            notify("Clicking on the '+' button to increase the guest number by 1 ...", id = id)
            Sys.sleep(3)
            
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button[jsname=\'TdyTDe\']").click()')
            notify(paste0("Looking for the price of ",room," room ..."), id = id)
            Sys.sleep(2)
            
            # Click on the submit button
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button[jsname=\'kZlJze\']").click()')            
            Sys.sleep(3)
          }
          
          if(length(roomPrices$Single) > 0 || length(roomPrices$Double) > 0 || length(roomPrices$Triple) > 0 || length(roomPrices$Family) > 0){
            prices_df <- data.frame()
            for(room in hotelRoomType){
              if(length(roomPrices[[room]]) > 0){
                tempPriceDf <- as.data.frame(do.call(cbind, roomPrices[[room]]))
                tempPriceDf <- cbind(City=city, Type=hotelStarType, RoomType = room, tempPriceDf)
                #tempPriceDf <- cbind(City=cityName, Type=hotelType, hotelRoomType = room, tempPriceDf)
                
                prices_df <- rbind(prices_df, tempPriceDf)
                #browser()
              }
            }
            # Create final table.
            prices_df <- apply(prices_df, c(1,2), as.character)
            # A empty row is eliminated.
            prices_df <- subset(prices_df, !apply(prices_df, 1, function(row) any(row == "")))
            prices_df <- prices_df %>% as.data.frame()
            prices_df$Ratings <- parse_number(prices_df$Ratings)
            
            targetHotel <- gsub("\\bin\\s.*", "", input$hotelName, ignore.case = TRUE)
            # browser()

            makeNetworkGraph(prices_df, targetHotel, hotelRating, hotelReviews, checkInDate, output)

            #output$data <- renderTable(prices_df)
            #output$targetHotelRating <- renderText(paste0("Target Hotel Rating : ",hotelRating))
            #output$targetHotelReview <- renderText(paste0("Target Hotel Reviews : ",hotelReviews))
            #output$variables <- renderPrint("Target Hotel/Hostel reviews : ",isExistingHotelReviews)
          }else{
            output$notFound <- renderText("Not found any hotel or hostel!")
            Waiter$new(html = spin_wave())$hide()
          }
          Waiter$new(html = spin_wave())$hide()
        }
      }
      else{
        output$notFound <- renderText("Not found any hotel type!")
        Waiter$new(html = spin_wave())$hide()
      }      
    }
  })
}


shinyApp(ui, server)