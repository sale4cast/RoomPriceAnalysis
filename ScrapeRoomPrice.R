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
#library(tibble)
#library(stringr)
#library(ggplot2)

# enabling 'enter' to submit hotel name
js <- '$(document).keyup(function(event) {
  if ($("#hotelName").is(":focus") && (event.key == "Enter")) {
    $("#hotelBtn").click();
  }
});'
debugmode = TRUE
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
    # Show a loading spinner
    Waiter$new(html = spin_loaders(id=3, color = "black"), color = "#00000010")$show()
    output$graph <- renderUI({});
    output$data <- renderTable({})
    output$notFound <- renderText("")
    splitHotelName <- findSplitHotelName(input$hotelName)
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
      hotelNamePattern <- findHotelNamePattern(input$hotelName, splitHotelName, googleSearchText)
      
      # ---------------- Plugin - Point ------------------------ #
      # checkActive() check the status of current session. If current session is inactive, then make it active. 
      checkHotelStarType <- hotelStarType <- hotelReviews <- hotelRating <- divSecFromGooglePage <- NULL
      checkActive()
      driver$Page$navigate("https://www.google.com")
      notify("Navigating a browser http://.... on the device", id = id)
      Sys.sleep(3)
      checkActive()
      driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', googleSearchText,'"'))
      queryGoogleSearchText1 <- queryGoogleSearchText2 <- queryGoogleSearchText3 <- NULL
      checkActive()
      queryGoogleSearchText1 <- driver$Runtime$evaluate('document.querySelector(".gNO89b")')
      checkActive()
      queryGoogleSearchText2 <- driver$Runtime$evaluate('document.querySelector("input[aria-label=\'Google Search\']")')
      
      if((queryGoogleSearchText1$result$subtype) == "node"){
        checkActive()
        driver$Runtime$evaluate('document.querySelector(".gNO89b").click()')
      }
      else if(queryGoogleSearchText2$result$subtype == "node"){
        checkActive()
        driver$Runtime$evaluate('document.querySelector("input[aria-label=\'Google Search\']").click()')
      }
      
      ### need to add js path
      notify("Looking for hotel * type ... ", id = id)
      Sys.sleep(3)
      # extract 3rd div of body of google page.
      # divSecFromGooglePage has character(0) value sometimes !!!!!!!!!!!
      checkActive()
      divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("div.fQtNvd").innerText')$result$value))
      if(is.null(divSecFromGooglePage)){
        checkActive()
        divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("#Odp5De > div > div > div > div > div.F0toYe > div > div > div.hkrXre > div > div > div:nth-child(1) > a").innerText')$result$value))
      }
      if(is.null(divSecFromGooglePage)){
        checkActive()
        divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("a[aria-labelledby=\'0_lbl\']").innerText')$result$value))
      }
      # Extract the whole HTML page text and convert it to lowercase
      checkActive()
      googlePageText <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("body").innerText')$result$value))
      occurrenceFound <-  str_count(googlePageText, hotelNamePattern) ### May need to check properly. Because the occurance for the pullman hotel in riga old town is only 4 ###
      checkActive()
      Sys.sleep(3)
      hotelStarString <- c("2-star","3-star","4-star","5-star", "2 star","3 star","4 star","5 star")
      if(occurrenceFound >= 1){
        # Get splitHotelName by removing the city name Ex: splitHotelName = c("the", "pullman", "hotel", "Riga")
        # Output: splitHotelName = c("the", "pullman", "hotel")            
        splitHotelName <- splitHotelName[splitHotelName != city] ## Need to understand why we are removing city name!
        # Scanning top right corner of google page to look if there is any star type over there.
        checkActive()
        checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span:nth-of-type(3)").innerText')
        if(is.null(checkHotelStarType$result$value)){
          checkActive()
          checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("span.E5BaQ").innerText')  
        }
        # For some hotel, both path and span can be change
        if(is.null(checkHotelStarType$result$value)){
          checkActive()
          checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("span.YhemCb").innerText')
        }
        # If STAR '*' type of target hotel exists in the top right corner of google page, extract star type, rating and user review number.
        if(!is.null(checkHotelStarType) && !is.null(checkHotelStarType$result$value) && !is.na(parse_number(checkHotelStarType$result$value)) && grepl(paste(hotelStarString, collapse = "|"), checkHotelStarType$result$value))
        {
          hotelStarType <- parse_number(checkHotelStarType$result$value)
          checkActive()
          hotelRating <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span").innerText')$result$value
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.Aq14fc").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          
          # For Hotel Reviews
          checkActive()
          hotelReviews <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > a").innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a.hqzQac").innerText')$result$value
          }
          hotelReviews <- parse_number(hotelReviews)
        }
        # Extracting hotel STAR * type, rating and review number from the 3rd div of body of google page.
        else if(!is.null(divSecFromGooglePage) && length(divSecFromGooglePage) > 0)
        {
          hotelStarType <- findHotelStarType(splitHotelName, divSecFromGooglePage, TRUE)     
          checkActive()
          
          hotelRating <- driver$Runtime$evaluate('document.querySelector("#Odp5De > div > div > div > div > div.F0toYe > div > div > div.hkrXre > div > div > div:nth-child(1) > a > div > div > div:nth-child(2) > div > span.Y0A0hc > span").innerText')$result$value
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.yi40Hd").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          
          checkActive()
          hotelReviews <- driver$Runtime$evaluate('document.querySelector("#Odp5De > div > div > div > div > div.F0toYe > div > div > div.hkrXre > div > div > div:nth-child(1) > a > div > div > div:nth-child(2) > div > span.Y0A0hc > span:nth-of-type(3)").innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("span.RDApEe").innerText')$result$value
          }
          
          if(grepl("k", hotelReviews, ignore.case = TRUE))
            hotelReviews <- parse_number(hotelReviews) * 1000
          else
            hotelReviews <- parse_number(hotelReviews)
        }
        # Finding hoter STAR '*' type from body of google page. However, it is not possible to find review number and rating.
        # !!! How do we set the review and rating then !!!
        # Input: The Social Hub in Eindhoven. Output: checkHotelStarType$result$type = "string", checkHotelStarType$result$value = "Hotel"
        # In special case when checkHotelStarType is not null but hotelRating and hotelReviews are null. Example: The Social Hub Eindhoven
        else if(is.null(hotelRating) && is.null(hotelReviews) && !is.null(checkHotelStarType$result$value) && !is.na(parse_number(checkHotelStarType$result$value)) && !grepl(paste(hotelStarString, collapse = "|"), checkHotelStarType$result$value))
        {
          hotelStarType <- findHotelStarType(splitHotelName, googlePageText, FALSE)     
          # For Hotel Rating
          checkActive()
          hotelRating <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > span").innerText')$result$value
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.Aq14fc").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          
          # For Hotel Reviews
          checkActive()
          hotelReviews <- driver$Runtime$evaluate('document.querySelector("#rhs > div.kp-wholepage-osrp > div.wPNfjb > div > div > div:nth-child(2) > div > div > div.nwVKo > div.loJjTe > div > a").innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a.hqzQac").innerText')$result$value
          }
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a[data-async-trigger=\'reviewDialog\']").innerText')$result$value
          }
          hotelReviews <- parse_number(hotelReviews)          
        }
      }   
      
      if(!is.null(hotelStarType)){
        searchTextForNeighborHotel <- paste0(hotelStarType, " star hotel in ", city)
        checkActive()
        driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', searchTextForNeighborHotel,'"'))
        #browser()
        checkActive()
        searchButton1 <- driver$Runtime$evaluate('document.querySelector("#tsf > div:nth-child(1) > div.A8SBwf > div.RNNXgb > button")')
        checkActive()
        searchButton2 <- driver$Runtime$evaluate('document.querySelector("button.Tg7LZd")')
        checkActive()
        searchButton3 <- driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Search\']")')
        if(searchButton1$result$subtype == "node"){
          driver$Runtime$evaluate('document.querySelector("#tsf > div:nth-child(1) > div.A8SBwf > div.RNNXgb > button").click()')
        }else if(searchButton2$result$subtype == "node"){
          driver$Runtime$evaluate('document.querySelector("button.Tg7LZd").click()')
        }else if(searchButton3$result$subtype == "node"){
          driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Search\']").click()')
        }
        Sys.sleep(2)
        
        notify(paste0("Searching neighbor hotels which are ", hotelStarType," star type"), id = id)
        
        # Click on the drop-down menu and check if it is exist by guestDropdownBtn$result$objectId 
        checkActive()
        guestDropdownBtn <- driver$Runtime$evaluate('document.querySelector("div.R2w7Jd")')
        Sys.sleep(3)   
        if(is.null(guestDropdownBtn$result$objectId)){
          output$notFound <- renderText("Not found dropdown button!")
          Waiter$new(html = spin_wave())$hide()
          
        }else{
          # Click to open the drop down-menu.
          checkActive()
          #browser()
          dropdown<-dropdown2<-NULL
          checkActive()
          dropdown<-driver$Runtime$evaluate('document.querySelector("div.R2w7Jd")')
          checkActive()
          dropdown2<-driver$Runtime$evaluate('document.querySelector("#ow62 > div:nth-child(1) > div > div > div > div.R2w7Jd")')
          #dropdown2<-driver$Runtime$evaluate('document.querySelector("#ow61 > div:nth-child(1) > div > div > div > div")')
          if(dropdown2$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("#ow62 > div:nth-child(1) > div > div > div > div.R2w7Jd").click()')
          }
          else if(dropdown$result$subtype=="node"){
            dropdown<-driver$Runtime$evaluate('document.querySelector("div.R2w7Jd").click()')
          }
          #browser()
          # Click to select guest number 1 from the drop-down menu.
          checkActive()
          button1<-button2<-NULL
          button2<-driver$Runtime$evaluate('document.querySelector("div.JWXKNd")')
          checkActive()
          button1<-driver$Runtime$evaluate('document.querySelector("#a452078e-b776-4137-bcc5-7dce2648d12c > a > div")')
          
          if(button1$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("#\\35 48b174e-bfe7-4c85-a5d9-45a143292c73 > a > div").click()')
          }else if(button2$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("div.JWXKNd").click()')
          }
          notify("Clicking on the dropdown button to select a number of guest ...", id = id)        
          Sys.sleep(3)
          #browser()
          hotelRoomType <- c("Single", "Double", "Triple", "Family")
          # Click to open filter tab in the right side of the window.
          checkActive()
          #browser()
          checkActive()
          filterButton<-driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div > div.wpMGDb.Vz4hIc.Vy4Vcc > div > button")')
          checkActive()
          filterButton2<-driver$Runtime$evaluate('document.querySelector("div.wpMGDb.Vz4hIc.Vy4Vcc")')
          if(filterButton$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div > div.wpMGDb.Vz4hIc.Vy4Vcc > div > button").click()')
          }else if(filterButton2$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("div.wpMGDb.Vz4hIc.Vy4Vcc").click()')
          }
          
          notify("Opening filter tab to filter neighbor hotels ...", id = id)
          Sys.sleep(3)
          
          # Click to select relevant review button from the filter tab.
          
          checkActive()
          #browser() 
          #not found
          checkActive()
          driver$Runtime$evaluate('document.querySelector("input[value=\'13\']").click()')
          notify("Selecting hotels which has relevant reviews and ratings as yours ...", id = id)
          Sys.sleep(3)
          
          # Click to close filter tab.
          checkActive()
          filter1<-filter2<-NULL
          checkActive()
          filter1<-driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div.dq93Ae.lqK3vd.cmKkVe.eXUIm > div.xkSIab > div > div.pD9FRb > div.i73Eid.I1vvIb > span > button")')
          checkActive()
          filter2<-driver$Runtime$evaluate('document.querySelector("[aria-label=\'Close dialogue\']")')
          if(filter1$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.AN2bF > div.mpSL5d > div > div.dq93Ae.lqK3vd.cmKkVe.eXUIm > div.xkSIab > div > div.pD9FRb > div.i73Eid.I1vvIb > span > button").click()')
          }else if(filter2$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("[aria-label=\'Close dialogue\']").click()')
          }
          Sys.sleep(3)
          
          #browser()
          userInputDate <- input$inputDate
          # Click to open date picker.
          checkActive()
          placeHolder1<-driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.tbDMNe > div.khj5De.Yxswxc > div > div > div.Ryi7tc.hh3Grb > div.GYgkab.YICvqf.M8oMHb.cd29Sd > div > input")')
          checkActive()
          placeHolder2<-driver$Runtime$evaluate('document.querySelector("input[placeholder=\'Check-in\']").click()')
          if(placeHolder1$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.tbDMNe > div.khj5De.Yxswxc > div > div > div.Ryi7tc.hh3Grb > div.GYgkab.YICvqf.M8oMHb.cd29Sd > div > input")')
          }else if(placeHolder2$result$subtype=="node"){
            driver$Runtime$evaluate('document.querySelector("input[placeholder=\'Check-in\']").click()')
          }
          #browser()
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
          checkActive()
          checkInDate <- driver$Runtime$evaluate('document.querySelector(".HDland").getAttribute("data-iso")')$result$value
          checkActive()
          checkInDate2 <-driver$Runtime$evaluate('document.querySelector("[jsname=\'mG3Az\'][aria-selected=\'true\']").getAttribute("data-iso")')$result$value
          if(is.null(checkInDate)){
            checkInDate<-driver$Runtime$evaluate('document.querySelector(".HDland").getAttribute("data-iso")')$result$value
          }else if (is.null(checkInDate2)){
            checkInDate<-driver$Runtime$evaluate('document.querySelector("[jsname=\'mG3Az\'][aria-selected=\'true\']").getAttribute("data-iso")')$result$value
          }
          # Click to submit check-in and check-out date.
          checkActive()
          #browser()
          driver$Runtime$evaluate('document.querySelector("#ow37 > div.fSZK0b.DzHPKc.iWO5td > div > div.g3VIld.pqCVeb.XkOXPd.yXO5Pd.k57Mic.J9Nfi.iWO5td > div.rSonUc > div > button.VfPpkd-LgbsSe.VfPpkd-LgbsSe-OWXEXe-k8QpJ.VfPpkd-LgbsSe-OWXEXe-dgl2Hf.nCP5yc.AjY5Oe.DuMIQc.LQeN7.z18xM.rtW97.Q74FEc.D9Ipie > div.VfPpkd-RLmnJb").click()')
          #driver$Runtime$evaluate('document.querySelector("button[jsname=\'iib5kc\']").click()') #Error!!!
          #driver$Runtime$evaluate('document.querySelector("button[jsname=\'iib5kc\'][jscontroller=\'soHxf\']").click()') #Error!!!
          
          
          
          notify("Submit date picker...", id = id)
          Sys.sleep(3)
          
          # Collect hotel prices, ratings and number of reviews from the page
          neighborHotelDetails <- list()
          for(room in hotelRoomType){
            
            # Collecting room price of neighbor hotels from the page.
            notify(paste0("Scanning ",room, " room price of neighbor hotels ..."), id = id)
            Sys.sleep(2)
            checkActive()
            #browser()
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
            
            neighborHotelDetails[[room]][["Prices"]] <- unlist(neighborHotelRoomPrices)
            neighborHotelDetails[[room]][["Ratings"]] <- unlist(neighborHotelRatings)
            neighborHotelDetails[[room]][["Reviews"]] <- unlist(neighborHotelReviews)
            neighborHotelDetails[[room]][["HotelName"]] <- unlist(neighborHotelNames)
            
            if(room == "Family"){
              notify("Ploting the optimized price for each types of room ...", id = id)
              Sys.sleep(3)
              break
            }
            
            # Click on the '+' button to increase the guest number by 1. !by following Nr. 281 Line!
            checkActive()
            #browser()
            buttonToSubmit<-buttonToSubmit2 <-NULL
            buttonToSubmit<-driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.tbDMNe > div.YFfNHd > div > div > div > div.cQnuXe.k0gFV > div")')
            checkActive()
            buttonToSubmit2<-driver$Runtime$evaluate('document.querySelector("[aria-label=\'Number of travellers. Current number of travellers is 1.\']")')        
            if(buttonToSubmit$result$subtype=="node"){
              buttonToSubmit<-driver$Runtime$evaluate('document.querySelector("#yDmH0d > c-wiz.zQTmif.SSPGKf.AglWE > div > c-wiz > div.dTQsx.gBIxsf > div.MawWP > div.wMMMNd > c-wiz > div > div > div.E4DaWc.AJr5uf > div > div.tbDMNe > div.YFfNHd > div > div > div > div.cQnuXe.k0gFV > div").click()')
            }else if(buttonToSubmit2$result$subtype=="node"){
              buttonToSubmit<-driver$Runtime$evaluate('document.querySelector("[aria-label=\'Number of travellers. Current number of travellers is 1.\']").click()') 
            }
            notify("Clicking on the '+' button to increase the guest number by 1 ...", id = id)
            Sys.sleep(3)
            checkActive()
            increaseButton<-increaseButton2<-NULL
            increaseButton<- driver$Runtime$evaluate('document.querySelector("button[jsname=\'TdyTDe\']")')
            checkActive()
            increaseButton2<-driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Add adult\']")')
            if(increaseButton$result$subtype=="node"){
              increaseButton<-driver$Runtime$evaluate('document.querySelector("button[jsname=\'TdyTDe\']").click()')
            }else if(increaseButton2$result$subtype=="node"){
              increaseButton<- driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Add adult\']").click()')
            }
            notify(paste0("Looking for the price of ",room," room ..."), id = id)
            Sys.sleep(2)
            
            # Click on the submit button
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button[jsname=\'kZlJze\']").click()')            
            Sys.sleep(3)
          }
          # The aim of this section is to convert list into matrix and then data frame.
          # do.call() convert list into matrix.
          if(length(neighborHotelDetails$Single$Prices) > 0 || length(neighborHotelDetails$Double$Prices) > 0 || length(neighborHotelDetails$Triple$Prices) > 0 || length(neighborHotelDetails$Family$Prices) > 0){
            neighHotelDetailsForAllRoomType <- data.frame()
            for(room in hotelRoomType){
              if(length(neighborHotelDetails[[room]]) > 0){
                neighHotelDetailsForARoomType <- as.data.frame(do.call(cbind, neighborHotelDetails[[room]]))
                neighHotelDetailsForARoomType <- cbind(City=city, Type=hotelStarType, RoomType = room, neighHotelDetailsForARoomType)
                neighHotelDetailsForAllRoomType <- rbind(neighHotelDetailsForAllRoomType, neighHotelDetailsForARoomType)
              }
            }
            # browser()
            # # Create final table.
            # neighHotelDetailsForAllRoomType <- apply(neighHotelDetailsForAllRoomType, c(1,2), as.character)
            # # A empty row is eliminated.
            # neighHotelDetailsForAllRoomType <- subset(neighHotelDetailsForAllRoomType, !apply(neighHotelDetailsForAllRoomType, 1, function(row) any(row == "")))
            # neighHotelDetailsForAllRoomType <- neighHotelDetailsForAllRoomType %>% as.data.frame()
            neighHotelDetailsForAllRoomType <- neighHotelDetailsForAllRoomType %>% filter(!is.na(parse_number(neighHotelDetailsForAllRoomType$Prices)))
            neighHotelDetailsForAllRoomType$Ratings <- parse_number(neighHotelDetailsForAllRoomType$Ratings)
            
            targetHotel <- gsub("\\bin\\s.*", "", input$hotelName, ignore.case = TRUE)
            #browser()
            currencySymbol <- str_remove_all(neighHotelDetailsForAllRoomType$Prices[1],"[0-9.,]")
            makeNetworkGraph(neighHotelDetailsForAllRoomType, targetHotel, hotelRating, hotelReviews, checkInDate, currencySymbol, output)
            
            #output$data <- renderTable(prices_df)
            #output$targetHotelRating <- renderText(paste0("Target Hotel Rating : ",hotelRating))
            #output$targetHotelReview <- renderText(paste0("Target Hotel Reviews : ",hotelReviews))
            #output$variables <- renderPrint("Target Hotel/Hostel reviews : ",isExistingHotelReviews)
          }else{
            output$notFound <- renderText("Not found any competitor hotel!")
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