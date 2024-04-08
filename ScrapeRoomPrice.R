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

# enabling 'enter' to submit hotel name
js <- '$(document).keyup(function(event) {
  if ($("#hotelName").is(":focus") && (event.key == "Enter")) {
    $("#hotelBtn").click();
  }
});

'
debugmode = TRUE
ui <- fluidPage(
  use_waiter(),
  shiny::tags$head(shiny::tags$script(HTML(js))),
  textInput("hotelName", "Enter Hotel Name with Location"),
  dateInput("inputDate", "Select a Date:",min = Sys.Date()),
  actionButton("hotelBtn", "Watch Nieghbor Hotel Room Price"),
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
      # ! ?? is there any chance to add here XPath as a button !
      queryGoogleSearchByAria <- queryGoogleSearchByClass <- NULL
      checkActive()
      queryGoogleSearchByAria <- driver$Runtime$evaluate('document.querySelector("input[aria-label=\'Google Search\']")')      
      checkActive()
      queryGoogleSearchByClass <- driver$Runtime$evaluate('document.querySelector(".gNO89b")')
      if(queryGoogleSearchByAria$result$subtype == "node"){
        checkActive()
        driver$Runtime$evaluate('document.querySelector("input[aria-label=\'Google Search\']").click()')
      }
      else if((queryGoogleSearchByClass$result$subtype) == "node"){
        checkActive()
        driver$Runtime$evaluate('document.querySelector(".gNO89b").click()')
      }      
      
      notify("Looking for hotel star * type ... ", id = id)
      Sys.sleep(3)
      # extract 3rd div of body of google page.
      # di vSecFromGooglePage has character(0) value sometimes !!!!!!!!!!!
      divSecFromGooglePage <- NULL
      # browser()
      checkActive()
      divSecFromGooglePage <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
      # divSecFromGooglePage <- divSecFromGooglePage %>% as.character() %>% tolower()
      if(is.null(divSecFromGooglePage)){
        checkActive()
        # divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[3]/div/div/div[1]/div/div/div[1]/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value))
        divSecFromGooglePage <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[3]/div/div/div[1]/div/div/div[1]/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
      }      
      if(is.null(divSecFromGooglePage)){
        checkActive()
        # divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("a[aria-labelledby=\'0_lbl\']").innerText')$result$value))
        divSecFromGooglePage <- driver$Runtime$evaluate('document.querySelector("a[aria-labelledby=\'0_lbl\']").innerText')$result$value
      }
      if(is.null(divSecFromGooglePage)){
        checkActive()
        # divSecFromGooglePage <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("div.fQtNvd").innerText')$result$value))
        divSecFromGooglePage <- driver$Runtime$evaluate('document.querySelector("div.fQtNvd").innerText')$result$value
      }
      # Extract the whole HTML page text and convert it to lowercase
      # ! ?? is the use of body tag stable to return page whole page text. Can we add XPath next to it !
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
        checkHotelStarType <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')
        
        if(is.null(checkHotelStarType$result$value)){
          checkActive()
          checkHotelStarType <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')
        }
        if(is.null(checkHotelStarType$result$value)){
          checkActive()
          checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("span.E5BaQ").innerText')  
        }
        if(is.null(checkHotelStarType$result$value)){
          checkActive()
          checkHotelStarType <- driver$Runtime$evaluate('document.querySelector("span.YhemCb").innerText') # For some hotel, 'span.E5BaQ' is replaced by 'span.YhemCb' 
        }
        # Extracting hoter STAR '*' type and review number from top right corner of google page but not hotel rating.
        if(!is.null(checkHotelStarType) && !is.null(checkHotelStarType$result$value) && !is.na(parse_number(checkHotelStarType$result$value)) && grepl(paste(hotelStarString, collapse = "|"), checkHotelStarType$result$value))
        {
          hotelStarType <- parse_number(checkHotelStarType$result$value)
          checkActive()
          # For Hotel Rating
          hotelRating <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          }          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.Aq14fc").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          
          # For Hotel Reviews
          checkActive()
          #browser()
          # !! TO DO 2 checked DONE 
          hotelReviews <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
            
          }
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a.hqzQac").innerText')$result$value
          }
          hotelReviews <- parse_number(hotelReviews)
        }
        # Extracting hotel STAR * type, rating and review number from the 3rd div of body of google page.
        else if(!is.null(divSecFromGooglePage) && length(divSecFromGooglePage) > 0)
        {
          #browser() 
          hotelStarType <- findHotelStarType(splitHotelName, divSecFromGooglePage, TRUE)     
          checkActive()
          # browser()
          # !! TO DO 4 done 
          hotelRating <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a/div/div/div[2]/div/span[1]/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          if(is.null(hotelRating)){
            #browser()
            # TO DO 5 done 
            hotelRating <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="0_lbl"]/div[2]/div/span[1]/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          }
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.yi40Hd").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          #browser()
          checkActive()
          # done browser
          hotelReviews <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a/div/div/div[2]/div/span[1]/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="0_lbl"]/div[2]/div/span[1]/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          }
          
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
        # Input: The Social Hub Eindhoven. Output: checkHotelStarType$result$type = "string", checkHotelStarType$result$value = "Hotel"
        # In special case when checkHotelStarType is not null but hotelRating and hotelReviews are null. Example: The Social Hub Eindhoven        
        else if(is.null(hotelRating) && is.null(hotelReviews) && !is.null(checkHotelStarType$result$value) && !is.na(parse_number(checkHotelStarType$result$value)) && !grepl(paste(hotelStarString, collapse = "|"), checkHotelStarType$result$value))
        {
          hotelStarType <- findHotelStarType(splitHotelName, googlePageText, FALSE) 
          # For Hotel Rating
          checkActive()
          hotelRating <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
            
          }
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- driver$Runtime$evaluate('document.querySelector("span.Aq14fc").innerText')$result$value
          }
          hotelRating <- parse_number(hotelRating)
          
          # For Hotel Reviews
          checkActive()
          #browser()
          hotelReviews <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a[data-async-trigger=\'reviewDialog\']").innerText')$result$value
          }
          if(is.null(hotelReviews)){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText')$result$value
            
          }
          if(is.null(hotelReviews)){
            checkActive()
            #browser()
            hotelReviews <- driver$Runtime$evaluate('document.querySelector("a.hqzQac").innerText')$result$value
          }
          hotelReviews <- parse_number(hotelReviews)          
        }
      }   
      
      if(!is.null(hotelStarType)){
        searchTextForNeighborHotel <- paste0(hotelStarType, " star hotel in ", city)
        checkActive()
        #browser()
        driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', searchTextForNeighborHotel,'"'))
        checkActive()
        #browser()
        searchButton1 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[3]/div[2]/form/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
        checkActive
        searchButton2 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="tsf"]/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
        
        checkActive()
        searchButton3 <- driver$Runtime$evaluate('document.querySelector("button.Tg7LZd")')
        checkActive()
        searchButton4 <- driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Search\']")')
        
        
        
        if(searchButton1$result$subtype == "node"){
          driver$Runtime$evaluate('document.evaluate(\'/html/body/div[3]/div[2]/form/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          
        }else if(searchButton2$result$subtype == "node"){
          driver$Runtime$evaluate('document.evaluate(\'//*[@id="tsf"]/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          
          
        }else if(searchButton4$result$subtype == "node"){
          driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Search\']").click()')
          
        }else if(searchButton3$result$subtype=="node"){
          driver$Runtime$evaluate('document.querySelector("button.Tg7LZd").click()')
        }
        Sys.sleep(2)
        
        notify(paste0("Searching neighbor hotels which are ", hotelStarType," star type"), id = id)
        
        # Click on the drop-down menu and check if it is exist by guestDropdownBtn$result$objectId
        checkActive()
        browser()
        guestDropdownBtn1 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
        checkActive()
        guestDropdownBtn2 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
        checkActive()
        guestDropdownBtn3 <- driver$Runtime$evaluate('document.querySelector("div.R2w7Jd")')
        checkActive()
        guestDropdownBtn4 <- driver$Runtime$evaluate('document.querySelector("div[aria-label=\'Select number of guests. Current selection is 2 guests\']")')
        Sys.sleep(3)  
        # browser()
        
        
        # Click to open the drop down-menu.
        if(guestDropdownBtn1$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          
        }else if(guestDropdownBtn2$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
        }else if(guestDropdownBtn4$result$subtype=="node"){
          checkActive()
          driver$Runtime$evaluate('document.querySelector("div[aria-label=\'Select number of guests. Current selection is 2 guests\']").click()')
        }else if(guestDropdownBtn3$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate('document.querySelector("div.R2w7Jd").click()')
          
        }
        
        if(is.null(guestDropdownBtn1$result$objectId) && is.null(guestDropdownBtn2$result$objectId) && is.null(guestDropdownBtn3$result$objectId)){
          output$notFound <- renderText("Not found dropdown button!")
          Waiter$new(html = spin_wave())$hide()
          
        }else{
          # Click to select guest number 1 from the drop-down menu.
          checkActive()
          #browser()
          button1 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          checkActive()
          button2<-driver$Runtime$evaluate('document.querySelector("div.JWXKNd")')
          checkActive()
          #browser()
          button3<-driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          if(button1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            
          }else if(button3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(button2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("div.JWXKNd").click()')
            
          }
          
          notify("Clicking on the dropdown button to select a number of guest ...", id = id)        
          Sys.sleep(3)
          
          hotelRoomType <- c("Single", "Double", "Triple", "Family")
          # Click to open filter tab in the right side of the window.
          checkActive()
          #browser()
          filterButton1<-driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          checkActive()
          filterButton2<-driver$Runtime$evaluate('document.querySelector("button.cd29Sd")')
          checkActive()
          filterButton3<-driver$Runtime$evaluate('document.querySelector("button[aria-label=\'All filters, 1 filter selected\']")')
          checkActive()
          filterButton4 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          
          if(filterButton4$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(filterButton1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            
          }else if(filterButton3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("document.querySelector("button[aria-label=\'All filters, 1 filter selected\']").click()')
          }else if(filterButton2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button.cd29Sd").click()')
          }
          notify("Opening filter tab to filter neighbor hotels ...", id = id)
          Sys.sleep(3)
          
          # Click to select relevant review button from the filter tab.
          checkActive()
          #browser()
          reviewBtn1 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          checkActive()
          reviewBtn2 <- driver$Runtime$evaluate('document.querySelector("input[value=\'13\']")')
          checkActive()
          reviewBtn3 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          if(reviewBtn3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(reviewBtn1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(reviewBtn2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("input[value=\'13\']").click()')
          }
          
          
          notify("Selecting hotels which has relevant reviews and ratings as yours ...", id = id)
          Sys.sleep(3)
          
          # Click to close filter tab.
          checkActive()
          #browser()
          filter1<-driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          checkActive()
          filter2<-driver$Runtime$evaluate('document.querySelector("button.HJuSVb")')
          checkActive()
          filter3<-driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Close dialogue\']")')
          checkActive()
          filter4 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          if(filter4$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            
          }else if(filter1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(filter3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Close dialogue\']").click()')
          }else if(filter2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button.HJuSVb").click()')
          }
          Sys.sleep(3)
          userInputDate <- input$inputDate
          # Click to open date picker.
          checkActive()
          #browser()
          placeHolder1<-driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          checkActive()
          placeHolder3 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          checkActive()
          placeHolder2<-driver$Runtime$evaluate('document.querySelector("input[placeholder=\'Check-in\']")')
          if(placeHolder3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(placeHolder1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(placeHolder2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("input[placeholder=\'Check-in\']").click()')
          } 
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
          if(is.null(checkInDate)){
            checkInDate<-driver$Runtime$evaluate('document.querySelector("div[jsname=\'mG3Az\'][aria-selected=\'true\']").getAttribute("data-iso")')$result$value
          }
          
          # Click to submit check-in and check-out date.
          checkActive()
          #browser()
          submitBtn1 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          checkActive()
          submitBtn2 <- driver$Runtime$evaluate('document.querySelector("button[jsname=\'iib5kc\']")')
          checkActive()
          submitBtn3 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
          
          if(submitBtn3$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(submitBtn1$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
          }else if(submitBtn2$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate('document.querySelector("button[jsname=\'iib5kc\']").click()')
          } 
          notify("Submit date picker...", id = id)
          Sys.sleep(3)
          
          # Collect hotel prices, ratings and number of reviews from the page
          neighborHotelDetails <- list()
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
            
            neighborHotelDetails[[room]][["Prices"]] <- unlist(neighborHotelRoomPrices)
            neighborHotelDetails[[room]][["Ratings"]] <- unlist(neighborHotelRatings)
            neighborHotelDetails[[room]][["Reviews"]] <- unlist(neighborHotelReviews)
            neighborHotelDetails[[room]][["HotelName"]] <- unlist(neighborHotelNames)
            
            if(room == "Family"){
              notify("Ploting the optimized price for each types of room ...", id = id)
              Sys.sleep(2)
              break
            }
            # Click on the '+' button to increase the guest number by 1. !by following Nr. 281 Line!
            checkActive()
            #browser()
            buttonToSubmit1<-driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            checkActive()
            buttonToSubmit2<-driver$Runtime$evaluate('document.querySelector("div.r0Ogod")')  
            checkActive()
            buttonToSubmit4 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            checkActive()
            buttonToSubmit3<-driver$Runtime$evaluate(paste0('document.querySelector("[aria-label=\'Number of travellers. Current number of travellers is ', i, '.\']")'))        
            if(buttonToSubmit4$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(buttonToSubmit1$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(buttonToSubmit3$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0('document.querySelector("[aria-label=\'Number of travellers. Current number of travellers is ', i, '.\']").click()'))
            }else if(buttonToSubmit2$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.querySelector("div.r0Ogod").click()')  
            } 
            i <- i+1
            
            notify("Clicking on the '+' button to increase the guest number by 1 ...", id = id)
            Sys.sleep(3)
            #browser()
            checkActive()
            increaseButton1 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            checkActive()
            increaseButton2 <- driver$Runtime$evaluate('document.querySelector("button.VfPpkd-ksKsZd-mWPk3d")')
            checkActive()
            increaseButton3<-driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Add adult\']")')
            checkActive()
            increaseButton4 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            
            if(increaseButton4$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(increaseButton1$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(increaseButton3$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.querySelector("button[aria-label=\'Add adult\']").click()')
            }else if(increaseButton2$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.querySelector("button.VfPpkd-ksKsZd-mWPk3d").click()')
            } 
            notify(paste0("Looking for the price of ",room," room ..."), id = id)
            Sys.sleep(2)
            
            # Click on the submit button
            checkActive()
            #browser()
            submitBtn1 <- driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            submitBtn2 <- driver$Runtime$evaluate('document.querySelector("button.TkZUKc ")')            
            submitBtn3 <- driver$Runtime$evaluate('document.querySelector("button[jsname=\'kZlJze\']")')   
            submitBtn4 <- driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue')
            
            if(submitBtn4$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(submitBtn1$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()')
            }else if(submitBtn3$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.querySelector("button[jsname=\'kZlJze\']").click()')   
            }else if(submitBtn2$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate('document.querySelector("button.TkZUKc ").click()')   
            } 
            Sys.sleep(3)
          }
          
          if(length(neighborHotelDetails$Single$Prices) > 0 || length(neighborHotelDetails$Double$Prices) > 0 || length(neighborHotelDetails$Triple$Prices) > 0 || length(neighborHotelDetails$Family$Prices) > 0){
            neighHotelDetailsForAllRoomType <- data.frame()
            for(room in hotelRoomType){
              if(length(neighborHotelDetails[[room]]) > 0){
                neighHotelDetailsForARoomType <- as.data.frame(do.call(cbind, neighborHotelDetails[[room]]))
                neighHotelDetailsForARoomType <- cbind(City=city, Type=hotelStarType, RoomType = room, neighHotelDetailsForARoomType)
                neighHotelDetailsForAllRoomType <- rbind(neighHotelDetailsForAllRoomType, neighHotelDetailsForARoomType)
              }
            }
            neighHotelDetailsForAllRoomType <- neighHotelDetailsForAllRoomType %>% filter(!is.na(parse_number(neighHotelDetailsForAllRoomType$Prices)))
            neighHotelDetailsForAllRoomType$Ratings <- parse_number(neighHotelDetailsForAllRoomType$Ratings)
            
            targetHotel <- gsub("\\bin\\s.*", "", input$hotelName, ignore.case = TRUE)
            currencySymbol = NULL
            currencySymbol <- str_remove_all(neighHotelDetailsForAllRoomType$Prices[1],"[0-9.,]")
            makeNetworkGraph(neighHotelDetailsForAllRoomType, targetHotel, hotelRating, hotelReviews, checkInDate, currencySymbol, output)
          }else{
            output$notFound <- renderText("Not found any competitor hotel!")
            Waiter$new(html = spin_wave())$hide()
          }
          Waiter$new(html = spin_wave())$hide()
        }
      }else{
        output$notFound <- renderText("Not found any hotel type!")
        Waiter$new(html = spin_wave())$hide()
      }      
    }
  })
}


shinyApp(ui, server)