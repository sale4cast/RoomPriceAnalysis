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
  actionButton("hotelBtn", "Optimize Nieghbor Hotel Room Price"),
  tableOutput("data"),
  span(textOutput("targetHotelRating"), style="color: blue;"),
  span(textOutput("targetHotelReview"), style="color: green;"),  
  span(textOutput("notFound"), style="color: red;"),
  
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=0.5")
  ),
  uiOutput("graphAndTable")
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
    debugFileUrl <- "C:/Users/Moon/Desktop/R/RSelenium/RoomPriceAnalysis/DebugOutput.txt"
    cat("", file = debugFileUrl)
    # Show a loading spinner
    Waiter$new(html = spin_loaders(id=3, color = "black"), color = "#00000010")$show()
    output$graph <- renderUI({});
    output$data <- renderTable({})
    output$notFound <- renderText("")
    
    divider <- function(){
      write("--------------------------------------------------------------------------\n", debugFileUrl, sep = "\n", append = TRUE)
    }
    
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
      Sys.sleep(3)
      checkActive()
      driver$Page$navigate("https://www.google.com")
      notify("Navigating a browser http://.... on the device", id = id)
      Sys.sleep(3)
      checkActive()
      textarea <- driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', googleSearchText,'"'))
      write(paste0("SearchField -> ", textarea$resul$value, "\n"), debugFileUrl, sep = "\n", append = TRUE)
      divider()
      # browser()
      # ! ?? is there any chance to add here XPath as a button !
      queryGoogleSearchByAria <- queryGoogleSearchByClass <- NULL
      
      areaLabelQuery <- 'document.querySelector("input[aria-label=\'Google Search\']")'
      classNameQuery <- 'document.querySelector(".gNO89b")'
      
      checkActive()
      queryGoogleSearchByAria <- driver$Runtime$evaluate(areaLabelQuery)  
      checkActive()
      queryGoogleSearchByClass <- driver$Runtime$evaluate(classNameQuery)

      write(paste0("queryGoogleSearchByAria -> ", queryGoogleSearchByAria$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
      write(paste0("queryGoogleSearchByClass -> ", queryGoogleSearchByClass$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
      divider()
      
      if(queryGoogleSearchByAria$result$subtype == "node"){
        checkActive()
        driver$Runtime$evaluate(paste0(areaLabelQuery,".click()"))
      }
      else if((queryGoogleSearchByClass$result$subtype) == "node"){
        checkActive()
        driver$Runtime$evaluate(classNameQuery, ".click()")
      }      
      
      notify("Looking for hotel star * type ... ", id = id)
      Sys.sleep(3)
      # extract 3rd div of body of google page.
      # di vSecFromGooglePage has character(0) value sometimes !!!!!!!!!!!
      # browser()
      
      fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
      halfXPathQuery <- 'document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[3]/div/div/div[1]/div/div/div[1]/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
      areaLabelQuery <- 'document.querySelector("a[aria-labelledby=\'0_lbl\']").innerText'
      classNameQuery <- 'document.querySelector("div.fQtNvd").innerText'
      
      checkActive()
      divSecFromGooglePageByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
      divSecFromGooglePageByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
      divSecFromGooglePageByAreaLabel <- driver$Runtime$evaluate(areaLabelQuery)$result$value
      divSecFromGooglePageByClass <- driver$Runtime$evaluate(classNameQuery)$result$value
      
      write(paste0("divSecFromGooglePageByFullXpath -> ", gsub("\n"," ", divSecFromGooglePageByFullXPath), "\n"), debugFileUrl, sep = "\n", append = TRUE)
      write(paste0("divSecFromGooglePageByPartialXpath -> ", gsub("\n", " ", divSecFromGooglePageByHalfXPath), "\n"), debugFileUrl, sep = "\n", append = TRUE)
      write(paste0("divSecFromGooglePageByAriaLabel -> ", gsub("\n", " ",divSecFromGooglePageByAreaLabel), "\n"), debugFileUrl, sep = "\n", append = TRUE)
      write(paste0("divSecFromGooglePageByClass -> ", gsub("\n", " ", divSecFromGooglePageByClass), "\n"), debugFileUrl, sep = "\n", append = TRUE)
      divider()
      
      # divSecFromGooglePage <- divSecFromGooglePage %>% as.character() %>% tolower()
      
      divSecFromGooglePage <- divSecFromGooglePageByFullXPath
      
      if(is.null(divSecFromGooglePage)){
        checkActive()
        divSecFromGooglePage <- divSecFromGooglePageByHalfXPath
      }      
      if(is.null(divSecFromGooglePage)){
        checkActive()
        divSecFromGooglePage <- divSecFromGooglePageByAreaLabel
      }
      if(is.null(divSecFromGooglePage)){
        checkActive()
        divSecFromGooglePage <- divSecFromGooglePageByClass
      }
      # Extract the whole HTML page text and convert it to lowercase
      # ! ?? is the use of body tag stable to return page whole page text. Can we add XPath next to it !
      checkActive()
      googlePageText <- tolower(as.character(driver$Runtime$evaluate('document.querySelector("body").innerText')$result$value))
      occurrenceFound <-  str_count(googlePageText, hotelNamePattern) ### May need to check properly. Because the occurance for the pullman hotel in riga old town is only 4 ###
      checkActive()
      Sys.sleep(3)
      # browser()
      hotelStarString <- c("2-star","3-star","4-star","5-star", "2 star","3 star","4 star","5 star")
      if(occurrenceFound >= 1){
        # Get splitHotelName by removing the city name Ex: splitHotelName = c("the", "pullman", "hotel", "Riga")
        # Output: splitHotelName = c("the", "pullman", "hotel")            
        splitHotelName <- splitHotelName[splitHotelName != city] ## Need to understand why we are removing city name!
        # Scanning top right corner of google page to look if there is any star type over there.
        
        checkHotelStarType <- NULL
        
        fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
        halfXPathQuery <- 'document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
        classNameQuery1 <- 'document.querySelector("span.E5BaQ").innerText'
        classNameQuery2 <- 'document.querySelector("span.YhemCb").innerText'
        
        checkActive()
        checkHotelStarTypeByFullXpath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
        checkHotelStarTypeByHalfXpath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
        checkHotelStarTypeByClassName1 <- driver$Runtime$evaluate(classNameQuery1)$result$value
        checkHotelStarTypeByClassName2 <- driver$Runtime$evaluate(classNameQuery2)$result$value
        
        write(paste0("checkHotelStarTypeByFullXpath -> ", checkHotelStarTypeByFullXpath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("checkHotelStarTypeByPartialXpath -> ", checkHotelStarTypeByHalfXpath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("checkHotelStarTypeByClassName1 -> ", checkHotelStarTypeByClassName1, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("checkHotelStarTypeByClassName2 -> ", checkHotelStarTypeByClassName2, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        divider()
        
        checkHotelStarType <- checkHotelStarTypeByFullXpath
        
        if(is.null(checkHotelStarType)){
          checkActive()
          checkHotelStarType <- checkHotelStarTypeByHalfXpath
        }
        if(is.null(checkHotelStarType)){
          checkActive()
          checkHotelStarType <- checkHotelStarTypeByClassName1
        }
        if(is.null(checkHotelStarType)){
          checkActive()
          checkHotelStarType <- checkHotelStarTypeByClassName2 # For some hotel, 'span.E5BaQ' is replaced by 'span.YhemCb' 
        }

        # Extracting hoter STAR '*' type and review number from top right corner of google page but not hotel rating.
        if(!is.null(checkHotelStarType) && !is.na(parse_number(checkHotelStarType)) && grepl(paste(hotelStarString, collapse = "|"), checkHotelStarType))
        {
          hotelStarType <- parse_number(checkHotelStarType)
          
          checkActive()
          # For Hotel Rating
          hotelRating <- NULL
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document,null,XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          classNameQuery <- 'document.querySelector("span.Aq14fc").innerText'
          
          hotelRatingByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelRatingByHalfXpath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelRatingByClassName <- driver$Runtime$evaluate(classNameQuery)$result$value
          
          write(paste0("hotelRatingByFullXpath -> ", hotelRatingByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingByPartialXpath -> ", hotelRatingByHalfXpath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingByClassName -> ",hotelRatingByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          hotelRating <- hotelRatingByFullXPath
          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByHalfXpath
          }          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByClassName
          }
          hotelRating <- parse_number(hotelRating)
          
          # For Hotel Reviews
          checkActive()
          #browser()
          # !! TO DO 2 checked DONE 
          hotelReviews <- NULL
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          classNameQuery <- 'document.querySelector("a.hqzQac").innerText'
          
          hotelReviewsByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelReviewsByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelReviewsByClassName <- driver$Runtime$evaluate(classNameQuery)$result$value
          
          write(paste0("hotelReviewsByFullXpath -> ", hotelReviewsByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsByPartialXpath -> ", hotelReviewsByHalfXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsByClassName -> ", hotelReviewsByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          hotelReviews <- hotelReviewsByFullXPath
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByHalfXPath
          }
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByClassName
          }
          hotelReviews <- parse_number(hotelReviews)
        }
        # Extracting hotel STAR * type, rating and review number from the 3rd div of body of google page.
        else if(!is.null(divSecFromGooglePage) && length(divSecFromGooglePage) > 0)
        {
          #browser() 
          hotelStarType <- findHotelStarType(splitHotelName, divSecFromGooglePage, TRUE)    
          
          hotelRating <- NULL
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a/div/div/div[2]/div/span[1]/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="0_lbl"]/div[2]/div/span[1]/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          classNameQuery <- 'document.querySelector("span.yi40Hd").innerText'
          
          checkActive()
          hotelRatingByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelRatingByHalfXpath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelRatingByClassName <- driver$Runtime$evaluate(classNameQuery)$result$value
          
          write(paste0("hotelRatingFromDivSectionByFullXpath -> ", hotelRatingByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingFromDivSectionByPartialXpath -> ", hotelRatingByHalfXpath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingFromDivSectionByClassName -> ",hotelRatingByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          # browser()
          # !! TO DO 4 done 
          
          hotelRating <- hotelRatingByFullXPath
          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByHalfXpath
          }          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByClassName
          }
          hotelRating <- parse_number(hotelRating)
          #browser()
          
          checkActive()

          hotelReviews <- NULL
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[3]/div/div/div/div[1]/div/div/div[1]/a/div/div/div[2]/div/span[1]/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="0_lbl"]/div[2]/div/span[1]/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          classNameQuery <- 'document.querySelector("a.hqzQac")'
          
          hotelReviewsByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelReviewsByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelReviewsByClassName <- driver$Runtime$evaluate(classNameQuery)$result$value
          
          write(paste0("hotelReviewsFromDivSectionByFullXpath -> ", hotelReviewsByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsFromDivSectionByPartialXpath -> ", hotelReviewsByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsFromDivSectionByClassName -> ", hotelReviewsByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          hotelReviews <- hotelReviewsByFullXPath
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByHalfXPath
          }
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByClassName
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
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[1]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null.singleNodeValue.innerText'
          classNameQuery <- 'document.querySelector("span.Aq14fc").innerText'
          
          hotelRatingByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelRatingByHalfXpath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelRatingByClassName <- driver$Runtime$evaluate(classNameQuery)$result$value
          
          write(paste0("hotelRatingByFullXpath -> ", hotelRatingByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingByPartialXpath -> ", hotelRatingByHalfXpath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelRatingByClassName -> ",hotelRatingByClassName, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          hotelRating <- hotelRatingByFullXPath
          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByHalfXpath
          }          
          if(is.null(hotelRating)){
            checkActive()
            hotelRating <- hotelRatingByClassName
          }
          hotelRating <- parse_number(hotelRating)

          # For Hotel Reviews
          checkActive()
          #browser()
          # !! TO DO 2 checked DONE 
          hotelReviews <- NULL
          
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[2]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/a\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="rhs"]/div[4]/div[3]/div/div/div[2]/div/div/div[2]/div[1]/div/span[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.innerText'
          classNameQuery1 <- 'document.querySelector("a.hqzQac").innerText'
          classNameQuery2 <- 'document.querySelector("a[data-async-trigger=\'reviewDialog\']").innerText'
          
          hotelReviewsByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)$result$value
          hotelReviewsByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)$result$value
          hotelReviewsByClassName1 <- driver$Runtime$evaluate(classNameQuery1)$result$value
          hotelReviewsByClassName2 <- driver$Runtime$evaluate(classNameQuery2)$result$value
          
          write(paste0("hotelReviewsByFullXpath -> ", hotelReviewsByFullXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsByPartialXpath -> ", hotelReviewsByHalfXPath, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsByClassName1 -> ", hotelReviewsByClassName1, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("hotelReviewsByClassName2 -> ", hotelReviewsByClassName2, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          hotelReviews <- hotelReviewsByFullXPath
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByHalfXPath
          }
          
          if(is.null(hotelReviews)){
            checkActive()
            hotelReviews <- hotelReviewsByClassName
          }
          hotelReviews <- parse_number(hotelReviews)
        }
      }   
      
      if(!is.null(hotelStarType)){
        searchTextForNeighborHotel <- paste0(hotelStarType, " star hotel in ", city)
        checkActive()
        # browser()
        searchTextForNeighborHotelForTextarea <- driver$Runtime$evaluate(paste0('document.querySelector("textarea").value = "', searchTextForNeighborHotel,'"'))

        write(paste0("SearchTextForNeighborHotel -> ", searchTextForNeighborHotelForTextarea$result$value, "\n"), debugFileUrl, sep = "\n", append = TRUE)

        fullXPathQuery <- 'document.evaluate(\'/html/body/div[3]/div[2]/form/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        halfXPathQuery <- 'document.evaluate(\'//*[@id="tsf"]/div[1]/div[1]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        classNameQuery <- 'document.querySelector("button.Tg7LZd")'
        areaLabelQuery <- 'document.querySelector("button[aria-label=\'Search\']")'
        
        checkActive()
        #browser()
        googleSearchButtonByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
        checkActive()
        googleSearchButtonByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
        
        checkActive()
        googleSearchButtonByClassName <- driver$Runtime$evaluate(classNameQuery)
        checkActive()
        googleSearchButtonByAreaLabel <- driver$Runtime$evaluate(areaLabelQuery)
        
        write(paste0("googleSearchButtonByFullXPath -> ", googleSearchButtonByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("googleSearchButtonByHalfXPath -> ", googleSearchButtonByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("googleSearchButtonByClassName -> ", googleSearchButtonByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("googleSearchButtonByAreaLabel -> ", googleSearchButtonByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        divider()
        
        
        if(googleSearchButtonByFullXPath$result$subtype == "node"){
          driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
        }else if(googleSearchButtonByHalfXPath$result$subtype == "node"){
          driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
        }else if(googleSearchButtonByAreaLabel$result$subtype == "node"){
         driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
        }else if(googleSearchButtonByClassName$result$subtype=="node"){
          driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
        }
        Sys.sleep(3)
        
        notify(paste0("Searching neighbor hotels which are ", hotelStarType," star type"), id = id)
        
        # Click on the drop-down menu and check if it is exist by guestDropdownBtn$result$objectId

        fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        halfXPathQuery <- 'document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[1]/div/div/div/div[3]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        classNameQuery <- 'document.querySelector("div.R2w7Jd")'
        areaLabelQuery <- 'document.querySelector("div[aria-label=\'Select number of guests. Current selection is 2 guests\']")'

        checkActive()
        # browser()
        Sys.sleep(3)  
        guestDropdownBtnByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
        checkActive()
        #Sys.sleep(3)  
        guestDropdownBtnByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
        checkActive()
        #Sys.sleep(3)  
        guestDropdownBtnByAreaLabel <- driver$Runtime$evaluate(classNameQuery)
        checkActive()
        #Sys.sleep(3)  
        guestDropdownBtnByClassName <- driver$Runtime$evaluate(areaLabelQuery)
        
        write(paste0("guestDropdownBtnByFullXPath -> ", guestDropdownBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("guestDropdownBtnByHalfXPath -> ", guestDropdownBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("guestDropdownBtnByAreaLabel -> ", guestDropdownBtnByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        write(paste0("guestDropdownBtnByClassName -> ", guestDropdownBtnByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
        divider()
        
        # Sys.sleep(3)
        #browser()
        
        
        # Click to open the drop down-menu.
        if(guestDropdownBtnByFullXPath$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
        }else if(guestDropdownBtnByHalfXPath$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
        }else if(guestDropdownBtnByAreaLabel$result$subtype=="node"){
          checkActive()
          driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
        }else if(guestDropdownBtnByClassName$result$subtype == "node"){
          checkActive()
          driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
        }
        
        if(is.null(guestDropdownBtnByFullXPath$result$objectId) && is.null(guestDropdownBtnByHalfXPath$result$objectId) && is.null(guestDropdownBtnByAreaLabel$result$objectId) && is.null(guestDropdownBtnByClassName$result$objectId)){
          output$notFound <- renderText("Not found dropdown button!")
          Waiter$new(html = spin_wave())$hide()
          
        }else{
          # Click to select guest number 1 from the drop-down menu.
          fullXPathQuery <- 'document.evaluate(\'/html/body/div[5]/div/div[13]/div[1]/div[2]/div/div/div/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="Odp5De"]/div/div/div/div/div[2]/div[3]/div/g-popup/div[2]/ul/li[1]/a/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          classNameQuery <- 'document.querySelector("div.JWXKNd")'

          checkActive()
          #browser()
          guestSelectbuttonByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
          checkActive()
          guestSelectbuttonByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
          checkActive()
          guestSelectbuttonByClassName <- driver$Runtime$evaluate(classNameQuery)
          #browser()
          
          
          write(paste0("guestSelectbuttonByFullXPath -> ", guestSelectbuttonByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("guestSelectbuttonByHalfXPath -> ", guestSelectbuttonByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("guestSelectbuttonByClassName -> ", guestSelectbuttonByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()

          if(guestSelectbuttonByFullXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(guestSelectbuttonByHalfXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(guestSelectbuttonByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          }
          
          notify("Clicking on the dropdown button to select a number of guest ...", id = id)        
          Sys.sleep(3)
          
          hotelRoomType <- c("Single", "Double", "Triple", "Family")
          # Click to open filter tab in the right side of the window.

        fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div/div[1]/div/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        classNameQuery <- 'document.querySelector("button.cd29Sd")'
        areaLabelQuery <- 'document.querySelector("button[aria-label=\'All filters, 1 filter selected\']")'

          checkActive()
          #browser()
          filterButtonByFullXPath<-driver$Runtime$evaluate(fullXPathQuery)
          checkActive()
          filterButtonByHalfXPath<-driver$Runtime$evaluate(halfXPathQuery)
          checkActive()
          filterButtonByAreaLabel<-driver$Runtime$evaluate(areaLabelQuery)
          checkActive()
          filterButtonByClassName <- driver$Runtime$evaluate(classNameQuery)
          
          
          write(paste0("filterButtonByFullXPath -> ", filterButtonByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("filterButtonByHalfXPath -> ", filterButtonByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("filterButtonByAreaLabel -> ", filterButtonByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("filterButtonByClassName -> ", filterButtonByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          
          if(filterButtonByFullXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(filterButtonByHalfXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(filterButtonByAreaLabel$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
          }else if(filterButtonByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          }
          notify("Opening filter tab to filter neighbor hotels ...", id = id)
          Sys.sleep(3)
          
          # Click to select relevant review button from the filter tab.
          fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[2]/div/div[1]/div/div/section[1]/div/div/div/div[4]/label\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
        classNameQuery <- 'document.querySelector("input[value=\'13\']")'

          checkActive()
          #browser()
          reviewBtnByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
          checkActive()
          reviewBtnByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
          checkActive()
          reviewBtnByClassName <- driver$Runtime$evaluate(classNameQuery)
          
          write(paste0("reviewBtnByFullXPath -> ", reviewBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("reviewBtnByHalfXPath -> ", reviewBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("reviewBtnByClassName -> ", reviewBtnByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()

          if(reviewBtnByFullXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(reviewBtnByHalfXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(reviewBtnByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          }
          
          notify("Selecting hotels which has relevant reviews and ratings as yours ...", id = id)
          Sys.sleep(3)
          
          # Click to close filter tab.
          fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[2]/div[1]/div/div[2]/div[3]/div/div[1]/div[2]/span/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          classNameQuery <- 'document.querySelector("button.HJuSVb")'
          areaLabelQuery <- 'document.querySelector("button[aria-label=\'Close dialogue\']")'

          checkActive()
          #browser()
          closeBtnFilterTabByFullXPath<-driver$Runtime$evaluate(fullXPathQuery)
          checkActive()
          closeBtnFilterTabByHalfXPath<-driver$Runtime$evaluate(halfXPathQuery)
          checkActive()
          closeBtnFilterTabByAreaLabel<-driver$Runtime$evaluate(areaLabelQuery)
          checkActive()
          closeBtnFilterTabByClassName <- driver$Runtime$evaluate(classNameQuery)
          
          write(paste0("closeBtnFilterTabByFullXPath -> ", closeBtnFilterTabByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("closeBtnFilterTabByHalfXPath -> ", closeBtnFilterTabByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("closeBtnFilterTabByAreaLabel -> ", closeBtnFilterTabByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("closeBtnFilterTabByClassName -> ", closeBtnFilterTabByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          if(closeBtnFilterTabByFullXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(closeBtnFilterTabByHalfXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(closeBtnFilterTabByAreaLabel$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
          }else if(closeBtnFilterTabByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          }
          Sys.sleep(3)
          userInputDate <- input$inputDate
          # Click to open date picker.
          fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div/div[2]/div[1]/div/input\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          classNameQuery <- 'document.querySelector("input[placeholder=\'Check-in\']")'
          checkActive()
          #browser()
          datePickerByFullXpath <- driver$Runtime$evaluate(fullXPathQuery)
          
          checkActive()
          datePickerByHalfXpath <- driver$Runtime$evaluate(halfXPathQuery)
          
          checkActive()
          datePickerByClassName<-driver$Runtime$evaluate(classNameQuery)
          
          write(paste0("datePickerByFullXpath -> ", datePickerByFullXpath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("datePickerByHalfXpath -> ", datePickerByHalfXpath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("datePickerByClassName -> ", datePickerByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()

          if(datePickerByFullXpath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(datePickerByHalfXpath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(datePickerByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          } 
          notify("Opening date picker ...", id = id)
          Sys.sleep(3)
          # Click to select check-in date from date picker.
          classNameQuery <- paste0('document.querySelector("',paste0("div[data-iso='", userInputDate, "']"),'")')
          
          checkActive()
          checkInDateBtn <- driver$Runtime$evaluate(classNameQuery)

          write(paste0("checkInDateBtn -> ", checkInDateBtn$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          
          notify("Selecting check-in date ...", id = id)
          Sys.sleep(3)
          
          # Click to select check-out date from date picker.
          classNameQuery <- paste0('document.querySelector("',paste0("div[data-iso='", userInputDate + 1, "']"),'")')
          
          checkActive()
          checkOutDateBtn <- driver$Runtime$evaluate(classNameQuery)
          write(paste0("checkOutDateBtn -> ", checkOutDateBtn$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          
          driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          
          notify("Selecting check-out date ...", id = id)
          Sys.sleep(3)
          
          # Retrieve current date from date picker.
          currentDate <- NULL

          classNameQuery1 <- 'document.querySelector(".HDland").getAttribute("data-iso")'
          classNameQuery2 <- 'document.querySelector("div[jsname=\'mG3Az\'][aria-selected=\'true\']").getAttribute("data-iso")'

          checkActive()
          currentDateByClassName1 <- driver$Runtime$evaluate(classNameQuery1)$result$value
          currentDateByClassName2 <- driver$Runtime$evaluate(classNameQuery2)$result$value
          # browser()

          write(paste0("currentDateByClassName1 -> ", currentDateByClassName1, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("currentDateByClassName2 -> ", currentDateByClassName2, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()

          currentDate <- currentDateByClassName1
          
          if(is.null(currentDate)){
            currentDate<-currentDateByClassName2
          }
          
          # Click to submit check-in and check-out date.
          fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[2]/div/div[2]/div/div[2]/div[4]/div/button[2]\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
          classNameQuery <- 'document.querySelector("button[jsname=\'iib5kc\']")'
          checkActive()
          #browser()
          submitDatePickerBtnByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
          checkActive()
          submitDatePickerBtnByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
          checkActive()
          submitDatePickerBtnByClassName <- driver$Runtime$evaluate(classNameQuery)
          
          write(paste0("submitDatePickerBtnByFullXPath -> ", submitDatePickerBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("submitDatePickerBtnByHalfXPath -> ", submitDatePickerBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          write(paste0("submitDatePickerBtnByClassName -> ", submitDatePickerBtnByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
          divider()
          
          if(submitDatePickerBtnByFullXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
          }else if(submitDatePickerBtnByHalfXPath$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
          }else if(submitDatePickerBtnByClassName$result$subtype=="node"){
            checkActive()
            driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
          } 
          notify("Submit date picker...", id = id)
          Sys.sleep(3)
          
          # Collect hotel prices, ratings and number of reviews from the page
          neighborHotelDetails <- list()
          for(room in hotelRoomType){
            
            # Collecting room price of neighbor hotels from the page.
            notify(paste0("Scanning ",room, " room price of neighbor hotels ..."), id = id)
            Sys.sleep(2)
            
            # collect hotel prices, ratings and number of review
            neighborHotelRoomPricesList <- list()
            neighborHotelRatingsList <- list()
            neighborHotelReviewsList <- list()
            neighborHotelNamesList <- list()
            
            numberOfPageToScan <- 2 
            
            for(i in 1:numberOfPageToScan){
              checkActive()
              priceElement <- driver$Runtime$evaluate(
                'var elements = document.querySelectorAll(".K1smNd > c-wiz[jsrenderer=\'hAbFdb\'] .PwV1Ac");
                 var elementPrices = [];
                 elements.forEach(function(element) {
                   elementPrices.push(element.innerText);
                 });
                 elementPrices.join("@");'
              )
              
              write(paste0(room,"RoomPriceForPage-",i ," -> ", gsub("\n", " ", priceElement$result$value), "\n"), debugFileUrl, sep = "\n", append = TRUE)
              
              splittedPriceElements <- unlist(strsplit(priceElement$result$value, "@"))
              
              neighborHotelRoomPrices <- lapply(splittedPriceElements, function(aElement){
                ## Great Deal\n$80    Deal 5%\n$90
                roomPrice <- unlist(strsplit(aElement,"\n"))
                if(length(roomPrice) > 1){
                  return(roomPrice[[2]])
                }
                return (aElement)
              })
              
              neighborHotelRoomPricesList <- c(neighborHotelRoomPricesList, neighborHotelRoomPrices)
              
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
              
              write(paste0(room,"RoomRatingForPage-",i ," -> ", gsub("\n", " ", ratingElement$result$value), "\n"), debugFileUrl, sep = "\n", append = TRUE)
              
              splittedRatingElements <- strsplit(ratingElement$result$value, "\n")
              neighborHotelRatings <- lapply(splittedRatingElements, function(aElement){
                return (aElement)
              })
              
              neighborHotelRatingsList <- c(neighborHotelRatingsList, neighborHotelRatings)
              
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
              
              write(paste0(room,"RoomReviewForPage-",i ," -> ", gsub("\n", " ", reviewElement$result$value), "\n"), debugFileUrl, sep = "\n", append = TRUE)
              
              splittedReviewElements <- strsplit(reviewElement$result$value, "\n")
              
              neighborHotelReviews <- lapply(splittedReviewElements, function(aElement){
                return(gsub("[()]", "", (aElement)))
              })
              
              neighborHotelReviewsList <- c(neighborHotelReviewsList, neighborHotelReviews)
              
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
              
              write(paste0(room,"RoomHotelNameForPage-",i ," -> ", gsub("\n", " ", hotelNameElement$result$value), "\n"), debugFileUrl, sep = "\n", append = TRUE)
              
              splittedHotelNameElements <- strsplit(hotelNameElement$result$value, "\n")
              
              neighborHotelNames <- lapply(splittedHotelNameElements, function(aElement){
                return(aElement)
              })
              
              neighborHotelNamesList <- c(neighborHotelNamesList, neighborHotelNames)
              
              # Clicking on next button 
              fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[2]/div[2]/main/c-wiz/span/c-wiz/c-wiz[22]/div[1]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
              halfXPathQuery <- 'document.evaluate(\'//*[@id="id"]/c-wiz/c-wiz[22]/div[1]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
              classNameQuery <- 'document.querySelector("button[jsname=\'OCpkoe\']")'

              checkActive()
              nextPageScanningBtnByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
              checkActive()
              nextPageScanningBtnByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
              checkActive()
              nextPageScanningBtnByClassName <- driver$Runtime$evaluate(classNameQuery)
              
              write(paste0(room, "RoomNextPageScanningBtnByFullXPath -> ", nextPageScanningBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
              write(paste0(room, "RoomNextPageScanningBtnByHalfXPath -> ", nextPageScanningBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
              write(paste0(room, "RoomNextPageScanningBtnByClassName -> ", nextPageScanningBtnByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
              divider()
              
              if(i == numberOfPageToScan){
                break
              }
              
              if(nextPageScanningBtnByFullXPath$result$subtype=="node"){
                checkActive()
                driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
              }else if(nextPageScanningBtnByHalfXPath$result$subtype=="node"){
                checkActive()
                driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
              }else if(nextPageScanningBtnByClassName$result$subtype=="node"){
                checkActive()
                driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
              }else{
                break
              }
              Sys.sleep(3.5)
            }
            
            neighborHotelDetails[[room]][["Prices"]] <- unlist(neighborHotelRoomPricesList)
            neighborHotelDetails[[room]][["Ratings"]] <- unlist(neighborHotelRatingsList)
            neighborHotelDetails[[room]][["Reviews"]] <- unlist(neighborHotelReviewsList)
            neighborHotelDetails[[room]][["HotelName"]] <- unlist(neighborHotelNamesList)
            
            if(room == "Family"){
              notify("Ploting the optimized price for each types of room ...", id = id)
              Sys.sleep(2)
              break
            }
            # Click on the '+' button to increase the guest number by 1. !by following Nr. 281 Line!
            fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[1]/div\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            classNameQuery <- 'document.querySelector("div.r0Ogod")'
            areaLabelQuery <- paste0('document.querySelector("[aria-label=\'Number of travellers. Current number of travellers is ', i, '.\']")')
            checkActive()
            #browser()
            guestDropdownBtnByFullXPath<-driver$Runtime$evaluate(fullXPathQuery)
            checkActive()
            guestDropdownBtnByHalfXPath<-driver$Runtime$evaluate(halfXPathQuery)  
            checkActive()
            guestDropdownBtnByAreaLabel <- driver$Runtime$evaluate(areaLabelQuery)
            checkActive()
            guestDropdownBtnByClassName<-driver$Runtime$evaluate(classNameQuery)      
            
            write(paste0(room, "RoomGuestDropdownBtnByFullXPath -> ", guestDropdownBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomGuestDropdownBtnByHalfXPath -> ", guestDropdownBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomGuestDropdownBtnByAreaLabel -> ", guestDropdownBtnByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomGuestDropdownBtnByClassName -> ", guestDropdownBtnByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            divider()
            
            if(guestDropdownBtnByFullXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
            }else if(guestDropdownBtnByHalfXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
            }else if(guestDropdownBtnByAreaLabel$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
            }else if(guestDropdownBtnByClassName$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
            } 
            i <- i+1
            
            notify("Clicking on the '+' button to increase the guest number by 1 ...", id = id)
            Sys.sleep(3)
            #browser()

            fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[1]/div[1]/div[2]/div/span[3]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            classNameQuery <- 'document.querySelector("button.VfPpkd-ksKsZd-mWPk3d")'
            areaLabelQuery <- 'document.querySelector("button[aria-label=\'Add adult\']")'

            checkActive()
            increaseGuestButtonByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
            checkActive()
            increaseGuestButtonByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)
            checkActive()
            increaseGuestButtonByAreaLabel <- driver$Runtime$evaluate(areaLabelQuery)
            checkActive()
            increaseGuestButtonByClassName <- driver$Runtime$evaluate(classNameQuery)
            
            write(paste0(room, "RoomIncreaseGuestButtonByFullXPath -> ", increaseGuestButtonByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomIncreaseGuestButtonByHalfXPath -> ", increaseGuestButtonByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomIncreaseGuestButtonByAreaLabel -> ", increaseGuestButtonByAreaLabel$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomIncreaseGuestButtonByClassName -> ", increaseGuestButtonByClassName$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            divider()
            
            if(increaseGuestButtonByFullXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
            }else if(increaseGuestButtonByHalfXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
            }else if(increaseGuestButtonByAreaLabel$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(areaLabelQuery, ".click()"))
            }else if(increaseGuestButtonByClassName$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(classNameQuery, ".click()"))
            } 
            notify(paste0("Looking for the price of ",room," room ..."), id = id)
            Sys.sleep(2)
            
            # Click on the submit button
            fullXPathQuery <- 'document.evaluate(\'/html/body/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            halfXPathQuery <- 'document.evaluate(\'//*[@id="yDmH0d"]/c-wiz[2]/div/c-wiz/div[1]/div[1]/div[1]/c-wiz/div/div/div[1]/div/div[1]/div[3]/div/div/div/div[2]/div[2]/div[2]/div[2]/button\', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue'
            classNameQuery1 <- 'document.querySelector("button.TkZUKc ")'
            classNameQuery2 <- 'document.querySelector("button[jsname=\'kZlJze\']")'

            checkActive()
            #browser()
            submitGuestBtnByFullXPath <- driver$Runtime$evaluate(fullXPathQuery)
            submitGuestBtnByHalfXPath <- driver$Runtime$evaluate(halfXPathQuery)            
            submitGuestBtnByClassName1 <- driver$Runtime$evaluate(classNameQuery1)   
            submitGuestBtnByClassName2 <- driver$Runtime$evaluate(classNameQuery2)
            
            write(paste0(room, "RoomSubmitGuestBtnByFullXPath -> ", submitGuestBtnByFullXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomSubmitGuestBtnByHalfXPath -> ", submitGuestBtnByHalfXPath$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomSubmitGuestBtnByClassName1 -> ", submitGuestBtnByClassName1$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            write(paste0(room, "RoomSubmitGuestBtnByClassName2 -> ", submitGuestBtnByClassName2$result$description, "\n"), debugFileUrl, sep = "\n", append = TRUE)
            divider()
            
            if(submitGuestBtnByFullXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(fullXPathQuery, ".click()"))
            }else if(submitGuestBtnByHalfXPath$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(halfXPathQuery, ".click()"))
            }else if(submitGuestBtnByClassName1$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(classNameQuery1, ".click()"))  
            }else if(submitGuestBtnByClassName2$result$subtype=="node"){
              checkActive()
              driver$Runtime$evaluate(paste0(classNameQuery2, ".click()")) 
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
            # print(neighHotelDetailsForAllRoomType)
            # browser()
            makeNetworkGraph(neighHotelDetailsForAllRoomType, targetHotel, hotelRating, hotelReviews, currentDate, currencySymbol, output)
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