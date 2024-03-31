findSplitHotelName <- function(inputHotelName) {
  # List of prepositions to remove
  prepositions <- c("on", "in", "at", "to", "of", "from")
  
  # Create a regular expression pattern for prepositions
  prepositionPattern <- paste(prepositions, collapse = "|")
  
  # Remove prepositions from the input hotel name.
  # EX: Input: hotelName = "the pullman hotel in riga old town", Output: hotelName = "the pullman hotel  riga old town"
  hotelName <- gsub(paste0("\\b(?:", prepositionPattern, ")\\b"), "", inputHotelName, ignore.case = TRUE)
  
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
  return(splitHotelName)
}

findHotelNamePattern <- function(inputHotelName, splitHotelName, googleSearchText){
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
  hotelNamePattern <- c(inputHotelName, googleSearchText, paste(splitHotelName, collapse = " "), hotelNameWithArticleAndSpace, hotelNameWithArticleWithoutSpace, hotelNameWithoutArticleAndSpace, hotelNameWithoutArticleWithSpace)
  
  hotelNamePattern <- paste(unique(hotelNamePattern), collapse = "|")
  return(hotelNamePattern)
}

findHotelStarType <- function(hotelName, pageText, isDivSecFromGooglePage){
  hotelStarType <- NULL
  # Split pageText by newline
  if(!isDivSecFromGooglePage){
    pageText <- strsplit(pageText, "\n")
    pageText <- unlist(pageText)
  }
  # Ex: hotelName = c("the", "pullman", "hotel")
  # Ex: aLineFromPageText = "In old town there is a pullman hotel"
  # Input: sapply(hotelName, function(hotelName) grepl(hotelName, aLineFromPageText, ignore.case = TRUE))
  # Output: the     pullman   hotel 
  #         FALSE    TRUE     TRUE   
  scanHotelNameWithinaLine <- function(aLineFromPageText) {
    findHotelNameWithinaLine <- sapply(hotelName, function(hotelName) grepl(paste0("\\b",hotelName,"\\b"), aLineFromPageText, ignore.case = TRUE))
    hotelNamePesent <- all(findHotelNameWithinaLine)
    findNumberWithinaLine <- grepl("\\b[1-5]\\b", aLineFromPageText)
    return(hotelNamePesent && findNumberWithinaLine)
  }
  # Scan line-by-line from pageText and separate only those lines which are matched with hotelName = c("the", "pullman", "hotel") 
  matchedLinesFromPageText <- pageText[sapply(pageText, scanHotelNameWithinaLine)]
  if(paste(hotelName, collapse = " ") == "the social hub"){
    hotelStarType <- 3
  }
  else if(length(matchedLinesFromPageText) != 0){
    # Regular expression format to extract the star rating
    # Input Ex1: 4 star hotel  Output: 4
    # Input Ex2: 4-star hotel  Output: 4
    # Input Ex3: 4_star hotel  Output: 4
    # Input Ex4: 4* hotel      Output: 4
    
    star_pattern <- "\\d(?:[-_\\s]star|\\sstar|[*])"
    # Scan line-by-line from the lines which are already extracted from googlePageText by matching hotel name.
    # Identify and extract the substring (Ex: "4-star") from scanned lines by using regexpr. 
    # using by regmatches function. And then parse it by using parse_number function.
    # Input: aLine <- "there is a 4-star hotel in Riga"
    # Output: regmatches(aLine, regexec(star_pattern, aLine) returns "4-star"
    
    # Exception: aLine = "the pullman hotel in Riga is offering a 4% discount in single room"
    # Input: hotelStarType <- parse_number(regmatches(aLine, regexec("\\d(?:[-_\\s]star|\\sstar|[*])", aLine))[[1]])
    # Output: numeric(0)
    # The output is empty and there is no row in hotelStarType. Therefore, it needs to check the length(hotelStarType) > 0 
    
    starTypeFromMatchedLines <- lapply(matchedLinesFromPageText, function(aLine){
      starType <- parse_number(regmatches(aLine, regexec(star_pattern, aLine))[[1]])
      if(length(starType) > 0)
        return(starType)
    })
    
    starTypeFromMatchedLines <- unlist(starTypeFromMatchedLines)
    
    #Input: starTypeFromMatchedLines = c(3,2,4,5,4,3,2,3,3,4) Output: 3
    if(length(starTypeFromMatchedLines) > 0)
      hotelStarType <- starTypeFromMatchedLines %>% as_tibble() %>% group_by(value) %>% count() %>% filter(n == max(.$n)) %>% select(value) %>% .[[1]]
    
  }  
  return(hotelStarType)
}

findSplitHotelName <- function(inputHotelName){
  # List of prepositions to remove
  prepositions <- c("on", "in", "at", "to", "of", "from")
  
  # Create a regular expression pattern for prepositions
  prepositionPattern <- paste(prepositions, collapse = "|")
  
  # Remove prepositions from the input hotel name.
  # EX: Input: hotelName = "the pullman hotel in riga old town", Output: hotelName = "the pullman hotel  riga old town"
  hotelName <- gsub(paste0("\\b(?:", prepositionPattern, ")\\b"), "", inputHotelName, ignore.case = TRUE)
  
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
}