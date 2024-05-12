# ggplot(df) + aes(x,y) + geom_col() + geom_text(aes(label = df$z), , position = position_stack(vjust = 0.5), angle = 90)
# df <- data.frame(x = c(2.4, 3.5, 4.6), y = c(7, 10, 4), z = c("Amsterdam", "Prague", "Berlin"))
# https://ggplot2.tidyverse.org/reference/geom_text.html
# infoRoom <- targetPlusNighbHotelSplitOnRoomTypeG2[["Single"]] %>% select(HotelName, Ratings, Reviews, Prices) %>% mutate(Ratings = as.numeric(Ratings), Reviews = parse_number(Reviews), Prices = parse_number(Prices))
# infoRoom <- infoRoom %>% distinct(Ratings, Reviews, .keep_all = TRUE) %>% mutate(rv = paste0("review-",Reviews))
# ggplot(infoRoom, aes(Ratings, Prices, label = rv)) + geom_point() + geom_point(aes(x = targetHotelRatings, y = infoRoom$Prices[[1]]), colour = "red", size = 3) +  geom_text(alpha = 0.6)
# tb <- tibble(Prices = 67, Ratings = 4.4, rv = "review-1900")
# ggplot() + geom_point(data = infoRoom, aes(x = Ratings, y = Prices, label = rv)) + geom_point(data = tb, aes(x = Ratings, y = Prices), colour = "red", size = 3) +  geom_text(size = 3)

makeNetworkGraph <- function(prices_df, targetHotel, targetHotelRatings, targetHotelReviews, checkInDate, currencySymbol, output) {
  roomType <- c("Single", "Double", "Triple", "Family")
  hotelInfo <- prices_df
  splitTargetHotel <- unlist(strsplit(targetHotel, " "))
  targetHotelPattern <- paste0(paste(splitTargetHotel, collapse = ""),"|",targetHotel)
  
  # Filter hotels based on rating criteria
  filteredNighHotel <- function(nighbHotelOnRoomType, ratingRangeFactor, ratingRangeFactorPlus){
    if(targetHotelReviews > 500)
      var <- nighbHotelOnRoomType %>% filter(Ratings >= targetHotelRatings - (ratingRangeFactor + ratingRangeFactorPlus), Ratings <= targetHotelRatings + (ratingRangeFactor + ratingRangeFactorPlus))
    else if(targetHotelReviews > 200 && targetHotelReviews < 500)
      var <- nighbHotelOnRoomType %>% filter(Ratings >= targetHotelRatings - (ratingRangeFactor + ratingRangeFactorPlus), Ratings <= targetHotelRatings + (ratingRangeFactor + ratingRangeFactorPlus / 2))
    else
      var <- nighbHotelOnRoomType %>% filter(Ratings >= targetHotelRatings - (ratingRangeFactor + ratingRangeFactorPlus))
    return(var)  
  }
  
  # Define rating factors
  ratingRangeFactor = 0.3
  ratingRangeFactorPlus = 0.2
  # Filter out all hotels(HotelName, Ratings, Prices, Reviews, RoomType) except targetHotel
  # browser()
  neighborHotelInfo <- hotelInfo %>% filter(!grepl(targetHotelPattern, HotelName, ignore.case = TRUE)) %>% select(c(HotelName, Ratings, Prices, Reviews, RoomType))
  neighborHotelInfo$RoomType <- factor(neighborHotelInfo$RoomType, levels = unique(neighborHotelInfo$RoomType)) # why do we need factor!
  splitNeighborHotelOnRoomType <- split(neighborHotelInfo, neighborHotelInfo$RoomType)
  
  # Initialize filteredHotelData, which will contain final filter data for each room type
  filteredNeighHotelData <- list()    
  for(Type in roomType){
    if(nrow(filteredNeighborHotel    <- filteredNighHotel(splitNeighborHotelOnRoomType[[Type]], ratingRangeFactor, 0*ratingRangeFactorPlus)) >= 4)
      filteredNeighHotelData[[Type]] <- filteredNeighborHotel
    else if(nrow(filteredNeighborHotel    <- filteredNighHotel(splitNeighborHotelOnRoomType[[Type]], ratingRangeFactor, 1*ratingRangeFactorPlus)) >= 4)
      filteredNeighHotelData[[Type]] <- filteredNeighborHotel
    else if(nrow(filteredNeighborHotel    <- filteredNighHotel(splitNeighborHotelOnRoomType[[Type]], ratingRangeFactor, 2*ratingRangeFactorPlus)) >= 4)
      filteredNeighHotelData[[Type]] <- filteredNeighborHotel
    else if(nrow(filteredNeighborHotel    <- filteredNighHotel(splitNeighborHotelOnRoomType[[Type]], ratingRangeFactor, 3*ratingRangeFactorPlus)) >= 4)
      filteredNeighHotelData[[Type]] <- filteredNeighborHotel
    else
      filteredNeighHotelData[[Type]] <- splitNeighborHotelOnRoomType[[Type]]
  }
  # Calculate the optimized price for the target hotel across different room types
  pricesOfTargetHotel <- sapply(filteredNeighHotelData, function(hotel){
    var <- hotel %>% mutate(RatingPriceColumn = Ratings * parse_number(Prices)) %>% summarise(optimizePrice = sum(RatingPriceColumn) / sum(Ratings)) %>% pull(optimizePrice)
    return(round(var))
  })
  if(pricesOfTargetHotel[[1]] > pricesOfTargetHotel[[2]]) 
    pricesOfTargetHotel[[2]] <- pricesOfTargetHotel[[1]] + 2
  # Add target hotel optimize price to splitNighHotelOnRoomType list
  targetPlusNighbHotelSplitOnRoomType <- lapply(splitNeighborHotelOnRoomType, function(hotel){
    hotel <- rbind(tibble(HotelName = paste0(currencySymbol, pricesOfTargetHotel[[unique(hotel$RoomType)]]), Ratings  = "" , Prices = "", Reviews = "", RoomType = unique(hotel$RoomType)), hotel)
    hotel <- hotel %>% head(10) 
    return(hotel)
  })
  
  targetPlusNighbHotelSplitOnRoomTypeG2 <- lapply(splitNeighborHotelOnRoomType, function(hotel){
    hotel <- rbind(tibble(HotelName = targetHotel, Ratings  = targetHotelRatings, Prices = paste0(currencySymbol, pricesOfTargetHotel[[unique(hotel$RoomType)]]), Reviews = targetHotelReviews, RoomType = unique(hotel$RoomType)), hotel)
    return(hotel)
  })
  
  # browser()
  # generate plot
  generatePlot <- function(targetHotel, allHotelForOneRoomType) {
    # select index of target hotel
    rowIndexOfTargetHotel <- 1
    # extract Ratings  number only
    allHotelForOneRoomType$Ratings  <- updateRatings  <- str_remove_all(allHotelForOneRoomType$Ratings , "[^0-9.]")
    # separate target hotel
    edgeRatings <- updateRatings [-rowIndexOfTargetHotel]
    # display first three words of hotel name 
    firstThreeWords <- sapply(str_split(allHotelForOneRoomType$HotelName, "\\s+"), function(words) paste(words[1:min(length(words),3)], collapse = " "))
    # create nodes
    nodes <- tibble(id = 1:length(allHotelForOneRoomType$HotelName), label = firstThreeWords)
    # create edges
    edges <- tibble(
      from = rep(rowIndexOfTargetHotel, length(allHotelForOneRoomType$HotelName) - 1),
      to = setdiff(1:length(allHotelForOneRoomType$HotelName), rowIndexOfTargetHotel),
      Ratings  = edgeRatings 
    )
    # create graph
    graph <- tbl_graph(nodes = nodes, edges = edges)
    # numeric edgeRatings  in graph
    E(graph)$edge_size <- as.numeric(edgeRatings)
    # layout algorithm
    layout <- layout_with_fr(graph, weights = E(graph)$edge_size)
    # plot the graph
    ggraph(graph, layout = layout) +
      # edge links
      geom_edge_link(aes(label=Ratings ),
                     label_colour = "gold",
                     color = "lightgrey", width=1,
                     alpha=1/5
      ) +
      # node points
      geom_node_point(aes(color='red')
      ) +
      # target node point
      geom_node_point(aes(x = layout[rowIndexOfTargetHotel, 1], y = layout[rowIndexOfTargetHotel, 2]),
                      color="black" 
      ) +
      # hotel names label
      geom_text(aes(x = layout[, 1], y = layout[, 2], label = str_wrap(nodes$label, width = 13)),
                color='white', vjust=0.2, hjust=0.5,
                fontface=ifelse(nodes$id == rowIndexOfTargetHotel,"bold", "bold"),
                size = ifelse(nodes$id == rowIndexOfTargetHotel, ifelse(length(allHotelForOneRoomType$HotelName) == 2, 5, 8),
                              ifelse(length(allHotelForOneRoomType$HotelName) == 2, 2.5, 2.8)), check_overlap = TRUE
      ) +
      # hotel Pricess label
      geom_node_text(aes(label = allHotelForOneRoomType$Prices),
                     color='gold',
                     fontface=ifelse(nodes$id == rowIndexOfTargetHotel,"bold", "bold"),
                     vjust=2.7,
                     hjust=0.5,
                     size = ifelse(nodes$id == rowIndexOfTargetHotel, ifelse(length(allHotelForOneRoomType$HotelName) == 2, 3, 3.3),
                                   ifelse(length(allHotelForOneRoomType$HotelName) == 2, 2.5, 3.2))
      ) +
      # Target hotel Ratings 
      annotate("text", x = layout[rowIndexOfTargetHotel, 1], y = layout[rowIndexOfTargetHotel, 2],
               label = allHotelForOneRoomType$Ratings [rowIndexOfTargetHotel],vjust = 4.6, hjust = 0.5, 
               size = ifelse(length(allHotelForOneRoomType$HotelName) == 2, 2.8, 3.5), color = "green", fontface="bold") +
      # background display
      theme_void() +
      theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.position = "none",
      )
  }  
  
  # output rendering
  output$graphAndTable <- renderUI({
    lapply(1:length(targetPlusNighbHotelSplitOnRoomType), function(roomType){
      if(NROW(targetPlusNighbHotelSplitOnRoomType[[roomType]]) > 1){
        # browser()
        fluidRow(
          style = "display: flex; justify-content: center; align-items: center;",
          column(
            width = 12, align = "center",
            
            h3(paste0(names(targetPlusNighbHotelSplitOnRoomType)[roomType], " Room"," Prices (Date - ", checkInDate, ")"), align = "center", style = "margin-bottom: 40px; margin-top: 50px"),
            #h4("Ratings: ", strong(targetHotelRatings, style="color: orange;"),", Reviews: ", strong(targetHotelReviews, style="color: orange;")),
            tagList(
              tags$h5(tags$b("Red dot is ", targetHotel, " with ratings = ", targetHotelRatings, " reviews = ", targetHotelReviews)),                
              plotOutput(paste0("plot-",roomType)),
              tags$h5(tags$b("Optimized price for ", names(targetPlusNighbHotelSplitOnRoomType)[roomType], " room ", tags$span(style="color:red"))),                
              plotOutput(paste0("graph-",roomType), width = "700px", height = "680px"),
              tags$br(),
              tags$br(),
              renderDataTable(targetPlusNighbHotelSplitOnRoomTypeG2[[roomType]] %>% select(HotelName, Ratings, Reviews, Prices) %>% arrange(Ratings))
            )
          )
        )
      }
    })
  })
  
  lapply(1:length(targetPlusNighbHotelSplitOnRoomType), function(roomType){
    if(NROW(targetPlusNighbHotelSplitOnRoomType[[roomType]]) > 1) {
      infoRoom <- targetPlusNighbHotelSplitOnRoomTypeG2[[roomType]] %>% select(Ratings, Reviews, Prices) %>% mutate(Reviews = parse_number(Reviews)) %>% distinct(Ratings, Reviews, .keep_all = TRUE)
      firstPlot <- ggplot(infoRoom, aes(x = Ratings, y = Reviews, label = Prices)) + 
        geom_point(alpha = 0.9, colour = "darkorange") + 
        geom_text(vjust = -0.4) + 
        geom_point(aes(x = targetHotelRatings, y = targetHotelReviews), colour = "red", size = 3, alpha = 0.5) +
        theme(
          axis.title.x = element_text(size = rel(1), face = "bold"),
          axis.title.y = element_text(size = rel(1), face = "bold"),
          axis.text.x =  element_text(size = rel(1), face = "bold"),
          axis.text.y =  element_text(size = rel(1), face = "bold")
        )
      # browser()
      output[[paste0("plot-",roomType)]] <- renderPlot(firstPlot)
      output[[paste0("graph-",roomType)]] <- renderPlot(generatePlot(targetHotel, targetPlusNighbHotelSplitOnRoomType[[roomType]]))  
      #output[[paste0("data-",roomType)]] <- renderDataTable(targetPlusNighbHotelSplitOnRoomTypeG2[[roomType]] %>% select(HotelName, Ratings, Reviews, Prices))
    }
  })
}