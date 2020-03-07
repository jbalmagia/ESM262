#'Revenue from fish catch by fishery and location
#'
#'This function calculates the frequency of fish species caught by location, total revenue by location, and total revenue by fishery
#'
#'@param price a that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return frequent the most frequently caught fish in each location
#'@return rev_location the total revenue of all fisheries by location
#'@return rev_fish the total revenue of each fishery across all locations



fish_info = function(catch,price){
  
  fish_max = function(catch){
    catch = as.factor(catch)
    max_catch = names(which.max(summary(catch)))
    return(max_catch)
  }
  max_by_loc = sapply(catch, fish_max)
  return(max_by_loc)
  }



