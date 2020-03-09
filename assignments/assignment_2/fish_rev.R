#'Revenue from fish catch by fishery and location
#'
#'This function calculates the frequency of fish species caught by location, total revenue by location, and total revenue by fishery
#'
#'@param price a data frame that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return frequent the most frequently caught fish in each location
#'@return rev_location the total revenue of all fisheries by location
#'@return rev_fish the total revenue of each fishery across all locations



fish_rev = function(catch,price){
  
#fish_max finds most freq fish caught for one location
   fish_max = as.factor(catch)
   fish_max = names(which.max(summary(catch)))
  
#Use sapply() to find most freq fish caught at each location in catch table
   # max_by_loc = sapply(catch, fish_max)
   # return(max_by_loc)
  
#loc_revenue finds total revenue at each location
   loc_revenue = price[,1]*catch
   loc_revenue = colSums(loc_revenue)
   
# fish_revenue() is a nested function within fish_info that finds total revenue for each fish species across locations
   fish_revenue = price[,1]*catch
   fish_revenue = rowSums(fish_revenue)
   
#Jenny - I created the remaining equations. To output them, I had to get rid of the nested functions. I also created a new data frame with fish counts by locations (fish_loc), but the fish_max equation isn't working properly now. Sorry! Trying to troubleshoot.
   
  return(list(Max_Catch = fish_max, Revenue_per_Location = loc_revenue, Revenue_per_Fish = fish_revenue))

}





