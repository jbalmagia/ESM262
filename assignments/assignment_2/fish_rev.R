#'Revenue from fish catch by fishery and location
#'
#'This function calculates the frequency of fish species caught by location, total revenue by location, and total revenue by fishery
#'
#'@param price a data frame that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return frequent the most frequently caught fish in each location
#'@return rev_location the total revenue of all fisheries by location
#'@return rev_fish the total revenue of each fishery across all locations



fish_info = function(catch,price){
  
#fish_max() is a nested function within fish_info that finds most freq fish caught for one location
  fish_max = function(catch){                     
    catch = as.factor(catch)
    max_catch = names(which.max(summary(catch)))
    return(max_catch)
  }
  
#Use sapply() to find most freq fish caught at each location in catch table
  max_by_loc = sapply(catch, fish_max)
  return(max_by_loc)
  
#loc_revenue() is a nested function within fish_info that finds total revenue one location
   loc_revenue = function(catch, price){
     loc_rev = summary(catch) * price
     return(loc_rev)
   }
   
# Use sapply() to find total revenue at each location in catch table
   rev_by_loc = sapply(catch, loc_revenue)
   return(rev_by_loc)
   
# fish_revenue() is a nested function within fish_info that finds total revenue for each fish species across locations
   fish_revenue = function(catch, price) {
     fish_rev = 
   }
  
# Use sapply() to find total revenue for each fish species
   rev_by_fish = sapply(catch, fish_revenue)
   return(rev_by_fish)
  

#Jessica - Now we need to make 2 more nested functions, one for revenue by location and one for revenue by fish species
   
 return(list(max_by_loc, loc_price, rev_by_loc))

}





