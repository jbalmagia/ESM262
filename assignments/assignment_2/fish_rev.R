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
  
#loc_revenue() is a nested function within fish_info that finds total revenue at each location
   loc_revenue = function(catch, price){
     loc_rev = price[,1]*catch
     loc_rev = colSums(loc_rev)
     return(loc_rev)
   }
   
# fish_revenue() is a nested function within fish_info that finds total revenue for each fish species across locations
    fish_revenue = function(catch, price) {
      fish_rev = price[,1]*catch
      fish_rev = rowSums(fish_rev)
      return(fish_rev)
    }

#Jessica - Now we need to make 2 more nested functions, one for revenue by location and one for revenue by fish species
   
#Jenny - I created the remaining nested functions. They output the right things, but can't get them to show up in function return:
   
 return(list(Max_Catch = max_catch, Most_Frequent_Fish = max_by_loc, Revenue_per_Location = loc_rev, Revenue_per_Fish = fish_rev))

}





