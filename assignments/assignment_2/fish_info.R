#'Revenue from fish catch by fishery and location
#'
#'This function calculates the frequency of fish species caught by location, total revenue by location, and total revenue by fishery
#'
#'@param price a data frame that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return frequent the most frequently caught fish in each location
#'@return rev_location the total revenue of all fisheries by location
#'@return rev_fish the total revenue of each fishery across all locations



fish_info = function(catch,price, graph = FALSE){
  
#fish_max finds most freq fish caught at each location
  fish_max = list(colnames(catch), rownames(catch)[apply(catch, 2, which.max)])
  
# total_revenue finds the total revenue for all fisheries at all locations
   total_revenue = sum(price[,1]*catch)
   
#loc_revenue finds total revenue at each location
   
   loc_revenue = price[,1]*catch
   loc_revenue = colSums(loc_revenue)
   loc_revenue_df = as.data.frame(loc_revenue)
   loc_revenue_df = rownames_to_column(loc_revenue_df, "location")
   
# fish_revenue finds total revenue for each fish species across locations
   fish_revenue = price[,1]*catch
   fish_revenue = rowSums(fish_revenue)
   
#Jenny - I created the remaining equations. To output them, I had to get rid of the nested functions. I also created a new data frame with fish counts by locations (fish_loc), but the fish_max equation isn't working properly now. Sorry! Trying to troubleshoot.
   
# I also created the graph, but it won't run because we need to figure out how to specify the "x = location". Basically, we need to rename a column in loc_revenue_df to "location"
   
   
   
   if(graph == TRUE) {
     graph <- ggplot(loc_revenue_df) +
       geom_col(aes(x = location, y = loc_revenue)) +
       labs(x = "Location", y = "Revenue ($)",
            title = "Fishing Revenue per Caribbean Country",
            subtitle = sprintf("The total revenue for fisheries within the Caribbean is $%d",
                               total_revenue)) +
       theme_classic()
     
     graph
   }
   else graph = NULL
   
  return(list(Max_Catch = fish_max, Revenue_per_Location = loc_revenue, Revenue_per_Fish = fish_revenue, Total_Revenue = total_revenue, Graph = graph))

}





