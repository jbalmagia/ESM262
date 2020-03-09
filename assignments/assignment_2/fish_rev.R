#'Revenue from fish catch by fishery and location
#'
#'This function calculates the frequency of fish species caught by location, total revenue by location, and total revenue by fishery
#'
#'@param price a data frame that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return frequent the most frequently caught fish in each location
#'@return rev_location the total revenue of all fisheries by location
#'@return rev_fish the total revenue of each fishery across all locations



fish_rev = function(catch,price, graph = FALSE){
  
# fish_max finds most freq fish caught for each location
   # fish_max = as.factor(catch)
   # fish_max = names(which.max(summary(catch)))
   fish_max = list(colnames(catch), rownames(catch)[apply(catch, 2, which.max)])
  
# Use sapply() to find most freq fish caught at each location in catch table
   # max_by_loc = sapply(catch, fish_max)
   # return(max_by_loc)
  
# total_revenue finds the total revenue at all locations
   total_revenue = sum(price[,1]*catch)
   
# loc_revenue finds total revenue at each location
   loc_revenue = price[,1]*catch
   loc_revenue = colSums(loc_revenue)
   loc_revenue_df = as.data.frame(loc_revenue)
   loc_revenue_df = rownames_to_column(loc_revenue_df, "location")
  # loc_revenue_df = colnames(c("location", "loc_revenue"))
  # colnames(test) <- (c("loc_revenue"))
   
# fish_revenue finds total revenue for each fish species across locations
   fish_revenue = price[,1]*catch
   fish_revenue = rowSums(fish_revenue)
   
   
   if(graph == TRUE) {
     graph <- ggplot(loc_revenue_df) +
       geom_col(aes(x = location, y = loc_revenue), fill = "darkblue") +
       labs(x = "Location", y = "Revenue ($)",
            title = "Fishing Revenue per Caribbean Country",
            subtitle = sprintf("The total revenue for fisheries within the Caribbean is $%d",
                               total_revenue)) +
       theme_classic()
     
     graph
   }
   
  return(list(Max_Catch = fish_max, Revenue_per_Location = loc_revenue, Revenue_per_Fish = fish_revenue, Total_Revenue = total_revenue, Graph = graph))

}





