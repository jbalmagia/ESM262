#'Summary of Revenue for Fisheries
#'
#'This function returns revenue information about fisheries including the most frequent fish species caught at each location, revenue of all fisheries at each location, total revenue by location, and total revenue by fishery
#'
#'@param price a data frame that has prices for different fish
#'@param catch a table that has the number caught for each fish species for each location, where locations are columns and fish are rows
#'@return Max_Catch the most frequent fish species caught at each location
#'@return Revenue_per_Location the total revenue at each location
#'@return Revenue_per_Fish the total revenue of each fishery across all locations
#'@return Total_Revenue the total revenue of all fisheries at all locations
#'@return Graph optional graph of revenue by location



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
   

#graph of revenue by location, optional if user requests
   
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





