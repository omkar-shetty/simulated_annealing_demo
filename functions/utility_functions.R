
# plot(coords.df)
# lines(x = coords.df$long, y = coords.df$lat, type = "l")
# 
# calc_cost(coords.df)

library(data.table)

calc_cost <- function(path){
  
  path <- setDT(path)
  path <- rbind(path,path[1,])
  
  #plot(path)
  #lines(x = path$long, y = path$lat, type = "l")
  
  path[,long_lag := shift(long, n =1, type = 'lead')]
  path[,lat_lag := shift(lat, n =1, type = 'lead')]
  
  path[,euc_dist := sqrt((long - long_lag)^2 + (lat - lat_lag)^2)]
  
  cost <- path[,sum(euc_dist, na.rm = T)]
  
  cost
}

plot_path <- function(path){
  
  path <- rbind(path,path[1,])
  
  plot(path)
  lines(x = path$long, y = path$lat, type = "l")
  
}

swap_cities <- function(path,city1 = NULL,city2 = NULL, random_var = TRUE){
  
  # if(city1 > nrow(path) | city2 > nrow(path)){
  #   stop('INCORRECT INDEX')}
  
  if(random_var){
    city1 <- sample(nrow(path),1)
    city2 <- sample(setdiff(1:nrow(path),city1),1)
    
  }
    
  city1_coords <- path[city1,]
  city2_coords <- path[city2,]
  
  path[city1,] <- city2_coords
  path[city2,] <- city1_coords
  
  return(path)
}
