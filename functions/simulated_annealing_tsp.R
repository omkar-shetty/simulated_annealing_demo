sa_tsp <- function(city_list,temp_initial,cooling_const,temp_min, max_iter){
  
  path <- city_list
  path <- path[sample(nrow(path)),]
  plot_path(path)
  record <- data.table("iter" = numeric(0), "cost" = numeric(0), "temp" = numeric(0))
  
  # stopping criteria
  temp_record <- data.table("temp_id" = integer(0),"temp" = numeric(0),"cost" = numeric(0))
  
  #Keep a record of the best solution
  best_cost <- calc_cost(path)
  best_path <- path
    
  temp <- temp_initial
  #k <- 1
  id <- 1  
  
  #while (temp >= temp_min & k <= max_iter){
  while (temp >= temp_min){
    
    for( k in 1:max_iter){
      new_path <- swap_cities(path)
      old_cost <- calc_cost(path)
      new_cost <- calc_cost(new_path)
      
      diff <- new_cost - old_cost
      
      if(best_cost - new_cost >  0){
        best_cost <- new_cost
        best_path <- new_path
      }
      
      
      if(acc_func(diff,temp) >= runif(1,0,1)){
        path <- new_path
      }
      
      record <- rbind(record,data.table("iter" = k,"cost" = calc_cost(path),"temp" = temp))
      
      if(k%%500 == 0){
        plot_path(path)
        cat(paste0('Cost at iteration ', k ,'& temp ',temp,' is ',calc_cost(path),'\n'))
      }
    } 
    
    temp_record <- rbind(temp_record,data.table("temp_id" = id,"temp" = temp,"cost" = best_cost))
    
    if (id > 31){
      if(temp_record[temp_id == id,cost] >= temp_record[temp_id == id-10,cost]){
      break
      }
    }
    
    id <- id + 1
    
    
    temp <- temp*cooling_const
    #k <- k+1
  }
  return(list(temp_record = temp_record,
              record = record,
              best_cost = best_cost,
              best_path = best_path))
  
}

sa_tsp_v2 <- function(city_list,temp_initial,cooling_const,temp_min, max_iter){
  
  path <- city_list
  path <- path[sample(nrow(path)),]
  plot_path(path)
  record <- data.table("iter" = numeric(0), "cost" = numeric(0), "temp" = numeric(0))
  
  
  #Keep a record of the best solution
  best_cost <- calc_cost(path)
  best_path <- path
  
  temp <- temp_initial
  k <- 1
  
  while (temp >= temp_min & k <= max_iter){
  #while (temp >= temp_min){
    
      new_path <- swap_cities(path)
      old_cost <- calc_cost(path)
      new_cost <- calc_cost(new_path)
      
      diff <- new_cost - old_cost
      
      if(best_cost - new_cost >  0){
        best_cost <- new_cost
        best_path <- new_path
      }
      
      
      if(acc_func(diff,temp) >= runif(1,0,1)){
        path <- new_path
      }
      
      record <- rbind(record,data.table("iter" = k,"cost" = calc_cost(path),"temp" = temp))
      
      if(k%%500 == 0){
        plot_path(path)
        cat(paste0('Cost at iteration ', k ,'& temp ',temp,' is ',calc_cost(path),'\n'))
      }
    
      if(k > 50000 & k%%1000 == 0){
        if(record[iter == k,.(cost)] >= record[iter == k-1000,.(cost)]){
          break
        }
      }
    
    temp <- temp*(cooling_const)
    k <- k+1
  }
  return(list(record = record,
              best_cost = best_cost,
              best_path = best_path))
  
}

x <- sa_tsp(city_list = coords.df, temp_initial = 3, cooling_const = 0.99, temp_min = 0.01, max_iter = 1000)

saveRDS(x,'C:/ORS/Hack_Clubs_Data/SA_TSP/run_6x6.RDS')
