hill_climber <- function(n = 10000, verbose = F, path){
  
  old_path <- path
  old_path <- old_path[sample(nrow(old_path)),]
  plot_path(old_path)
  if(verbose){cat(paste0('The cost value is ',calc_cost(old_path),'\n'))}
  
  record <- data.table(iter = numeric(0), cost = numeric(0))
  
  for (i in 1:n){
    
    new_path <- swap_cities(old_path)
    
    if( calc_cost(old_path) > calc_cost(new_path)){
      old_path <- new_path
    }
    record <- rbind(record,data.table(iter = i,cost = calc_cost(old_path)))
    
    if(i%%100 == 0 & verbose){
      plot_path(old_path)
      cat(paste0('The cost value is ',calc_cost(old_path),'\n'))
    }
    
  }
  
  return(list(path = old_path,
              record = record))
  
}