source('functions/utility_functions.R')

# Local Descent Search ----------------------------------------------------

long <- rep(seq(1:6),each = 6)
lat <- rep(seq(1:6),6)

coords.df <- data.frame(long=long, lat=lat)
old_path <- coords.df

plot_path(old_path)
cat(paste0('The cost value is ',calc_cost(old_path),'\n'))

record <- data.frame(iter = numeric(0), cost = numeric(0))

for (i in 1:100000){
  
  new_path <- swap_cities(old_path)
  
  if( calc_cost(old_path) > calc_cost(new_path)){
    old_path <- new_path
  }
  record <- rbind(record,c(i,calc_cost(old_path)))
  
  if(i%%100 == 0){
    plot_path(old_path)
    cat(paste0('The cost value is ',calc_cost(old_path),'\n'))
  }
    
}

setnames(record,c('iter','cost'))
ggplot(data = record) + geom_point(aes(x = iter, y = cost))
