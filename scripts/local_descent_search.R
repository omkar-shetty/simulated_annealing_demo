source('functions/utility_functions.R')

# Local Descent Search ----------------------------------------------------

long <- rep(seq(1:6),each = 6)
lat <- rep(seq(1:6),6)

coords.df <- data.frame(long=long, lat=lat)
old_path <- coords.df

old_path <- ws_coords
old_path <- old_path[sample(nrow(old_path)),]
plot_path(old_path)
cat(paste0('The cost value is ',calc_cost(old_path),'\n'))

record <- data.frame(iter = numeric(0), cost = numeric(0))

for (i in 1:4000){
  
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
ggplot(data = all_record) + geom_line(aes(x = iter, y = cost,
                                          color = as.factor(run)), size = 1.1) +
  theme_classic() + geom_hline(yintercept = 27603, color = 'red', linetype = 'dashed') +
  xlab('iteration') + ylab('distance')

record <- setDT(record)
record2 <- setDT(record2)
record3 <- setDT(record3)
record4 <- setDT(record4)

record[,run := 1]
record2[,run := 2]
record3[,run := 3]
record4[,run := 4]

all_record <- rbind(record,record2,record3,record4)
