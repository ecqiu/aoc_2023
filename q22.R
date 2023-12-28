library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_22',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_22',open="r")
linn <-readLines(conn)
close(conn)

# map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) as.character(unlist(strsplit(x,split=''))) ))))

linn_split=strsplit(linn,'~')

get_coords=function(l){
  x_dt=as.data.table(lapply(l,function(x) as.numeric(gsub('(.*),(.*),(.*)','\\1',x))))
  names(x_dt)=c('x1','x2')
  
  y_dt=as.data.table(lapply(l,function(x) as.numeric(gsub('(.*),(.*),(.*)','\\2',x))))
  names(y_dt)=c('y1','y2')
  
  z_dt=as.data.table(lapply(l,function(x) as.numeric(gsub('(.*),(.*),(.*)','\\3',x))))
  names(z_dt)=c('z1','z2')
  
  return(do.call(cbind,list(x_dt,y_dt,z_dt)))
}

coord_list=lapply(linn_split,get_coords)
coord_stack=rbindlist(coord_list)

coord_stack=coord_stack[order(z1)]

coord_stack[,index:=1:.N]

coord_stack_copy=copy(coord_stack)
crit_bricks=c()
for(i in 1:nrow(coord_stack_copy)){
  # stop=0
  x1_=coord_stack_copy[i,]$x1
  x2_=coord_stack_copy[i,]$x2
  y1_=coord_stack_copy[i,]$y1
  y2_=coord_stack_copy[i,]$y2
  z1_=coord_stack_copy[i,]$z1
  # while(stop==0){
  if(coord_stack_copy[i,]$z1==1){
    # stop=1
  }else{
    lower_blocks=coord_stack_copy[z2<z1_ & x1<=x2_ &x2>=x1_ &y1<=y2_&y2>=y1_,]
    
    if(nrow(lower_blocks)==0){
      coord_stack_copy[i,`:=`(z2=z2-(z1-1))]
      coord_stack_copy[i,`:=`(z1=1)]
    }else{
      first_intersect=lower_blocks[,max(z2)]
      intersect_blocks=lower_blocks[z2==first_intersect]
      if(nrow(intersect_blocks)==1){
        # print(i)
        crit_bricks=c(crit_bricks,intersect_blocks$index)
      }
      
      coord_stack_copy[i,`:=`(z2=z2-(z1-first_intersect-1))]
      coord_stack_copy[i,`:=`(z1=first_intersect+1)]
    }
    # stop=0
  }
  # }
}
crit_bricks=unique(crit_bricks)

nrow(coord_stack_copy)-length(crit_bricks)

#singlethreaded version fo the code
# j=1
# fall=c()
# for(j in 1:nrow(coord_stack_copy)){
#   print(j)
#   print(fall)
#   fall_i=0
#   if(!(j %in% crit_bricks)){
#     fall=c(fall,fall_i)
#     next
#   }
#   coord_stack_copy2=copy(coord_stack_copy)
#   coord_stack_copy2=coord_stack_copy2[-j,]
#   
# 
#   
# 
# for(i in j:nrow(coord_stack_copy2)){
#   x1_=coord_stack_copy2[i,]$x1
#   x2_=coord_stack_copy2[i,]$x2
#   y1_=coord_stack_copy2[i,]$y1
#   y2_=coord_stack_copy2[i,]$y2
#   z1_=coord_stack_copy2[i,]$z1
#   # while(stop==0){
#   if(coord_stack_copy2[i,]$z1==1){
#     # stop=1
#   }else{
#     lower_blocks=coord_stack_copy2[z2<z1_ & x1<=x2_ &x2>=x1_ &y1<=y2_&y2>=y1_,]
#     
#     if(nrow(lower_blocks)==0){
#       coord_stack_copy2[i,`:=`(z2=z2-(z1-1))]
#       coord_stack_copy2[i,`:=`(z1=1)]
#       fall_i=fall_i+1
#     }else{
#       first_intersect=lower_blocks[,max(z2)]
#       intersect_blocks=lower_blocks[z2==first_intersect]
#       if(nrow(intersect_blocks)==1){
#         crit_bricks=c(crit_bricks,intersect_blocks$index)
#       }
#       if( (z1_-first_intersect-1)>0){
#         fall_i=fall_i+1
#       }
#       coord_stack_copy2[i,`:=`(z2=z2-(z1-first_intersect-1))]
#       coord_stack_copy2[i,`:=`(z1=first_intersect+1)]
#     }
#     # stop=0
#   }
# }
# fall=c(fall,fall_i)
# 
# }
# 
# sum(fall)


#parallel version of the code
library(foreach)
library(doParallel)
registerDoParallel(cores=24)
j=1
# fall=c()
fall=foreach(j=1:nrow(coord_stack_copy),.combine=c)  %dopar%{
  print(j)
  # print(fall)
  fall_i=0
  if(!(j %in% crit_bricks)){
    # fall=c(fall,fall_i)
    # next
  }else{
  coord_stack_copy2=copy(coord_stack_copy)
  coord_stack_copy2=coord_stack_copy2[-j,]
  

  

for(i in j:nrow(coord_stack_copy2)){
  x1_=coord_stack_copy2[i,]$x1
  x2_=coord_stack_copy2[i,]$x2
  y1_=coord_stack_copy2[i,]$y1
  y2_=coord_stack_copy2[i,]$y2
  z1_=coord_stack_copy2[i,]$z1
  # while(stop==0){
  if(coord_stack_copy2[i,]$z1==1){
    # stop=1
  }else{
    lower_blocks=coord_stack_copy2[z2<z1_ & x1<=x2_ &x2>=x1_ &y1<=y2_&y2>=y1_,]
    
    if(nrow(lower_blocks)==0){
      coord_stack_copy2[i,`:=`(z2=z2-(z1-1))]
      coord_stack_copy2[i,`:=`(z1=1)]
      fall_i=fall_i+1
    }else{
      first_intersect=lower_blocks[,max(z2)]
      intersect_blocks=lower_blocks[z2==first_intersect]
      if(nrow(intersect_blocks)==1){
        crit_bricks=c(crit_bricks,intersect_blocks$index)
      }
      if( (z1_-first_intersect-1)>0){
        fall_i=fall_i+1
      }
      coord_stack_copy2[i,`:=`(z2=z2-(z1-first_intersect-1))]
      coord_stack_copy2[i,`:=`(z1=first_intersect+1)]
    }
    # stop=0
  }
}
  }
# fall=c(fall,fall_i)
  fall_i
  

}

sum(fall)

# 16:48
# 16:51