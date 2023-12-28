library(data.table)
options(scipen=999,digits=22)
# library(bit64)

test=fread('/home/eqiu/code_projects/aoc_2023/data/input_18',header=F,sep=' ')

test[,`:=`(num=strtoi(paste0('0x',substr(V3,3,7))),dir_num=substr(V3,8,8))]

test[dir_num==0,V1_x:='R']
test[dir_num==1,V1_x:='D']
test[dir_num==2,V1_x:='L']
test[dir_num==3,V1_x:='U']
test[,V2_x:=num]
# test[,`:=`(V1=V1_x,V2=V2_x)]
test[1,`:=`(init_x=1,init_y=1)]
for(i in 1:nrow(test)){
  if(i!=1){
    r_last=test[i-1]
    e_x=r_last$end_x
    e_y=r_last$end_y
    test[i,`:=`(init_x=e_x,init_y=e_y)]
  }
  r=test[i,]
  dir=r$V1
  if(dir=='R'){
    e_x=r$init_x+r$V2
    e_y=r$init_y
  }
  if(dir=='L'){
    e_x=r$init_x-r$V2
    e_y=r$init_y
  }
  if(dir=='U'){
    e_x=r$init_x
    e_y=r$init_y+r$V2
  }
  if(dir=='D'){
    e_x=r$init_x
    e_y=r$init_y-r$V2
  }
  
  test[i,`:=`(end_x=e_x,end_y=e_y)]
}

xs=unlist(test[,.(init_x,end_x)])
ys=unlist(test[,.(init_y,end_y)])
x_range=c(min(xs)-1,max(xs)+1)
y_range=c(min(ys)-1,max(ys)+1)


# test_dash=test[V1 %in% c('U','D')]
# test_un_dash=test[V1 %in% c('U','D')]
# count=0
# out_list=c(1,1)
# names(out_list)=c('Var1','Var2')
# for(y in y_range[1]:y_range[2]){
#   in_trench=0
#   for(x in x_range[1]:x_range[2]){
#     if(in_trench%%2==1){
#       # out_list=c(out_list,list(c(x,y)))
#       out_list=rbind(out_list,c(x,y))
#       count=count+1
#     }
#     if(nrow(test_dash[ y>=pmin(init_y,end_y)&y<pmax(init_y,end_y)&x==init_x])>0){
#       in_trench=in_trench+1
#       count=count+1
#     }
#     # else if(nrow(test_un_dash[x>=init_y&y<end_y&x==init_x])>0){
#       # in_trench=in_trench+1
#       # count=count+1
#   # }
#   }
# }
# print(count)
# out_list[order(out_list[,2],out_list[,1]),]
# 
# for(i in 1:nrow(test)){
#   r=test[i,]
#   out_list=rbind(out_list,expand.grid(r$init_x:r$end_x,r$init_y:r$end_y))
# }
# 
# out_list2=(unique(out_list))
# nrow(out_list2)

# 36679
# out_list2[order(out_list2[,2],out_list2[,1]),]
# dim(out_list)

############
test[,lag_v1:=shift(V1,type='lag')]
test[1,lag_v1:='U']
test[,corner_type:=paste0(lag_v1,V1)]

test[V1 %in% c('U','D'),.N,by=corner_type]


xs=unlist(test[,.(init_x,end_x)])
ys=unlist(test[,.(init_y,end_y)])
x_range=c(min(xs)-1,max(xs)+1)
y_range=c(min(ys)-1,max(ys)+1)


test_dash=test[V1 %in% c('U','D')]
# test_un_dash=test[V1 %in% c('U','D')]
count=0
out_list=c(1,1)
names(out_list)=c('Var1','Var2')

out_interval_stacks=list()
y=y_range[1]
test_dash[,min_y:=pmin(init_y,end_y)]
test_dash[,max_y:=pmax(init_y,end_y)]
while(y<y_range[2]){
# for(y in y_range[1]:y_range[2]){
  print(y)
  in_trench=0
  
  pertinent_rows=test_dash[ y>=min_y&y<max_y][order(init_x)]
  
  
  
  
  if(nrow(pertinent_rows)>0){
    
    # next_y=pertinent_rows[,min(pmax(init_y,end_y)-1)]
    next_y=min(test_dash[min_y>y]$min_y,pertinent_rows[,min(max_y)])-1
    out_intervals=list()
    print(floor(nrow(pertinent_rows)/2))
    for(j in 1:floor(nrow(pertinent_rows)/2)){
      out_intervals[[j]]=data.table(init_y=y,end_y=next_y,init_x=pertinent_rows[2*j-1]$init_x,end_x=pertinent_rows[2*j]$init_x)
    }
    out_i_stack=rbindlist(out_intervals)
    out_interval_stacks[[as.character(y)]]=out_i_stack
    y=next_y+1
  }else{
    y=y+1
  }
}
out_intervals_all=rbindlist(out_interval_stacks)
# 35887
##################
# names(out_list)=c('Var1','Var2')
# 
# out_interval_stacks=list()
# for(y in y_range[1]:y_range[2]){
#   print(y)
#   in_trench=0
#   
#   pertinent_rows=test_dash[ y>=pmin(init_y,end_y)&y<pmax(init_y,end_y)][order(init_x)]
#   
#   if(nrow(pertinent_rows)>0){
#     out_intervals=list()
#     print(floor(nrow(pertinent_rows)/2))
#     for(j in 1:floor(nrow(pertinent_rows)/2)){
#       out_intervals[[j]]=data.table(init_y=y,end_y=y,init_x=pertinent_rows[2*j-1]$init_x,end_x=pertinent_rows[2*j]$init_x)
#     }
#     out_i_stack=rbindlist(out_intervals)
#     out_interval_stacks[[as.character(y)]]=out_i_stack
#   }
#   
#   
# }
# out_intervals_all2=rbindlist(out_interval_stacks)
# 
# 
# out_intervals_all[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]
# out_intervals_all2[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]



#################
intersections=0
for(i in 1:nrow(test)){
  r=test[i,]
  if(r$V1 =='L'){
    r$end_x=r$end_x+1
  }
  if(r$V1 =='R'){
    r$end_x=r$end_x-1
  }
  if(r$V1 =='U'){
    r$end_y=r$end_y-1
  }
  if(r$V1 =='D'){
    r$end_y=r$end_y+1
  }
  
  xs=c(min(r$init_x,r$end_x),max(r$init_x,r$end_x))
  ys=c(min(r$init_y,r$end_y),max(r$init_y,r$end_y))
  int_temp=0
  for(j in 1:nrow(out_intervals_all)){
    # print(j)
    r2=out_intervals_all[j,]
    xs2=c(min(r2$init_x,r2$end_x),max(r2$init_x,r2$end_x))
    ys2=c(min(r2$init_y,r2$end_y),max(r2$init_y,r2$end_y))
    
    square_leftcorner=c(pmax(xs[1],xs2[1]),pmax(ys[1],ys2[1]))
    square_rightcorner=c(pmin(xs[2],xs2[2]),pmin(ys[2],ys2[2]))
    
    if(all(square_leftcorner<=square_rightcorner)){
      intersect_area=pmax(prod(square_rightcorner-square_leftcorner+1),0)
      intersections=intersections+intersect_area
      int_temp=int_temp+intersect_area
      # print(intersect_area)
    }
  }
  test[i,ints:=int_temp]
}
out_intervals_all[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]+
test[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) -1 )]+
-intersections


# intersect_area=function(xs,ys,xs2,ys2){
#   square_leftcorner=c(pmax(xs[1],xs2[1]),pmax(ys[1],ys2[1]))
#     square_rightcorner=c(pmin(xs[2],xs2[2]),pmin(ys[2],ys2[2]))
#     
#   if(all(square_leftcorner<=square_rightcorner)){
#     intersect_area=pmax(prod(square_rightcorner-square_leftcorner+1),0)
#   }else{
#     intersect_area=0
#   }
#   return(intersect_area)
# }
# intersect_area(c(0,2),c(0,2),c(1,1),c(-1,1))


# ####################
# test_dash=test[V1 %in% c('R','L')]
# out_list_row_adds=list()
# for(i in 1:nrow(test_dash)){
#   r=test_dash[i,]
#   r_dim=c(min(r$init_x,r$end_x),max(r$init_x,r$end_x))
#   out_temp=out_intervals_all[init_y==r$init_y]
#   if(nrow(out_temp)>0){
#     r_out=NULL
#     for(j in 1:nrow(out_temp)){
#       r2=out_temp[j,]
#       if(r_dim[1]<=max(r2$end_x,r2$init_x)&r_dim[2]>=min(r2$end_x,r2$init_x) ){
#         if(r_dim[1]>=min(r2$end_x,r2$init_x)){
#           r_dim[1]=max(r2$end_x,r2$init_x)+1
#         }else{
#           r_dim[2]=min(r2$end_x,r2$init_x)-1
#         }
#       }
#     }
#     if(r_dim[1]<=r_dim[2]){
#       r_out=data.table(init_y=r$init_y,end_y=r$init_y,init_x=r_dim[1],end_x=r_dim[2])
#     }
#   }else{
#     r_out=r
#   }
#   out_list_row_adds[[i]]=r_out
# }
# row_adds=rbindlist(out_list_row_adds,use.names=T,fill=T)
# out_intervals_all2=rbind(out_intervals_all,row_adds,use.names=T,fill=T)
# 
# out_intervals_all2[,`:=`(min_x=pmin(init_x,end_x),max_x=pmax(init_x,end_x))]
# 
# 
# intersections=0
# for(i in 1:nrow(test_un_dash)){
#   r=test_un_dash[i,]
#   inter_temp=0
#   for(j in 1:nrow(out_intervals_all2)){
#     r2=out_intervals_all2[j]
#     if(r$init_x >=r2$min_x&r$init_x <=r2$max_x &r2$init_y>=min(r$init_y,r$end_y)&r2$init_y<=max(r$init_y,r$end_y) ){
#       intersections=intersections+1
#       inter_temp=inter_temp+1
#     }
#   }
#   test_un_dash[i,inters:=inter_temp]
# }
# out_intervals_all[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]+
# row_adds[,sum(abs(init_x-end_x)+1)]+
# test_un_dash[,sum(abs(init_y-end_y)+1)]-intersections
# 
# # out_intervals_all[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]+
# # test[,sum((abs(init_x-end_x)+1)*(abs(init_y-end_y)+1) )]-intersections