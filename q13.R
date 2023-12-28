library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_13',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_13',open="r")
linn <-readLines(conn)
close(conn)




i=1

count=0
while(i<length(linn)){
  l=linn[i]
  l_dsh=c()
  while(l!=''&i<=length(linn)){
    l_dsh=c(l_dsh,l)
    i=i+1
    l=linn[i]
  }
  map_mat=t(as.matrix(as.data.table(lapply(l_dsh,function(x) unlist(strsplit(x,split='')) ))))
  print(map_mat)
  
  # v=2
  # map_mat[max(1,v- (nrow(map_mat)-v) ):v,]
  
  hor_line=-1
  for(v in 1:(nrow(map_mat)-1)){
    print(v)
    if(all(map_mat[max(1,v- (nrow(map_mat)-v-1) ):v,]==map_mat[min(nrow(map_mat),v+v):(v+1),])){
      hor_line=v
      break
    }
  }
  
  vert_line=-1
  for(v in 1:(ncol(map_mat)-1)){
    print(v)
    if(all(map_mat[,max(1,v- (ncol(map_mat)-v-1) ):v]==map_mat[,min(ncol(map_mat),v+v):(v+1)])){
      vert_line=v
      break
    }
  }
  
  if(vert_line>0){
    count=count+vert_line
  }else if(hor_line>0){
    count=count+hor_line*100
  }else{
    print('error')
  }

  i=i+1
}


# map_mat2=copy(map_mat)





i=1

count=0
while(i<length(linn)){
  l=linn[i]
  l_dsh=c()
  while(l!=''&i<=length(linn)){
    l_dsh=c(l_dsh,l)
    i=i+1
    l=linn[i]
  }
  map_mat=t(as.matrix(as.data.table(lapply(l_dsh,function(x) unlist(strsplit(x,split='')) ))))
  print(map_mat)
  
  # v=2
  # map_mat[max(1,v- (nrow(map_mat)-v) ):v,]
  
  hor_line=-1
  for(v in 1:(nrow(map_mat)-1)){
    print(v)
    if(sum(!(map_mat[max(1,v- (nrow(map_mat)-v-1) ):v,]==map_mat[min(nrow(map_mat),v+v):(v+1),]))==1){
      hor_line=v
      break
    }
  }
  
  vert_line=-1
  for(v in 1:(ncol(map_mat)-1)){
    print(v)
    if(sum(!(map_mat[,max(1,v- (ncol(map_mat)-v-1) ):v]==map_mat[,min(ncol(map_mat),v+v):(v+1)]))==1 ){
      vert_line=v
      break
    }
  }
  
  if(vert_line>0){
    count=count+vert_line
  }else if(hor_line>0){
    count=count+hor_line*100
  }else{
    print('error')
  }

  i=i+1
}