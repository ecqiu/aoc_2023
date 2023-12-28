library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_11',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_11',open="r")
linn <-readLines(conn)
close(conn)


map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) unlist(strsplit(x,split='')) ))))
map_mat2=copy(map_mat)
dim(map_mat2)
no_gal_row=c()
added_rows=0
for(i in 1:nrow(map_mat)){
  if(sum(map_mat[i,]=='#')==0){
    no_gal_row=c(no_gal_row,i)
    map_mat2=rbind(map_mat2[1:(i+added_rows-1),],rep('.',ncol(map_mat2)),map_mat2[(i+added_rows):nrow(map_mat2),])
    added_rows=added_rows+1
  }
}
dim(map_mat2)


no_gal_col=c()
added_cols=0
for(i in 1:ncol(map_mat)){
  if(sum(map_mat[,i]=='#')==0){
    no_gal_col=c(no_gal_col,i)
    map_mat2=cbind(map_mat2[,1:(i+added_cols-1)],rep('.',nrow(map_mat2)),map_mat2[,(i+added_cols):ncol(map_mat2)])
    added_cols=added_cols+1
  }
}

dim(map_mat2)

# data.table(which(map_mat=='#',arr.ind=T))
galaxy_list=data.table(which(map_mat2=='#',arr.ind=T))

dist=0
for(i in 1:(nrow(galaxy_list)-1)){
  dist=dist+sum(abs(galaxy_list[i,]$row-galaxy_list[(i+1):nrow(galaxy_list),]$row))+sum(abs(galaxy_list[i,]$col-galaxy_list[(i+1):nrow(galaxy_list),]$col))
  print(i)
  print(dist)
}

no_gal_row
no_gal_col


galaxy_list=data.table(which(map_mat=='#',arr.ind=T))

dist=0
added=0
for(i in 1:(nrow(galaxy_list)-1)){
  print(i)
  for(j in (i+1):(nrow(galaxy_list))){
    dist=dist+abs(galaxy_list[i,]$row-galaxy_list[j,]$row)+abs(galaxy_list[i,]$col-galaxy_list[j,]$col)
    added_cols=sum(no_gal_col> min(galaxy_list[i,]$col,galaxy_list[j,]$col) &no_gal_col< max(galaxy_list[i,]$col,galaxy_list[j,]$col))
    added_rows=sum(no_gal_row> min(galaxy_list[i,]$row,galaxy_list[j,]$row) &no_gal_row< max(galaxy_list[i,]$row,galaxy_list[j,]$row))
    dist=dist
    added=added+added_cols+added_rows
  }
  print(dist)
}
dist+added
dist+added*(1000000-1)

# added*1000000

 # 411134