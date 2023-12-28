library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_10',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_10',open="r")
linn <-readLines(conn)
close(conn)


map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) unlist(strsplit(x,split='')) ))))

dists=matrix(-1,140,140)

start_pos=which(map_mat=='S',arr.ind=T)

dists[start_pos]=0

# map_mat[88:90,19:21]
map_mat[19:21,88:90]

dists[10:31,68:110]

grid2[(2*10):(2*31),2*68:110]

# map_mat[start_pos+c(0,1)]
# map_mat[start_pos+c(0,-1)]
map_mat[start_pos+c(1,0)]
map_mat[start_pos+c(-1,0)]

start_poses=matrix(c(start_pos+c(-1,0),start_pos+c(1,0)),ncol=2,byrow=T)
dists[start_poses]=1

d=1
vecs=which(dists==1,arr.ind=T)

while(length(vecs)>0){
  for(i in 1:nrow(vecs)){
    pos=vecs[i,]
    sym=map_mat[matrix(pos,nrow=1)]
    if(sym=='|'){
      new_poses=matrix(c(pos+c(1,0) ,pos+c(-1,0)),ncol=2,byrow=T)
    }
    if(sym=='-'){
      new_poses=matrix(c(pos+c(0,-1) ,pos+c(0,1)),ncol=2,byrow=T)
    }
    if(sym=='L'){
      new_poses=matrix(c(pos+c(0,1) ,pos+c(-1,0)),ncol=2,byrow=T)
    }
    if(sym=='J'){
      new_poses=matrix(c(pos+c(0,-1) ,pos+c(-1,0)),ncol=2,byrow=T)
    }
    if(sym=='7'){
      new_poses=matrix(c(pos+c(0,-1) ,pos+c(1,0)),ncol=2,byrow=T)
    }
    if(sym=='F'){
      new_poses=matrix(c(pos+c(0,1) ,pos+c(1,0)),ncol=2,byrow=T)
    }
    
    for(j in 1:nrow(new_poses)){
      if(dists[matrix(new_poses[j,],nrow=1)]==-1){
        dists[matrix(new_poses[j,],nrow=1)]=d+1
      }
    }
  }
  
  d=d+1
  print(d)
  vecs=which(dists==d,arr.ind=T)
  print(vecs)
}
##########
dists2=copy(dists)

# grid2=matrix(0,280,280)
# grid2[as.matrix(expand.grid(1:140*2,1:140*2))]=dists2[as.matrix(expand.grid(1:140,1:140))]
# 
# off_inds=as.matrix(expand.grid(1:140*2-1,1:140*2-1))
# for(i in 1:nrow(off_inds)){
#   index=off_inds[i,,drop=F]
#   if(!(index[1]  %in%c(1,140)) &!(index[2]  %in%c(1,140))){
#     if(grid2[index+c(0,1)]>0&grid2[index+c(0,-1)]>0 | grid2[index+c(1,0)]>0&grid2[index+c(-1,0)]>0){
#       grid2[index]=1000
#     }
#   }
# }


# dists2[which(map_mat=='.',arr.ind=T)]=-10

init_points=which(dists==-1,arr.ind=T)
inits=init_points[init_points[,1]%in%c(1,140) | init_points[,2]%in%c(1,140),]
inits=as.matrix()


dists2[inits]=-10
new_points=inits

new_points

while(length(new_points)>0){
  old_points=new_points
  old_points1=old_points
  old_points1[,1]=old_points1[,1]+1
  old_points2=old_points
  old_points2[,1]=old_points2[,1]-1
  old_points3=old_points
  old_points3[,2]=old_points3[,2]+1
  old_points4=old_points
  old_points4[,2]=old_points4[,2]-1
  
  old_points1=as.data.table(old_points1)
  old_points2=as.data.table(old_points2)
  old_points3=as.data.table(old_points3)
  old_points4=as.data.table(old_points4)
  
  new_points=rbindlist(list(old_points1,old_points2,old_points3,old_points4))
  names(new_points)=c('row','col')
  
  new_points=unique(new_points)
  new_points=new_points[row>=1&row<=140&col>=1&col<=140]
  new_points=as.matrix(new_points)
  if(!is.matrix(new_points)){
    new_points=matrix(new_points,ncol=2)
    print('reshape')
  }
  
  new_points=new_points[dists2[new_points]<0,drop=FALSE]
  new_points=new_points[dists2[new_points]!=-10,drop=FALSE]
  
  dists2[new_points]=-10
  print(length(new_points))
  
  if(!is.matrix(new_points)){
    new_points=matrix(new_points,ncol=2)
    # ,dimnames=as.list(c('row','col'))
    # dimnames(new_points)=
    print('reshape')
  }
}

sum(map_mat=='.')-sum(dists2==-10&map_mat=='.')

inits=which(dists<0,arr.ind=T)
dt=as.data.table(inits)
for(i in 1:nrow(dt)){
  print(i)
  c=0
  c2=0
  r=dt[i,]
  
  for(j in r$col:140){
    sym=map_mat[r$row,j]
    if(sym %in% c('|','7','F','S')&dists[r$row,j]>=0){#
      c=c+1
    }
  }
  
  for(j in 1:r$col){
    sym=map_mat[r$row,j]
    if(sym %in% c('|','7','F','S')&dists[r$row,j]>=0){#
      c2=c2+1
    }
  }
  
  dt[i,x_count:=c]
  dt[i,x_count2:=c2]
}
# dt[count%%2!=0]
# 1365

# inits=which(dists<0,arr.ind=T)
# dt=as.data.table(inits)
# nrow(dt)
for(i in 1:nrow(dt)){
  print(i)
  c=0
  c2=0
  r=dt[i,]
  
  for(j in r$row:140){
    sym=map_mat[j,r$col]
    if(sym %in% c('-','L','F') &dists[j,r$col]>=0 ){
      c=c+1
    }
    # print(c)
  }
    for(j in 1:r$row){
    sym=map_mat[j,r$col]
    if(sym %in% c('-','L','F') &dists[j,r$col]>=0 ){
      c2=c2+1
    }
  }
  
  dt[i,y_count:=c]
  dt[i,y_count2:=c2]
}


dt[x_count%%2!=0]
dt[y_count%%2!=0]

dt[x_count2%%2!=0]
dt[y_count2%%2!=0]



dt[x_count2%%2!=0 &x_count%%2==0]
dt[x_count%%2!=0 &x_count2%%2==0]

# dt[y_count%%2!=0 &x_count2%%2==0]

# dt[,.N,by=count%%2]

# LF,7,5

# dt[,.N,by=count]



