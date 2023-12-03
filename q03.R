library(data.table)
# ,sep='|'
test=fread('/home/eqiu/code_projects/aoc_2023/data/input_3',header=F)


str2=strsplit(test$V1,split='')


mat=do.call(rbind, str2)

sum=0

ncol(mat)

surround=function(v,x_min=1,y_min=x_min,x_max=140,y_max=x_max){
  grid_out=expand.grid(c( pmax(v[1]-1,x_min),v[1],pmin(v[1]+1,x_max)),c(pmax(v[2]-1,y_min),v[2],pmin(v[2]+1,y_max) ))
  grid_out=unique(grid_out)
  return(grid_out)
}

sum=0
gear_list=as.data.table(which(mat=='*',arr.ind=T))
gear_list[,sum:=0]
gear_list[,prod:=1]
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    if(grepl('[0-9]',mat[i,j]) &&(j==1|| !grepl('[0-9]',mat[i,j-1])  )){
      
      num=''
      grid_out=NULL
      while((j)<=140 &&grepl('[0-9]',mat[i,j])){
        num=paste0(num,mat[i,j])
        grid_out=rbind(grid_out,surround(c(i,j)))
        j=j+1
      }
      grid_out=unique(grid_out)
      
      if(any(grepl('[^0-9.]',mat[as.matrix(grid_out)]))){
        print(paste0(i,'_',j))
        print(num)
        sum=sum+as.numeric(num)
      }
      # gear_list[,sum:=sum+1]
      if(sum(grepl('[*]',mat[as.matrix(grid_out)]))>0){
        gear_list[grid_out[grepl('[*]',mat[as.matrix(grid_out)]),],`:=`(prod=prod*as.numeric(num),sum=sum+1),on=c(row='Var1',col='Var2')]
      }
    }
  }
}

gear_list[order(sum)]
gear_list[sum==2,sum(prod)]

# for(j in 1:10){
#   print(j)
#   if(j==3){
#     j=j+1
#   }
# }