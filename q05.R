library(data.table)
# library(bit64)
# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_5',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_5',open="r")
linn <-readLines(conn)
close(conn)


seeds=as.numeric(unlist(strsplit(gsub('[^0-9 ]','',linn[1]), ' ')))
seeds=seeds[!is.na(seeds)]

# sum(seeds[2:(length(seeds)/2)])

create_map=function(x){
  out=as.data.table(do.call(Map, c(f = c, strsplit(x,split= ' ')) ))
  out=as.data.table(lapply(out,as.numeric))
  
  names(out)=c('dest_start','source_start','length')
  return(out)
}

seed_dt=data.table(seed=seeds)
seed_dt[,map_0:=seed]
# as.matrix(, ' '))

maps=list(
seed_to_soil=create_map(linn[4:39])
,soil_to_fert=create_map(linn[42:65])
,fert_to_water=create_map(linn[68:101])
,water_to_light=create_map(linn[104:149])
,light_to_temp=create_map(linn[152:180])
,temp_to_humid=create_map(linn[183:212])
,humid_to_loc=create_map(linn[215:252])
)
i=1
for(i in 1:length(maps)){
  seed_dt[,(paste0('map_',i)):=as.numeric(NA)]
  m=maps[[i]]
  # m[,`:=`(source_start=as.numeric(source_start))]
  for(j in 1:nrow(m)){
    print(j)
    seed_dt[get(paste0('map_',i-1))>=m[j,]$source_start&(get(paste0('map_',i-1))< (m[j,]$source_start+m[j,]$length)),(paste0('map_',i)):=(m[j,]$dest_start+ get(paste0('map_',i-1))-m[j,]$source_start )]
    print(seed_dt)
  }
  
  seed_dt[is.na(get(paste0('map_',i))),(paste0('map_',i)):=get(paste0('map_',i-1))]
}

min(seed_dt$map_7)

##3
seeds=as.numeric(unlist(strsplit(gsub('[^0-9 ]','',linn[1]), ' ')))

seed_dt2=data.table(init=seeds[(1:(length(seeds)/2))*2-1 ],length=seeds[(1:(length(seeds)/2))*2 ] )
i=1
out_map_list=list(seed_dt2)

a=c(map_row$source_start,map_row$source_start+map_row$length)
b=c(in_row$init,in_row$init+in_row$length)

a=c(1,3)
b=c(2,5)
pair_slice=function(a_raw,b_raw){
  a=c(a_raw[1],a_raw[1]+a_raw[2]-1)
  b=c(b_raw[1],b_raw[1]+b_raw[2]-1)

  if(a[1]<=b[1]){
    out=c(b[1],min(b[2],a[2]) )
    if(b[2]>a[2]){
      leftover=list(c(a[2]+1,b[2]))
    }else{
      leftover=NULL
    }
  }else if(a[1]>b[1]){
    out=c(a[1],min(a[2],b[2]) )
    leftover1=c(b[1],a[1]-1)
    leftover2=c(a[2]+1,b[2])

    if(leftover2[2]<leftover2[1]){leftover2=NULL}
    leftover=list(leftover1,leftover2)
  }
  if(out[2]<out[1]){out=NULL}
  out_raw=c(out[1],out[2]-out[1]+1)
  leftover_raw=list()
  for(i in 1:length(leftover)){
    if(length(leftover[[i]])>0){
    leftover_raw[[i]]=c(leftover[[i]][1],leftover[[i]][2]-leftover[[i]][1]+1)
    }
  }
  

  return(list(mapped=out_raw,leftover=leftover_raw))
}
p=pair_slice(c(1,2),c(0,5))





for(i in 1:length(maps)){
  print(i)
  out_dt=NULL
  in_dt=copy(out_map_list[[i]])
  in_dt[,del:=0]
  m=maps[[i]]

  for(j in 1:nrow(m)){
    map_row=m[j,]
    in_dt=in_dt[del==0]
    in_dt[,del:=0]
    for(k in 1:nrow(in_dt)){
      in_row=in_dt[k,]
      pair_out=pair_slice( unlist(map_row[,2:3]),unlist(in_row[,1:2]))
      if(length(pair_out$mapped)>0){
        out_dt=rbind(out_dt,data.table(init=pair_out$mapped[1]-map_row$source_start+map_row$dest_start,length=pair_out$mapped[2] ))
        leftover_dt=as.data.table(do.call(Map, c(f = c,  pair_out$leftover) ))
        in_dt[k,del:=1]
        if(nrow(leftover_dt)>0){
          names(leftover_dt)=c('init','length')
          leftover_dt[,del:=0]
          in_dt=rbind(in_dt,leftover_dt)
        }
      }

    }
    # in_dt

  }
  in_dt=in_dt[del==0]
  in_dt[,del:=NULL]
  out_dt=rbind(out_dt,in_dt)
  out_map_list[[i+1]]=out_dt
}
out_map_list[[8]][,min(init)]




# # Create a list of vectors
# my_list_of_vectors <- list(
#   c(1, 2),
#   c(3, 4)
# )
# 
# # Transpose the list of vectors
# transposed_list_of_vectors <- lapply(seq_along(my_list_of_vectors[[1]]), function(i) sapply(my_list_of_vectors, `[`, i))
# 
# # Print the original and transposed lists
# print("Original List of Vectors:")
# print(my_list_of_vectors)
# 
# print("Transposed List of Vectors:")
# print(transposed_list_of_vectors)
