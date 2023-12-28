library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_13',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_14',open="r")
linn <-readLines(conn)
close(conn)

map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) unlist(strsplit(x,split='')) ))))

count=0
for(i in 1:ncol(map_mat)){
  print(i)
  v_in=map_mat[,i]
  
  which(v_in=='#')
  
  s_out=c()
  s=''
  for(i in 1:(length(v_in))){
   s=paste0(s,v_in[i]) 
    if( i==length(v_in) ||xor(v_in[i]=='#' , v_in[i+1]=='#')){
      s_out=c(s_out,s)
      s=''
    }
  }
  
  
  sorted_str=paste(unlist(lapply(s_out,function(x) paste(sort(unlist(strsplit(x, "")),decreasing=T ), collapse = ""))),collapse='')
  
  count=count+sum(nchar(sorted_str)- which(strsplit(sorted_str,'')[[1]]=='O')+1)
  print(count)
}

######################################################
spin_mat=function(map_mat,ud=T,dec=T){
  count=0
  out_list=list()
  for(j in 1:ncol(map_mat)){
    # print(j)
    if(ud){
      v_in=map_mat[,j]
    }else{
      v_in=map_mat[j,]
    }
    s_out=c()
    s=''
    for(i in 1:(length(v_in))){
     s=paste0(s,v_in[i]) 
      if( i==length(v_in) ||xor(v_in[i]=='#' , v_in[i+1]=='#')){
        s_out=c(s_out,s)
        s=''
      }
    }
    sorted_str=paste(unlist(lapply(s_out,function(x) paste(sort(unlist(strsplit(x, "")),decreasing=dec ), collapse = ""))),collapse='')
    out_list[[j]]=strsplit(sorted_str,'')[[1]]
  }
  
  if(ud){
  return(as.matrix(as.data.table(out_list)))
  }else{
    return(matrix(unlist(out_list),byrow=T,nrow=100))
  }
  
}

get_count=function(m2){
  count=0
  for(i in 1:ncol(m2)){
    # print(i)
    v_in=paste(m2[,i],collapse='')
    
    
    count=count+sum(nchar(v_in)- which(strsplit(v_in,'')[[1]]=='O')+1)
    # print(count)
  }
  return(count)
}



m=map_mat

out_list=list()
hash_list=list()
for(i in 1:5000){
  print(i)
  m=spin_mat(m) #north
  m=spin_mat(m,ud=F,dec=T)#west
  m=spin_mat(m,ud=T,dec=F)#south
  m=spin_mat(m,ud=F,dec=F)#east
  hash_str=paste(as.vector(m),collapse='')  
  if(hash_str %in% names(hash_list)){
    break
  }else{
    hash_list[[hash_str]]=i
  }
  count=get_count(m)
  print(count)
  out_list[[i]]=data.table(cycle_no=i,c=count)
}

# hash_list[[hash_str]]
# i

# 170%%28
# 142%%28
# 
cycle_length=hash_list[[hash_str]]-i
out_list[[hash_list[[hash_str]]+1000000000%%cycle_length-hash_list[[hash_str]]%%cycle_length]]




# m2=spin_mat(map_mat) #north
# m2=spin_mat(map_mat,ud=T,dec=F)#south
# m2=spin_mat(map_mat,ud=F,dec=T)#west
# m2=spin_mat(map_mat,ud=F,dec=F)#east
# ret=get_count(m2)
