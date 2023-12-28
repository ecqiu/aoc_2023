library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_5',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_9',open="r")
linn <-readLines(conn)
close(conn)

l=linn[1]

out=c()
for(l in linn){
l2=as.numeric(unlist(strsplit(l,split=' ')))

diff_level=1
diffed_list=list()
diffed_list[[diff_level]]=l2
while(!all(diffed_list[[diff_level]]==0)){
  diff_level=diff_level+1
  # diffed_old=diffed
  diffed_list[[diff_level]]=diff(diffed_list[[diff_level-1]])
}
# diffed_old=c(diffed_old,diffed_old[1])

# d=diffed_old
for(i in diff_level:2){
  print(i)
  last_val=rev(diffed_list[[i]])[1]
  last_val_prev=rev(diffed_list[[i-1]])[1]
  diffed_list[[i-1]]=c(diffed_list[[i-1]],last_val_prev+last_val)
  # print(diffed_list)
}
out=c(out,rev(diffed_list[[1]])[1])
}

sum(out)
########################################
out=c()
for(l in linn){
l2=as.numeric(unlist(strsplit(l,split=' ')))

diff_level=1
diffed_list=list()
diffed_list[[diff_level]]=l2
while(!all(diffed_list[[diff_level]]==0)){
  diff_level=diff_level+1
  # diffed_old=diffed
  diffed_list[[diff_level]]=diff(diffed_list[[diff_level-1]])
}
# diffed_old=c(diffed_old,diffed_old[1])

# d=diffed_old
for(i in diff_level:2){
  print(i)
  last_val=diffed_list[[i]][1]
  last_val_prev=diffed_list[[i-1]][1]
  diffed_list[[i-1]]=c(last_val_prev-last_val,diffed_list[[i-1]])
  # print(diffed_list)
}
out=c(out,diffed_list[[1]][1])
}

sum(out)
