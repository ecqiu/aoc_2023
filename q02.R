library(data.table)
library(stringr)

t=test$V1[1]
get_n=function(t,color='green'){
  indices=gregexpr(paste0('(\\d{1,5}) ',color),t)[[1]]
  return(max(unlist(lapply(1:length(indices),function(x) as.numeric(substr(t,indices[x],indices[x]+1)) ))))
}


test=fread('/home/eqiu/code_projects/aoc_2023/data/input_2',header=F,sep='|')
test[,n_green:=unlist(lapply(test$V1,function(x) get_n(x,'green')))]
test[,n_red:=unlist(lapply(test$V1,function(x) get_n(x,'red')))]
test[,n_blue:=unlist(lapply(test$V1,function(x) get_n(x,'blue')))]

# test[,V1_mod:=copy(V1)]
# test[,n_green:=0]
# while(T){
#   test[,n_green:=pmax(fcoalesce(as.numeric(gsub('.*(\\d{1,5}) green.*','\\1',V1_mod)),0),n_green)]
#   test[,V1_mod2:=gsub(paste0(n_green,' green'),'',V1_mod)]
#   
#   if(all(test$V1_mod2==test$V1_mod) ){break}
#   test[,V1_mod:=copy(V1_mod2)]
# }
# 
# # test[,.(V1_mod,n_green)]
# test[,V1_mod:=copy(V1)]
# test[,n_red:=0]
# while(T){
#   test[,n_red:=pmax(fcoalesce(as.numeric(gsub('.*(\\d{1,5}) red.*','\\1',V1_mod)),0),n_red)]
#   test[,V1_mod2:=gsub(paste0(n_red,' red'),'',V1_mod)]
#   
#   if(all(test$V1_mod2==test$V1_mod) ){break}
#   test[,V1_mod:=copy(V1_mod2)]
# }
# 
# test[,V1_mod:=copy(V1)]
# test[,n_blue:=0]
# while(T){
#   test[,n_blue:=pmax(fcoalesce(as.numeric(gsub('.*(\\d{1,5}) blue.*','\\1',V1_mod)),0),n_blue)]
#   test[,V1_mod2:=gsub(paste0(n_blue,' blue'),'',V1_mod)]
#   
#   if(all(test$V1_mod2==test$V1_mod) ){break}
#   test[,V1_mod:=copy(V1_mod2)]
# }

test[,id:=as.numeric(gsub('Game (\\d+).*','\\1',V1))]
# test[,.(id,V1,n_red,n_blue,n_green)]
test[,power:=n_red*n_blue*n_green]

sum(test[n_green<=13&n_red<=12&n_blue<=14]$id)
sum(test$power)


