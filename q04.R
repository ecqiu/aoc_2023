library(data.table)
# library(bit64)
test=fread('/home/eqiu/code_projects/aoc_2023/data/input_4',header=F,sep='~')

test[,win_numbers:=gsub('.*:(.*)\\|(.*)','\\1',V1)]
test[,my_numbers:=gsub('.*\\|(.*)','\\1',V1)]


test[,power:=unlist(lapply(1:nrow(test),function(i) sum(intersect(strsplit(test$win_numbers[i],' ')[[1]], strsplit(test$my_numbers[i],' ')[[1]]  )!='' )  ) )]

test[,points:=ifelse(power==0,0,2^(power-1))]
test[,sum(points)]


test[,copies:=1]
nrow(test)
for(i in 1:nrow(test)){
  if(test$power[i]>0){
    test[(i+1):(min(i+test$power[i],216)),copies:=copies+test$copies[i]]
  }
}

# options(scipen=999)
test[,sum(copies)]



