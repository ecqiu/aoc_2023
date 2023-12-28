library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_13',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_15',open="r")
linn <-readLines(conn)
close(conn)


x='rn=1'

hash_func=function(x){
val=0

for(i in 1:nchar(x)){
  c=substr(x,i,i)
  ascii=as.numeric(charToRaw(c))
  val=val+ascii
  val=val*17
  val=val%%256
  # print(val)
}
return(val)
}


# linn='rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7'
inputs=strsplit(linn,split=',')[[1]]
out_vals=lapply(inputs,hash_func)
sum(unlist(out_vals))

c=inputs[1]

boxes=list()
for(i in 1:256){
  boxes[[i]]=c(-1)
}

labels=gsub('(.*)(-|=)(\\d*)','\\1',inputs)
box_nums=unlist(lapply(gsub('(.*)(-|=)(\\d*)','\\1',inputs),hash_func ) )+1
ops=gsub('(.*)(-|=)(\\d*)','\\2',inputs)
op_nums=as.numeric(gsub('(.*)(-|=)(\\d*)','\\3',inputs))

for(j in 1:length(box_nums)){
  b=box_nums[j]
  if(ops[j]=='='){
    if(labels[j] %in% names(boxes[[b]])) {
      ind=which(labels[j]==names(boxes[[b]]))
      boxes[[b]][ind]=op_nums[j]
    }
    else{
      new_v=c(op_nums[j])
      names(new_v)=labels[j]
      boxes[[b]]=c(boxes[[b]],new_v)
    }
  }
  
  if(ops[j]=='-'){
    if(labels[j] %in% names(boxes[[b]])) {
      ind=which(labels[j]==names(boxes[[b]]))
      boxes[[b]]=boxes[[b]][-ind]
    }
  }
}

sum(unlist(lapply(1:256,function(i){ sum(i*0:(length( boxes[[i]])-1)*boxes[[i]]) } )))