library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_20',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_19',open="r")
linn <-readLines(conn)
close(conn)


rules=linn[1:513]
objects=linn[515:length(linn)]

# rules=linn[1:11]
# objects=linn[13:length(linn)]

labels=gsub('([^{]*)(.*)','\\1',rules)
rule_stuff=gsub('([^{]*)(.*)','\\2',rules)
names(rule_stuff)=labels


lapply(regmatches(rule_stuff, gregexpr(".(>|<)", rule_stuff)),function(x) gsub('(>|<)','',x))



rule_r_form=gsub(':',',',gsub('\\}',')',gsub(',(.(>|<|=))',',ifelse(\\1',gsub('\\{','ifelse(',rule_stuff))))

rep_brackets=lengths(regmatches(rule_r_form, gregexpr("\\(", rule_r_form)))-lengths(regmatches(rule_r_form, gregexpr("\\)", rule_r_form)))

rule_r_form2=lapply(1:length(rule_r_form),function(x) paste( c(rule_r_form[x],rep(')',rep_brackets[x])),collapse=''))
names(rule_r_form2)=labels

rule_r_form3=gsub(paste0('(R|A|',paste(labels,collapse='|'),')'),'"\\1"',rule_r_form2)
names(rule_r_form3)=labels


# rule_r_form3['in']

# x=0
# a=0
# m=0
# s=0

# eval(parse(text=rule_r_form3[1]))



o=objects[1]
r_all=0
for(i in 1:length(objects)){
  o=objects[i]
  print(i)
  x=as.numeric(gsub('\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}','\\1',o))
  m=as.numeric(gsub('\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}','\\2',o))
  a=as.numeric(gsub('\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}','\\3',o))
  s=as.numeric(gsub('\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}','\\4',o))
  
  out=eval(parse(text=rule_r_form3['in']))
  while(!(out %in% c('R','A'))){
    out=eval(parse(text=rule_r_form3[out]))
  }
  
  if(out=='R'){}
  if(out=='A'){
    r_all=r_all +x+m+a+s
  }
  
}


# 'drv'
init=data.table(name=c('x','m','a','s'),start=c(1,1,1,1),end=c(4000,4000,4000,4000),rule='in')
vals=data.table(name=c('x','m','a','s'),start=c(1,1,1,1),end=c(4000,4000,4000,4000),rule='tq')
val_list=list(init)
count=1
reject_list=list()
accept_list=list()
while(length(val_list)>0){
print(count)
print(length(accept_list))

vals=val_list[[1]]
val_list=val_list[-1]
r=rule_stuff[vals$rule[1]]

cutoffs=regmatches(r,gregexpr("(x|m|a|s)(<|>)(\\d+)",r))[[1]]
tf_grid=expand.grid(rep(list(c(T,F)),length(cutoffs)))
names(tf_grid)=cutoffs

i=1
for(i in 1:nrow(tf_grid)){
  row=tf_grid[i,]
  init_copy=copy(vals)
  j=1
  for(j in 1:length(row)){
    rule=cutoffs[j]
    char=substr(rule,1,1)
    op=substr(rule,2,2)
    cutoff=as.numeric(substr(rule,3,6))
    
    
    if(op=='>'&row[j]==T){
      cutoff=cutoff+1
      init_copy[name==eval(char),start:=pmax(cutoff,start)]
    }
    else if(op=='>'&row[j]==F){
      cutoff=cutoff
      init_copy[name==eval(char),end:=pmin(cutoff,end)]
    }
    else if(op=='<'&row[j]==T){
      cutoff=cutoff-1
      init_copy[name==eval(char),end:=pmin(cutoff,end)]
    }
    else if(op=='<'&row[j]==F){
      cutoff=cutoff
      init_copy[name==eval(char),start:=pmax(cutoff,start)]
    }
    else{
      print(rule)
    }
    # assign(char,cutoff)
    
  }
  for(z in 1:nrow(init_copy)){
    assign(init_copy[z,]$name,init_copy[z,]$start)
  }
  
  out=eval(parse(text=rule_r_form3[vals$rule[1]]))
  init_copy[,rule:=out]
  
  
  
  
  if(nrow(init_copy[start>end])==0 ){
    if(out %in% c('R','A')){
      if(out=='A'){
        accept_list=c(accept_list,list(init_copy))
      }else if(out=='R'){
        reject_list=c(reject_list,list(init_copy))
      }
    }else{
      val_list=c(val_list,list(init_copy))
    }
    
  }
}
count=count+1
}
# print(accept_list)
# print(reject_list)

sum(unlist(lapply(accept_list,function(x) prod(x[,.(end-start+1)]$V1) )))
# sum(unlist(lapply(reject_list,function(x) prod(x[,.(end-start+1)]$V1) )))

# prod(accept_list[[1]])+prod(accept_list[[2]][,.(end-start+1)]$V1)

# 17901120000000 too low
# 120111310843090 #too low

# out=eval(parse(text=rule_r_form3['in']))






