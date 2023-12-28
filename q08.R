library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_5',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_8',open="r")
linn <-readLines(conn)
close(conn)
instr=linn[1]
test=fread('/home/eqiu/code_projects/aoc_2023/data/input_8',header=F,skip=2)
insts=linn[3:length(linn)]

dt_klr=data.table(
  k=gsub('(.*) = \\((.*), (.*)\\)','\\1',insts),
  l=gsub('(.*) = \\((.*), (.*)\\)','\\2',insts),
  r=gsub('(.*) = \\((.*), (.*)\\)','\\3',insts)
)


init='AAA'
step=init
n_steps=0
while(step!='ZZZ'){
  n_steps=n_steps+1
  print(n_steps)
  print(step)
  step=dt_klr[k==step,get(tolower(substr(instr, (n_steps-1)%%nchar(instr) +1 ,(n_steps-1)%%nchar(instr) +1) ))]
}

########################
init=dt_klr[substr(k,3,3)=='A']
step=init
n_steps=0
perm_list=as.data.table(as.list(init$k))
perm_list[,d:=substr(instr,1,1)]
perm_list[,ind:=1]
while(!all(substr(step$k,3,3)=='Z')&&n_steps<500000){
  n_steps=n_steps+1
  if(n_steps%%10000==1){
    print(n_steps)
    print(step$k)
  }
  
  dir=substr(instr, (n_steps-1)%%nchar(instr) +1 ,(n_steps-1)%%nchar(instr) +1)
  next_dir=substr(instr, (n_steps)%%nchar(instr) +1 ,(n_steps)%%nchar(instr) +1)
  step[dt_klr,k:=get(paste0('i.',tolower(dir))),on=c('k')]
  
  new=as.data.table(as.list(step$k))
  new[,d:=next_dir]
  new[,ind:=(n_steps)%%nchar(instr) +1]
  perm_list=rbind(perm_list,new)
}
print(n_steps)
perm_list[,index:=1:.N]
perm_list[,n_count1:=1:.N,by=.(V1,ind)]
perm_list[,n_count2:=1:.N,by=.(V2,ind)]
perm_list[,n_count3:=1:.N,by=.(V3,ind)]
perm_list[,n_count4:=1:.N,by=.(V4,ind)]
perm_list[,n_count5:=1:.N,by=.(V5,ind)]
perm_list[,n_count6:=1:.N,by=.(V6,ind)]
# 
# 
# perm_list[ind%in%c(17,18,78,79)]
# 
# perm_list[n_count1>1]
# 
# perm_list[V1=='LLJ']
# 
# 
cycle_length=c()
init_index=c()


z_index_run1=c(perm_list[n_count1==1&substr(V1,3,3)=='Z']$index
  ,perm_list[n_count2==1&substr(V2,3,3)=='Z']$index
  ,perm_list[n_count3==1&substr(V3,3,3)=='Z']$index
  ,perm_list[n_count4==1&substr(V4,3,3)=='Z']$index
  ,perm_list[n_count5==1&substr(V5,3,3)=='Z']$index
  ,perm_list[n_count6==1&substr(V6,3,3)=='Z']$index
)


# library(fst)
# write_fst(perm_list,'/home/eqiu/code_projects/aoc_2023/perm_list.fst')



for(i in 1:6){
  cycle_length=c(cycle_length,min(perm_list[get(paste0('n_count',i))>2]$index)-min(perm_list[get(paste0('n_count',i))>1]$index))
  init_index=c(init_index,min(perm_list[get(paste0('n_count',i))>1]$index))
}
cycle_length
z_index_cycle=z_index_run2-init_index
z_index_cycle[1]+k*cycle_length[1]=z_index_cycle[2]+k_2*cycle_length[2]



base_length=cycle_length[1]
base_index=z_index_run1[1]
i=2
for(i in 2:6){
  n_calc_cycles=min(which((base_length*1:20000)%%cycle_length[i]==(z_index_run1[i]-base_index)%%cycle_length[i] ))
  first_match=base_index+n_calc_cycles*base_length
  
  # n_calc_cycles=min(which( ((cycle_length[i]*1:20000)%%base_length) == ((base_index-z_index_run1[i])%%base_length)  ))
  # first_match=z_index_run1[i]+n_calc_cycles*cycle_length[i]
  
  cycle_length_calc=pracma::Lcm(base_length,cycle_length[i])
  
  base_length=cycle_length_calc
  base_index=first_match
  print(paste0('i:',i))
  print(base_length)
  print(base_index-1)
}



