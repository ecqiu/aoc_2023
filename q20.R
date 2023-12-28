library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_20',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_20',open="r")
linn <-readLines(conn)
close(conn)

module_list=list()

i=1
for(i in 1:length(linn)){
  splitted=strsplit(linn[i],'->')[[1]]
  
  outputs=trimws(strsplit(splitted[2],',')[[1]])
  
  mod_type=substr(splitted[1],1,1)
  mod_name=trimws(substr(splitted[1],2,3))
  if(mod_type=='%'){
    out=list(type='flipflop',onoff=0,outputs=outputs)
    module_list[[mod_name]]=out
  }
  else if(mod_type=='&'){
    out=list(type='conj',inputs=list(),outputs=outputs)
    module_list[[mod_name]]=out
  }
  else{
    out=list(type='broadcaster',outputs=outputs)
    module_list[['broadcaster']]=out
  }
}


conj_modules=names(module_list)[ lapply(module_list,function(x) x$type)=='conj']

for(i in 1:length(module_list)){
  conj_outputs=intersect(conj_modules,module_list[[i]]$outputs)
  if(length(conj_outputs)>0){
    print(i)
    for(j in 1:length(conj_outputs)){
      module_list[[conj_outputs[j]]]$inputs[[names(module_list)[i]]]=0
    }
  }
}
names(module_list)[names(module_list)=='in']='inv'

flipflops=names(lapply(module_list,function(x) x$type)=='flipflop')[lapply(module_list,function(x) x$type)=='flipflop']
conjs=names(lapply(module_list,function(x) x$type)=='conj')[lapply(module_list,function(x) x$type)=='conj']



flipflop_summ=rbindlist(lapply(1:length(flipflops),function(x) data.table(n=flipflops[x],onoff=module_list[[flipflops[x]]]$onoff)))
conj_summ=rbindlist(lapply(1:length(conjs),function(x) data.table(n=conjs[x],on_inputs=sum(unlist(module_list[[conjs[x]]]$inputs)),n_inputs=length(module_list[[conjs[x]]]$inputs),on=paste(names(which(module_list[[conjs[x]]]$inputs==1)),collapse=',') )))

check_list=list()




p0_all=0
p1_all=0
mr_vec=c()
bb_vec=c()
gl_vec=c()
kk_vec=c()
for(i in 1:10^4){
  # print(i)
init_pulses=rep(0,length(module_list$broadcaster$outputs))
names(init_pulses)=module_list$broadcaster$outputs

pulses=init_pulses
pulse_origins=rep('broadcaster',length(pulses))

n_p0=1
n_p1=0
while(length(pulses)>0){
  pulse=pulses[1]
  p_origin=pulse_origins[1]
  pulses=pulses[-1]
  pulse_origins=pulse_origins[-1]
  
  if(pulse==0){
    n_p0=n_p0+1
  }
  if(pulse==1){
    n_p1=n_p1+1
  }
  
  rec_mod=names(pulse)
  if(rec_mod=='rx'){
    # print(i)
    if(pulse==1){
      next
    }else{
      print('stop!!!!')
      break
    }
  }
  
  if(rec_mod=='bb'&&pulse==0){
    print('bb')
    print(i)
    bb_vec=c(bb_vec,i)
  }
  if(rec_mod=='kk'&&pulse==0){
    print('kk')
    print(i)
    kk_vec=c(kk_vec,i)
  }
  if(rec_mod=='mr'&&pulse==0){
    print('mr')
    print(i)
    mr_vec=c(mr_vec,i)
  }
  if(rec_mod=='gl'&&pulse==0){
    print('gl')
    print(i)
    gl_vec=c(gl_vec,i)
  }
  
  if(module_list[[rec_mod]]$type=='flipflop'){
    if(pulse==0){
      module_list[[rec_mod]]$onoff=(module_list[[rec_mod]]$onoff+1)%%2
      out_pulses=rep(module_list[[rec_mod]]$onoff,length(module_list[[rec_mod]]$outputs))
      names(out_pulses)=module_list[[rec_mod]]$outputs
      pulses=c(pulses,out_pulses)
      pulse_origins=c(pulse_origins,rep(rec_mod,length(out_pulses)))
    }
  }
  
  if(module_list[[rec_mod]]$type=='conj'){
    module_list[[rec_mod]]$inputs[[p_origin]]=pulse
    
    if(all(module_list[[rec_mod]]$inputs==1)){
      out_pulses=rep(0,length(module_list[[rec_mod]]$outputs))
      names(out_pulses)=module_list[[rec_mod]]$outputs
      pulses=c(pulses,out_pulses)
      pulse_origins=c(pulse_origins,rep(rec_mod,length(out_pulses)))
    }else{
      out_pulses=rep(1,length(module_list[[rec_mod]]$outputs))
      names(out_pulses)=module_list[[rec_mod]]$outputs
      pulses=c(pulses,out_pulses)
      pulse_origins=c(pulse_origins,rep(rec_mod,length(out_pulses)))
    }
    
  }
  
  # print(pulse)
}
# print(n_p0)
# print(n_p1)
p0_all=p0_all+n_p0
p1_all=p1_all+n_p1
# flipflop_summ=rbindlist(lapply(1:length(flipflops),function(x) data.table(n=flipflops[x],onoff=module_list[[flipflops[x]]]$onoff)))
# conj_summ=rbindlist(lapply(1:length(conjs),function(x) data.table(n=conjs[x],on_inputs=sum(unlist(module_list[[conjs[x]]]$inputs)),n_inputs=length(module_list[[conjs[x]]]$inputs) )))
# print(flipflop_summ)
# print(conj_summ)
# print(conj_summ[n_inputs!=1][order(n)])
# if(i%%1==0){
#   conj_summ=rbindlist(lapply(1:length(conjs),function(x) data.table(n=conjs[x],on_inputs=sum(unlist(module_list[[conjs[x]]]$inputs)),n_inputs=length(module_list[[conjs[x]]]$inputs),on=paste(names(which(module_list[[conjs[x]]]$inputs==1)),collapse=',') )))
#   conj_summ=conj_summ[n_inputs!=1][order(n)]
#   print(conj_summ)
#   print(i)
# }

}
# 
# 01111
# 01110
# 02221
# 01001
# 02112
# 
# adds 1 to all
# subtracs 1 from jx
# adds 1 to all


dt_out_list=list()
for(i in 1:length(module_list)){
  dt_out_list[[i]]=data.table(from=names(module_list)[i],to=module_list[[i]]$outputs)
}
dt_out=rbindlist(dt_out_list)

# p0_all*p1_all
library(igraph)
v_temps=c('bb','cm','fh','jj','vj','dr','gq','nb','xl','lc','tf','qc','cz')

# v_temps=c('nl','db','bn','hh','hk','ql','ff','ml','rs','rg','kf','mr','hr','xx')

nw=graph_from_data_frame(dt_out[from %in% v_temps |to %in%v_temps],directed=T)
plot(nw)


pracma::Lcm(
pracma::Lcm(3967,3931),
pracma::Lcm(3907,3989))

