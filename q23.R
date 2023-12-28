library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_22',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_23',open="r")
linn <-readLines(conn)
close(conn)

map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) as.character(unlist(strsplit(x,split=''))) ))))
dot_inds=which(map_mat=='.',arr.ind=T)
s=dot_inds[dot_inds[,1]==1]
e=dot_inds[dot_inds[,1]==nrow(map_mat)]


live_path_list=list(paste(s,collapse='_'))
out_path_list=list()

#breadth first search
#(just change the position of live path list for depth first - )
i=1
while(length(live_path_list)>0){
  if(i%%1000==0){
    print(i)
    print(length(live_path_list))
  }

  path=live_path_list[[1]]
  live_path_list[[1]]=NULL
  
  # paste(loc,collapse='_')
  
  loc_char=path[length(path)]
  loc=as.numeric(strsplit(loc_char,split='_')[[1]])
  
  if(all(loc==e)){
    out_path_list=c(out_path_list,list(path))
    next
  }
  
  r=loc+c(0,1)
  l=loc+c(0,-1)
  d=loc+c(1,0)
  u=loc+c(-1,0)
  
  out_dirs=list()
  #part1
  if( min(r)>0 && max(r)<=dim(map_mat)[1]&& !(paste(r,collapse='_') %in% path) && map_mat[matrix(r,ncol=2)]%in%c('.','>') ){ out_dirs=c(out_dirs,list(r))}
  if( min(l)>0 && max(l)<=dim(map_mat)[1]&& !(paste(l,collapse='_') %in% path) && map_mat[matrix(l,ncol=2)]%in%c('.','<') ){ out_dirs=c(out_dirs,list(l))}
  if( min(d)>0 && max(d)<=dim(map_mat)[1]&& !(paste(d,collapse='_') %in% path) && map_mat[matrix(d,ncol=2)]%in%c('.','v') ){ out_dirs=c(out_dirs,list(d))}
  if( min(u)>0 && max(u)<=dim(map_mat)[1]&& !(paste(u,collapse='_') %in% path) && map_mat[matrix(u,ncol=2)]%in%c('.','^') ){ out_dirs=c(out_dirs,list(u))}
  
  #part2 (too slow)
  # if( min(r)>0 && max(r)<=dim(map_mat)[1]&& !(paste(r,collapse='_') %in% path) && map_mat[matrix(r,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(r))}
  # if( min(l)>0 && max(l)<=dim(map_mat)[1]&& !(paste(l,collapse='_') %in% path) && map_mat[matrix(l,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(l))}
  # if( min(d)>0 && max(d)<=dim(map_mat)[1]&& !(paste(d,collapse='_') %in% path) && map_mat[matrix(d,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(d))}
  # if( min(u)>0 && max(u)<=dim(map_mat)[1]&& !(paste(u,collapse='_') %in% path) && map_mat[matrix(u,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(u))}
  
  for(d in out_dirs){
    live_path_list=c(live_path_list, list(c(path,paste(d,collapse='_'))))
  }
  i=i+1
}

max(unlist(lapply(out_path_list,length))-1)


################
#part 2
locs=as.matrix(expand.grid(1:dim(map_mat)[1],1:dim(map_mat)[2]))

#find how many exit nodes from each point
loc_list=list()
i=1
for(i in 1:nrow(locs)){
  loc=locs[i,]
  if(!(map_mat[matrix(loc,ncol=2)] %in%c('.','>','<','v','^'))){
    next
  }
  
  r=loc+c(0,1)
  l=loc+c(0,-1)
  d=loc+c(1,0)
  u=loc+c(-1,0)
  
  out_dirs=list()
  #part2
  if( min(r)>0 && max(r)<=dim(map_mat)[1]&& map_mat[matrix(r,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(r=r))}
  if( min(l)>0 && max(l)<=dim(map_mat)[1]&& map_mat[matrix(l,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(l=l))}
  if( min(d)>0 && max(d)<=dim(map_mat)[1]&& map_mat[matrix(d,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(d=d))}
  if( min(u)>0 && max(u)<=dim(map_mat)[1]&& map_mat[matrix(u,ncol=2)]%in%c('.','>','<','v','^') ){ out_dirs=c(out_dirs,list(u=u))}
  loc_list[[paste(loc,collapse='_') ]]=out_dirs
}

#filter to only nodes with more than 2 neighbours (otherwise path is determined), i.e. 'key nodes' 
key_nodes=loc_list[lapply(loc_list,length)>2]
key_nodes=c(names(key_nodes),paste(s,collapse='_'),paste(e,collapse='_'))

#create map between 'key nodes'
key_node_map_list=list()
for(k in key_nodes){
  dirs=names(loc_list[[k]])
  
  out_keynode_dir_list=list()
  for(d in dirs){
    loc=loc_list[[k]][[d]]
    path=c(paste(loc,collapse='_'))
    while(!(path[length(path)] %in% key_nodes)){
      new_loc=unlist(lapply(loc_list[[paste(loc,collapse='_')]],function(x)paste(x,collapse='_')))
      new_loc=setdiff(new_loc,c(path,k) )
      if(length(new_loc)!=1){
        stop('error')
      }
      path=c(path,new_loc)
      loc=new_loc
    }
    
    out_keynode_dir_list[[d]]=path
  }
  key_node_map_list[[k]]=out_keynode_dir_list
}

#######################

graph_dt=lapply(names(key_node_map_list), function(n){
  out=data.table(unlist(lapply(key_node_map_list[[n]],function(x) x[length(x)])))
  edge_lengths=unlist(lapply(key_node_map_list[[n]],function(x) length(x)))
  names(out)='to'
  out[,from:=n]
  out[,edge_length:=edge_lengths]
  return(out)
  }
)

graph_dt_stack=rbindlist(graph_dt)

layer_nodes=list()
layer_nodes[[1]]=c('20_20','20_42','18_66','10_90','16_110')
layer_nodes[[2]]=c('30_10','40_32','42_64','44_84','42_112','30_132')
layer_nodes[[3]]=c('60_20','64_32','54_68','58_84','60_102','64_138')
layer_nodes[[4]]=c('78_12','80_44','90_62','80_86','78_108','84_138')
layer_nodes[[5]]=c('108_14','104_40','110_68','100_78','108_110','110_136')
layer_nodes[[6]]=c('136_34','134_68','130_76','126_112','128_138')

# graph_dt_stack[from %in% layer_nodes[[3]],.N,by=from]

for(i in 1:6){
  graph_dt_stack[from %in% layer_nodes[[i]],from_layer:=i]
  graph_dt_stack[to %in% layer_nodes[[i]],to_layer:=i]
}

obligatory_edges=graph_dt_stack[from=='1_2' | to=='141_140']
graph_dt_stack_half=graph_dt_stack[from>to]


for(l in 1:5){
  layer_edges=graph_dt_stack_half[from_layer==to_layer&from_layer==l]
  next_edges=graph_dt_stack_half[(from_layer==l&to_layer==(l+1))|(from_layer==(l+1)&to_layer==l)]
}


#7460 max
# sum(graph_dt_stack_half[!is.na(from_layer)&!is.na(to_layer)][order(-edge_length)][1:34]$edge_length)+sum(graph_dt_stack_half[is.na(from_layer)|is.na(to_layer)][order(-edge_length)][1:2]$edge_length)

# graph_dt_stack_half[!is.na(from_layer)&!is.na(to_layer)][order(-edge_length)][1:33][,.N,by=to]

#should probably add early stopping (i.e. kill if cant possibly beat max so far) 6334
#also if like not that many edges? (idk about that one)

#edge search?
graph_dt_stack[,edge_index:=1:.N]
graph_dt_stack[,key:=ifelse(from<to,paste0(from,'|',to),paste0(to,'|',from))]
graph_dt_stack[,edge_index2:=frank(key,ties.method='dense')]
graph_dt_stack[,key:=NULL]


init_edge_list=graph_dt_stack[from==paste(s,collapse='_')|to==paste(e,collapse='_')]
edge_list=copy(init_edge_list)
live_edge_list=list( edge_list)
out_edge_list=list()
state_list <- new.env(hash=TRUE)
i=1
# max_val=6000
max_val=0
max_val_set=6549
# max_val_set=0
n_nodes=length(unique(graph_dt_stack$to))

graph_dt_stack=graph_dt_stack[order(-edge_length)]
while(length(live_edge_list)>0){
  i=i+1
  edge_list=live_edge_list[[1]]
  live_edge_list[[1]]=NULL
  
  if(i%%1000==0){
    print(i)
    print(length(live_edge_list))
    if(length(out_edge_list)>0){
      print(max_val)
    }
  }
  
  # 
  if( '128_138' %in% edge_list$to && sum(edge_list$edge_length)>max_val ){
    out_edge_list=c(out_edge_list,list(edge_list) )
    max_val=max(unlist(lapply(out_edge_list,function(x) x[,sum(edge_length)])))
    next
  }
  
  edge_key=paste(edge_list[order(edge_index)]$edge_index,collapse='|')
  hash_val=state_list[[edge_key]]
  if(is.null(hash_val)){
    state_list[[edge_key]]=1
  }else{
    # next
  }
  
  
  banned_to_list=c(edge_list[,.N,by=to][N>0]$to,paste(s,collapse='_'))
  banned_from_list=c(edge_list[,.N,by=from][N>0]$from,paste(e,collapse='_'))
  exist_edge_index=edge_list$edge_index2
  # max_to=max(edge_list[to!=paste(e,collapse='_')&from!=paste(s,collapse='_')]$to)
  # if(is.na(max_to)){max_to='0'}
  avail_edge_ever_list=graph_dt_stack[!(to %in%banned_to_list)&!(from %in%banned_from_list)&!(edge_index2 %in% exist_edge_index)]
  if(nrow(avail_edge_ever_list)==0){
    next
  }
  
  avail_edge_ever_list[,edge_ind:=1:.N,by=to]
  avail_edge_ever_list[,edge_ind2:=1:.N,by=from]
  
  # avail_edge_ever_list=avail_edge_ever_list[edge_ind==1]
  x1=avail_edge_ever_list[edge_ind==1]
  x2=avail_edge_ever_list[edge_ind2==1]
  
  avail_edge_ever_list[,`:=`(edge_ind=NULL,edge_ind2=NULL)]
  
  max_avail_left1=sum(x1[1:(n_nodes-1-nrow(edge_list))]$edge_length,na.rm=T)
  max_avail_left2=sum(x2[1:(n_nodes-1-nrow(edge_list))]$edge_length,na.rm=T)
  max_avail_left=min(max_avail_left1,max_avail_left2)
  max_nodes_left=min(nrow(x1),nrow(x2))
  # 
  if((max_nodes_left+nrow(edge_list)<(n_nodes-3)) ||(max_avail_left+sum(edge_list$edge_length)<=pmax(max_val,max_val_set)) ){
    next
  }
  
  avail_edge_list=avail_edge_ever_list[!(to %in%banned_to_list)&!(from %in%banned_from_list)&!(edge_index2 %in% exist_edge_index)&(from %in%banned_to_list)]
  if(nrow(avail_edge_list)>0){
    for(u in 1:nrow(avail_edge_list)){
      live_edge_list=c( list(rbind(edge_list,avail_edge_list[u,])),live_edge_list)
    }
  }
}

unlist(lapply(out_edge_list,function(x) x[,sum(edge_length)]))
#6628 (but it takes a couple of hours...)


# graph_dt_stack[!is.na(from_layer)&!is.na(to_layer)]



# 
# last_crosses=1
# edge_list=list()
# for(i in 1:5){
#   n_pairs_max=floor((nrow(graph_dt_stack[from %in% layer_nodes[[i]],.N,by=from] )-last_crosses)/2)
#   
#   for(x in (2*0:n_pairs_max+1)){
#   }
# }





# 
# # setkeyv(graph_dt_stack,c('from'))
# # graph_dt_stack=graph_dt_stack[from<to]
# 
# # library(igraph)
# # # v_temps=c('bb','cm','fh','jj','vj','dr','gq','nb','xl','lc','tf','qc','cz')
# # # v_temps=c('nl','db','bn','hh','hk','ql','ff','ml','rs','rg','kf','mr','hr','xx')
# # 
# # nw=graph_from_data_frame(graph_dt_stack_half,directed=F)
# # plot(nw,edge.label=graph_dt_stack_half$edge_length)
# 
# live_path_list=list( list(path=paste(s,collapse='_'),total_length=0) )
# out_path_list=list()
# 
# current_loc=paste(s,collapse='_')
# path=c(current_loc)
# total_length=0
# i=1
# print(Sys.time())
# # state_list=list()
# state_list <- new.env(hash=TRUE)
# while(length(live_path_list)>0){
#   if(i %%1000==0 &length(out_path_list)>0){
#     print(i)
#     print(length(live_path_list))
#     length_list=unlist(lapply(out_path_list,function(x) x$total_length))
#     max_val=max(unlist(lapply(out_path_list,function(x) x$total_length)))
#     out_path_list=out_path_list[length_list==max_val]
#     print(max_val)
#   }
#   
#   next_p=live_path_list[[1]]
#   live_path_list[[1]]=NULL
#   old_path=next_p$path
#   old_total_length=next_p$total_length
#   current_loc=old_path[length(old_path)]
#   
#   
#   
#   old_path_key=paste0(paste(sort(old_path),collapse='|'),'-',current_loc)
#   hash_val=state_list[[old_path_key]]
#   if(is.null(hash_val)|| old_total_length>hash_val ){
#     state_list[[old_path_key]]=old_total_length
#   }else{
#     next
#   }
#   # if(old_path_key %in% names(state_list) &&state_list[[old_path_key]]>=old_total_length ){
#   #   next
#   # }else{
#   #   state_list[[old_path_key]]=old_total_length
#   # }
#   
#   
#   if(current_loc==paste(e,collapse='_')){
#     out_path_list=c(out_path_list,list(next_p))
#     next
#   }
#   
#   # graph_dt_stack[from==current_loc]
#   graph_dt_next=graph_dt_stack[from==current_loc & !(to %in%old_path)][order(-edge_length)]
#   
#   if(current_loc=='128_138'){
#     graph_dt_next=graph_dt_next[to=='141_140']
#   }
#   
#   current_layer=graph_dt_stack[from==current_loc]$from_layer[1]
#   if(!is.na(current_layer) &&  all(graph_dt_stack[from_layer==current_layer]$from %in%old_path )){
#     graph_dt_next=graph_dt_next[to_layer>current_layer]
#   }
#   
#   # if( length(intersect(old_path, c('126_112','110_136')))==2  ){
#   #   graph_dt_next=graph_dt_stack[to=='128_138']
#   # }
#   
#   if(nrow(graph_dt_next)==0){
#     next
#   }
#   
#   for(u in 1:nrow(graph_dt_next)){
#     r=graph_dt_next[u,]
#     out_list=list(path=c(old_path,r$to),total_length=old_total_length+r$edge_length )
#     live_path_list=c(live_path_list,list(out_list))
#   }
#   
#   i=i+1
# }
# max_val=max(unlist(lapply(out_path_list,function(x) x$total_length)))
# print(max_val)
# print(Sys.time())
# # 3922
# #6146

##########3

#do search between key nodes only
# live_path_list=list(paste(s,collapse='_'))
# out_path_list=list()
# i=1
# while(length(live_path_list)>0){
#   if(i%%1000==0){
#     print(i)
#     print(length(live_path_list))
#   }
# 
#   path=live_path_list[[1]]
#   live_path_list[[1]]=NULL
#   
#   # paste(loc,collapse='_')
#   
#   loc_char=path[length(path)]
#   loc=as.numeric(strsplit(loc_char,split='_')[[1]])
#   
#   if(all(loc==e)){
#     out_path_list=c(out_path_list,list(path))
#     next
#   }
#   
#   for(x in key_node_map_list[[paste(loc,collapse='_')]]){
#     if(!(x[length(x)] %in% path)){
#       live_path_list=c(live_path_list, list(c(path, x)))
#     }
#   }
#   i=i+1
# }
# 
# max(unlist(lapply(out_path_list,length))-1)

