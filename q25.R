library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_22',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_25',open="r")
linn <-readLines(conn)
close(conn)

splitted=strsplit(linn,split=':')

str1=unlist(lapply(splitted,function(x) x[1]))

str2s=(lapply(splitted,function(x) strsplit(trimws(x[2]),' ')[[1]] ))
dt_list=lapply(1:length(str1),function(x) data.table(from=str1[x],to=unlist(str2s[x]) ))

dt_stack=rbindlist(dt_list)
dt_stack[,edge_ind:=1:.N]


all_nodes=sort(unique(c(dt_stack$from,dt_stack$to)))

adj_mat=matrix(0,nrow=length(all_nodes),ncol=length(all_nodes))



# for(i in 1:nrow(dt_stack)){
#   a_ind=which(all_nodes==dt_stack[i,from])
#   b_ind=which(all_nodes==dt_stack[i,to])
#   
#   adj_mat[a_ind,b_ind]=1
#   adj_mat[b_ind,a_ind]=1
# }

dt_stack_copy=copy(dt_stack)

dt_stack_copy2=data.table(from=dt_stack_copy$to,to=dt_stack_copy$from,edge_ind=dt_stack_copy$edge_ind)

dt_stack_full=rbind(dt_stack_copy,dt_stack_copy2)
dt_stack_full[,edge_ind:=1:.N]

s='zsx'
e='vlc'

# s='jqt'
# e='frs'

current=s
canonical_list=list()


cancelled_edges=c()
while(length(canonical_list)<3){ 
  print(length(canonical_list))
  cancelled_edges=c(cancelled_edges,unlist(canonical_list[length(canonical_list)]))
  
  uncancelled=intersect(cancelled_edges,cancelled_edges-nrow(dt_stack_full)/2)
  uncancelled=c(uncancelled,intersect(cancelled_edges,cancelled_edges+nrow(dt_stack_full)/2))
  cancelled_edges=setdiff(cancelled_edges,uncancelled)

  path_list=list()
  for(i in 1:nrow(dt_stack_full[from==s])){
    path_list[[i]]=c(dt_stack_full[!(edge_ind %in%cancelled_edges )&from==s][i,]$edge_ind)
  }
  
  while(length(path_list)>0){
    p=path_list[[1]]
    path_list[[1]]=NULL
    
    current=dt_stack_full[edge_ind==p[length(p)]]$to
    
    if(current==e){
      canonical=p
      canonical_list=c(canonical_list,list(canonical))
      end=0
      break
    }
    possible_edges=dt_stack_full[!(edge_ind %in%cancelled_edges )& from==current&!(edge_ind %in% p)&!( (edge_ind-nrow(dt_stack_full)/2) %in% p)&!((edge_ind+nrow(dt_stack_full)/2) %in% p)]
    if(nrow(possible_edges)>0){
      for(j in 1:nrow(possible_edges)){
        r=possible_edges[j,]
        path_list=c(list(c(p,r$edge_ind) ),path_list)
      }
    }
    end=1
  }
  
  if(length(canonical_list)==3){
    cancelled_edges=c(cancelled_edges,unlist(canonical_list[length(canonical_list)]))
  
    uncancelled=intersect(cancelled_edges,cancelled_edges-nrow(dt_stack_full)/2)
    uncancelled=c(uncancelled,intersect(cancelled_edges,cancelled_edges+nrow(dt_stack_full)/2))
    cancelled_edges=setdiff(cancelled_edges,uncancelled)
    
    
    component=s
    to_add=dt_stack_full[!(edge_ind %in%cancelled_edges )&from %in% component & !(to%in% component)]
    
    while(nrow(to_add)>0){
      component=c(component,to_add$to)
      to_add=dt_stack_full[!(edge_ind %in%cancelled_edges )&from %in% component & !(to%in% component)]
    }
    
  }
  # print('next')
  # if(end==1){break}
}
component=unique(component)
dt_stack_full[from%in% component & !(to %in% component)]

length(component)*(length(all_nodes)- length(component))

# canonical




# sum(adj_mat)

