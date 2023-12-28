library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_13',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_16',open="r")
linn <-readLines(conn)
close(conn)


map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) unlist(strsplit(x,split='')) ))))

start_beam=c(1,0,0)
get_energy_length=function(start_beam){

  energy_list=0
  beam_heads=list(start_beam)
  beam_heads_remember=paste(start_beam,collapse='_')
  
  x_=1
  while(T){
    # print(x_)
    new_beam_heads=c()
    # print(nrow(energy_list))
    # print(length(beam_heads))
  
    
    
  for(i in 1:length(beam_heads)){
    
    # last_energy_list=copy(energy_list)
    
    
    b=beam_heads[[i]]
    if(x_!=1&&min(beam_heads[[i]][1:2])<=0||x_!=1&&max(beam_heads[[i]][1:2])>max(dim(map_mat)) ){
      next
    }
    
    
    if(b[3]==0){
      beam_heads[[i]]=beam_heads[[i]]+c(0,1,0)
    }
    if(b[3]==1){
      beam_heads[[i]]=beam_heads[[i]]+c(1,0,0)
    }
    if(b[3]==2){
      beam_heads[[i]]=beam_heads[[i]]+c(0,-1,0)
    }
    if(b[3]==3){
      beam_heads[[i]]=beam_heads[[i]]+c(-1,0,0)
    }
    
    if(min(beam_heads[[i]][1:2])<=0||max(beam_heads[[i]][1:2])>max(dim(map_mat)) ){
      next
    }
    
    if(!is.matrix(energy_list)){
      energy_list=matrix(beam_heads[[i]][1:2],ncol=2)
    }else{
      energy_list=rbind(energy_list,beam_heads[[i]][1:2])
    }
    
    new_tile=map_mat[beam_heads[[i]][1],beam_heads[[i]][2]]
    
    b_alt=NULL
    if(new_tile=="\\"){
      if(b[3]==0){beam_heads[[i]][3]=1 }
      if(b[3]==1){beam_heads[[i]][3]=0 }
      if(b[3]==2){beam_heads[[i]][3]=3 }
      if(b[3]==3){beam_heads[[i]][3]=2 }
      # beam_heads_remember=unique(c(beam_heads_remember,paste(beam_heads[[i]],collapse='_')))
    }
    if(new_tile=="/"){
      if(b[3]==0){beam_heads[[i]][3]=3 }
      if(b[3]==1){beam_heads[[i]][3]=2 }
      if(b[3]==2){beam_heads[[i]][3]=1 }
      if(b[3]==3){beam_heads[[i]][3]=0 }
      # beam_heads_remember=unique(c(beam_heads_remember,paste(beam_heads[[i]],collapse='_')))
    }
    
    
    if(new_tile=="|" & b[3]%%2==0 ){
      beam_heads[[i]][3]=1
      b_alt=copy(beam_heads[[i]])
      b_alt[3]=3
      beam_heads=c(beam_heads,list(b_alt))
      # beam_heads_remember=unique(c(beam_heads_remember,paste(beam_heads[[i]],collapse='_'),paste(b_alt,collapse='_') ))
    }
    if(new_tile=="-"& b[3]%%2==1){
      beam_heads[[i]][3]=0
      b_alt=copy(beam_heads[[i]])
      b_alt[3]=2
      beam_heads=c(beam_heads,list(b_alt))
    }
    
    
    new_beam_heads=c(new_beam_heads,paste(beam_heads[[i]],collapse='_'),paste(b_alt,collapse='_') )
    
    new_beam_heads=new_beam_heads[new_beam_heads!='']
    
    # if( identical(beam_heads,last_beam_heads)){
    #   break
    # }
    
    
  }
    new_beam_heads=unique(new_beam_heads)
    energy_list=unique(energy_list)
    del_inds=c()
    for(i in 1:length(beam_heads)){
      if(paste(beam_heads[[i]],collapse='_')%in% beam_heads_remember){
        del_inds=c(del_inds,i)
      }else if(min(beam_heads[[i]][1:2])<=0||max(beam_heads[[i]][1:2])>max(dim(map_mat))){
        del_inds=c(del_inds,i)
      }
    }
    if(length(del_inds)>0){
      beam_heads[del_inds]=NULL
    }
    
    last_beam_heads=copy(beam_heads)
    
    if(length(setdiff(new_beam_heads,beam_heads_remember))==0){
      break
    }else{
      beam_heads_remember=unique(c(beam_heads_remember,new_beam_heads))
    }
    x_=x_+1
  }
  return((nrow(energy_list)))
}

starts=c(lapply(1:110,function(x) c(x,0,0))
,lapply(1:110,function(x) c(x,111,2))
,lapply(1:110,function(x) c(0,x,1))
,lapply(1:110,function(x) c(111,x,3))
)

# test=get_energy_length(c(1,0,0))

out_val=c()
u=331
for(u in 1:length(starts)){
  print(u)
  test=get_energy_length(starts[[u]])
  out_val=c(out_val,test)
}
max(out_val)
