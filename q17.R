library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_13',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_17',open="r")
linn <-readLines(conn)
close(conn)


map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) as.numeric(unlist(strsplit(x,split=''))) ))))


# score_list=list('1,1|0|0'=0,'1,1|1|0'=0)
new_heads=list('001,001|0|00'=0,'001,001|1|00'=0)
score_list <- new.env(hash=TRUE)
score_list[['001,001|0|00']]=0
score_list[['001,001|1|00']]=0

counter=0
while(length(new_heads)>0){
  next_new_heads=list()
  for(i in 1:length(new_heads)){
    n_temp=names(new_heads)[i]
    
    # tilex=as.numeric(c(
    #   gsub('([^|,]+),([^|,]+)\\|([^|]+)\\|([^|]+)','\\1',n_temp)
    #   ,gsub('([^|,]+),([^|,]+)\\|([^|]+)\\|([^|]+)','\\2',n_temp)
    # ))
    # 
    # dirx=as.numeric(gsub('([^|,]+),([^|,]+)\\|([^|]+)\\|([^|]+)','\\3',names(new_heads)[i]))
    # in_arowx=as.numeric(gsub('([^|,]+),([^|,]+)\\|([^|]+)\\|([^|]+)','\\4',names(new_heads)[i]))
    
    tile=as.numeric(c(
      substr(n_temp,1,3)
      ,substr(n_temp,5,7)
    ))
    dir=as.numeric(substr(n_temp,9,9))
    in_arow=as.numeric(substr(n_temp,11,12))

    # if(!all(tilex==tile)){
    #   print(tilex)
    #   print(tile)
    # }
    # 
    
    dirs=(dir+c(-1,0,1))%%4
    
    for(x in dirs){
      #p2 condition
      if(in_arow<4&&x!=dir){
        next
      }
      if(in_arow>=10&&x==dir){
        next
      }
      
      #p1 condition
      # if(in_arow>=3&&x==dir){
      #   next
      # }
      
      if(x==dir){
        new_in_arow=in_arow+1
      }else{
        new_in_arow=1
      }

      
  
      if(x==0){
        new_tile=tile+c(0,1)
      }
      if(x==1){
        new_tile=tile+c(1,0)
      }
      if(x==2){
        new_tile=tile+c(0,-1)
      }
      if(x==3){
        new_tile=tile+c(-1,0)
      }
  
      if(min(new_tile)<1||max(new_tile)>max(dim(map_mat))){
        next
      }
      
      new_n=paste0(paste(formatC(new_tile,width=3,format='d',flag='0'),collapse=','),'|',x,'|',formatC(new_in_arow,width=2,format='d',flag='0'))
      val=new_heads[[i]]+map_mat[new_tile[1],new_tile[2]]
      
      
      old_val=score_list[[new_n]]
      if(is.null(old_val)|| val<old_val ){
        score_list[[new_n]]=val
        next_new_heads[[new_n]]=val
      }
      
    }
  }
  new_heads=copy(next_new_heads)
  counter=counter+1
  if(counter%%1==0){
    print(counter)
    print(length(new_heads))
  }
}
min(unlist(lapply(grep(paste(formatC(dim(map_mat),width=3,format='d',flag='0'),collapse=','), names(score_list),value=T),function(x) score_list[[x]])))
