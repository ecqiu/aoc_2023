library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_11',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_12',open="r")
linn <-readLines(conn)
close(conn)

# # l=linn[1]
# total_counts=c()
# # total_counts_end_hash=c()
# # total_counts2=c()
# # total_counts2_..=c()
# # total_counts2_h.=c()
# # total_counts2_.h=c()
# # total_counts2_hh=c()
# for(l in linn){
#   print(l)
#   total_count=0
#   total_count_end_hash=0
#   splitted=strsplit(l,split=' ')[[1]]
#   
#   s1=splitted[1]
#   s2=splitted[2]
#   
#   s1=paste(rep(s1,5),collapse='?')
#   s2=paste0(rep(s2,5),collapse=',')
# 
# 
#   split_in1=strsplit(s1,split='')[[1]]
#   
# 
#   n_spring_current=sum(split_in1=='#')
#   n_q=sum(split_in1=='?')
#   n_q_inds=which(split_in1=='?')
# 
#   
#   # s2=paste(rep(s2,5),collapse=',')
#   total_springs=sum(as.numeric(strsplit(s2,split=',')[[1]]))
#   max_springs=total_springs-n_spring_current
#   
#   split_in1_copy=copy(split_in1)
#   
#   split_in1[n_inds[1]]='#'
#   
#   
#   # combos=combn(n_q,(total_springs-n_spring_current))
#   # print(ncol(combos ))
#   # for(i in 1:ncol(combos )){
#   #   comb_inds=combos[,i]
#   # 
#   #   spring_inds=n_q_inds[as.logical(unlist(comb_inds))]
#   #   
#   #   split_in1_copy=copy(split_in1)
#   #   split_in1_copy[spring_inds]='#'
#   #   split_in1_copy[setdiff(n_q_inds,spring_inds)]='.'
#   #   
#   #   curr_n_splits=0
#   #   out_str=''
#   #   
#   #   v_out=nchar(strsplit(paste(split_in1_copy,collapse=''),'\\.')[[1]])
#   #   v_out=v_out[v_out!=0]
#   #   out_str=paste0(',',paste(v_out,collapse=','))
#   # 
#   #     
#   #   if(split_in1_copy[length(split_in1_copy)]=='#'){
#   #     out_str=paste0(out_str,'#')
#   #   }else{
#   #     out_str=paste0(out_str,'.')
#   #   }
#   # 
#   #   if(out_str==paste0(s2)){
#   #     total_count=total_count+1
#   #   }
#   # 
#   # }
#   total_counts=c(total_counts,total_count)
#   # total_counts_end_hash=c(total_counts_end_hash,total_count_end_hash)
# }


# 
# # total_counts
# # total_counts_end_hash
# # 
# # total_counts2
# # total_counts2_start_hash
# # 
# # total_counts2^4*total_counts
# # total_counts
# # total_counts_end_hash
# 
# total_counts2_..
# total_counts2_h.
# total_counts2_.h
# total_counts2_hh
# 
# 
# ccombo=function(
#   l
# ){
#   total_counts2_..=l[[1]]
#   total_counts2_h.=l[[2]]
#   total_counts2_.h=l[[3]]
#   total_counts2_hh=l[[4]]
#                      
#   xtotal_counts2_..=total_counts2_..*total_counts2_..+total_counts2_.h*total_counts2_..+total_counts2_..*total_counts2_h.
#   xtotal_counts2_h.=total_counts2_h.*total_counts2_..+total_counts2_hh*total_counts2_..+total_counts2_h.*total_counts2_h.
#   xtotal_counts2_.h=total_counts2_..*total_counts2_.h+total_counts2_..*total_counts2_hh+total_counts2_.h*total_counts2_.h
#   xtotal_counts2_hh=total_counts2_h.*total_counts2_.h+total_counts2_h.*total_counts2_hh+total_counts2_hh*total_counts2_.h
#   
#   return(list(xtotal_counts2_..
# ,xtotal_counts2_h.
# ,xtotal_counts2_.h
# ,xtotal_counts2_hh))
# }
# 
# ccombo_out=ccombo(ccombo(list(total_counts2_..
#   ,total_counts2_h.
#   ,total_counts2_.h
#   ,total_counts2_hh
# )))
# 
# total_combos=unlist(lapply(1:length(ccombo_out[[1]]), function(x) sum(unlist(lapply(ccombo_out,function(y) y[[x]])))))
# dot_start_combos=unlist(lapply(1:length(ccombo_out[[1]]), function(x) sum(unlist(lapply(ccombo_out[c(1,3)],function(y) y[[x]])))))
# 
# (total_counts-total_counts_end_hash)*total_combos+total_counts_end_hash*dot_start_combos
# 

######################################






out_dt_zero_list=list()
out_dt_join_list=list()
for(l in linn){
  
  out_dt_zero_mini=list()
  out_dt_join_mini=list()

  print(l)
  splitted=strsplit(l,split=' ')[[1]]
  split_in1=strsplit(splitted[1],split='')[[1]]
  split_in1_mod=c('?',split_in1)

  n_spring_current=sum(split_in1=='#')
  n_q=sum(split_in1=='?')
  n_q_inds=which(split_in1=='?')
  combos=as.matrix(expand.grid(rep(list(0:1),n_q)))
  # s2=splitted[2]
  # total_springs=sum(as.numeric(strsplit(s2,split=',')[[1]]))
  # combos=combn(n_q,(total_springs-n_spring_current))
  split_in1_copy=copy(split_in1)
  m=matrix(0,nrow=nrow(combos),ncol=2)
  for(i in 1:nrow(combos )){
    
    comb_inds=combos[i,]
    spring_inds=n_q_inds[as.logical(comb_inds)]
    
    split_in1_copy[n_q_inds]='.'
    split_in1_copy[spring_inds]='#'
    
    v_out=nchar(strsplit(paste(split_in1_copy,collapse=''),'\\.')[[1]])
    v_out=v_out[v_out!=0]
    out_str=paste0(paste(v_out,collapse=','))

    if(split_in1_copy[1]=='#'){
      out_str=paste0('#',out_str)
    }else{
      out_str=paste0('.',out_str)
    }
    if(split_in1_copy[length(split_in1_copy)]=='#'){
      out_str=paste0(out_str,'#')
    }else{
      out_str=paste0(out_str,'.')
    }
    m[i,]=c(out_str,1)
    # out_dt_zero_mini[[i]]=data.table(k=out_str,count=1) 

  }
  # out_dt_zero=rbindlist(out_dt_zero_mini)
  # out_dt_zero=out_dt_zero[,.(count=sum(count)),by=k]
  out_dt_zero=as.data.table(m)
  names(out_dt_zero)=c('k','count')
  out_dt_zero=out_dt_zero[,.(count=sum(as.numeric(count))),by=k]

  
  # n_spring_current=sum(split_in1_mod=='#')
  n_q=sum(split_in1_mod=='?')
  n_q_inds=which(split_in1_mod=='?')
  combos=as.matrix(expand.grid(rep(list(0:1),n_q)))
  split_in1_copy=copy(split_in1_mod)
  m=matrix(0,nrow=nrow(combos),ncol=2)
  for(i in 1:nrow(combos )){

    comb_inds=combos[i,]

    spring_inds=n_q_inds[as.logical(comb_inds)]

    split_in1_copy[n_q_inds]='.'
    split_in1_copy[spring_inds]='#'
    
    v_out=nchar(strsplit(paste(split_in1_copy,collapse=''),'\\.')[[1]])
    v_out=v_out[v_out!=0]
    out_str=paste0(paste(v_out,collapse=','))

    if(split_in1_copy[length(split_in1_copy)]=='#'){
      out_str=paste0(out_str,'#')
    }else{
      out_str=paste0(out_str,'.')
    }
    if(split_in1_copy[1]=='#'){
      out_str=paste0('#',out_str)
    }else{
      out_str=paste0('.',out_str)
    }

    # out_dt_join_mini[[i]]=data.table(k=out_str,count=1) 
    m[i,]=c(out_str,1)

  }
  out_dt_join=as.data.table(m)
  names(out_dt_join)=c('k','count')
  out_dt_join=out_dt_join[,.(count=sum(as.numeric(count))),by=k]

  out_dt_zero_list[[l]]=out_dt_zero
  out_dt_join_list[[l]]=out_dt_join
}


# mask_str=paste(rep('3,2,1',5),collapse=',')
# test=join_two_tables(t1,t2,mask_str='')



join_two_tables=function(t1,t2,mask_str='',front_match=T,back_match=F){
  # out_dt=data.table()
  out_dt_list=list()
  if(mask_str!=''){
    max_mask=max(as.numeric(strsplit(mask_str,',')[[1]]),na.rm=T)
    vec_vals=unlist(lapply(strsplit(gsub('(#|\\.)','',t1$k),','),function(x) max(as.numeric(x),na.rm=T)))
    t1=t1[vec_vals<=max_mask]
    # 
    # total_mask=sum(as.numeric(strsplit(mask_str,',')[[1]]),na.rm=T)
    # vec_valst=unlist(lapply(strsplit(gsub('(#|\\.)','',t1$k),','),function(x) sum(as.numeric(x),na.rm=T)))
    # t1=t1[vec_valst<=total_mask]
    
    if(front_match){
      init_strs=substr(t1$k,2,nchar(t1$k)-1)
      init_strs=gsub('(,|^)\\d+$','',init_strs)
      t1=t1[unlist(lapply(init_strs,function(x) (x=='')|x==substr(mask_str,1,nchar(x))))]
    }
    
    vec_vals=unlist(lapply(strsplit(gsub('(#|\\.)','',t2$k),','),function(x) max(as.numeric(x),na.rm=T)))
    t2=t2[vec_vals<=max_mask]
    
    # total_mask=sum(as.numeric(strsplit(mask_str,',')[[1]]),na.rm=T)
    # vec_valst=unlist(lapply(strsplit(gsub('(#|\\.)','',t2$k),','),function(x) sum(as.numeric(x),na.rm=T)))
    # t2=t2[vec_valst<=total_mask]
    
    if(back_match){
      fin_strs=substr(t2$k,3,nchar(t2$k)-1)
      t2=t2[unlist(lapply(fin_strs,function(x) (x=='')|x==substr(mask_str,nchar(mask_str)-nchar(x)+1,nchar(mask_str)) ))]
    }
  }
  
  t1=as.matrix(t1)
  t2=as.matrix(t2)
  m=matrix(0,nrow=nrow(t1)*nrow(t2),ncol=2)
  for(i in 1:nrow(t1)){
    for(j in 1:nrow(t2)){
      r1=list(k=t1[i,1],count=as.numeric(t1[i,2]))
      r2=list(k=t2[j,1],count=as.numeric(t2[j,2]))
      
      if(!( substr(r1$k,nchar(r1$k),nchar(r1$k))=='#' &&substr(r2$k,1,1)=='#' ) ){
        k_new= paste0(substr(r1$k,1,nchar(r1$k)-1),',',substr(r2$k,2,nchar(r2$k)))
      }else{
        count1=as.numeric(gsub('.*(,|#|\\.)(\\d+).$','\\2',r1$k))
        count2=as.numeric(gsub('^.(\\d+).*','\\1',r2$k))
        comb_count=count1+count2
        # k_new= paste0(substr(r1$k,1,nchar(r1$k)-2),comb_count,substr(r2$k,3,nchar(r2$k)))
        k_new= paste0(substr(r1$k,1,nchar(r1$k)-1-nchar(count1)),comb_count,substr(r2$k,2+nchar(count2),nchar(r2$k)))
      }
      count_new=r1$count*r2$count
      m[(j-1)*nrow(t1)+i,]=c(k_new,count_new)
      # out_dt_list[[paste0(i,'_',j)]]=data.table(k=k_new,count=count_new)
      # out_dt=rbind(out_dt,data.table(k=k_new,count=count_new))
    }
  }
  # out_dt=rbindlist(out_dt_list)
  out_dt=as.data.table(m)
  
  names(out_dt)=c('k','count')
  out_dt[,k:=gsub('\\.,','.',k)]
  out_dt[,k:=gsub(',\\.','.',k)]
  out_dt=out_dt[,.(count=sum(as.numeric(count))),by=k]
  # out_dt=out_dt[,.(count=sum(count)),by=k]
  if(mask_str!=''){
    # out_dt=out_dt[Vectorize(grepl)(out_dt[,substr(k,5,nchar(k)-3)],mask_str)]
    
    max_mask=max(as.numeric(strsplit(mask_str,',')[[1]]),na.rm=T)
    vec_vals=unlist(lapply(strsplit(gsub('(#|\\.)','',out_dt$k),','),function(x) max(as.numeric(x),na.rm=T)))
    out_dt=out_dt[vec_vals<=max_mask]
    
    # total_mask=sum(as.numeric(strsplit(mask_str,',')[[1]]),na.rm=T)
    # vec_valst=unlist(lapply(strsplit(gsub('(#|\\.)','',out_dt$k),','),function(x) sum(as.numeric(x),na.rm=T)))
    # out_dt=out_dt[vec_valst<=total_mask]
    
    if(front_match){
      init_strs=substr(out_dt$k,2,nchar(out_dt$k)-1)
      init_strs=gsub('(,|^)\\d+$','',init_strs)
      out_dt=out_dt[unlist(lapply(init_strs,function(x) (x=='')|(x==substr(mask_str,1,nchar(x))) ) )]
    }
    
    if(back_match){
      fin_strs=substr(out_dt$k,3,nchar(out_dt$k)-1)
      out_dt=out_dt[unlist(lapply(fin_strs,function(x) (x=='')| (x==substr(mask_str,nchar(mask_str)-nchar(x)+1,nchar(mask_str)))  ))]
    }
  }
  
  return(out_dt)
}

saveRDS(out_dt_zero_list,'/home/eqiu/code_projects/aoc_2023/q12_out_dt_zero_list2.rds')
saveRDS(out_dt_join_list,'/home/eqiu/code_projects/aoc_2023/q12_out_dt_join_list2.rds')

# out_dt_zero_list=readRDS('/home/eqiu/code_projects/aoc_2023/q12_out_dt_zero_list.rds')
# out_dt_join_list=readRDS('/home/eqiu/code_projects/aoc_2023/q12_out_dt_join_list.rds')

# out_vals=c()
# for(q in 1:length(linn)){
#   print(paste0(q,'/',length(linn)))
#   t0=out_dt_zero_list[[q]]
#   t1=out_dt_join_list[[q]]
#   # t2=out_dt_join_list[[q]]
#   
#   mask_str=paste(rep(strsplit(linn[q],split=' ')[[1]][2],5),collapse=',')
#   print(mask_str)
#   
#   
#   tstart=join_two_tables(t0,t1,mask_str=mask_str,front_match=T,back_match=F) 
#   # t3=join_two_tables(t2,t1,mask_str=mask_str)
#   # t4=join_two_tables(t3,t1,mask_str=mask_str)
#   tend=join_two_tables(t1,t1,mask_str=mask_str,front_match=F,back_match=T)
#   
#   
#   # tx=join_two_tables(t1,t1,mask_str=mask_str,front_match=F,back_match=T)
#   # ty=join_two_tables(t1,tx,mask_str=mask_str,front_match=F,back_match=T)
#   
#   if(nrow(tstart)<nrow(tend)){
#     t3=join_two_tables(tstart,t1,mask_str=mask_str,front_match=T,back_match=F) 
#     t5=join_two_tables(t3,tend,mask_str=mask_str,front_match=T,back_match=T) 
#   }else{
#     t3=join_two_tables(t1,tend,mask_str=mask_str,front_match=F,back_match=T) 
#     t5=join_two_tables(tstart,t3,mask_str=mask_str,front_match=T,back_match=T) 
#   }
#   
#   
#   c=t5[substr(k,2,nchar(k)-1)==mask_str,sum(count)]
#   out_vals=c(out_vals,c)
# }
# print(out_vals)
# sum(out_vals)

library(jsonlite)
library(data.table)
library(fst)
library(foreach)
library(doParallel)
cores=detectCores()
cl <- makeCluster(16)
registerDoParallel(cl)

out_list=foreach(q=1:length(out_dt_zero_list),.packages=c('jsonlite','data.table','fst'),.combine='c') %dopar% {
  t0x=out_dt_zero_list[[q]]
  t1x=out_dt_join_list[[q]]
  # t2=out_dt_join_list[[q]]
  
  mask_str=paste(rep(strsplit(linn[q],split=' ')[[1]][2],5),collapse=',')
  print(mask_str)
  
  
  t2x=join_two_tables(t0x,t1x,mask_str=mask_str,front_match=T,back_match=F)
  t3x=join_two_tables(t2x,t1x,mask_str=mask_str,front_match=T,back_match=F)
  t4x=join_two_tables(t3x,t1x,mask_str=mask_str,front_match=T,back_match=F)
  t5x=join_two_tables(t4x,t1x,mask_str=mask_str,front_match=T,back_match=F)
  
  # tstart=join_two_tables(t0,t1,mask_str=mask_str,front_match=T,back_match=F) 
  # tend=join_two_tables(t1,t1,mask_str=mask_str,front_match=F,back_match=T)
  # 
  # if(nrow(tstart)<nrow(tend)){
  #   t3=join_two_tables(tstart,t1,mask_str=mask_str,front_match=T,back_match=F) 
  #   t5=join_two_tables(t3,tend,mask_str=mask_str,front_match=T,back_match=F) 
  # }else{
  #   t3=join_two_tables(t1,tend,mask_str=mask_str,front_match=F,back_match=T) 
  #   t5=join_two_tables(tstart,t3,mask_str=mask_str,front_match=T,back_match=F) 
  # }
  
  
  cx=t5x[substr(k,2,nchar(k)-1)==mask_str,sum(count)]
  list(c(q,cx))
  # out_vals=c(out_vals,c)
}
sum(unlist(lapply(out_list,function(x) x[2])))
# sum(unlist(lapply(out_list,function(x) x[1])))
# 3173244692406
# 3190812705252
# 3899461851819

# 3920437278260


# v1=unlist(lapply(out_dt_zero_list,function(x) x[,sum(count)]))
# v2=unlist(lapply(out_dt_join_list,function(x) x[,sum(count)]))
my_answer=unlist(lapply(out_list,function(x) x[2]))

# saveRDS(my_answer,'/home/eqiu/code_projects/aoc_2023/data/q12_my_answer_temp.rds')

true_answer=c(
748, 211315169336, 9615614, 3376977, 91192, 2773, 1, 512, 1489, 1355, 1024, 1216304, 32, 18683879, 162, 14380500, 32, 243, 1, 1, 2592, 1024, 4782969, 218990, 5184, 89914, 9530704, 52488, 243, 149033475, 1476, 1024, 256, 1, 812859, 248832, 527682, 15620,
 1024, 2385, 243, 1, 243, 74928, 32, 32, 1024, 3888, 7776, 340724, 60360, 1875, 41, 243, 1, 1367, 5184, 1, 313308, 1332, 3888, 2562250, 9157, 16384, 16, 18612765, 16, 2592, 469675066, 32, 162, 243, 62838528296, 512, 938, 28552744, 40283, 4278,
  1109316160, 2500, 12005, 12789, 1024, 1024, 512, 124416, 2121670, 2500, 162, 74096, 1182, 243, 398568, 24576, 6281, 32, 768, 60000, 246672, 512, 1250, 2009677, 5184, 4134625218, 11241911020, 207360, 16, 2077841542, 768, 1587805, 32, 193587, 162, 1024,
   19275770, 1552128, 162, 124416, 4396861, 162, 768, 32, 32, 1102, 267473, 32, 3406756, 248832, 571, 3888, 32, 162, 81, 1875, 243, 5184, 162, 16, 16, 243, 162, 243, 768, 243, 156580, 4770264250, 260, 243, 5184, 240150705, 243, 15124, 55807, 12849494,
    61825229, 2592, 2976006, 1024, 7776, 3308, 89, 683, 16, 1024, 32, 16253, 1, 1609484, 16384, 1, 5355744, 1, 1875, 32184, 162, 26244, 522119, 444898, 243, 1875, 32, 1024, 2500, 2118492144, 12754584, 98334, 1, 1024, 32, 4997120, 272, 2019, 1875, 1024,
     81, 257049, 1920000, 1, 3888, 1, 3222798645, 5184, 768, 243, 1679616, 135882, 243, 162, 16807, 44498198202, 32, 162, 1, 7776, 32, 7203, 106390, 162, 32, 2400000, 32, 243, 4487214, 32, 16, 32, 600282, 26244, 27109, 32, 322464, 32, 4954283, 32, 32,
      13912, 1, 1024, 1, 32, 19683, 82165632, 7588, 32, 1, 162, 25483467, 13492, 5657338, 26244, 32, 768, 142805, 14883656, 133864, 1471735100, 2786126, 3012867, 11780, 243, 6440, 1024, 16, 39366, 280, 252, 3525216, 32, 1875, 32768, 162, 111889, 7776,
       484208, 71044, 32, 60000, 171485, 4778, 7407140, 32416, 303750, 17861, 32768, 1, 24912, 3190187286, 26999487632, 162, 68168, 537664, 183579396, 20729865, 32, 54700816, 243, 6621843924, 8393284, 1, 162, 9132076, 1866240, 16, 94923, 3888, 1875, 26244, 243, 3888, 6397664, 111279, 32, 16, 32, 2500, 3280157, 4780008, 32, 19029, 9604, 553648128, 3888, 932718222, 2365959, 117128, 81, 1024, 1123494, 256, 32, 199927, 81543597, 3125, 13191680, 3390656, 162, 512, 1, 16, 683925, 16, 512, 458752, 32, 1,
        243, 1875, 81, 7776, 39366, 243, 162, 32, 1182, 39366, 307830, 2500, 1, 7635, 463, 2848, 243, 4444732, 1024, 5184, 768, 3125, 42120518, 1, 1, 3888, 31367, 4096, 153, 1, 1, 16, 81, 512, 8757047, 24576, 763, 243, 1024, 975, 5184, 23694128, 1,
         1007706922, 13780028, 26244, 32, 1939178, 1651520, 2366920, 3102599889, 847660528, 32, 18958, 3348432, 243, 243, 7776, 100000, 1, 3125, 107840, 5184, 3169870830126, 2592, 2658, 125952594, 243, 6480, 162, 262087, 81, 16, 38125, 9925, 158577,
          184231936, 171366, 1424, 32, 5184, 512, 537824, 53366912, 156806, 1, 140431, 32, 162, 162, 1983, 7353, 32, 32, 2754, 246672, 59969536, 512, 3938, 1440000, 24576, 243, 128511, 3125, 12630808, 9604, 60000, 524288, 7203, 71789, 32, 364088,
           12324366, 74272,
          3888, 32, 512, 45927, 689199, 1, 32, 512, 5184, 162, 378032352, 528, 19469077, 32, 768, 1926368, 1024, 162, 8192, 44149958, 162, 1, 916287, 23757, 7776, 162, 1, 1, 1, 2086, 162, 1, 107904, 2116786, 12005, 16, 2361500884, 2500, 81, 16249336,
           32, 32, 13049, 40000, 26244, 162, 32, 1, 625, 1259712, 175200, 7776, 1450274, 18634124, 2895978, 307328, 93330, 5184, 1024, 3888, 162, 3618, 32, 32, 3888, 7776, 243, 15637290, 32, 2065293, 32, 9243, 175701, 165888, 1875, 3888, 32, 2500, 3888, 196608, 1, 5184, 157282236, 4687500, 1119650860, 16384, 58454016, 32, 2338693, 573706, 1024, 60000, 264510, 22539, 1759100, 1227150, 14406, 9604, 243, 3229515, 27187840, 162, 1, 16384, 32, 5502735, 3888, 162, 3888, 243, 1024, 71214, 17960762960,
            2730971, 2849904, 32, 768, 2993376, 32, 6320104, 169658, 230496, 32, 100000, 2203, 237528, 66276, 3699100, 74303, 32, 110237, 512, 16, 32, 162, 768, 81, 477096, 96806, 60000, 5184, 351087, 1024, 130691232, 18837, 10911, 162, 5184, 15727842,
             32907226, 23586, 2667, 16065630, 162, 1744173, 30045015, 512, 243, 89880, 10816840, 566, 29770261011, 243, 1024, 243, 3888, 56028, 14406, 1385832, 102744, 32768, 124416, 5184, 32, 2912754, 16807, 768, 20464, 32, 1024, 1048576, 243, 142218, 1, 32, 153664, 16, 409000, 874313172, 27064, 3375420, 16, 1, 32, 11584661, 768, 2072848, 1334468, 7997, 39366, 124416, 3268760, 28439188, 289288288, 12005, 1131120, 45743, 76530356, 1, 1, 2402950, 19275, 88472, 16, 1600000, 32, 1683, 32805, 525413,
              478896, 32, 27064, 10515, 1440000, 1024, 22347, 1963, 1, 1910800, 1, 1948860, 256, 1, 18996628, 1024, 6404, 3124, 749346416, 7745024, 1068716, 16, 112944, 2083438, 32, 16, 32, 5852, 380840, 383820818, 7013605, 106473678, 342032, 5184,
               514331, 16, 1, 2403, 153664, 512, 154116, 131529, 100000, 8783776376, 3661915058, 120924, 243, 398052, 16807, 6480, 1024, 25952, 307328, 162, 537824, 68516, 512, 81, 80643, 2089, 100000, 3888, 9604, 1, 5184, 32, 1, 1037, 124416, 192194,
                1, 26244, 199927, 1, 512, 1875, 50000, 1875, 20206, 272, 1875, 187224, 32, 80000, 32, 30000, 10784345, 3200000, 60000, 32, 9089618, 87510, 367632, 32, 2424, 155117520, 168625, 90776183065, 71044, 256, 993054601, 57867364, 1538, 3200000,
                 768, 16, 32, 100000, 59049, 3981760, 32, 1875, 1, 2486582, 6921576, 256, 262144, 6480, 1600000, 273455, 2500, 32, 80000, 32, 29282, 32, 243, 512, 162, 16807, 1, 1563852, 17739008000, 168790, 3294, 243, 32, 25404, 314, 2500, 2354383517,
                  1, 5971968, 1, 927293093, 80000, 243, 10864, 5184, 243, 401301, 1024, 20939430, 16, 32, 183579396, 60208, 664624, 81, 653079, 912247, 512, 162, 96687, 3888, 516213, 162, 28349789, 1024, 91048005, 165504, 1, 589824, 4626882, 24576,
                   59049, 16, 808448, 3888, 162, 32, 1975179, 31757969376, 8054, 512, 8225568, 1024, 7642, 165888, 7203, 243, 248832, 44485, 512, 1, 14406, 41273982, 8139498, 7203, 5405220, 256432, 15842092, 512, 212154, 32, 1, 5184, 1260488, 32, 2500,
                    7776, 50000, 1875, 788768, 243, 162, 4443707744, 252, 243, 243, 7776, 630886, 416476, 2500, 32, 768, 1, 32, 414684020, 32, 1259712, 162, 10242460, 5280447, 124416, 13875766, 39366, 553878, 11584, 32, 52165681, 1, 1, 1, 243, 3888, 81,
                     243, 167352, 32, 16, 1024, 30000, 136248234600, 1024, 7276, 7314, 5747, 1024, 512, 207375, 28796625, 512, 80000, 32, 16, 1, 29729109, 16, 1, 32, 243, 211459, 1, 18392, 48909, 256, 9604, 636840, 60466176, 1725764, 1024, 81, 243,
                      22871, 2500, 7776, 2500, 243, 595288, 14753987, 162, 243, 26244, 5184, 64893, 512, 5184, 452612529, 8381524, 19029, 1, 243, 6424, 768, 512, 8126194, 2500, 185836, 918366604, 10977307, 7776, 665637061, 360290736, 9760, 1, 7776,
                       7776, 559976, 23041216, 3409, 47002
)
# which(my_answer!=true_answer)
linn[which(my_answer!=true_answer)]