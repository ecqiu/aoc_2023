library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_21',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_21',open="r")
linn <-readLines(conn)
close(conn)

map_mat=t(as.matrix(as.data.table(lapply(linn,function(x) as.character(unlist(strsplit(x,split=''))) ))))

s=which(map_mat=='S',arr.ind = T)
# s

# plots=s
# 
# nesw=function(s){
#   out=list(s+rep(c(0,1),each=nrow(s)),s+rep(c(0,-1),each=nrow(s)),s+rep(c(-1,0),each=nrow(s)),s+rep(c(1,0),each=nrow(s)))
#   outm=unique(do.call(rbind,out))
#   outm=outm[outm[,1]>=1 & outm[,1]<=nrow(map_mat),]
#   outm=outm[outm[,2]>=1 & outm[,2]<=ncol(map_mat),]
#   outm=outm[map_mat[outm]%in%c('.','S'),]
#   return(outm)
# }
# 
# for(i in 1:64){
#   print(i)
#   new_plots=nesw(plots)
#   plots=copy(new_plots)
#   print(nrow(plots))
# }
#######
#p2

nesw2=function(s){
  out=list(s+rep(c(0,1),each=nrow(s)),s+rep(c(0,-1),each=nrow(s)),s+rep(c(-1,0),each=nrow(s)),s+rep(c(1,0),each=nrow(s)))
  outm=unique(do.call(rbind,out))
  # outm=outm[outm[,1]>=1 & outm[,1]<=nrow(map_mat),]
  # outm=outm[outm[,2]>=1 & outm[,2]<=ncol(map_mat),]
  outmx=(outm-rep(c(1,1),each=nrow(outm)))%%dim(map_mat)[1] +rep(c(1,1),each=nrow(outm))
  outm=outm[map_mat[outmx]%in%c('.','S'),]
  return(outm)
}

plots=s
n_s=c()
for(i in 1:(65+131*3)){
  print(i)
  new_plots=nesw2(plots)
  plots=copy(new_plots)
  # plotsx=plots[plots[,2]>=1 & plots[,2]<=ncol(map_mat)&plots[,1]>=1 & plots[,1]<=ncol(map_mat),]
  print(nrow(plots))
  n_s=c(n_s,nrow(plots))
  # print(max(plots[,1]))
}
print(nrow(plots))
m=nrow(plotsx)

#@65
#196
#196+131
#196+131+131


# odd
# 7329
# even
# 7367

a=n_s[65+131]-7367 #(4 tops) (1 cycle)
a_4b=n_s[65+131*2]-1*7329-4*7367  #(4 tops + 4corners) (2 cycles)
a_8b=n_s[65+131*3]-1*7367-8*7367-4*7329 #(4 tops + 8corners)

# 1 4+0*4+4*1*4+4*2*4+4*3*4
b4=(a_4b-a)
b4


n_cycles=(26501365-65)/131
# n_cycles=3
n_c=n_cycles
u1=floor((n_c-1)/2)
u2=floor((n_c)/2)
n_odd=(2*u1+1)^2
n_even=(2*u2)^2
  
(n_cycles-1)*b4+a+n_odd*ifelse(n_cycles%%2==0,7329,7367)+n_even*ifelse(n_cycles%%2==0,7367,7329)

#######
# out_list=list()
# 
# d=dim(map_mat)[1]
# top=c(1,(d+1)/2)
# bottom=c(d,(d+1)/2)
# left=c((d+1)/2,1)
# right=c((d+1)/2,d)
# 
# ns=c('t','b','l','r','tl','tr','bl','br')
# mat_list=list(
#   matrix(c(top),ncol=2,byrow=T),
#   matrix(c(bottom),ncol=2,byrow=T),
#   matrix(c(left),ncol=2,byrow=T),
#   matrix(c(right),ncol=2,byrow=T),
#   matrix(c(top,left),ncol=2,byrow=T),
#   matrix(c(top,right),ncol=2,byrow=T),
#   matrix(c(bottom,left),ncol=2,byrow=T),
#   matrix(c(bottom,right),ncol=2,byrow=T)
# )
# 
# 
# for(u in 1:length(ns)){
# n=ns[u]
# plots2=mat_list[[u]]
# for(i in 1:131){
#   print(i)
#   new_plots2=nesw2(plots2)
#   plots2=copy(new_plots2)
#   plotsx=plots2[plots2[,2]>=1 & plots2[,2]<=ncol(map_mat)&plots2[,1]>=1 & plots2[,1]<=ncol(map_mat),]
#   print(nrow(plotsx))
#   # print(plots2[plots2[,1]==1,])
#   # ns=c(ns,nrow(plots2))
#   # if(i==65){
#   #   plots_65=copy(plots2)
#   # }
#   # if(i==(65+131)){
#   #   plots_196=copy(plots2)
#   # }
# }
# 
# out_list[[n]]=nrow(plotsx)
# }
# 
# k=1
# max=7329
# 
# 
# out_list[['t']]+out_list[['b']]+out_list[['l']]+out_list[['r']]+
# (k-1)*(out_list[['tl']]+out_list[['tr']]+out_list[['bl']]+out_list[['br']] )+m*(1+2*(k-1)*(k))
# 
# # 22450
# 
# 
# # 7329
# 
# nrow(plots_65)
# 
# 
# filt=plots_196[,1]>=1&plots_196[,1]<=131&plots_196[,2]>=1&plots_196[,2]<=131
# nrow(plots_196[filt,])
# 
# plots_65[plots_65[,1]==131,]
# plots_196[plots_196[,1]==262,]
# 
# min(plots_196[,1])
# max(plots_196[,2])
# 
# #55
# 
# 
# 
# 
# 5,11,481843
# 
# (26501365-65)/131
# 
# 
# # 202300
# 
# # 26501365
# # gmp::factorize(26501365)
# 
# 
# 
# 
# # nrow(plots)
# # lapply(1:nrow(out1),function(i)nesw(out1[i,]))
# # rbind()
# # lapply(s