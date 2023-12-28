library(data.table)
options(scipen=999,digits=22)
# library(bit64)

# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_22',header=F,sep='->')

conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_24',open="r")
linn <-readLines(conn)
close(conn)



coords_char=strsplit(unlist(lapply(strsplit(linn,split='@'),function(x) x[1])),split=',')
coord_dt=as.data.table(t(as.data.table(coords_char)))
coord_dt[,V1:=as.numeric(V1)]
coord_dt[,V2:=as.numeric(V2)]
coord_dt[,V3:=as.numeric(V3)]
names(coord_dt)=c('px','py','pz')

velo_char=strsplit(unlist(lapply(strsplit(linn,split='@'),function(x) x[2])),split=',')
velo_dt=as.data.table(t(as.data.table(velo_char)))
velo_dt[,V1:=as.numeric(V1)]
velo_dt[,V2:=as.numeric(V2)]
velo_dt[,V3:=as.numeric(V3)]
names(velo_dt)=c('vx','vy','vz')

full_dt=cbind(coord_dt,velo_dt)


int=0
for(i in 1:(nrow(full_dt)-1) ){
  # print(i)
  l1=full_dt[i,]
  for(j in (i+1):(nrow(full_dt))){
    # print(j)
    l2=full_dt[j,]
    
    l1_px=-l1$px/l1$vx
    l1_py=-l1$py/l1$vy
    l1_vx=1/l1$vx
    l1_vy=1/l1$vy
    
    l2_px=-l2$px/l2$vx
    l2_py=-l2$py/l2$vy
    l2_vx=1/l2$vx
    l2_vy=1/l2$vy
    
    # y_int=(l1$px*l2$vx+l2$py*l1$vx-l2$px*l1$vx-l1$py*l2$vx )/(l1$vy*l2$vx-l2$vy*l1$vx)
    # x_int=(l1$py*l2$vy+l2$px*l1$vy-l2$py*l1$vy-l1$px*l2$vy )/(l1$vx*l2$vy-l2$vx*l1$vy)
    
    x_int=(l1_py*l2_vy+l2_px*l1_vy-l2_py*l1_vy-l1_px*l2_vy )/(l1_vx*l2_vy-l2_vx*l1_vy)
    y_int=(l1_px*l2_vx+l2_py*l1_vx-l2_px*l1_vx-l1_py*l2_vx )/(l1_vy*l2_vx-l2_vy*l1_vx)
    
    if(x_int>=200000000000000 && x_int<=400000000000000 &&y_int>=200000000000000 && y_int<=400000000000000& (x_int-l1$px)/l1$vx>0& (x_int-l2$px)/l2$vx>0){
      int=int+1
    }
    # if(x_int>=7 && x_int<=27 &&y_int>=7 && y_int<=27& (x_int-l1$px)/l1$vx>0& (x_int-l2$px)/l2$vx>0 ){
    #   int=int+1
    #   # print(paste0(i,'_',j))
    #   # print('int')
    #   # print(x_int)
    #   # print(y_int)
    #   # print(int)
    # }
  }
}
print(int)


# 
# l1=full_dt[1,]$px
# 
# 
# vx=1:100
# t=1:100
# 
# px=l1$px+(l1$vx-vx)*t
# 
# #integer position velocity
# 
# # solve
# # qr.solve
# 
# A <- matrix(runif(12), 4)
# b <- 1:4
# qr.solve(A, b) # or solve(qr(A), b)
# solve(qr(A, LAPACK = TRUE), b)

# A <- matrix(runif(12), 3)
# b <- 1:3
# qr.solve(A, b)
# solve(qr(A, LAPACK = TRUE), b)


# A <- matrix(runif(12), 3)
# A=rbind(A,A[3,])
# b <- c(1:3,3)
# qr.solve(A, b)
# solve(qr(A, LAPACK = TRUE), b)

# x_possib_all=list()
# y_possib_all=list()
# z_possib_all=list()
# for(i in 1:nrow(full_dt)){
#   r=full_dt[i,]
#   x_factors_raw=c(1,pracma::factors(r$px))
#   x_factors=c()
#   for(u in 1:length(x_factors_raw)){
#     x_factors=unique(c(x_factors,combn(x_factors_raw,u,prod)))
#   }
#   
#   y_factors_raw=c(1,pracma::factors(r$py))
#   y_factors=c()
#   for(u in 1:length(y_factors_raw)){
#     y_factors=unique(c(y_factors,combn(y_factors_raw,u,prod)))
#   }
#   
#   z_factors_raw=c(1,pracma::factors(r$pz))
#   z_factors=c()
#   for(u in 1:length(z_factors_raw)){
#     z_factors=unique(c(z_factors,combn(z_factors_raw,u,prod)))
#   }
#   
#   x_possibs=x_factors-r$vx
#   y_possibs=y_factors-r$vy
#   z_possibs=z_factors-r$vz
#   
#   x_possib_all[[i]]=x_possibs
#   y_possib_all[[i]]=y_possibs
#   z_possib_all[[i]]=z_possibs
# }






















###############################################################################
# #just doing intersections with points like a normal person
# xprod <- function(...) {
#   args <- list(...)
# 
#   # Check for valid arguments
# 
#   if (length(args) == 0) {
#     stop("No data supplied")
#   }
#   len <- unique(sapply(args, FUN=length))
#   if (length(len) > 1) {
#     stop("All vectors must be the same length")
#   }
#   if (len != length(args) + 1) {
#     stop("Must supply N-1 vectors of length N")
#   }
# 
#   # Compute generalized cross product by taking the determinant of sub-matricies
# 
#   m <- do.call(rbind, args)
#   sapply(seq(len),
#          FUN=function(i) {
#            det(m[,-i,drop=FALSE]) * (-1)^(i+1)
#          })
# }
xprod3=function(a){
  # a[2]*(a[3]+a[3+length(a)])-a[3]*(a[2]+a[2+length(a)])
  return(c(a[2]*a[3+3]-a[3]*a[2+3],-a[1]*a[3+3]+a[3]*a[1+3],a[1]*a[2+3]-a[2]*a[1+3]) )
}
unlist64=function(x){
  y=unlist(x)
  class(y)='integer64'
  return(y)
}

# xprod(c(1,2,3),c(4,5,6))
# xprod2(c(1,2,3),c(4,5,6))

sum_vals=c()
for(i in 1:nrow(full_dt)){
  dt_shifted=full_dt-as.vector(full_dt[i,])
  dt_shifted[,mag:=pmax(abs(px),abs(py),abs(pz))]
  dt_shifted=dt_shifted[order(mag)]
  sum_val=sum(dt_shifted$mag[1:5])
  sum_vals=c(sum_vals,sum_val)
}
library(bit64)
full_dt2=copy(full_dt)
full_dt2[,`:=`(px=as.integer64(px),py=as.integer64(py),pz=as.integer64(pz),vx=as.integer64(vx),vy=as.integer64(vy),vz=as.integer64(vz))]

#206
# dt_shifted=full_dt2-as.vector(full_dt2[206,])
dt_shifted=full_dt2-as.vector(full_dt2[1,])
dt_shifted[,mag:=pmax(abs(px),abs(py),abs(pz))]
dt_shifted=dt_shifted[order(mag)]

# unlist64(dt_shifted[2,1:6])
xp0=xprod( as.numeric(unlist64(dt_shifted[2,1:3])),as.numeric(unlist64(dt_shifted[2,1:3]+dt_shifted[2,4:6])) )
xp=xprod3(unlist64(dt_shifted[2,1:6]))
gcd_xp=pracma::gcd(as.numeric(xp[1]),as.numeric(xp[2]))
gcd_xp=pracma::gcd(gcd_xp,as.numeric(xp[3]))
xp2=as.integer64(xp/gcd_xp)

# xp=xprod2((dt_shifted[2,1:3]),
# (dt_shifted[2,1:3])+unlist(dt_shifted[2,4:6]))

# sum(unlist64(dt_shifted[4,4:6]) *xp)

# mid=unlist64(dt_shifted[4,.(px/vx,py/vy,pz/vz)])[1]
# unlist64(dt_shifted[4,1:3])+45067251619*unlist64(dt_shifted[4,4:6])


t_calc1=-sum(unlist64(dt_shifted[4,1:3])*xp2)/sum(unlist64(dt_shifted[4,4:6]) *xp2)
pos1=dt_shifted[4,1:3]+t_calc1*dt_shifted[4,4:6]

t_calc2=-sum(unlist64(dt_shifted[5,1:3])*xp2)/sum(unlist64(dt_shifted[5,4:6]) *xp2)
pos2=dt_shifted[5,1:3]+t_calc2*dt_shifted[5,4:6]

v_calc= (pos2-pos1)/(t_calc2-t_calc1)
pos_calc=pos1-t_calc1*v_calc

v_calc_true=v_calc+full_dt[1,4:6]
pos_calc_true=pos_calc+full_dt[1,1:3]
sum(unlist64(pos_calc_true))
#####################
# #trying to hand program the diophantine solution
# 
# full_dt[,.((px)/(vx+3))][order(V1)]$V1
# full_dt[,.((py-13)/(vy-1))][order(V1)]$V1
# full_dt[,.((pz-10)/(vz-2))][order(V1)]$V1
# 
# full_dt[,.(px%%abs(vx+3))]
# full_dt[,.(py%%abs(vy-1))]
# # full_dt[,.(vx+3,vy-1,vz-3)]
# 
# full_dt[,.(pz%%abs(vz-2), -10%%(vz-2) )]
# 
# # full_dt[,.(px%%abs(vx+3),px%/%abs(vx+3),vx+3 )]
# 
# # full_dt[,.(py%%abs(vy-1), -13%%(vy-1) )]
# 
# # full_dt[,.(py%%abs(vy-1))]
# 
# min_bound=min(full_dt[,.( pmin(px,vx),pmax(px,vx) )]$V1)
# max_bound=min(full_dt[,.( pmin(px,vx),pmax(px,vx) )]$V2)
# 
# 
# v_alpha=-3
# # full_dt[,.(pz,vz-v_alpha)]
# # pracma::gcd()
# # full_dt[.(1,)]
# 
# 
# 
# out_xs=c()
# for(v_alpha in -10:10){
#   print(v_alpha)
#   mat_base=matrix(data=0,nrow=nrow(full_dt),ncol=nrow(full_dt)+1)
#   mat_base[,1]=1
#   for(i in 1:nrow(full_dt)){
#     # print(i)
#     mat_base[i,i+1]=full_dt[i,]$vx-v_alpha
#   }
#   
#   # sub_dets=c()
#   # for(i in 1:ncol(mat_base)){
#   #   sub_dets=c(sub_dets,round(det(mat_base[,-i])))
#   # }
#   # 
#   # gcd_val=sub_dets[1]
#   # for(i in 2:ncol(mat_base)){
#   #   gcd_val=pracma::gcd(gcd_val,sub_dets[i])
#   # }
#   # print(gcd_val)
#   
#   # smith(mat_base)
#   hnf=numbers::hermiteNF(mat_base)
#   u=hnf$U
#   u2=numbers::hermiteNF(t(hnf$H))$U
#   
#   divs=diag(t(u2)%*%mat_base %*% hnf$U)
#   coeffs=t(u2)%*%full_dt$px
#   
#   u%*%c(coeffs/divs,-1)
#   
#   
#   
#   
#   if(all(fcoalesce(coeffs%%divs==0,F))){
#     #check if can find an all negative solution
#     base_vals=u%*%c(coeffs/divs,0)
#     base_vals=base_vals[2:length(base_vals)]
#     dir=u[,6]
#     dir=dir[2:length(dir)]
#     
#     max_vals=ifelse(dir>0,-base_vals/dir,NA)
#     min_vals=ifelse(dir<0,-base_vals/dir,NA)
#     
#     min_val_all=max(min_vals,na.rm=T)
#     max_val_all=min(max_vals,na.rm=T)
#     
#     if(min_val_all >=max_val_all){
#       next
#     }else{
#       out_xs=c(out_xs,v_alpha)
#       print(out_xs)
#     }
#     
#     # if(is.finite(max_val_all)){
#     #   init_val=max_val_all-1
#     #   ind=-1
#     # }else if(is.finite(min_val_all)){
#     #   init_val=min_val_all+1
#     #   ind=+1
#     # }
#     # while(init_val<max_val_all & init_vaL>min_val_all){
#     #   test_ts=u%*%c(coeffs/divs,init_val)
#     #   test_ts=test_ts[2:length(test_ts)]
#     #   diff(full_dt$py-test_ts*full_dt$vy)/diff(test_ts)
#     #   diff(full_dt$pz-test_ts*full_dt$vz)/diff(test_ts)
#     # }
#     # ifelse(base_vals>0,-ceiling(base_vals/dir),NA)
#     # p_alpha=full_dt[]
#     #check if y/z work
#   }
# }
# 
# 
# 
# # # diag(smith(mat_base))
# # 
# # # S=mat_base
# # # S <- numbers::hermiteNF(S)$H
# # # S <- t(numbers::hermiteNF(t(S))$H)
# # 
# # full_dt$px
# # 
# # # m2r::snf(mat_base)
# # 
# # # numbers::hermiteNF(mat_base)$U %*% full_dt$px
# # v_mat=numbers::hermiteNF(mat_base)$U
# # u_mat=numbers::hermiteNF(t(numbers::hermiteNF(mat_base)$H))$U
# # (t(u_mat)%*% full_dt$px) %% diag(smith(mat_base))
# # 
# # t(u_mat) %*% mat_base %*%v_mat
# # 
# # numbers::hermiteNF(t(numbers::hermiteNF(mat_base)$H))$H
# # 
# # 
# # 
# # # px=shift_px& vx=shift_vx
# # #or (px-shift_px)/(vx-shift_vx) all negative integers
# # 
# # library(quhomology)
# # # test_mat <- matrix(c(2,4,4, -6,6,12, 10,-4,-16), nrow=4, ncol=3, byrow=TRUE)
# # # smith(test_mat)