comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

comb(4,0)+comb(4,2)+comb(4,4)
comb(4,0)+comb(3,1)+comb(2,2)

comb(12,0)+comb(11,1)+comb(10,2)+comb(9,3)+comb(8,4)+comb(7,5)+comb(6,6)

# 5,0-9,{1,8},3,2

#5,2,{1,8},{7,8},2

# 52132
# 52832
# 52172


#5 XXX 2

#There are not exactly two 2s
#There are not exactly two even numbers

# A (is not 5)== 2
# B==1
# C is not 0,1,3,4,5,6,8,9 {2,7}
# D is 9
# E is not 0,2,3,6,9 {1,4,5,7,8}

# 21291
# 21791 (this is actually the answer

#00291
#00294
#00295


q_7_recurse=function(n_el,N=14){
  if(n_el==0){
    return(1)
  }
  if(n_el==1){
    return(N)
  }
  
  if(n_el>1){
    if(N>=(2*n_el-1)){
      s=0
      for(i in (2*n_el-3):(N-2)){
        s=s+q_7_recurse(n_el-1,i)
      }
      return(s)
    }else{
      return(0)
    }
    
    
  }
  
}
q_7_recurse(0,4)+
q_7_recurse(1,4)+
q_7_recurse(2,4)+
q_7_recurse(3,4)

q_7_recurse(0,5)+
q_7_recurse(1,5)+
q_7_recurse(2,5)+
q_7_recurse(3,5)

8,12

q_7_recurse(2,3)



q_7_recurse(0,14)+
q_7_recurse(1,14)+
q_7_recurse(2,14)+
q_7_recurse(3,14)+
q_7_recurse(4,14)+
q_7_recurse(5,14)+
q_7_recurse(6,14)+
q_7_recurse(7,14)
# q_7_recurse(8,14)

for(i in 1:20){
  k=as.character(15*i)
  print(k)
  s=sum(as.numeric(unlist(strsplit(k,''))))
  print(s)
  
}

c=0
for(i in 100:1000){
  s=sum(as.numeric(strsplit(as.character(i),'')[[1]]))
  if(s%%2==0){print(i);c=c+1}
}

########
#12
out_list=list()
for(i in 2:999){
  dt=data.table(num=as.numeric(gmp::factorize(i)))
  dt_summ=dt[,.(count=.N),by=num]
  out_list[[i]]=dt_summ[,val:=i]
}
dt_packed=rbindlist(out_list)

fac_out_list=list()
for(i in 2:999){
  fac_out_list[[i]]=dt_packed[val<=i,.(count=sum(count)),by=num]
  fac_out_list[[i]][,fac_val:=i]
}
fac_packed=rbindlist(fac_out_list)

factors_500=fac_packed[fac_val<=500,.(count=sum(count)),by=num]

for(i in 2:999){
  print(i)
  # val_first=fac_packed
  skele=sort(unique(c(factors_500$num,fac_packed[fac_val==i]$num)))
  dt_base=data.table(num=skele)
  dt_base[factors_500,plus:=i.count,on='num']
  dt_base[fac_packed[fac_val==i],minus:=i.count,on='num']
  dt_base[,val:=plus-fcoalesce(as.numeric(minus),0)]
  vals=dt_base[val!=0]$val
  
  if(all(vals%%2==0)){
    print('success')
    break
  }
}
u=500

##
#13
812
765
623
digitsum <- function(x) sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)






# q15
for(i in 400:499){
  print(i)
  digits=as.numeric(unlist(strsplit(as.character(i),'')))
  if(sum(digits)!=0&&prod(digits)!=0&&sum(digits)%%3==0 && prod(digits)%in% (1:20)^3){
    break
  }else{
    print(sum(digits))
    print(prod(digits))
  }
}

#q16
k=1
out=c()
for(k in 0:999){
  n =2:floor(-(0.5+k)+sqrt((0.5+k)^2+2000))
  out=unique(c(out,n^2/2+(1/2+k)*n))
}

# sort(out)
setdiff(100:999,out)


###
#q17

choose(6,4)+
choose(6,3)+
choose(6,2)+
choose(6,1)


choose(3,1)+
choose(3,2)


6+6*5+6*5+6*choose(5,2)+choose(6,2)*4+6*choose(5,3)+choose(6,5)

5
4,1
3,2
3,1,1
2,2,1
2,1,1,1
1,1,1,1,1




##q18
out=c()
for(i in 2:6){
  p=0
  init=1
  while(p<1000){
    p=prod(init:(init+i-1))
    out=c(out,p)
    init=init+1
  }
}

prod(1:6)


#q22
n=8
a=(n-2)*180/n

n1=1
n2=2

s1=(180*n1-a*n1)/n1
s2=(180*n2-a*n2)/n2

n=172
n/2*(n-2)


#####################3
x=10
2*(x-1)^2+2*(x-1)*(2*x-2-x/2)



for(i in 100:236){
  print(i)
  x=paste(rev(as.integer(intToBits(i))), collapse="") 
  x=as.character(as.integer(x))
  if(substr(x,nchar(x),nchar(x))==1 &&max(data.table(x=cumsum(as.numeric(strsplit(x,'')[[1]])))[,.N,by=x]$N)<3){
    
    print(x)
    break
  }
}