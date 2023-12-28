library(data.table)
options(scipen=999,digits=30)#hmm
# library(bit64)
# test=fread('/home/eqiu/code_projects/aoc_2023/data/input_5',header=F,sep='~')
conn <- file('/home/eqiu/code_projects/aoc_2023/data/input_6',open="r")
linn <-readLines(conn)
close(conn)



times=as.numeric(unlist(strsplit(linn[1],' ')))
times=times[!is.na(times)]

dists=as.numeric(unlist(strsplit(linn[2],' ')))
dists=dists[!is.na(dists)]

out=c()
for(i in 1:length(times)){
  v=0:times[i]
  out=c(out,sum(times[i]*v-v^2 > dists[i]))
}
prod(out)
######
time2=as.numeric(gsub('[^0-9]','',linn[1]))
dist2=as.numeric(gsub('[^0-9]','',linn[2]))

b=as.numeric(-time2)
a=1
c=as.numeric(dist2)


floor((-b+sqrt(b^2-4*a*c))/(2*a)) - ceiling(((-b-sqrt(b^2-4*a*c))/(2*a)))+1


# ceiling((-b+30125202.5)/2)