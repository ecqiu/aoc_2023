library(data.table)
options(scipen=999,digits=22)
# library(bit64)
test=fread('/home/eqiu/code_projects/aoc_2023/data/input_7',header=F)

c=test$V1[1]


hand_rank=function(c,j_is_joker=F){
  if(j_is_joker){
    hand_count=data.table(card=c('A','K','Q','T',as.character(9:2),'J'),count=0 )
  }else{
    hand_count=data.table(card=c('A','K','Q','J','T',as.character(9:2)),count=0 )
  }
  hand_count[,ind:=letters[1:.N]]
  hand=unlist(strsplit(c,split=''))
  
  for(h in hand){
    hand_count[card==h,count:=count+1]
  }
  
  
  hand_join=data.table(card=hand)
  hand_join[hand_count,join:=i.ind,on='card']
  
  if(j_is_joker){
    n_j=hand_count[card=='J',]$count
    hand_count=hand_count[card!='J']
    hand_count=hand_count[order(-count)]
    hand_count[1,count:=count+n_j]
  }
  
  if(max(hand_count$count)==5){
    out=1
  }else if(max(hand_count$count)==4){
    out=2
  }else if(max(hand_count$count)==3 && min(hand_count[count!=0]$count)==2){
    out=3
  }else if(max(hand_count$count)==3 ){
    out=4
  }else if(max(hand_count$count)==2 && hand_count[,.N,by=count][count==2,]$N==2 ){
    out=5
  }else if(max(hand_count$count)==2  ){
    out=6
  }else{
    out=7
  }
  
  return(paste0(c(out,hand_join$join),collapse='' ))
}


#part 1
test[,hand_rank:=unlist(lapply(V1,function(x) hand_rank(x,j_is_joker=F)))]
test[,r:=rank(hand_rank)]
test[,r2:=.N-r+1]
test[,sum(r2*as.numeric(V2))]

#part 2
test[,hand_rank:=unlist(lapply(V1,function(x) hand_rank(x,j_is_joker=T)))]
test[,r:=rank(hand_rank)]
test[,r2:=.N-r+1]
test[,sum(r2*as.numeric(V2))]


#five
#four
#full house
#three
#two pair
#one pair
#high card