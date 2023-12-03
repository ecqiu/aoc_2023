library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2023/data/input',header=F)
# test[,sum(as.numeric(gsub('[^0-9]','',V1)))]
test[,parsed:=trimws(gsub('[^0-9]','',V1))]
test[,parsed2:=paste0(substr(parsed,1,1),substr(parsed,nchar(parsed),nchar(parsed)))]
test[,sum(as.numeric(parsed2))]

test[,V2:=copy(V1)]
test[,V2:=gsub('one','one1one',V2)]
test[,V2:=gsub('two','two2two',V2)]
test[,V2:=gsub('three','three3three',V2)]
test[,V2:=gsub('four','four4four',V2)]
test[,V2:=gsub('five','five5five',V2)]
test[,V2:=gsub('six','six6six',V2)]
test[,V2:=gsub('seven','seven7seven',V2)]
test[,V2:=gsub('eight','eight8eight',V2)]
test[,V2:=gsub('nine','nine9nine',V2)]

test[,parsedx:=trimws(gsub('[^0-9]','',V2))]
test[,parsedx2:=paste0(substr(parsedx,1,1),substr(parsedx,nchar(parsedx),nchar(parsedx)))]
test[,sum(as.numeric(parsedx2))]




