library(RPostgreSQL)
library(plyr)
source("data_generation_function.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "xxx",host='xxx',user =
                     "xxx",pass="xxx")

diseasesd<-c('asthma','chf','hypertension','ihd','copd','diabetes','ami','stroke')
indicatord<-c('incidence','prevalence','mortality')
schemad<-c('cubes')
checkgd<-expand.grid("diseases"=diseasesd,"indicator"=indicatord,"schema"=schemad)

mld<-mapply(checkexist,checkgd$diseases,checkgd$indicator,checkgd$schema)
mld<-checkgd[mld!="NULL",]

qd<-lapply(seq(1:nrow(mld)),function(x) queryexist(mld[x,]))

names(qd)<-  unique(paste0(cexd$diseases,"_",mld$indicator))
qd<-lapply(names(qd),function(x) cbind(qd[[x]],disease_indicator=rep(x,each=nrow(qd[[x]]))))

qd<-lapply(qd,function(x) x<-cbind(x,indicator= gsub(".*_","",x$disease_indicator)))
qd<-rbind.fill(qd)

write.csv(qd, file = "Data/disease_clsc.csv")
