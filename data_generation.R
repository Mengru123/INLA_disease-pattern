library(RPostgreSQL)
#library(plyr)
library(dplyr)
source("data_generation_function.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "pophr",host='132.216.183.58',user =
                     "mengru",pass="yuan836200")

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

#write.csv(qd, file = "Data/disease_clsc.csv")
rm(qd); rm(mld) ; rm(checkgd)

# generate age and sex data for each clsc
df = dbGetQuery(con, paste0(
    "select *  from obs.obs as ob
    join dim.g_ct2clsc g on ob.geo = g.src"))
df$male = ifelse(df$sex == "TRUE", 1, 0)
df$eld = ifelse(df$age >64, 1, 0)
df = df%>%
    group_by(dst) %>%
    summarise(M_num = sum(male), eld_num = sum(eld), N_pop = n())

#write.csv(df, file = "Data/sex_age_clsc.csv")
rm(df)
