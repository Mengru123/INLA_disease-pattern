checkexist<-function(diseasex, indicatorx,schemax) {
    try(dbGetQuery(con, paste0("select * from ",schemax,".",diseasex,indicatorx,"indicator","_0", " limit 1")))
}

queryexist<-function(cxx){
    dbGetQuery(con, paste0(
        "select '", cxx$diseases,"'::VARCHAR(30) as disease, o.year, o.age, o.sex, c.dst as clsc,",  
        ifelse(cxx$indicator=="prevalence", 
               "sum(i.count_of_case)::integer as num,sum(i.count_of_people)::integer as denom", 
               "sum(i.count_of_events)::integer as num,sum(i.person_time)::integer as denom"),
        " from ",paste0(cxx$schema,".",cxx$diseases,cxx$indicator,"indicator","_0"), " as i 
        join obs.obs as o on i.fk_obs=o.id 
        join dim.g_ct2clsc as c on c.src=o.geo 
        
        group by c.dst, o.year, o.age, o.sex 
        order by c.dst"))}


