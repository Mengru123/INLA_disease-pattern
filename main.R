#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(dplyr)
library(ggplot2)
library(gridExtra)
source("functions.R")

###### raw data #####
# clsc.dis = read.table(file = "Data/disease_clsc.csv", head=TRUE, sep = ',') # "disease_clsc.csv" is moved to 3.Data folder,18.08.17
# clsc.dis = clsc.dis[clsc.dis$indicator == "incidence" & clsc.dis$year == 2014,-1]
# write.csv(clsc.dis, file = "Data/disease_clsc_incidence_2014.csv")


#### Input data ####
clsc.dis = read.table(file = "Data/disease_clsc_incidence_2014.csv", header = TRUE, sep = ',')
clsc.dis = clsc.dis[, -1]

#### Model.0 ####
clsc.dis = clsc.dis %>%
    group_by(disease, clsc) %>%
    summarise(cases = sum(num), pop = sum(denom))

clsc.dis$dis_fct = as.numeric(clsc.dis$disease)
clsc.dis$clsc_fct = as.numeric(as.factor(clsc.dis$clsc))

# built the model with random effect on disease and clsc region 
formula.0 = cases ~ 1 +
    f(dis_fct, model="iid") +
    f(clsc_fct, model ="iid")
inla.model.0 = inla(formula.0, family="poisson", data=clsc.dis, offset=log(pop),
                    control.compute=list(cpo=TRUE,dic=TRUE),
                    control.predictor=list(compute=TRUE))

# plot the CI for posterior distribution of the random effects of each disease
dis.0 = tbl_for_plot(inla.model.0, 8, 1)
p0.1 = plot_disease_effect(df = dis.0, "varb", "value", labels_dis = unique(clsc.dis$disease), 
                           n_dis = 8, title = "Disease effect, Model.0")

# plot the CI for posterior distribution of the random effects of each clsc
clsc.0 = tbl_for_plot(inla.model.0, 57, 2)
clsc.0$clsc = unique(clsc.dis$clsc) ; clsc.0 = clsc.0[order(clsc.0$value), ]
clsc.0$y = c(1:57)
p0.2 = plot_clsc_effect(df = clsc.0,  "y", "value", labels_clsc = as.character(clsc.0$clsc),
                        n_clsc = 57, title = "Spatial effect, Model.0")

#### Model.1 ####
covar.df = read.csv(file = "Data/sex_age_clsc.csv", header = TRUE, sep = ',')[, -1]
clsc.dis.1 = left_join(clsc.dis, covar.df, by = c("clsc" = "dst"))
clsc.dis.1$M_prop = clsc.dis.1$M_num/clsc.dis.1$N_pop*100  #N_pop is the number of population, pop is the number of person-year at risk
clsc.dis.1$eld_prop = clsc.dis.1$eld_num/clsc.dis.1$N_pop*100
rm(covar.df)

# built the model with random effect on disease and clsc region 
formula.1 = cases ~ 1 + M_prop + eld_prop +
    f(dis_fct, model="iid") +
    f(clsc_fct, model ="iid")
inla.model.1 = inla(formula.1, family="poisson", data=clsc.dis.1, offset=log(pop),
                    control.compute=list(cpo=TRUE,dic=TRUE),
                    control.predictor=list(compute=TRUE))

# plot the CI for posterior distribution of the random effects of each disease
dis.1 = tbl_for_plot(inla.model.1, 8, 1)
p1.1 = plot_disease_effect(df = dis.1, "varb", "value", labels_dis = unique(clsc.dis$disease), 
                         n_dis = 8, title = "Disease effect, Model.1 with covariates")
grid.arrange(p0.1, p1.1)

# plot the CI for posterior distribution of the random effects of each clsc
clsc.1 = tbl_for_plot(inla.model.1, 57, 2)
clsc.1$clsc = unique(clsc.dis$clsc) ; clsc.1 = clsc.1[order(clsc.1$value), ]
clsc.1$y = c(1:57)
p1.2 = plot_clsc_effect(df = clsc.1,  "y", "value", labels_clsc = as.character(clsc.1$clsc),
                        n_clsc = 57,title = "Spatial effect, Model.1 with covariates")

grid.arrange(p0.2, p1.2)

