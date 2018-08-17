#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(dplyr)
library(ggplot2)
source("functions.R")

# clsc.dis = read.table(file = "Data/disease_clsc.csv", head=TRUE, sep = ',')
# clsc.dis = clsc.dis[clsc.dis$indicator == "incidence" & clsc.dis$year == 2014,-1]
# write.csv(clsc.dis, file = "Data/disease_clsc_incidence_2014.csv")

clsc.dis = read.table(file = "Data/disease_clsc_incidence_2014.csv", head=TRUE, sep = ',')
clsc.dis = clsc.dis[, -1]
clsc.dis = clsc.dis %>%
    group_by(disease, clsc) %>%
    summarise(cases = sum(num), pop = sum(denom))

str(clsc.dis)
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
data = tbl_for_plot(inla.model.0, 8, 1)
ggplot(data, aes(varb, value)) +
    geom_point(size = 3, shape = 15) +
    geom_abline(intercept = 1, slope = 0, linetype = "dotted", size = 1) +
    geom_errorbar(aes(ymin = li, ymax = ui), size = 1, width = 0.3) +
    labs(x = "disease", y = "Exp_disease_effect", title = "Disease effect") +
    scale_x_discrete(name = "Diseases", limits = c(1:8), labels = unique(clsc.dis$disease)) +
    theme(axis.text = element_text(size =10)) 

# plot the CI for posterior distribution of the random effects of each clsc
data = tbl_for_plot(inla.model.0, 57, 2)
data$clsc = unique(clsc.dis$clsc)
data = data[order(data$value), ]
data$y = c(1:57)

ggplot(data, aes(y, value)) +
    geom_point(size = 3, shape = 15) +
    geom_abline(intercept = 1, slope = 0, linetype = "dotted", size = 1) +
    geom_errorbar(aes(ymin = li, ymax = ui), size = 1, width = 0.3) +
    labs(x = "CLSC", y = "Exp_CLSC_effect", title = "Region effect") +
    scale_x_discrete(name = "CLSC", limits = c(1:57), labels = as.character(data$clsc)) +
    theme(axis.text = element_text(size =10), 
          axis.text.x = element_text(angle = 45, hjust = 1)) 