tbl_for_plot = function(inla_model, n, n_var){
    df = matrix(NA, 1000, n)
    for(i in 1:n){
        df [,i] <- inla.rmarginal(1000,marg = inla_model$marginals.random[[n_var]][[i]])}
    df.quartiles <- exp(t(apply(df, MARGIN=2,
                                function(x) quantile( x, probs= c(0.025,0.5,0.975)))))
    
    data.test <- data.frame(varb = c(1:n),
                            value = df.quartiles[,2],
                            ui=df.quartiles[,3],
                            li=df.quartiles[,1])
    return(data.test)
}


# original code for creating the data to plot
# disease.0 <- matrix(NA,1000,8)
# for(i in 1:8){
#     disease.0[,i] <- inla.rmarginal(1000,
#                                     marg = inla.model.0$marginals.random$dis_fct[[i]])}
# disease.0.quartiles <- exp(t(apply(disease.0, MARGIN=2,
#                                    function(x) quantile( x, probs= c(0.025,0.5,0.975)))))
# 
# data <- data.frame(disease = c(1:8),
#                    value = disease.0.quartiles[,2],
#                    ui=disease.0.quartiles[,3],
#                    li=disease.0.quartiles[,1])