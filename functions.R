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

plot_disease_effect = function (df,var_x, var_y, labels_dis, n_dis, title){
    ggplot(df, aes_string(x = var_x, y = var_y)) +
        geom_point(size = 3, shape = 15) +
        geom_abline(intercept = 1, slope = 0, linetype = "dotted", size = 1) +
        geom_errorbar(aes(ymin = li, ymax = ui), size = 1, width = 0.3) +
        labs(x = "disease", y = "Exp_disease_effect", title = title) +
        scale_x_discrete(name = "Diseases", limits = c(1:n_dis), labels = labels_dis) +
        theme(axis.text = element_text(size =10)) 
}

plot_clsc_effect = function (df,var_x, var_y, labels_clsc, n_clsc,title) {
    ggplot(df, aes_string(var_x, var_y)) +
        geom_point(size = 3, shape = 15) +
        geom_abline(intercept = 1, slope = 0, linetype = "dotted", size = 1) +
        geom_errorbar(aes(ymin = li, ymax = ui), size = 1, width = 0.3) +
        labs(x = "CLSC", y = "Exp_CLSC_effect", title = title) +
        scale_x_discrete(name = "CLSC", limits = c(1:n_clsc), labels = labels_clsc) +
        theme(axis.text = element_text(size =10), 
              axis.text.x = element_text(angle = 45, hjust = 1)) 
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