#' @importFrom stats pnorm qnorm

iv_meta = function(mean, sd, n, se=NULL){
        if(is.null(se)){
                weighted_mean = sum(mean*(1/(sd/sqrt(n))^2))/sum(1/(sd/sqrt(n))^2)
                return(weighted_mean)
        } else {
                weighted_mean = sum(mean*(1/se^2))/sum(1/se^2)
                weighted_se = sqrt(1/sum(1/se^2))
                ci_lb = weighted_mean - qnorm(0.975) * weighted_se
                ci_ub = weighted_mean + qnorm(0.975) * weighted_se
                return(c(weighted_mean, ci_lb, ci_ub))
        }
}


rr_meta = function(ai=ai, bi=bi, ci=ci, di=di, data){
        ai = data$ai
        bi = data$bi
        ci = data$ci
        di = data$di
        
        n1i = ai+bi
        n2i = ci+di
        Ni = n1i + n2i
        
        R = sum(ai * (n2i/Ni))
        S = sum(ci * (n1i/Ni))
        
        beta.exp = R/S
        beta = log(beta.exp)
        se = sqrt(sum(((n1i/Ni)*(n2i/Ni)*(ai+ci) - (ai/Ni)*ci)) / (R*S))
        zval = beta / se
        pval = 2*pnorm(abs(zval), lower.tail=FALSE)
        ci.lb = beta - qnorm(0.975, lower.tail=FALSE) * se
        ci.ub = beta + qnorm(0.975, lower.tail=FALSE) * se
        
        return(c(beta*100, ci.lb*100, ci.ub*100))
        
        
}


rd_meta = function(ai=ai, bi=bi, ci=ci, di=di, data){
        ai = data$ai
        bi = data$bi
        ci = data$ci
        di = data$di
        
        n1i = ai+bi
        n2i = ci+di
        Ni = n1i + n2i
        
        beta = sum(ai*(n2i/Ni) - ci*(n1i/Ni)) / sum(n1i*(n2i/Ni))
        se = sqrt((beta * (sum(ci*(n1i/Ni)^2 - ai*(n2i/Ni)^2 + (n1i/Ni)*(n2i/Ni)*(n2i-n1i)/2)) + sum(ai*(n2i-ci)/Ni + ci*(n1i-ai)/Ni)/2) / sum(n1i*(n2i/Ni))^2) # equation in: Sato, Greenland, & Robins (1989)
        zval = beta / se
        pval = 2*pnorm(abs(zval), lower.tail=FALSE)
        ci.lb = beta - qnorm(0.975) * se
        ci.ub = beta + qnorm(0.975) * se
        
        return(c(beta*100, ci.lb*100, ci.ub*100))
}


prop_meta = function(n_c=n_c, n_e=n_e, data){
        xi = data$n_c
        n_e = data$n_e
        ni = xi+n_e
        
        yi = 0.5*(asin(sqrt(xi/(ni+1))) + asin(sqrt((xi+1)/(ni+1))))
        vi = 1/(4*ni + 2)
        
        return(c(yi, vi))
        
}