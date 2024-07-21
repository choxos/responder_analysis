#' @import shiny
#' @import shinydashboard
#' @import DT
#' @importFrom stats pnorm qnorm
NULL

#' Inverse Variance Meta-Analysis
#'
#' @param mean Vector of means
#' @param sd Vector of standard deviations
#' @param n Vector of sample sizes
#' @param se Vector of standard errors (optional)
#'
#' @return A vector with weighted mean and confidence interval
#' @export
iv_meta = function(mean, sd = NULL, n = NULL, se = NULL) {
        if (is.null(se)) {
                if (is.null(sd) || is.null(n)) {
                        stop("If se is not provided, both sd and n must be provided")
                }
                se = sd / sqrt(n)
        }
        
        weights = 1 / se^2
        weighted_mean = sum(mean * weights) / sum(weights)
        weighted_se = sqrt(1 / sum(weights))
        ci_lb = weighted_mean - qnorm(0.975) * weighted_se
        ci_ub = weighted_mean + qnorm(0.975) * weighted_se
        
        return(c(weighted_mean, ci_lb, ci_ub))
}

#' Risk Ratio Meta-Analysis
#'
#' @param ai Vector of event counts in group 1
#' @param bi Vector of non-event counts in group 1
#' @param ci Vector of event counts in group 2
#' @param di Vector of non-event counts in group 2
#' @param data Data frame containing ai, bi, ci, di columns
#'
#' @return A vector with log risk ratio (multiplied by 100) and confidence interval
#' @export
rr_meta = function(ai = ai, bi = bi, ci = ci, di = di, data) {
        ai = data$ai
        bi = data$bi
        ci = data$ci
        di = data$di
        
        n1i = ai + bi
        n2i = ci + di
        Ni = n1i + n2i
        
        R = sum(ai * (n2i / Ni))
        S = sum(ci * (n1i / Ni))
        
        beta.exp = R / S
        beta = log(beta.exp)
        se = sqrt(sum(((n1i / Ni) * (n2i / Ni) * (ai + ci) - (ai / Ni) * ci)) / (R * S))
        zval = beta / se
        pval = 2 * pnorm(abs(zval), lower.tail = FALSE)
        ci.lb = beta - qnorm(0.975, lower.tail = FALSE) * se
        ci.ub = beta + qnorm(0.975, lower.tail = FALSE) * se
        
        return(c(beta * 100, ci.lb * 100, ci.ub * 100))
}

#' Risk Difference Meta-Analysis
#'
#' @param ai Vector of event counts in group 1
#' @param bi Vector of non-event counts in group 1
#' @param ci Vector of event counts in group 2
#' @param di Vector of non-event counts in group 2
#' @param data Data frame containing ai, bi, ci, di columns
#'
#' @return A vector with risk difference (multiplied by 100) and confidence interval
#' @export
rd_meta = function(ai = ai, bi = bi, ci = ci, di = di, data) {
        ai = data$ai
        bi = data$bi
        ci = data$ci
        di = data$di
        
        n1i = ai + bi
        n2i = ci + di
        Ni = n1i + n2i
        
        beta = sum(ai * (n2i / Ni) - ci * (n1i / Ni)) / sum(n1i * (n2i / Ni))
        se = sqrt((beta * (sum(ci * (n1i / Ni)^2 - ai * (n2i / Ni)^2 + (n1i / Ni) * (n2i / Ni) * (n2i - n1i) / 2)) + sum(ai * (n2i - ci) / Ni + ci * (n1i - ai) / Ni) / 2) / sum(n1i * (n2i / Ni))^2)
        zval = beta / se
        pval = 2 * pnorm(abs(zval), lower.tail = FALSE)
        ci.lb = beta - qnorm(0.975) * se
        ci.ub = beta + qnorm(0.975) * se
        
        return(c(beta * 100, ci.lb * 100, ci.ub * 100))
}

#' Proportion Meta-Analysis
#'
#' @param n_c Vector of event counts
#' @param n_e Vector of sample sizes
#' @param data Data frame containing n_c and n_e columns
#'
#' @return A vector with transformed proportion and variance
#' @export
prop_meta = function(n_c = n_c, n_e = n_e, data) {
        xi = data$n_c
        n_e = data$n_e
        ni = xi + n_e
        
        yi = 0.5 * (asin(sqrt(xi / (ni + 1))) + asin(sqrt((xi + 1) / (ni + 1))))
        vi = 1 / (4 * ni + 2)
        
        return(c(yi, vi))
}

#' Launch Responder Analysis App
#'
#' This function launches the Responder Analysis Shiny application.
#'
#' @export
launch_responder_analysis = function() {
        app_dir = system.file("shiny-apps", "ResponderAnalysisApp", package = "ResponderAnalysisApp")
        if (app_dir == "") {
                stop("Could not find example directory. Try re-installing `ResponderAnalysisApp`.", call. = FALSE)
        }
        
        shiny::runApp(app_dir, display.mode = "normal")
}