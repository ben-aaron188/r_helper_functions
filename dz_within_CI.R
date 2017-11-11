###############################################################################
### Calculates Cohen's d effect size for within subjects comparisons #########
### returns full APA style report for t-test incl. 95% confidence interval ####
###############################################################################

dz_within_ci <-
function(var1, var2, report){
    require("MBESS")
    a = mean(var1 - var2)
    b = ((sd(var2))^2 + (sd(var1))^2)-
        2*(cor(var1, var2, use = "pairwise.complete.obs"))*
        sd(var1)*sd(var2)
    c = a / (sd(var2))
    d = a / sqrt(b)
    e = t.test(var1, var2, paired=T)
    f = round(as.vector(e$statistic), 2)
    df = round(as.vector(e$parameter), 2)
    #pvalue = round(e$p.value, 4)
    pvalue = e$p.value
    t = t.test(var1, var2, paired = T)$statistic
    l = ci.sm(ncp = t, N = length(var1), conf.level = .95)
    if(report == T){
      out = paste("t(", df, ")", " = ", f, ", p = ", pvalue, ", dwithin = ", round(d, 2), " [95% CI: ", round(l$Lower.Conf.Limit.Standardized.Mean, 2), " - ", round(l$Upper.Conf.Limit.Standardized.Mean, 2), "].", sep="")
    } else {
      out = list("Cohen's d within uncorrected" = round(c, 4),
                 "Cohen's dz within (Lakens, 2013)" = round(d, 4))
    }
    print(out)
}

#usage example:
# dz_within_ci(data$measurement1
#              , data$measurement2
#              , report = T)