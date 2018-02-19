###############################################################################
### Calculates Cohen's d effect size for between subjects comparisons #########
### returns full APA style report for t-test incl. 95% confidence interval ####
###############################################################################
require(MBESS)

ds_between_ci <-
function(outcome, categ, report, varequal){

    a = tapply(outcome, categ, mean)
    a = a[!is.na(a)]
    b = tapply(outcome, categ, sd)
    b = b[!is.na(b)]
    c = as.vector((a[1] - a[2]) / b[2])
    e = t.test(outcome ~ categ, var.equal = varequal)
    f = round(as.vector(e$statistic), 2)
    df = round(as.vector(e$parameter), 2)
    pvalue = round(e$p.value, 4)
    table_categ = as.data.frame(table(categ))
    table_categ$Freq[table_categ$Freq == 0] = NA
    table_categ = na.omit(table_categ)
    n1 = table_categ[1,2]
    n2 = table_categ[2,2]
    g = sqrt(1/n1 + 1/n2)
    d = f*g
    h = a[1] - a[2]
    i = ((n1-1)*b[1]^2 + (n2-1)*b[2]^2)/(n1+n2-2)
    k = sqrt(i)
    j = as.vector(h/k)
    l = ci.smd(ncp = f, n.1 = n1, n.2 = n2, conf.level = .95)
  if(report == T){
    out = paste("t(", df, ")", " = ", f, ", p = ", pvalue, ", dbetween = ", round(d, 2), " [95% CI: ", round(l$Lower.Conf.Limit.smd, 2), " - ", round(l$Upper.Conf.Limit.smd, 2), "].", sep="")
  } else {
    out = list("Cohen's d between: Mdiff / SD" = round(c, 4),
               "Cohen's ds between: t-test based (Lakens, 2013)" = round(d, 4),
               "Cohen's ds between SD based (Lakens, 2013)" = round(j, 4),
               "95% CI lower boundary" = round(l$Lower.Conf.Limit.smd, 4),
               "95% CI upper boundary" = round(l$Upper.Conf.Limit.smd, 4))
  }
    print(out)
}

#CHANGE LOG:
#30 DEC 2017: fixed bug in line 20 which transformed 0 class labels to NAs.

#usage example:
# ds_between_ci(data$outcome
# , data$condition
# , report = T
# , varequal = F)


#load as:
# source('./ds_between_ci.R')