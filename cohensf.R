###############################################################################
### Calculates Cohen's f effect size from F-statistic and degrees of freedom ##
###############################################################################


cohensf <-
function(F, dfnum, dfden) {
    paretasq = F * 1/(F*dfnum + dfden)
    f = sqrt(paretasq / (1-paretasq))
    out = list("Partial eta squared" = paretasq,
               "Cohen's f" = f)
    print(out)
}

#usage example:
# cohensf(4.12, 1, 100)