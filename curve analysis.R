# Libs

library(survival)
library(readr)
library(survminer)
library(data.table)

### Data ###

dt <- read_delim("HOD_NHL.csv", delim = ";",
                 escape_double = FALSE, trim_ws = TRUE) |> as.data.table()

### Data pre processing ###

dt[, Karnofsky_cat := ifelse(Karnofsky >= 80, ">= 80", "< 80")]

### Kaplan Meyer Curve ###

# Types of transplant
fit_graft <- survfit(Surv(time = Time, event = D_R) ~ Graft,
                   data = dt)

#Category of escore of Karnofsky
fit_escore <- survfit(Surv(time = Time, event = D_R) ~ Karnofsky_cat,
                     data = dt)

### Graphic of curve ###

ggsurvplot(fit_graft, data = dt,
           pval = TRUE, conf.int = TRUE, conf.int.style = "step",
           ylab = "Sobrevida", xlab = "Tempo em dias",
           legend.title = "Tipo de Linfoma")
           #surv.median.line = "hv")

ggsurvplot(fit_escore, data = dt,
           pval = TRUE, conf.int = TRUE, conf.int.style = "step",
           ylab = "Sobrevida", xlab = "Tempo em dias",
           legend.title = "Tipo de Linfoma")
           #surv.median.line = "hv")

### Log rank test ###

logrank_graft <- survdiff(Surv(time = Time, event = D_R) ~ Graft,
                    data = dt)

logrank_escore <- survdiff(Surv(time = Time, event = D_R) ~ Graft,
                    data = dt)

### Wilcoxon test ###

wilconxon_graft <- survdiff(Surv(time = Time, event = D_R) ~ Graft, rho = 1,
                      data = dt)

wilconxon_escore <- survdiff(Surv(time = Time, event = D_R) ~ Graft, rho = 1,
                      data = dt)
