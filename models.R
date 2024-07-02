# Libs

library(survival)
library(readr)
library(survminer)
library(data.table)

# Data

dt <- read_delim("HOD_NHL.csv", delim = ";",
                 escape_double = FALSE, trim_ws = TRUE) |> as.data.table()


dt[, Karnofsky_cat := ifelse(Karnofsky >= 80, ">= 80", "< 80")]
dt[, Karnofsky := NULL]
dt[, Graft := NULL]


# Weibull

model_weibull <- survreg(Surv(Time, D_R) ~ ., data = dt[, Karnofsky := NULL], dist = "weibull")
model_weibull1 <- survreg(Surv(Time, D_R) ~ ., data = dt, dist = "weibull")
# Exponencial

model_exp <- survreg(Surv(Time, D_R) ~ ., data = dt, dist = "exponential")

# Taxa de falha acumulada

taxa_weibull <- basehaz(model_weibull)

