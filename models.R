# Libs

library(survival)
library(readr)
library(survminer)
library(data.table)
library(lmtest)

# Data

dt <- read_delim("HOD_NHL.csv", delim = ";",
                 escape_double = FALSE, trim_ws = TRUE) |> as.data.table()


dt[, Karnofsky_cat := ifelse(Karnofsky < 80,"< 80",">= 80")]
dt[, Karnofsky := NULL]


# Weibull

model_weibull <- survreg(Surv(Time, D_R) ~ ., data = dt,
                         dist = "weibull")
# Exponencial

model_exp <- survreg(Surv(Time, D_R) ~ ., data = dt, dist = "exponential")

# Riscos proporcionais no modelo weibull

# Risco de transplante alogenico sobre transplante autologo
risco_graft <- exp(-model_weibull[["coefficients"]][["Graft"]])^(1/model_weibull[["scale"]])

# Risco de escore de Karnofsky bom sobre escore ruim
risco_escore <- exp(-model_weibull[["coefficients"]][["Karnofsky_cat>= 80"]])^(1/model_weibull[["scale"]])

# Modelo Log Logistica

model_log <-survreg(Surv(Time,D_R) ~., data = dt,
                    dist = "loglogistic")

# Razão de chances no modelo log logistico

# Razão de chance de transplante aloginico sobre transplante autólogo
odds_graft <- exp(-model_log[["coefficients"]][["Graft"]]/model_log[["scale"]])

odds_escore <- exp(-model_log[["coefficients"]][["Karnofsky_cat>= 80"]]/model_log[["scale"]])





