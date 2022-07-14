
# Cargar paquetes y datos -------------------------------------------------
library(compareGroups)
library(survival)

data(predimed)
setDT(predimed)



# Los datos de supervivencia ----------------------------------------------
ini    <- c(0, 1, 3, 2, 0.5, 6.5, 1.5, 2.5, 7, 0)
fin    <- c(3.5, 8, 7, 6.5, 3.5, 8, 6.5, 5, 10, 10)
com    <- c(1, 0, 0, 1, 1, 0, 1, 1, 0, 0) # dato completo (=1) o censurado (=0)
sup    <- fin - ini                       # fecha final del seguimiento
datos0 <- data.table(inicio=ini,final=fin,supervivencia=sup,censura=com, id = 1:10)
datos0[, `:=`(inicio2 = 0, duracion = final - inicio)][]
opar   <- par()
par(mar = c(5, 5.2, 2, 1))
plot(
  datos0$final,
  datos0$id,
  col  = ifelse(datos0$censura == 0, 2, 1),
  xlim = c(0, 10),
  ylim = c(1, 10),
  axes = FALSE,
  pch  = 19,
  xlab = "Tiempo en meses",
  ylab = "Sujeto",
  main = "Entrada y salida de cada sujeto estudiado"
)
box(bty = "l")
axis(1)
axis(2, at = seq(1, 10, 1), labels = 1:10)
segments(
  x0 = datos0$inicio,
  y0 = datos0$id,
  x1 = datos0$final,
  y1 = datos0$id
)
abline(v = 10, col = 4, lty = 2)
abline(v = 0, col = 4, lty = 2)

plot(
  datos0$duracion,
  datos0$id,
  col  = ifelse(datos0$censura == 0, 2, 1),
  xlim = c(0, 10),
  ylim = c(1, 10),
  axes = FALSE,
  pch  = 19,
  xlab = "Tiempo en meses",
  ylab = "Sujeto",
  main = "Duración en el estudio tomando tiempo 0 como entrada"
)
box(bty = "l")
axis(1)
axis(2, at = seq(1, 10, 1), labels = 1:10)
segments(
  x0 = datos0$inicio2,
  y0 = datos0$id,
  x1 = datos0$duracion,
  y1 = datos0$id
)
abline(v = 10, col = 4, lty = 2)
abline(v = 0, col = 4, lty = 2)
par(opar)

head(Surv(datos0$duracion, datos0$censura == 0))


# Estimación de curva de supervivencia ------------------------------------
km_fit <- survfit(Surv(toevent, event == "Yes") ~ 1, predimed)
summary(km_fit, times = 1:4)
plot(
  x        = km_fit,
  ylim     = c(0.9, 1),
  xlab     = "Años",
  ylab     = "Supervivencia",
  axes     = FALSE,
  conf.int = TRUE
)
box(bty = "l")
axis(side = 1)
axis(side = 2)


km_fit_sex <- survfit(Surv(toevent, event == "Yes") ~ sex, predimed)
# Prueba log-rank (asume proporcionalidad en riesgos), pues otorga el mismo pero a toda la curva
survdiff(Surv(toevent, event == "Yes") ~ sex, predimed)

# Hay más pruebas que optan por otras ponderaciones (más importancia a eventos tempranos):
# 1) Gehan-Breslow, 2) Peto-Peto, 3) Tarone-Ware...
# Hay una implementación muy buiena en el paquete coin
coin::logrank_test(Surv(toevent, event == "Yes") ~ sex, predimed, type = "logrank")
coin::logrank_test(Surv(toevent, event == "Yes") ~ sex, predimed, type = "Gehan-Breslow")
coin::logrank_test(Surv(toevent, event == "Yes") ~ sex, predimed, type = "Peto-Peto")
coin::logrank_test(Surv(toevent, event == "Yes") ~ sex, predimed, type = "Tarone-Ware")


par(mar = c(5, 5.2, 2, 1))
plot(
  x        = km_fit_sex,
  ylim     = c(0.85, 1),
  xlab     = "Años",
  ylab     = "Supervivencia",
  axes     = FALSE,
  col      = c("orange", "dodgerblue"),
  conf.int = TRUE
)
box(bty = "l")
axis(side = 1)
axis(side = 2)
legend(
  x      = "topright",
  legend = c("Male", "Female"),
  lty    = 1,
  col    = c("orange", "dodgerblue"),
  bty    = "n"
)
par(opar)

# Modelo de Cox -----------------------------------------------------------
cox_reg1 <- coxph(Surv(toevent, event == "Yes") ~ sex, predimed)
summary(cox_reg1)
cbind(exp(coef(cox_reg1)), exp(confint(cox_reg1)))
tbl_regression(cox_reg1)

# Probando más covariables
cox_reg2 <- coxph(
  formula = Surv(toevent, event == "Yes") ~ group + sex + I(age - mean(age)) + smoke + I(bmi - mean(bmi)) + I(wth - mean(wth)) + famhist,
  data = predimed
)
summary(cox_reg2)
cbind(exp(coef(cox_reg2)), exp(confint(cox_reg2)))
tbl_regression(cox_reg2, exp = TRUE, add_estimate_to_reference_rows = TRUE)

# Hay que comprobar la proporcionalidad en los riesgos!!
cox.zph(cox_reg2)
plot(cox.zph(cox_reg2))


# ¿Qué hacer si no se cumple proporcionalidad?
# Variables categóricas: considerar estratos
# Supongamos que el sexo no cumple proporcionalidad

cox_reg3 <- coxph(
  formula = Surv(toevent, event == "Yes") ~ group + strata(sex) + I(age - mean(age)) + smoke + I(bmi - mean(bmi)) + I(wth - mean(wth)) + famhist,
  data = predimed
)
cox.zph(cox_reg3) # La estimación es idéntica
summary(cox_reg3)
cbind(exp(coef(cox_reg3)), exp(confint(cox_reg3)))
tbl_regression(cox_reg3, exp = TRUE, add_estimate_to_reference_rows = TRUE)
