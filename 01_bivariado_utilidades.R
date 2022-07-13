
# Cargar paquetes y datos -----------------------------------------------------------
library(data.table)
library(rcompanion)
library(FSA)
library(car)
library(compareGroups)
library(gtsummary)
library(simstudy)

signed_rank <- function(x) sign(x) * rank(abs(x))

# Carga de datos
# En este caso no es aplicable, pero siempre resulta recomendable
# importar los datos siempre en modo carácter.

# Generamos unos datos para trabajar con medidas repetidas (tres drogas con)
set.seed(2022)
res_def <- defDataAdd(
  varname  = "resultado",
  dist     = "normal",
  formula  = "3 + 1.5 * periodo + 2 * grupo + 1 * periodo * grupo",
  variance = 3
)
tmp <- genData(50)
tmp <- trtAssign(tmp, nTrt = 2, balanced = TRUE, grpName = "grupo")
tmp <- addPeriods(tmp, nPeriods = 4, idvars = "id", perName = "periodo")
tmp <- addColumns(dtDefs = res_def, tmp)
tmp[, grupo := factor(grupo, labels = c("Droga A", "Droga B"))]
tmp[, periodo := factor(periodo, labels = paste0("periodo_", 1:4))]
bdd_rm_wide <- dcast(tmp, id + grupo ~ periodo, value.var = "resultado")
str(bdd_rm_wide)
bdd_rm_long <- melt(
  data          = bdd_rm_wide,
  id.vars       = c("id", "grupo"),
  measure.vars  = paste0("periodo_", 1:4),
  variable.name = "periodo",
  value.name    = "resultado"
)
str(bdd_rm_long)

# visualizar los datos
boxplot(
  formula = resultado ~ periodo + grupo,
  data    = bdd_rm_long,
  col     = c("orange", "dodgerblue"),
  xlab    = "Medición",
  ylab    = "Longitud herida (mm.)",
  axes    = FALSE
)
box(bty = "l")
axis(
  side = 1,
  lwd.ticks = 0,
  at = seq(1.5, length.out = 4, by = 2),
  labels = c("Inicio", "1 mes", "2 meses", "3 meses")
)
axis(side = 2)
legend(
  x      = "topleft",
  legend = c("Droga A", "Droga B"),
  fill   = c("orange", "dodgerblue"),
  bty    = "n"
)


# Datos de PREDIMED
data(predimed)
setDT(predimed)
str(predimed)
summary(predimed)

layout.matrix <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 2, ncol = 3)
layout(
  mat     = layout.matrix,
  heights = c(1, 2),
  widths  = c(3, 3, 3)
)
hist(
  predimed$bmi,
  xlab = "Exposición",
  ylab = "Frecuencia",
  main = "Todos"
)
hist(
  predimed$bmi[predimed$group == "Control"],
  xlab = "Exposición",
  ylab = "Frecuencia",
  main = "Control"
)
hist(
  predimed$bmi[predimed$group == "MedDiet + Nuts"],
  xlab = "Exposición",
  ylab = "Frecuencia",
  main = "Frutos secos"
)
hist(
  predimed$bmi[predimed$group == "MedDiet + VOO"],
  xlab = "Exposición",
  ylab = "Frecuencia",
  main = "Aceite oliva"
)
layout(1)

boxplot(bmi ~ group, predimed, xlab = "Exposición", ylab = "IMC")


# Dos medias / medianas--------------------------------------------------------------
t.test(age ~ sex, data = predimed, var.equal = TRUE)  # Prueba t
t.test(age ~ sex, data = predimed, var.equal = FALSE) # Prueba de Welch

wilcox.test(age ~ sex, data = predimed) # U de Mann-Whitney

# equivalencia lm
t.test(age ~ sex, data = predimed, var.equal = TRUE)  # Prueba t
summary(lm(age ~ sex, data = predimed))

wilcox.test(hp ~ am, data = mtcars)
summary(lm(rank(hp) ~ am, data = mtcars))


# Dos medias / medianas apareadas -----------------------------------------
t.test(bdd_rm_wide$periodo_1, bdd_rm_wide$periodo_2, paired = TRUE)
wilcox.test(bdd_rm_wide$periodo_1, bdd_rm_wide$periodo_2, paired = TRUE)


# equivalencia lm
summary(lm(I(periodo_2 - periodo_1) ~ 1, data = bdd_rm_wide))
summary(lm(signed_rank(periodo_2 - periodo_1) ~ 1, data = bdd_rm_wide))


# Dos o más medias - medianas -------------------------------------------------------
# OJO: si los datos no están balanceados, hay que usar car::Anova(modelo_lm) para tipo III y II (en función de si hay o no interacción).
mi_aov <- aov(bmi ~ group, data = predimed)
plot(mi_aov)
summary(mi_aov)
car::Anova(mi_aov, type = 2)
car::Anova(mi_aov, type = 3)
TukeyHSD(mi_aov)

kruskal.test(bmi ~ group, data = predimed)
dunnTest(bmi ~ group, data = predimed)


# equivalencia lm
summary(lm(bmi ~ group, data = predimed))
summary(lm(rank(bmi) ~ group, data = predimed))


# ANOVA dos vías
mi_aov <- aov(bmi ~ group * sex, data = predimed)
plot(mi_aov)
summary(mi_aov)
car::Anova(mi_aov, type = 2)
car::Anova(mi_aov, type = 3)
TukeyHSD(mi_aov)


# equivalencia lm
summary(lm(bmi ~ group * sex, data = predimed))
summary(lm(rank(bmi) ~ group * sex, data = predimed))


# ANOVA medidas repetidas
mi_aov_rep <- aov(resultado ~ grupo + Error(id), data = bdd_rm_long)

# El resultado se queda corto, así que hay que aprovechar la equivalencia con lm
mod_lmer1 <- lme4::lmer(resultado ~ grupo + (1 | id), data = bdd_rm_long)
summary(mod_lmer1)

mod_lmer2 <- lme4::lmer(resultado ~ grupo + periodo + (1 | id), data = bdd_rm_long)
summary(mod_lmer2)

mod_lmer3 <- lme4::lmer(resultado ~ grupo * periodo + (1 | id), data = bdd_rm_long)
summary(mod_lmer3)


# En estos casos, tiene ventajas el abordaje bayesiano: veamos una prueba
mod_lmer_bayes <- brms::brm(
  formula = resultado ~ grupo * periodo + (1 | id),
  data    = bdd_rm_long,
  silent  = 2
)
summary(mod_lmer_bayes)


# Dos o más proporciones ------------------------------------------------------------
chisq.test(table(predimed$group, predimed$sex))


tabla_predi <- predimed[, .(.N), by = .(group, sex)]
tabla_predi[, voo  := ifelse(group == "MedDiet + VOO", 1, 0)]
tabla_predi[, nuts := ifelse(group == "MedDiet + Nuts", 1, 0)]
tabla_predi[, homb := ifelse(sex   == "Male", 1, 0)]

tabla_ji <- dcast(tabla_predi, group ~ sex, value.var = "N")
chisq.test(tabla_ji[, -1])



# equivalencia log-lineal
anova(glm(N ~ group * sex, tabla_predi, family = poisson), test = "Chisq")


# Correlación -----------------------------------------------------------------------
ggplot(predimed, aes(x = wth, y = bmi)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_classic() +
  labs(x = "Razón cintura - altura", y = "IMC")


plot(
  predimed$wth,
  predimed$bmi,
  xlab = "Razón cintura - altura",
  ylab = "IMC"
)
abline(reg = lm(predimed$bmi ~ predimed$wth))


cor.test(predimed$wth, predimed$bmi)
cor.test(predimed$wth, predimed$bmi, method = "spearman")


# equivalencia lm
summary(lm(scale(predimed$wth) ~ scale(predimed$bmi)))
summary(lm(rank(predimed$wth) ~ rank(predimed$bmi)))


# El modelo lineal ------------------------------------------------------------------
mimod <- lm(bmi ~ I(wth - mean(wth)) + I(age - mean(age)), data = predimed)
summary(mimod)

opar <- par()
par(mfrow = c(2, 2))
plot(mimod)
par(opar)
car::vif(mimod)
acf(mimod$resid)


# Modelo lineal generalizado ----------------------------------------------
miglm <- glm(event ~ sex + I(age - mean(age)), data = predimed, family = binomial)
summary(miglm)
anova(miglm, test = "Chisq")
residuos <- resid(miglm, type = "pearson")

with(
  miglm, c(pchisq(deviance, df.residual,low= F),pchisq(null.deviance, df.null,low= F)))

sum(abs(residuos) > 2) / length(residuos)


# Extra: descriptiva fácil ----------------------------------------------------------
# gtsummary (mi opción preferida)
tabla_gtsum <- predimed %>%
  tbl_summary(
    by = group
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_header(label = "") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Grupo asignado**") %>%
  bold_labels()

# compareGroups
tabla_compg <- createTable(
  compareGroups(
    group ~ .,
    data = predimed
  ),
  show.all     = TRUE,
  digits       = 1,
  digits.ratio = 3,
)

# Por ejemplo, con resultado tiempo hasta evento
library(survival)
predimed$tmain <- with(predimed, Surv(toevent, event == "Yes"))
attr(predimed$tmain, "label") <- "AMI, stroke, or CV Death"
createTable(
  compareGroups(
    tmain ~ group + age + sex,
    data = predimed
  ),
  show.ratio = TRUE,
  digits.ratio = 3
)
