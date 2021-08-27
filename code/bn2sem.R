library (bnlearn)
library (tidyverse)
library (lavaan)
library (semPlot)
library (mlbench)
library (nonnest2)
library (semTools)
library (Rgraphviz)

set_threshold <- 0.7 # 0.7 more sparse than 0.5, try 0.5 0.6 0.7

################################### Example 1######################################################
data (BostonHousing2)
df <- BostonHousing2 %>% dplyr::select(
  cmedv, #median value of home in 1000s
  crim, #per capita crime by town
  nox, #nitric oxide concentration
  lstat, #proportion of lower status
  rad #proximity to radial highways
) %>%
  mutate(log_crim = log2(crim),
             rad = as.numeric (rad)) %>%
  mutate_all (scale, center = TRUE, scale = TRUE) %>%
  select (- crim)


# Initial SEM model
lavaan_m <- '
cmedv ~ log_crim + lstat
nox ~ rad + log_crim
'
m1 <- sem(lavaan_m, data=df,  conditional.x = FALSE, meanstructure = TRUE)
summary(m1)

semPaths(m1, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

fitMeasures(m1, fit.measures = c("rmsea", "pvalue", "cfi", "tli"))

# Bayesian network

set.seed (123)
boot <- boot.strength(df, algorithm = "hc", R = 200)
avg <-  averaged.network(boot, threshold = set_threshold)
fit <-  bn.fit (avg, df, method = "mle")


g = strength.plot(avg,
                  boot,
                  shape = "rectangle",
                  main = "Figure")

renderGraph(g)
# Second SEM model from BN model

fits <- as.lm (avg, df)
fit.form <- map (fits, formula)
fit.form <- fit.form[!grepl ("~ 1", fit.form)]

lavaan_m2 <- map_chr (fit.form, deparse)

m2 <- sem (lavaan_m2, data = df,  conditional.x = FALSE, meanstructure = TRUE)
summary (m2)

semPaths (m2, what='std', nCharNodes=6, sizeMan=10,
          edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
fitMeasures(m2, fit.measures = c("rmsea", "pvalue", "cfi", "tli"))

# Model comparison
vuongtest(m1, m2)

# Net manual
# https://psu-psychology.github.io/psy-597-sem-sp2019/09_model_comparison/model_comparison.html#bentler-net-approach
#http://www.statmodel.com/download/NET.pdf
df1 <- fitmeasures(m1)["df"]
chi1 <- fitmeasures(m1)["chisq"]

f1 <- fitted (m1)$cov
m2_1 <- sem(lavaan_m2, sample.cov = f1, sample.nobs = nrow (BostonSmall))
df2_1 <- fitmeasures(m2_1)["df"]
chi2_1 <- fitmeasures(m2_1)["chisq"]

test <- net (m1, m2)
summary (test)


################################### Example 2######################################################

df <- HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "x5", "x6")]

# Model 1
HS.model1 <- '

x1~ x2 + x3
x4~x1 + x5 + x6

'

m1 <- sem(HS.model1, data=df, conditional.x = FALSE, meanstructure = TRUE)
summary (m1)

semPaths(m1, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

fitMeasures(m1, fit.measures = c("rmsea", "pvalue", "cfi", "tli", "aic", "bic"))

# Bayesian network

set.seed (123)
boot <- boot.strength(df, algorithm = "hc", R = 200)
avg <-  averaged.network(boot, threshold = set_threshold)
fit <-  bn.fit (avg, df, method = "mle")


g = strength.plot(avg,
                  boot,
                  shape = "rectangle",
                  main = "Figure")

renderGraph(g)
# Second SEM model from BN model

fits <- as.lm (avg, df)
fit.form <- map (fits, formula)
fit.form <- fit.form[!grepl ("~ 1", fit.form)]

lavaan_m2 <- map_chr (fit.form, deparse)

m2 <- sem (lavaan_m2, data = df,  conditional.x = FALSE, meanstructure = TRUE)
summary (m2)

semPaths (m2, what='std', nCharNodes=6, sizeMan=10,
          edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
fitMeasures(m2, fit.measures = c("rmsea", "pvalue", "cfi", "tli"))

# Compare

vuongtest(m1, m2)
