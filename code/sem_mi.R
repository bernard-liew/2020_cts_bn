sem_form1 <- "

  # Latent
  Sensitivity =~ PPTcx + PPThx + PPTrm
  Severity =~ HDura + HFreq + HInten
  Disability =~ HDI_E + HDI_P
  Distress =~ Dep + Anx

  # Paths
  Sensitivity ~ Sex
  TrPs ~ Sensitivity
  Distress + Sensitivity ~ Sleep
  TrPs + Disability ~ Age
  Disability + TrPs ~ YearsP
  Severity ~ Distress
  Disability ~ Severity

  HInten~~Anx
  PPThx~~HInten
  PPTcx~~HDura
  PPTcx~~HInten
"

df3 <- mice::complete(dat_imp, "all") %>%
  map( ~ .x %>%
         mutate_if (is.numeric, scale, center = TRUE, scale = TRUE) %>%
         mutate (Sex = as.numeric (Sex) - 1))


m2 <- sem.mi (sem_form1,
           data = df3,
           orthogonal = TRUE,
           std.lv = FALSE,
           auto.fix.first = TRUE,
           estimator = "MLR",
           se = "robust.huber.white")

exprz <- str_split (sem_form1, "\\\n") %>%
  unlist() %>%
  str_trim()

exprz <- exprz [!grepl ("#", exprz)]
exprz <- exprz [nzchar(exprz)]

semPaths (m1,
          what = "path")

summary (m2)
coef(m2)

fitMeasures(m2, fit.measures = c("rmsea", "pvalue", "cfi", "tli", "srmr", "nnfi"), test = "D2" , pool.robust = TRUE)
parameterEstimates(m2)

params <- letters[1: length (coef(m2)[!grepl("=~|~~", labels (coef(m2)))])]

paths <- coef(m2)[!grepl("=~|~~", labels (coef(m2)))]
names(paths) <- letters[1: length (coef(m2)[!grepl("=~|~~", labels (coef(m2)))])]
var_mat <- vcov(m2)[!grepl("=~|~~", labels (coef(m2))),
                           !grepl("=~|~~", labels (coef(m2)))]
colnames(var_mat) <- rownames(var_mat) <- letters[1: length (coef(m2)[!grepl("=~|~~", labels (coef(m2)))])]

monteCarloCI(expr = params,
             ACM = var_mat,
             coefs = paths)
