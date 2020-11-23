
ProjectTemplate::reload.project()

# Logistic regression -----------------------------------------------------

mod_sglt2i <- glm(sos_covidconfirmed == "Yes" ~ sos_ddr_sglt2i,
  data = match_sglt2i,
  family = binomial(link = "logit")
)

mod_glp1a <- glm(sos_covidconfirmed == "Yes" ~ sos_ddr_glp1a,
  data = match_glp1a,
  family = binomial(link = "logit")
)

mod_dpp4i <- glm(sos_covidconfirmed == "Yes" ~ sos_ddr_dpp4i,
  data = match_dpp4i,
  family = binomial(link = "logit")
)

## Outliers --------------------------------------------------------------

plot(mod_sglt2i, which = 4, id.n = 3)
plot(mod_glp1a, which = 4, id.n = 3)
plot(mod_dpp4i, which = 4, id.n = 3)

# Cox regression ----------------------------------------------------------

modcox_sglt2i <- coxph(Surv(sos_outtime_death, sos_death == "Yes") ~ sos_ddr_sglt2i + strata(par),
  data = match_sglt2i_cov
)
modcox_glp1a <- coxph(Surv(sos_outtime_death, sos_death == "Yes") ~ sos_ddr_glp1a + strata(par),
  data = match_glp1a_cov
)
modcox_dpp4i <- coxph(Surv(sos_outtime_death, sos_death == "Yes") ~ sos_ddr_dpp4i + strata(par),
  data = match_dpp4i_cov
)

## Checking for non-prop hazards ------------------------------------------

print(testpat <- cox.zph(modcox_sglt2i))
survminer::ggcoxzph(testpat[1])

print(testpat <- cox.zph(modcox_glp1a))
survminer::ggcoxzph(testpat[1])

print(testpat <- cox.zph(modcox_dpp4i))
survminer::ggcoxzph(testpat[1])

## Checking for outliers --------------------------------------------------

x11()
survminer::ggcoxdiagnostics(modcox_sglt2i,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

survminer::ggcoxdiagnostics(modcox_glp1a,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

survminer::ggcoxdiagnostics(modcox_dpp4i,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)
