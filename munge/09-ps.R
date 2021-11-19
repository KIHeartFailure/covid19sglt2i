

# Propensity scores -------------------------------------------------------

# All patients ------------------------------------------------------------

ps_sglt2i <- glm(formula(paste0(
  "sos_ddr_sglt2i == 'Yes' ~ sos_ddr_glp1a + sos_ddr_dpp4i + ",
  paste(modvarsns,
    collapse = " + "
  )
)),
data = pop,
family = binomial
)

ps_glp1a <- glm(formula(paste0(
  "sos_ddr_glp1a == 'Yes' ~ sos_ddr_sglt2i + sos_ddr_dpp4i + ",
  paste(modvarsns,
    collapse = " + "
  )
)),
data = pop,
family = binomial
)

ps_dpp4i <- glm(formula(paste0(
  "sos_ddr_dpp4i == 'Yes' ~ sos_ddr_sglt2i + sos_ddr_glp1a + ",
  paste(modvarsns,
    collapse = " + "
  )
)),
data = pop,
family = binomial
)

popps <- bind_cols(
  na.omit(pop %>%
    select(LopNr, !!!syms(modvars))),
  ps_sglt2i = ps_sglt2i$fitted,
  ps_glp1a = ps_glp1a$fitted,
  ps_dpp4i = ps_dpp4i$fitted
)

pop <- left_join(pop,
  popps %>%
    select(
      LopNr,
      ps_sglt2i,
      ps_glp1a,
      ps_dpp4i
    ),
  by = "LopNr"
)

# Covid-19 patients -------------------------------------------------------

ps_sglt2i_cov <- glm(formula(paste0(
  "sos_ddr_sglt2i == 'Yes' ~ sos_ddr_glp1a + sos_ddr_dpp4i + ",
  paste(c(modvarsns, "covidmonthyear"),
    collapse = " + "
  )
)),
data = pop %>% filter(sos_covidconfirmed == "Yes" & !is.na(sos_out_death30d)),
family = binomial
)

ps_glp1a_cov <- glm(formula(paste0(
  "sos_ddr_glp1a == 'Yes' ~ sos_ddr_sglt2i + sos_ddr_dpp4i + ",
  paste(c(modvarsns, "covidmonthyear"),
    collapse = " + "
  )
)),
data = pop %>% filter(sos_covidconfirmed == "Yes" & !is.na(sos_out_death30d)),
family = binomial
)

ps_dpp4i_cov <- glm(formula(paste0(
  "sos_ddr_dpp4i == 'Yes' ~ sos_ddr_sglt2i + sos_ddr_glp1a + ",
  paste(c(modvarsns, "covidmonthyear"),
    collapse = " + "
  )
)),
data = pop %>% filter(sos_covidconfirmed == "Yes" & !is.na(sos_out_death30d)),
family = binomial
)

popps_cov <- bind_cols(
  na.omit(pop %>%
    filter(sos_covidconfirmed == "Yes" & !is.na(sos_out_death30d)) %>%
    select(LopNr, !!!syms(modvars))),
  ps_sglt2i_cov = ps_sglt2i_cov$fitted,
  ps_glp1a_cov = ps_glp1a_cov$fitted,
  ps_dpp4i_cov = ps_dpp4i_cov$fitted
)

pop <- left_join(pop,
  popps_cov %>%
    select(
      LopNr,
      ps_sglt2i_cov,
      ps_glp1a_cov,
      ps_dpp4i_cov
    ),
  by = "LopNr"
)

# Matching ----------------------------------------------------------------

matchfunc <- function(var, psvar) {
  tmpdata <- pop %>%
    filter(!is.na(!!sym(psvar))) %>%
    mutate(
      tmpmatchvar = if_else(!!sym(var) == "Yes", 1, 0),
      par = NA
    )

  cal <- 0.01 / sd(tmpdata %>% pull(!!sym(psvar)))

  set.seed(2334325)
  match1 <- Match(
    Tr = tmpdata$tmpmatchvar,
    X = tmpdata %>% pull(!!sym(psvar)),
    estimand = "ATT",
    caliper = cal,
    replace = F,
    ties = F,
    M = 1
  )

  # set.seed(2334325)
  # match2 <- Match(
  #   Tr = tmpdata$tmpmatchvar,
  #   X = tmpdata %>% pull(!!sym(psvar)),
  #   estimand = "ATT",
  #   caliper = cal,
  #   replace = F,
  #   ties = F,
  #   M = 2
  # )

  tmpdata$par[c(unique(match1$index.treated), match1$index.control)] <-
    c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))

  matchdata <- tmpdata[c(unique(match1$index.treated), match1$index.control), ]

  return(matchdata)
}

match_sglt2i <- matchfunc("sos_ddr_sglt2i", "ps_sglt2i")
match_glp1a <- matchfunc("sos_ddr_glp1a", "ps_glp1a")
match_dpp4i <- matchfunc("sos_ddr_dpp4i", "ps_dpp4i")

match_sglt2i_cov <- matchfunc("sos_ddr_sglt2i", "ps_sglt2i_cov")
match_glp1a_cov <- matchfunc("sos_ddr_glp1a", "ps_glp1a_cov")
match_dpp4i_cov <- matchfunc("sos_ddr_dpp4i", "ps_dpp4i_cov")
