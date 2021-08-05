rrmy <- function(outvar,
                 modname,
                 alldata,
                 matchdata,
                 modvarsmy,
                 sensanalysis = FALSE,
                 fg = FALSE) {
  if (fg) sensanalysis <- TRUE

  if (!sensanalysis) {
    out <- data.frame(matrix(NA, ncol = 12, nrow = 3, 1))
    row <- 3
  }

  if (sensanalysis) {
    out <- data.frame(matrix(NA, ncol = 12, nrow = 1, 1))
    row <- 1
  }
  colnames(out) <- c(
    "Patient group", "Model", "No_overall", "Yes_overall", "p-value_overall",
    "No_matched", "Yes_matched", "p-value_matched",
    "logrr", "loglci", "loguci", "modnameforest"
  )

  out[1, 1] <- modname

  if (!sensanalysis) {
    out[1, 2] <- "n (%) event"

    ## all data
    ns <- alldata %>%
      group_by(!!sym(modvarsmy[1])) %>%
      count(!!sym(outvar)) %>%
      mutate(
        p = n / sum(n),
        np = paste0(n, " (", dF(p * 100, dig = 2), "%)")
      ) %>%
      ungroup() %>%
      filter(!!sym(outvar) == "Yes")

    out[1, 3:4] <- ns %>% pull(np)

    ## match data
    ns <- matchdata %>%
      group_by(!!sym(modvarsmy[1])) %>%
      count(!!sym(outvar)) %>%
      mutate(
        p = n / sum(n),
        np = paste0(n, " (", dF(p * 100, dig = 2), "%)")
      ) %>%
      ungroup() %>%
      filter(!!sym(outvar) == "Yes")

    out[1, 6:7] <- ns %>% pull(np)

    # Crude
    out[2, 2] <- "Crude RR (95% CI)"
    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      modvarsmy[1]
    )),
    data = alldata,
    id = LopNr,
    family = poisson("log"),
    corstr = "independence"
    )
    smod <- summary(mod)
    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[2, 3:4] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )

    ## adj covariates

    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      paste(modvarsmy, collapse = " + ")
    )),
    data = alldata,
    id = LopNr,
    family = poisson("log"),
    corstr = "independence"
    )
    smod <- summary(mod)
    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[3, 3:4] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )
  }

  if (!fg) {
    out[row, 2] <- "Adjusted RR (95% CI)"

    ## adj matching

    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      modvarsmy[1]
    )),
    data = matchdata %>% arrange(par),
    id = par,
    family = poisson("log"),
    corstr = "exchangeable"
    )
    smod <- summary(mod)
    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[row, 6:7] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )

    if (!sensanalysis) {
      # for forestplot
      out[3, 12] <- out[1, 1]
      out[3, 9:11] <- c(coeff.c, lci.c, uci.c)
    }
  }

  if (fg) {
    if (outvar != "sos_covidconfirmed") stop("Only possible for covid outcome")
    matchdata <- matchdata %>%
      mutate(cov = if_else(!!sym(modvarsmy[1]) == "Yes", 1, 0))
    mod <- summary(crr(matchdata %>% pull(sos_outtime_hospcovidconfirmed),
      matchdata %>% pull(sos_covidconfirmed_cr),
      matchdata %>% pull(cov),
      failcode = 1, cencode = 0
    ))

    out[row, 2] <- "Adjusted HR (95% CI)"

    out[row, 6:7] <- c(
      "ref",
      paste0(
        dF(mod$conf.int[, 1], dig = 2),
        " (", dF(mod$conf.int[, 3], dig = 2),
        "-", dF(mod$conf.int[, 4], dig = 2), "), ",
        dF(mod$coef[, 5], dig = 3, p = TRUE)
      )
    )
  }
  return(out)
}

rrmyint <- function(outvar,
                    modname,
                    alldata,
                    matchdata,
                    modvarsmy,
                    interactionvar) {
  rows <- c(1, 4)
  levs <- levels(pop %>% pull(!!sym(interactionvar)))

  if (length(levs) > 2) stop("> 2 levels in interaction variable")

  out <- data.frame(matrix(NA, ncol = 12, nrow = 6, 1))
  colnames(out) <- c(
    "Patient group", "Model", "No_overall", "Yes_overall", "p-value_overall",
    "No_matched", "Yes_matched", "p-value_matched",
    "logrr", "loglci", "loguci", "modnameforest"
  )

  out[rows, 1] <- paste0(modname, "=", levs)

  for (i in seq_along(levs)) {
    out[rows[i], 2] <- "n (%) event"

    ## all data

    ns <- alldata %>%
      filter(!!sym(interactionvar) == levs[i]) %>%
      group_by(!!sym(modvarsmy[1])) %>%
      count(!!sym(outvar)) %>%
      mutate(
        p = n / sum(n),
        np = paste0(n, " (", dF(p * 100, dig = 2), "%)")
      ) %>%
      ungroup() %>%
      filter(!!sym(outvar) == "Yes")

    out[rows[i], 3:4] <- ns %>% pull(np)

    ## match data
    ns <- matchdata %>%
      filter(!!sym(interactionvar) == levs[i]) %>%
      group_by(!!sym(modvarsmy[1])) %>%
      count(!!sym(outvar)) %>%
      mutate(
        p = n / sum(n),
        np = paste0(n, " (", dF(p * 100, dig = 2), "%)")
      ) %>%
      ungroup() %>%
      filter(!!sym(outvar) == "Yes")

    out[rows[i], 6:7] <- ns %>% pull(np)

    # Crude
    out[rows[i] + 1, 2] <- "Crude RR (95% CI)"
    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      modvarsmy[1], " * relevel(", interactionvar, ", ref = '", levs[i], "')"
    )),
    data = alldata,
    id = LopNr,
    family = poisson("log"),
    corstr = "independence"
    )
    smod <- summary(mod)

    out[rows[i] + 1, 5] <- dF(last(smod$coefficients$`Pr(>|W|)`), dig = 3, p = TRUE)

    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[rows[i] + 1, 3:4] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )

    # ## adj covariates
    out[rows[i] + 2, 2] <- "Adjusted RR (95% CI)"
    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      modvarsmy[1], " * relevel(", interactionvar, ", ref = '", levs[i], "') + ",
      paste(modvarsmy, collapse = " + ")
    )),
    data = alldata,
    id = LopNr,
    family = poisson("log"),
    corstr = "independence"
    )
    smod <- summary(mod)

    out[rows[i] + 2, 5] <- dF(last(smod$coefficients$`Pr(>|W|)`), dig = 3, p = TRUE)
    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[rows[i] + 2, 3:4] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )

    # adj matching
    mod <- geeglm(formula(paste0(
      outvar, " == 'Yes' ~ ",
      modvarsmy[1], " * relevel(", interactionvar, ", ref = '", levs[i], "')"
    )),
    data = matchdata,
    id = par,
    family = poisson("log"),
    corstr = "exchangeable"
    )
    smod <- summary(mod)

    out[rows[i] + 2, 8] <- dF(last(smod$coefficients$`Pr(>|W|)`), dig = 3, p = TRUE)

    coeff.c <- smod$coefficients$Estimate[2]
    lci.c <- coeff.c - global_z05 * smod$coefficients$Std.err[2]
    uci.c <- coeff.c + global_z05 * smod$coefficients$Std.err[2]

    out[rows[i] + 2, 6:7] <- c(
      "ref", paste0(
        dF(exp(coeff.c), dig = 2),
        " (", dF(exp(lci.c), dig = 2),
        "-", dF(exp(uci.c), dig = 2), "), ",
        dF(smod$coefficients$`Pr(>|W|)`[2], dig = 3, p = TRUE)
      )
    )

    # for forestplot
    out[rows[i] + 2, 12] <- out[rows[i], 1]
    out[rows[i] + 2, 9:11] <- c(coeff.c, lci.c, uci.c)
  }
  return(out)
}
