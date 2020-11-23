

# Final fix to variables -----------------------------------------------------

# gruppindelning inkomst
inc <- pop %>%
  summarise(incsum = quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )) %>%
  pull(incsum)

pop <- pop %>%
  mutate(
    coviddtm = case_when(
      sos_out_hospcovidconfirmed == "Yes" ~ indexdtm + sos_outtime_hospcovidconfirmed,
      sos_deathcovidconfulorsak == "Yes" ~ sos_deathdtm,
      TRUE ~ as.Date(NA)
    ),

    covidmonth = month(coviddtm),
    covidperiod = case_when(
      covidmonth <= 5 ~ "Feb-May",
      covidmonth > 5 ~ "Jun-"
    ),

    sos_covidconfirmed = if_else(sos_deathcovidconfulorsak == "Yes" |
      sos_out_hospcovidconfirmed == "Yes", "Yes", "No"),

    sos_ddr_rasiarni = if_else(
      sos_ddr_acei == "Yes" |
        sos_ddr_arb == "Yes" |
        sos_ddr_arni == "Yes", "Yes", "No"
    ),

    scb_dispincome_cat = case_when(
      scb_dispincome < inc[[1]] ~ 1,
      scb_dispincome < inc[[2]] ~ 2,
      scb_dispincome >= inc[[2]] ~ 3
    ),
    scb_dispincome_cat = factor(scb_dispincome_cat, labels = c("Low", "Medium", "High")),
    scb_region_stockholm = if_else(scb_region == "01", "Yes", "No"),

    scb_age_cat = case_when(
      scb_age < 75 ~ "<75",
      scb_age >= 75 ~ ">=75",
    ),

    sos_outtime_death = case_when(
      sos_out_hospcovidconfirmed == "No" & sos_deathcovidconfulorsak == "Yes" ~ 0,
      TRUE ~ as.numeric(censdtm - coviddtm)
    ),
    sos_outtime_death = if_else(sos_outtime_death < 0, 0, sos_outtime_death),
    # sos_outtime_death = sos_outtime_death + 1, # if found in CDR or die on same day as hosp will otherwise have 0 days fu
    
    # for competing risk analysis for outcome covid
    sos_covidconfirmed_cr = case_when(
      sos_covidconfirmed == "Yes" ~ 1,
      sos_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
  ) %>%
  select(
    LopNr,
    indexdtm,
    coviddtm,
    covidmonth,
    covidperiod,
    censdtm,
    contains("scb_"),
    contains("sos_ddr_"),
    contains("sos_com_"),
    contains("sos_out"),
    contains("sos_death"),
    contains("sos_covid")
  )


pop <- pop %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    scb_region = as.character(scb_region)
  )
