

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

    tmp_covidmonth = month(coviddtm),
    tmp_covidyear = year(coviddtm),
    covidmonthyear = case_when(tmp_covidyear == 2020 ~ tmp_covidmonth, 
                               tmp_covidyear == 2021 ~ 12 + tmp_covidmonth),
    covidperiod = case_when(
      covidmonthyear <= 6 ~ "Feb-Jun 2020",
      covidmonthyear <= 12 ~ "Jul-Dec 2020", 
      covidmonthyear > 12 ~ "Jan-May 2021", 
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
    
    # death within 30 days 
    sos_out_death30d = case_when(
      sos_covidconfirmed == "No" ~ NA_character_, # patients without covid, exclude 
      coviddtm > global_enddtm - 30 ~ NA_character_, # not 30 days follow-up, exclude
      sos_outtime_death <= 30 & sos_death == "Yes" ~ "Yes",
      TRUE ~ "No"
    ), 
    
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
    covidmonthyear,
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
