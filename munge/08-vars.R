

# Variables for tabs and mods ---------------------------------------------

tabvars <- names(pop)[str_detect(names(pop), "^(scb_|sos_com_|sos_ddr_|covidperiod)")]
tabvars <- tabvars[!tabvars %in% c("scb_region", "sos_com_diabetes")]

tabvars <- sort(tabvars)

# vars fox log reg
tabvars_not_in_mod <- c(
  "covidperiod",

  "scb_countryofbirth_africaasia",
  "scb_dispincome",
  "scb_age_cat",
  "scb_maritalstatus",

  "sos_ddr_acei",
  "sos_ddr_arb",
  "sos_ddr_arni",

  "sos_ddr_sglt2i",
  "sos_ddr_glp1a",
  "sos_ddr_dpp4i",

  "sos_ddr_sglt2i_iu",
  "sos_ddr_glp1a_iu",
  "sos_ddr_dpp4i_iu",
  
  "sos_ddr_sglt2i_time",
  "sos_ddr_glp1a_time",
  "sos_ddr_dpp4i_time"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]
modvarsns <- modvars
modvarsns[modvars == "scb_age"] <- "ns(scb_age, 4)"
