
pop <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pop,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "dka",
  diakod = " E1110",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)

match_sglt2i_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_sglt2i_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "dka",
  diakod = " E1110",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)
match_glp1a_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_glp1a_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "dka",
  diakod = " E1110",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)
match_dpp4i_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_dpp4i_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "dka",
  diakod = " E1110",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)

pop <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pop,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "hypoglycemia",
  diakod = " E160| E162| E161W| E110C| E116A",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)

match_sglt2i_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_sglt2i_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "hypoglycemia",
  diakod = " E160| E162| E161W| E110C| E116A",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)
match_glp1a_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_glp1a_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "hypoglycemia",
  diakod = " E160| E162| E161W| E110C| E116A",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)
match_dpp4i_cov <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = match_dpp4i_cov,
  patid = LopNr,
  indexdate = coviddtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  censdate = censdtm,
  name = "hypoglycemia",
  diakod = " E160| E162| E161W| E110C| E116A",
  valsclass = "fac",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)

metaout <- metaout %>%
  group_by(Variable) %>%
  slice(1) %>%
  ungroup()
