

# Death -------------------------------------------------------------------

dorsall <- full_join(
  dors %>% select(LopNr, DODSDAT, AR, ULORSAK),
  dors_cov,
  by = "LopNr",
  suffix = c("_all", "_cov")
) %>%
  mutate(
    DODSDAT = coalesce(DODSDAT_cov, DODSDAT_all),
    sos_deathdtm = ymd(case_when(
      substr(DODSDAT, 5, 8) == "0000" ~ paste0(substr(DODSDAT, 1, 4), "0101"),
      substr(DODSDAT, 7, 8) == "00" ~ paste0(substr(DODSDAT, 1, 6), "01"),
      TRUE ~ DODSDAT
    )),
    sos_deathdtm = case_when(
      sos_deathdtm > global_enddtm ~ as.Date(NA),
      TRUE ~ sos_deathdtm
    )
  ) %>%
  filter(!is.na(sos_deathdtm)) %>%
  mutate(
    ULORSAK = coalesce(ULORSAK_all, ULORSAK_cov),
    sos_death = if_else(!is.na(sos_deathdtm), "Yes", "No"),
    sos_death_hasulorsak = if_else(!is.na(ULORSAK), "Yes", "No"),
    sos_deathcovidconfulorsak = if_else(stringr::str_detect(ULORSAK, "U071"), "Yes", "No")
  ) %>%
  select(LopNr, starts_with("sos_"))

pop <- left_join(pop,
  dorsall,
  by = "LopNr"
) %>%
  mutate(
    sos_death = replace_na(sos_death, "No"),
    sos_deathcovidconfulorsak = replace_na(sos_deathcovidconfulorsak, "No"),
    sos_death_hasulorsak = replace_na(sos_death_hasulorsak, "No"),
    censdtm = pmin(global_enddtm, sos_deathdtm, na.rm = TRUE)
  )
