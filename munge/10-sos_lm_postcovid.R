lmtmp <- inner_join(pop,
  lmsglt2_postc,
  by = "LopNr"
) %>%
  filter(
    EDATUM > coviddtm,
    EDATUM <= coviddtm + 5 * 30.5
  )


lmtreatspostc <- function(atc, name) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc)
    ) %>%
    filter(
      atcneed
    )

  treatname <- paste0("sos_ddr_postcovid_", name)
  treatnameprior <- paste0("sos_ddr_", name)

  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, !!sym(treatname))

  pop <<- left_join(pop,
    lmtmp2,
    by = "LopNr"
  ) %>%
    mutate(
      !!treatname := replace_na(!!sym(treatname), "No"),
      !!treatname := if_else(sos_out_hospcovidconfirmed == "Yes" & coviddtm <= censdtm - 5 * 30.5 & !!sym(treatnameprior) == "Yes",
        !!sym(treatname), NA_character_
      )
    )
}

lmtreatspostc("^(A10BK|A10BD1[5-6]|A10BD19|A10BD2[0-1]|A10BD2[3-5]|A10BX09|A10BX11|A10BX12)", "sglt2i")

lmtreatspostc("^(A10BJ|A10BX04|A10BX07|A10BX10|A10BX1[3-4])", "glp1a")

lmtreatspostc("^(A10BH|A10BD0[7-9]|A10BD1[0-3]|A10BD1[8-9]|A10BD2[1-2]|A10BD2[4-5])", "dpp4i")
