
# Time on treatment --------------------------------------------------------

lmtime <- function(atc, treatname, lmdata = lmsglt2) {
  treatvar <- paste0("sos_ddr_", treatname)
  treattimevar <- paste0("sos_ddr_", treatname, "_time")

  lmtmp <- inner_join(pop %>%
    filter(!!sym(treatvar) == "Yes") %>%
    select(LopNr),
  lmdata %>%
    mutate(atcneed = stringr::str_detect(ATC, atc)) %>%
    filter(
      atcneed,
      EDATUM < global_indexdtm
    ),
  by = "LopNr"
  )

  lmtmp <- lmtmp %>%
    group_by(LopNr) %>%
    arrange(EDATUM) %>%
    slice(c(1, n())) %>%
    mutate(firstlast = ifelse(row_number() == 1, "firstdtm", "lastdtm")) %>%
    ungroup()

  lmtmp2 <- inner_join(lmtmp %>%
    filter(firstlast == "firstdtm") %>%
    select(LopNr, EDATUM),
  lmtmp %>%
    filter(firstlast == "lastdtm") %>%
    select(LopNr, EDATUM),
  by = "LopNr",
  suffix = c("_first", "_last")
  ) %>%
    mutate(
      EDATUM_last = pmin(EDATUM_last + 90, global_indexdtm - 1),
      !!treattimevar := as.numeric(EDATUM_last + 90 - EDATUM_first) / 365.25
    )

  pop <<- left_join(pop,
    lmtmp2,
    by = "LopNr"
  )
}

lmtime("^(A10BK|A10BD1[5-6]|A10BD19|A10BD2[0-1]|A10BD2[3-5]|A10BX09|A10BX11|A10BX12)", "sglt2i")

lmtime("^(A10BJ|A10BX04|A10BX07|A10BX10|A10BX1[3-4])", "glp1a")

lmtime("^(A10BH|A10BD0[7-9]|A10BD1[0-3]|A10BD1[8-9]|A10BD2[1-2]|A10BD2[4-5])", "dpp4i")
