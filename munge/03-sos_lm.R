
# Treatments from the DDR -------------------------------------------------

# within 5 months prior to index

lm_covid <- lm %>%
  filter(
    EDATUM > global_indexdtm - 30.5 * 5,
    EDATUM < global_indexdtm
  )

lmtreats <- function(atc, treatname, lmdata = lm_covid, metatab = TRUE) {
  lmtmp2 <- lmdata %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc)
    ) %>%
    filter(
      atcneed
    )

  treatname <- paste0("sos_ddr_", treatname)

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
    mutate(!!treatname := replace_na(!!sym(treatname), "No"))

  if (metatab) {
    metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
    if (exists("metalm")) {
      metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
    } else {
      metalm <<- metatmp # global variable, writes to global env
    }
  }
}

# Diabetes meds

lmtreats("^(A10BK|A10BD1[5-6]|A10BD19|A10BD2[0-1]|A10BD2[3-5]|A10BX09|A10BX11|A10BX12)", "sglt2i")

lmtreats("^(A10BJ|A10BX04|A10BX07|A10BX10|A10BX1[3-4])", "glp1a")

lmtreats("^(A10BH|A10BD0[7-9]|A10BD1[0-3]|A10BD1[8-9]|A10BD2[1-2]|A10BD2[4-5])", "dpp4i")

# Diabetes meds for adjustment

lmtreats("^A10A", "insulin")

lmtreats("^(A10BA02|A10BD0[2-3]|A10BD05|A10BD0[7-8]|A10BD1[0-1]|A10BD1[3-8]|A10BD20|A10BD2[2-3]|A10BD25)", "metformin")

lmtreats("^(A10BB|A10BC01|A10BD0[2-6]|A10BD09|A10BD12|A10BD14|A10BD17|A10BF|A10BG|A10BE|A10XA01|A10BX0[2-3]|A10BX0[5-6]|A10BX08)", "otheroralantidiabetic")

# Adj meds

lmtreats("^(C09A|C09B)", "acei")

lmtreats("^(C09C|C09D(?!X04))", "arb")

lmtreats("^(C03(?!DA)|C07B|C07C|C07D|C08GA|C09BA|C09DA|C09DX01)", "diuretics")

lmtreats("^C07", "bbl")

lmtreats("^(C08|C07FB|C09BB|C09DB|C09DX01)", "ccb")

lmtreats("^C03DA", "mra")

lmtreats("^C09DX04", "arni")

lmtreats("^B01AC", "antiplatlet")

lmtreats("^B01A(?!C)", "anticoagulant")

lmtreats("^C10", "lipidlowering")

lmtreats("^C01AA05", "digoxin")

lmtreats("^C01DA", "nitrate")

lmtreats("^C01B", "antiarrhythmic")

colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry",
    Period = "-5mo--1",
  )
