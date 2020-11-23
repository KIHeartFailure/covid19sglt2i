
# all data below (except LISA) from 2019-12-31

# Reused and changed PINs -------------------------------------------------

atersenpnr <- fodelseuppg %>%
  filter(AterPNr == 1 | SenPNr == 0) %>%
  group_by(LopNr) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(scb_atersenpnr = 1) %>%
  select(LopNr, starts_with("scb_"))


# Country of birth, sex and age -------------------------------------------

fodelseuppg <- fodelseuppg %>%
  group_by(LopNr) %>%
  arrange(desc(SenPNr)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    scb_countryofbirth = case_when(
      FodelselandGrp %in% c(
        "Afrika",
        "Asien",
        "Nordamerika",
        "Oceanien",
        "Sovjetunionen",
        "Sydamerika",
        "Statslos"
      ) ~ 3,
      FodelselandGrp %in% c(
        "EU28 utom Norden",
        "Europa utom EU28 och Norden",
        "Norden utom Sverige"
      ) ~ 2,
      FodelselandGrp == "Sverige" ~ 1
    ),
    scb_countryofbirth = factor(scb_countryofbirth,
      labels = c("Sweden", "Europe", "Other")
    ),
    scb_countryofbirth_africaasia = case_when(
      FodelselandGrp %in% c(
        "Afrika",
        "Asien"
      ) ~ 1,
      FodelselandGrp %in% c(
        "Nordamerika",
        "Oceanien",
        "Sovjetunionen",
        "Sydamerika",
        "Statslos",
        "EU28 utom Norden",
        "Europa utom EU28 och Norden",
        "Norden utom Sverige",
        "Sverige"
      ) ~ 0
    ),
    scb_countryofbirth_africaasia = factor(scb_countryofbirth_africaasia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),

    scb_sex = case_when(
      Kon == 1 ~ "Male",
      Kon == 2 ~ "Female"
    ),

    scb_age = 2019 - as.numeric(FoddAr)
  ) %>%
  select(LopNr, starts_with("scb_"))


# Number of children ------------------------------------------------------

antalbarn <- antalbarn %>%
  group_by(LopNr) %>%
  arrange(desc(SenPNr)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(scb_child = ifelse(!is.na(AntalBarn), "Yes", "No")) %>%
  select(LopNr, starts_with("scb_"))

# Education, Income, region, civil ----------------------------------------

# lan, civil, famtyp from RTB 2019 since LISA only 2018
# Disp inkomst and education from LISA

lisa <- lisa %>%
  mutate(
    scb_education = case_when(
      Sun2000niva_Old %in% c(1, 2) ~ "Compulsory school",
      Sun2000niva_Old %in% c(3, 4) ~ "Secondary school",
      Sun2000niva_Old %in% c(5, 6, 7) ~ "University"
    ),
    scb_dispincome = DispInk04
  ) %>%
  select(LopNr, starts_with("scb_"))

rtb <- rtb %>%
  mutate(
    scb_region = Lan,
    scb_maritalstatus = case_when(
      Civil %in% c("A", "EP", "OG", "S", "SP") ~ "Single/widowed/divorced",
      Civil %in% c("G", "RP") ~ "Married"
    ),
    scb_famtype = case_when(
      FamTyp %in% c(11, 12, 13, 21, 22, 23, 31, 32, 41, 42) ~ "Cohabitating",
      FamTyp %in% c(50, 60) ~ "Living alone"
    )
  ) %>%
  select(LopNr, starts_with("scb_"))


# Migration ---------------------------------------------------------------

emigration <- migration %>%
  group_by(LopNr) %>%
  arrange(Datum) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(Posttyp == "Utv") %>%
  mutate(scb_emigrated = 1) %>%
  select(LopNr, starts_with("scb_"))

immigration <- migration %>%
  mutate(migdat = ymd(Datum)) %>%
  group_by(LopNr) %>%
  arrange(Datum) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(Posttyp == "Inv" &
    migdat >= ymd("20150101")) %>%
  mutate(scb_immigratedpost2015 = 1) %>%
  select(LopNr, starts_with("scb_"))

# All ---------------------------------------------------------------------

pop <- Reduce(
  function(...) {
    full_join(...,
      by = "LopNr"
    )
  },
  list(atersenpnr, emigration, immigration, fodelseuppg, rtb, lisa, antalbarn)
)

pop <- pop %>%
  mutate(
    indexdtm = global_indexdtm
  )
