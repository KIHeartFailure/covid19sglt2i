
ProjectTemplate::reload.project(cache_loading = FALSE)
memory.limit(size=10000000000000)

sospath <- "./raw-data/patreg/"

# Data 2019, 2020 ov, sv --------------------------------------------------

load(paste0(sospath, "svov1920.RData"))

svov1920 <- bind_rows(
  sv_2019 %>% mutate(sos_source = "sv"),
  ov_2019 %>% mutate(sos_source = "ov"),
  sv_cov2020 %>% mutate(sos_source = "sv"),
  ov_cov2020 %>% mutate(sos_source = "ov")
) %>% 
  select(-starts_with("OPD"), -KON, -ALDER, -MVO, -VTID, -UTSATT, -PVARD, -LK, -INDATUMA, -UTDATUMA)

svov1920 <- svov1920 %>%
  rename_at(
    vars(starts_with("op")),
    list(~ toupper(.))
  ) %>%
  rename_at(
    vars(starts_with("EKOD")),
    list(~ tolower(.))
  ) %>%
  rename(HDIA = hdia) %>%
  filter(!is.na(INDATUM))

svov1920 <- prep_sosdata(svov1920, utdatum = FALSE)

save(file = paste0(sospath, "svov1920_prep.RData"), "svov1920")

rm(list = c(
  "sv_2019",
  "ov_2019",
  "sv_cov2020",
  "ov_cov2020", 
  "svov_cov"))


# SV ----------------------------------------------------------------------

load(paste0(sospath, "sv.RData"))

sv <- sv %>%
  select(-starts_with("OPD"), -KON, -ALDER, -MVO, -VTID, -UTSATT, -PVARD, -lk, -LKF, -INDATUMA, -UTDATUMA) %>%
  mutate(sos_source = "sv") %>%
  rename_at(
    vars(starts_with("op")),
    list(~ toupper(.))
  ) %>%
  rename_at(
    vars(starts_with("EKOD")),
    list(~ tolower(.))
  ) %>%
  rename(HDIA = hdia) %>%
  filter(!is.na(INDATUM))

sv <- prep_sosdata(sv, utdatum = FALSE)

save(file = paste0(sospath, "sv_prep.RData"), "sv")

rm(sv)

# OV ----------------------------------------------------------------------

load(paste0(sospath, "ov.RData"))

ov <- ov %>%
  select(-starts_with("OPD"), -KON, -ALDER, -MVO, -PVARD, -lk, -LKF, -INDATUMA) %>%
  mutate(sos_source = "ov") %>%
  rename_at(
    vars(starts_with("op")),
    list(~ toupper(.))
  ) %>%
  rename_at(
    vars(starts_with("EKOD")),
    list(~ tolower(.))
  ) %>%
  rename(HDIA = hdia) %>%
  filter(!is.na(INDATUM))

ov <- prep_sosdata(ov, utdatum = FALSE)

save(file = paste0(sospath, "ov_prep.RData"), "ov")

rm(ov)


# All together now https://www.youtube.com/watch?v=73lj5qJbrms ------------

load(paste0(sospath, "svov1920_prep.RData"))
load(paste0(sospath, "sv_prep.RData"))
load(paste0(sospath, "ov_prep.RData"))

patreg <- bind_rows(
  sv,
  ov,
  svov1920
)

rm(list = c(
  "sv",
  "ov",
  "svov1920"))

patreg <- patreg %>%
  mutate(LopNr = as.numeric(LopNr))

save(file = "./data/rawData_sos_patreg.RData", "patreg")