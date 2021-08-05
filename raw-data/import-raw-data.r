
ProjectTemplate::reload.project(cache_loading = FALSE)
memory.limit(size=10000000000000)

datapath <- "C:/Users/Lina/STATISTIK/Projects/20200423_covid19/raw-data/"

# Import data from SCB ----------------------------------------------------

scbpath <- paste0(datapath, "20200612/SCB/Leverans/")

lisa <- readdata_sas(
  path = scbpath, filename = "lbenson_lev_lisa_2018",
  checkdups = TRUE
)
fodelseuppg <- readdata_sas(
  path = scbpath, filename = "lbenson_lev_fodelseuppg",
  checkdups = TRUE
)
migration <- readdata_sas(
  path = scbpath, filename = "lbenson_lev_migrationer",
  lopnr = "Lopnr"
)
rtb <- readdata_sas(
  path = scbpath, filename = "lbenson_lev_rtb_2019",
  checkdups = TRUE
)
# doddatum <- readdata_sas(path = scbpath, filename = "lbenson_lev_doddatum",
#                      checkdups = TRUE, lopnr = "Lopnr")
antalbarn <- readdata_sas(
  path = scbpath, filename = "lbenson_lev_antalbarn",
  checkdups = TRUE
)


# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawData_scb.RData", list = c(
  "lisa",
  "fodelseuppg",
  "migration",
  "rtb",
  "antalbarn"
))

# Import data from SoS ----------------------------------------------------

sospathorg <- paste0(datapath, "20200612/SOS/")
sospathnov <- paste0(datapath, "20201113/SOS/Ut_18194_2020/")
sospath2021 <- paste0(datapath, "20210617/SOS/lev4_18194_2020/")

## LM ---------------------------------------------------------------------

lm <- readdata_sas(path = sospathorg, filename = "ut_lmed_19_20_18194_2020/lmed_19_20_18194_2020", clean = FALSE)

### Store as RData in /data folder ----------------------------------------

save(file = "./data/rawData_sos_lm.RData", "lm")

lmsglt2 <- lm %>%
  mutate(
    atcneed = stringr::str_detect(ATC, "^A10")
  ) %>%
  filter(atcneed)

rm(lm)

## Only sglt2 ect LM -------------------------------------------------------

lmread <- function(years) {
  lmtmp <- readdata_sas(path = sospathorg, filename = paste0("ut_lmed_", years, "_18194_2020/lmed_", years, "_18194_2020"), clean = FALSE)
  lmtmp <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, "^A10")
    ) %>%
    filter(atcneed)

  lmsglt2 <<- rbind(lmsglt2, lmtmp)
}

lmread("17_18")
lmread("14_16")
lmread("11_13")
lmread("08_10")
lmread("05_07")

### Store as RData in /data folder ----------------------------------------

save(file = "./data/rawData_sos_lmsglt2.RData", "lmsglt2")

rm(lmsglt2)

## Death -----------------------------------------------------------------

dors <- readdata_sas(path = sospath2021, filename = "r_dors__18194_2020", clean = FALSE, checkdups = TRUE)
dors_cov <- readdata_sas(path = sospath2021, filename = "r_dors_hermes_18194_2020", clean = FALSE, checkdups = TRUE)

### Store as RData in /data folder ----------------------------------------

save(file = "./data/rawData_sos_dors.RData", list = c(
  "dors",
  "dors_cov"
))

## Patreg ------------------------------------------------------------------

sv <- readdata_sas(path = sospathorg, filename = "ut_par_97_18_18194_2020/par_sv_1997_2018_18194_2020", clean = FALSE)
save(file = "./raw-data/patreg/sv.RData", "sv")
rm(sv)

ov <- readdata_sas(path = sospathorg, filename = "ut_par_97_18_18194_2020/par_ov_1997_2018_18194_2020", clean = FALSE)
save(file = "./raw-data/patreg/ov.RData", "ov")
rm(ov)

sv_2019 <- readdata_sas(path = sospathnov, filename = "t_t_t_t_s_r_par_sv_18194_2020", clean = FALSE)
ov_2019 <- readdata_sas(path = sospathnov, filename = "t_t_t_t_s_r_par_ov_18194_2020", clean = FALSE)
sv_2020 <- readdata_sas(path = sospath2021, filename = "t_t_t_t_s_r_par_sv_18194_2020", clean = FALSE)
ov_2020 <- readdata_sas(path = sospath2021, filename = "t_t_t_t_s_r_par_ov_18194_2020", clean = FALSE)
sv_cov2021 <- readdata_sas(path = sospath2021, filename = "t_t_t_t_s_covparsv_18194_2020", clean = FALSE)
ov_cov2021 <- readdata_sas(path = sospath2021, filename = "t_t_t_t_s_covparov_18194_2020", clean = FALSE)

save(file = "./raw-data/patreg/svov1921.RData", list = c(
  "sv_2019",
  "ov_2019",
  "sv_2020",
  "ov_2020",
  "sv_cov2021",
  "ov_cov2021"
))

rm(list = c(
  "sv_2019",
  "ov_2019",
  "sv_2020",
  "ov_2020",
  "sv_cov2021",
  "ov_cov2021"
))
