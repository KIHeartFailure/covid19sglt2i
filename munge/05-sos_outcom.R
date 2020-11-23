

# Comorbidities -----------------------------------------------------------

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "hypertension",
  diakod = " I1[0-5]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "hf",
  diakod = " I110| I130| I132| I255| I420| I423| I42[5-9]| I43| I50| J81| K761| R570",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "diabetes",
  diakod = " E1[0-4]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "renal",
  diakod = " N1[7-9]| Z491| Z492",
  opkod = " KAS00| KAS10| KAS20| DR01[2-6]| DR020| DR023| DR024| TJA33| TJA35",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "ihd",
  diakod = " I2[0-5]| Z951| Z955",
  opkod = " FN[A-H]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "obesity",
  diakod = " E66",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "anemia",
  diakod = " D5| D6[0-4]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "mi",
  diakod = " I21| I22| I252",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "pci",
  opkod = " FNG",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "cabg",
  diakod = " Z951| Z955",
  opkod = " FN[A-F]| FNH",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  starttime = -1,
  type = "com",
  name = "peripheralartery",
  diakod = " I7[0-3]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "af",
  diakod = " I48",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "stroketia",
  diakod = " I6[0-4]| I69[0-4]| G45",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "valvular",
  diakod = " I0[5-8]| I3[4-9]| Q22| Q23[0-3]| Z95[2-4]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "hyperkalemia",
  diakod = " E875",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "hypokalemia",
  diakod = " E876",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "dialysis",
  diakod = " Z491| Z492",
  opkod = " DR01[2-6]| DR020| DR023| DR024| TJA33| TJA35",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "copd",
  diakod = " J4[0-4]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "liver",
  diakod = " B18| I85| I864| I982| K70| K710| K711| K71[3-7]| K7[2-4]| K760| K76[2-9]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "dementia",
  diakod = " F0[0-4]",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "cancer3y",
  diakod = " C",
  stoptime = -3 * 365.25,
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  starttime = -1,
  name = "muscoloskeletal3y",
  diakod = " M",
  stoptime = -3 * 365.25,
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  evar = ekod_all,
  type = "com",
  starttime = -1,
  name = "alcohol",
  diakod = " E244| E52| F10| G312| G621| G721| I426| K292| K70| K860| O354| P043| Q860| T51| Z502| Z714",
  ekod = " Y90| Y91",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "icdcrt",
  opkod = " FPG| FPE26",
  valsclass = "num",
  warnings = FALSE
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  starttime = -1,
  name = "bleed",
  diakod = " S06[4-6]| I850| I983| K226| K250| K252| K254| K256| K260| K262| K264| K266| K270| K272| K274| K276| K280| K284| K286| K290| K625| K661| K92[0-2]| H431| N02| R04| R58| T810| D629",
  opkod = " DR029",
  valsclass = "num",
  warnings = FALSE
)

# Covid -------------------------------------------------------------------

pop <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pop,
  patid = LopNr,
  indexdate = indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  starttime = 0,
  name = "hospcovidconfirmed",
  diakod = " U071",
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE,
  meta_reg = "NPR (in)"
)

pop <- pop %>%
  mutate_at(vars(starts_with("sos_out_")), ynfac) %>%
  mutate_at(vars(starts_with("sos_com_")), ynfac)

metaout <- metaout %>%
  mutate(
    Position = gsub("DIA_all", "All DIA", Position),
    Position = gsub("OP_all", "All OP", Position),
    Position = gsub("ekod_all", "All Ekod", Position),
    Period = gsub("-1--1095.75", "-3yrs--1", Period),
    Period = gsub("-1-", "--1", Period)
  )
