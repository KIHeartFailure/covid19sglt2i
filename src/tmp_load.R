
ProjectTemplate::reload.project(reset = TRUE)
memory.limit(size = 10000000000000)

load("./data/rawData_scb.RData")
load("./data/rawData_sos_dors.RData")

# run 01-02
source("./munge/01-scb.R")
source("./munge/02-death.R")

save(file = "./data/tmpdata/pop_scb_dors.RData", list = c("pop"))

ProjectTemplate::reload.project()
load(file = "./data/tmpdata/pop_scb_dors.RData")
load("./data/rawData_sos_lm.RData")

# run 03
source("./munge/03-sos_lm.R")

save(file = "./data/tmpdata/pop_scbdorslm.RData", list = c("pop", "metalm"))

ProjectTemplate::reload.project()
load(file = "./data/tmpdata/pop_scbdorslm.RData")
load("./data/rawData_sos_lmsglt2.RData")

# run 04
source("./munge/04-sos_lmsglt2_use.R")

save(file = "./data/tmpdata/pop_scbdorslm2.RData", list = c("pop", "metalm"))

ProjectTemplate::reload.project()
load(file = "./data/tmpdata/pop_scbdorslm2.RData")
load("./data/rawData_sos_patreg.RData")

# run 05
source("./munge/05-sos_outcom.R")

save(file = "./data/tmpdata/pop_scbdorslmpatreg.RData", list = c("pop", "metalm", "metaout"))

ProjectTemplate::reload.project()
load("./data/tmpdata/pop_scbdorslmpatreg.RData")

# run 06-09
source("./munge/06-inclusionexclusion.R")
source("./munge/07-createvars.R")
source("./munge/08-vars.R")
source("./munge/09-ps.R")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("pop")
ProjectTemplate::cache("metalm")
ProjectTemplate::cache("metaout")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarsns")

ProjectTemplate::cache("match_sglt2i")
ProjectTemplate::cache("match_glp1a")
ProjectTemplate::cache("match_dpp4i")

ProjectTemplate::cache("match_sglt2i_cov")
ProjectTemplate::cache("match_glp1a_cov")
ProjectTemplate::cache("match_dpp4i_cov")

# Rev comments so assuming all data cached previously
ProjectTemplate::reload.project()

# run 10-11 (rev comment)
load("./data/rawData_sos_lmsglt2_postc.RData")
source("./munge/10-sos_lm_postcovid.R")
rm(lmsglt2_postc)

load("./data/rawData_sos_patreg.RData")
source("./munge/11-sos_outcom_postcovid.R")

ProjectTemplate::cache("pop")
ProjectTemplate::cache("metaout")

ProjectTemplate::cache("match_sglt2i_cov")
ProjectTemplate::cache("match_glp1a_cov")
ProjectTemplate::cache("match_dpp4i_cov")
