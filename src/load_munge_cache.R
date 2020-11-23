# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

memory.limit(size = 10000000000000)

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("flow")
ProjectTemplate::cache("pop")
ProjectTemplate::cache("metalm")
ProjectTemplate::cache("metaout")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarsns")

ProjectTemplate::cache("matchpop_sglt2i")
ProjectTemplate::cache("matchpop_glp1a")
ProjectTemplate::cache("matchpop_dpp4i")

ProjectTemplate::cache("match_sglt2i_iu")
ProjectTemplate::cache("match_glp1a_iu")
ProjectTemplate::cache("match_dpp4i_iu")

ProjectTemplate::cache("matchpop_sglt2i_cov")
ProjectTemplate::cache("matchpop_glp1a_cov")
ProjectTemplate::cache("matchpop_dpp4i_cov")
