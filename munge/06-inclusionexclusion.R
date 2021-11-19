
# Inclusion/exclusion criteria --------------------------------------------

flow <- c(
  "No patients recieved from Socialstyrelsen", nrow(pop)
)

pop <- pop %>%
  filter(sos_com_diabetes == "Yes")

flow <- rbind(flow, c(paste0("Include patients with diabetes type II 1997/1998-01-01 -- ", global_indexdtm), nrow(pop)))

pop <- pop %>%
  filter(is.na(sos_deathdtm) | sos_deathdtm >= ymd(global_indexdtm))

flow <- rbind(flow, c(paste0("Exclude patients died prior to ", global_indexdtm), nrow(pop)))

pop <- pop %>%
  filter(is.na(scb_emigrated))

flow <- rbind(flow, c("Exclude patients emigrated prior to 2019-12-31", nrow(pop)))

pop <- pop %>%
  filter(is.na(scb_immigratedpost2015))

flow <- rbind(flow, c("Exclude patients immigrated post 2015-01-01", nrow(pop)))

pop <- pop %>%
  filter(is.na(scb_atersenpnr)) # reused/changed personr

flow <- rbind(flow, c("Exclude patients with reused or changed PINs", nrow(pop)))

pop <- pop %>%
  filter(scb_age >= 18)

flow <- rbind(flow, c("Exclude patients < 18 years", nrow(pop)))

pop <- pop %>%
  filter(!is.na(scb_countryofbirth) & !is.na(scb_dispincome) &
    !is.na(scb_education) & !is.na(scb_famtype) & !is.na(scb_region))

flow <- rbind(flow, c("Exclude patients with missing data for SCB variables included in the models", nrow(pop)))

pop <- pop %>%
  select(-scb_emigrated, -scb_immigratedpost2015, -scb_atersenpnr)

flow <- rbind(flow, c(".  Patients with Covid-19 and 30 days follow-up", 
                      nrow(pop %>% filter(sos_covidconfirmed == "Yes" & !is.na(sos_out_death30d)))))
