# create a study cohort (in this case gestational diabetes)
# first time in history
# using cohort constructor
# https://ohdsi.github.io/CohortConstructor/

cdm$study_cohort <- cdm %>% 
  CohortConstructor::conceptCohort(
    conceptSet = list("pih" = pregnancy_hypertension_codes$concept_id),
    name = "study_cohort") %>% 
  CohortConstructor::requireIsFirstEntry()


# OR you can create them all at the same time i.e gest diabetes, thyroid, covid vaccine
codes <- list("gest_hyp" = pregnancy_hypertension_codes$concept_id,
              "thyroid_dis" = thyroid_disorders_codes$concept_id,
              "covid_vaccine" = covid_vaccine_codes$concept_id)

#create the cohorts
cdm$my_cohort <- CohortConstructor::conceptCohort(cdm = cdm,
                               conceptSet = codes, 
                               exit = "event_end_date",
                               overlap = "merge",
                               name = "my_cohort") %>% 
  CohortConstructor::requireIsFirstEntry()

# look at counts
cohortCount(cdm$my_cohort)

# see what cohorts are which
settings(cdm$my_cohort)

# run the phenotypeDiagnostics using phenotypeR (just on gestational diabetes for now)
# https://ohdsi.github.io/PhenotypeR/
result <- phenotypeDiagnostics(cdm$study_cohort)

resultsFolder <- here("Results", db_name)

# output files ---- 
if (!file.exists(resultsFolder)){
  dir.create(resultsFolder, recursive = TRUE)}

# Save result with unique filename
PhenotypeR::exportSummarisedResult(
  result,
  fileName = paste0("phenotypeR_results_",db_name,".csv"),
  path = resultsFolder
)

# you can create a shiny from phenotypeR here
PhenotypeR::shinyDiagnostics(result, here())
