# code list generator ----
# https://darwin-eu.github.io/CodelistGenerator/index.html

# 1 gestational hypertension/pregnancy induced hypertension 
# 2 thyroid disorders
# 3 covid vaccines by brand

# 1 -----
# find the initial search terms
# search MeSH
#https://www.ncbi.nlm.nih.gov/mesh/?term=gestational+hypertension
# OR
# diamed
# https://libguides.bodleian.ox.ac.uk/clinical/dynamed
# OR
# athena
#https://athena.ohdsi.org/search-terms/start

# codelists ----------------
pregnancy_hypertension_codes <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("Pregnancy-Induced Hypertension",
               "pre eclampsia",
               "Gestational Hypertension"),
  exclude = c("postpartum"),
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# have a look at the record and person counts from achilles
result_achilles_preg <- summariseAchillesCodeUse(list(pih = pregnancy_hypertension_codes$concept_id), cdm = cdm)

# second attempt search in Synonyms >> this searches the concept synonym table to identify concepts to include
# also set search in non standard to TRUE 
pregnancy_hypertension_codes1 <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("Pregnancy-Induced Hypertension",
               "pre eclampsia",
               "Gestational Hypertension"),
  exclude = c("postpartum"),
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = TRUE,
  searchNonStandard = TRUE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# compare the two codelists
overlap_preg <- compareCodelists(pregnancy_hypertension_codes, 
                                 pregnancy_hypertension_codes1)

# you can just extract out the new ones from new search
newCodes1To2 <- compareCodelists(pregnancy_hypertension_codes, 
                                 pregnancy_hypertension_codes1) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")

# want to see how many codes from your codelist are actually being used? You can subset
subset_codes <- subsetToCodesInUse(cdm = cdm ,
                                   list("pih" = pregnancy_hypertension_codes$concept_id))


# get orphan codes (there might be some codes that you missed?)
orphan_codes_preg <- summariseOrphanCodes(x = list("pih" = pregnancy_hypertension_codes$concept_id),
                                     cdm = cdm)

#create a nice table
tableOrphanCodes(orphan_codes_preg)

# only want a specific domain?
preg_domain <- subsetOnDomain(list(pih = pregnancy_hypertension_codes$concept_id), cdm, domain = "Condition")

# want to have a look at the code use? this gives some counts by record, person for each concept,
# can also look at concepts stratified by year, sex, age groups as well as specify a specific time window
# this section of code can take some time
sum_codelists_preg <- summariseCodeUse(list(pih = pregnancy_hypertension_codes$concept_id),
                                       cdm = cdm,
                                       countBy = c("record", "person"),
                                       byConcept = TRUE,
                                       byYear = FALSE,
                                       bySex = FALSE,
                                       ageGroup = NULL,
                                       dateRange = as.Date(c(NA, NA))
)

# create a pretty table
sum_codelists_preg_tidy <- tableCodeUse(sum_codelists_preg)


# have a look for unmapped codes?
unmapped_preg <- summariseUnmappedCodes(
  list(pih = pregnancy_hypertension_codes$concept_id),
  cdm = cdm,
  table = c("condition_occurrence",
            "observation")
)

# create a pretty table
tableUnmappedCodes(unmapped_preg)

# 2 ---------------
# disorder of thyroid
#https://athena.ohdsi.org/search-terms/terms/141253
#https://www.ncbi.nlm.nih.gov/mesh/?term=thyroid+diseases

# codelist
thyroid_disorders_codes <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("disorder of thyroid", "thyroid disease"),
  exclude = c("neoplasm"),
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# get orphan codes
orphan_codes_thyroid <- summariseOrphanCodes(x = list("td" = thyroid_disorders_codes$concept_id),
                                          cdm = cdm)

#create a nice table
tableOrphanCodes(orphan_codes_thyroid)

# 3 ------------------------------
# covid vaccines
covid_vaccine_codes <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("covid vaccine"),
  domains = c("Drug"),
  standardConcept = "Standard",
  searchInSynonyms = TRUE,
  searchNonStandard = TRUE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# 4 ---------------------------------
# sepsis (added this one in for fun :P)
sepsis_codes <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("sepsis", "septic shock"),
  exclude = c("neonatal", "at risk", "no current evidence", "family education about sepsis", "neonate", "newborn", "miscarriage", "pregnancy", "labor", "fetus", "perinatal"),
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# 5 -----------------------------------
# antibiotics (example for extracting concepts with different routes/domains)
ciprofloxacin_codes <- getDrugIngredientCodes(
  cdm,
  name = "ciprofloxacin",
  nameStyle = "{concept_code}_{concept_name}",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  ingredientRange = c(1, Inf),
  type = "codelist"
)

# get the codelists stratified by route and keep the original codelist
ciprofloxacin_codes_by_route <- stratifyByRouteCategory(ciprofloxacin_codes, cdm, keepOriginal = TRUE)

# next step do we want to phenotype these codelists? uncomment next line or 
# open up the file called phenotyper.R
# source(here("phenotyper.R"))

