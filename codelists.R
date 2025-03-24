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

# second attempt search in Synonyms >> this searches the concept synonym table to identify concepts to include
pregnancy_hypertension_codes1 <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = c("Pregnancy-Induced Hypertension",
               "pre eclampsia",
               "Gestational Hypertension"),
  exclude = c("postpartum"),
  domains = c("Condition", "Observation"),
  standardConcept = "Standard",
  searchInSynonyms = TRUE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# compare the two codelists
overlap_preg <- compareCodelists(pregnancy_hypertension_codes, 
                                 pregnancy_hypertension_codes1)

# you can just pull out the new ones
newCodes1To2 <- compareCodelists(pregnancy_hypertension_codes, 
                                 pregnancy_hypertension_codes1) |>
  filter(codelist == "Only codelist 2") |>
  select(-"codelist")



# get orphan codes (there might be some codes that you missed?)
orphan_codes_preg <- summariseOrphanCodes(x = list("pih" = pregnancy_hypertension_codes$concept_id),
                                     cdm = cdm)

#create a nice table
tableOrphanCodes(orphan_codes_preg)


# only want a specific domain?
subsetOnDomain(pregnancy_hypertension_codes, cdm, domain = "Condition")

# want to have a look at the code use? this gives some counts by record, person for each concept,
# can also look at concepts stratified by year, sex, age groups as well as specify a specific time window
sum_codelists_preg <- summariseCodeUse(pregnancy_hypertension_codes,
                                       cdm = cdm,
                                       countBy = c("record", "person"),
                                       byConcept = TRUE,
                                       byYear = FALSE,
                                       bySex = FALSE,
                                       ageGroup = NULL,
                                       dateRange = as.Date(c(NA, NA))
)

# create a pretty table
sum_codelists_preg_tidy <- summariseCodeUse(sum_codelists_preg,
                                            cdm = cdm)


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
  searchNonStandard = TRUE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

orphan_codes_thyroid <- summariseOrphanCodes(x = list("td" = thyroid_disorders_codes$concept_id),
                                          cdm = cdm)

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
