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

# get orphan codes
orphan_codes_preg <- summariseOrphanCodes(x = list("pih" = pregnancy_hypertension_codes$concept_id),
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


# sepsis
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
