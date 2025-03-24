# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

# packages ---
library(here)
library(DBI)
library(CDMConnector)
library(dplyr)
library(tidyr)
library(CodelistGenerator)
library(RPostgres)
library(odbc)

# db with vocab ----
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..."


# connect
db <- DBI::dbConnect("...",
                     dbname = server_dbi,
                     port = port,
                     host = host,
                     user = user,
                     password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_schema <- "..."

# The name of the schema where results tables will be created
write_schema <- "..."

# The name of the schema which contains the achilles results
achilles_schema <- "..."

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "..."

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten
study_prefix <- "..."

# create cdm reference ----
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdm_schema,
  writeSchema = write_schema,
  achillesSchema = achilles_schema,
  cdmName = db_name,
  writePrefix = study_prefix
)

# check patient numbers
cdm$person %>%
  tally()
