# Alcohol Risk Factors analysis with classification trees
# Catalina Canizares
# Sept 27 2023

library(tidymodels)
library(tidyverse)
library(dissertationData)
library(gtsummary)


# Data --------------------------------------------------------------------

data("clean_yrbs_2021")

analysis <-
  clean_yrbs_2021 |>
  select(- c(Weight, DrivingDrinking, CurrentlySmokingCigarette,
             StoreSourceVaping, CurrentlySmokingCigar, CurrentlyBingeDrinking,
             UseCondom, NoFruitJuice, NoSalad, NoCarrots, NoSoda, NoBreakfast,
             HIVTested, DentistVisit, NoSportsDrinks, ExcerciseMuscles, Height,
             SexofSexualContact, DrinkingDriver, TextingDriving,
             MoreThan10CigPerDay, CurrentlyUseMarihuana , Sex4OrMorePartners,
             AlcoholOrDrugsSex, BirthControl, WeightLoss, NoFruit,
             NoPotatoes, NoOtherVeggies, NoMilk, PhysicalActivity,
             AttendedPEClass, STDTested, NoDrinksWater, Sunburn,
             EnglishProficiency, CurrentlySmokelessTobacco, MoreThan10Drinks,
             FirstAlcoholBefore13, SexBefore13, SexuallyActive, EverHadSex,
             FirstMarihuanaBefore13, SuicideInjuryMedicalAttention
             ))


# Demogrpahic Table -------------------------------------------------------

# This options worked to fix the warning: "Stratum (103) has only one
#  PSU at stage 1"
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")


alcohol_use_demographics <-
  suicide_ideation_demographics |>
  mutate(SuicideIdeation = case_when(
    SuicideIdeation == 0 ~ "No",
    SuicideIdeation == 1 ~ "Yes"
  )) |>
  mutate(Grade = factor(Grade, levels = c("9", "10", "11", "12"))) |>
  mutate(SuicideIdeation = factor(SuicideIdeation, levels = c("Yes", "No")))


survey::svydesign(
  data = suicide_ideation_demographics_1,
  ids = ~psu,
  weights = ~weight,
  strata = ~stratum,
  nest = TRUE
) |>
  tbl_svysummary(
    include = c(SuicideIdeation, Sex, Race, SexOrientation, Grade),
    type = SuicideIdeation ~ "categorical",
    sort = c(SexOrientation, Race) ~ "frequency") |>
  modify_caption("Demographic Characteritics for Suicide Ideation YRBS 2019") |>
  bold_labels()
