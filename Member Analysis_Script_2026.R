
# (AP only) Establish Git Connection ------------------------------------------------
gitcreds::gitcreds_set()
# paste token when prompted -- selection, enter token if needed
usethis::use_github()
#currently using 'git push origin main' in Terminal to get around not having branch/head, which is stopping me from pushing from Git tab.
# Load Packages and Import/Join Files --------------------------------------------------------------

#install.packages(pacman)
#pacman::p_load()
#install.packages("devtools") #to get urbnmapr
library(pacman)
p_load(gitcreds, here, readxl, tidyverse, skimr, flextable, purrr, dplyr, janitor, tidygeocoder, plotly, urbnmapr)
AAPCHOMembers <- c("H80CS02327", "H80CS29016", "H80CS26615", "H80CS00773",
                   "H80CS26574", "H80CS02326", "H80CS00358", "H80CS04290",
                   "H80CS00722", "H80CS28986", "H80CS02468", "H80CS06640",
                   "H80CS24153", "H80CS00437", "H80CS31624", "H80CS00814",
                   "H80CS00776", "H80CS33646", "H80CS35350", "H80CS08775",
                   "H80CS00397", "H80CS26582", "H80CS00221", "H80CS26623",
                   "H80CS00600", "H80CS06653", "H80CS00807", "H80CS00646")
read_aapcho <- function(file, sheet, col_select = NULL) {
  df <- read_excel(file, sheet = sheet)
  
  if (!is.null(col_select)) {
    keep <- union(which(names(df) == "GrantNumber"), col_select)
  } else {
    keep <- union(which(names(df) == "GrantNumber"), setdiff(seq_along(df), 1))
  }
  
  df[keep] |> filter(GrantNumber %in% AAPCHOMembers)
}

load_year <- function(file, year) {
  
  hci <- read_excel(file, sheet = "HealthCenterInfo") |>
    select(GrantNumber, ReportingYear, HealthCenterName, HealthCenterCity,
           HealthCenterState, HealthCenterZIPCode, FundingCHC, FundingMHC,
           FundingHO, FundingPH, UrbanRuralFlag) |>
    filter(GrantNumber %in% AAPCHOMembers)
  
  tables <- list(
    read_aapcho(file, "Table3A"),
    read_aapcho(file, "Table3B"),
    read_aapcho(file, "Table4", col_select = 2:30),
    read_aapcho(file, "Table5"),
    read_aapcho(file, "Table6A"),
    read_aapcho(file, "Table6B"),
    read_aapcho(file, "Table7_1"),
    read_aapcho(file, "Table7_2"),
    read_aapcho(file, "Table8A"),
    read_aapcho(file, "Table9E"),
    read_aapcho(file, "HITInformation"),
    read_aapcho(file, "OtherDataElements")
  )
  
  merged <- c(list(hci), tables) |>
    reduce(full_join, by = "GrantNumber") |>
    filter(!is.na(GrantNumber), !grepl("^-+$", GrantNumber))
  
  keep_as_character <- c("GrantNumber", "HealthCenterName", "HealthCenterCity",
                         "HealthCenterState", "HealthCenterZIPCode",
                         "UrbanRuralFlag", "ReportingYear")
  merged[keep_as_character] <- lapply(merged[keep_as_character], as.character)
  
  cols_to_convert <- setdiff(names(merged), keep_as_character)
  merged[cols_to_convert] <- lapply(merged[cols_to_convert], as.numeric)
  
  merged
}
year_files <- list(
  "2021" = here("H802021.xlsx"),
  "2022" = here("H802022.xlsx"),
  "2023" = here("H802023.xlsx"),
  "2024" = here("H802024.xlsx"))
all_years <- imap(year_files, ~load_year(.x, .y))
AAPCHOMembers20212025 <- bind_rows(all_years)
#Results File -- need to recommit to Git 
pacman::p_load(here, rio)
ResultsFile <- import(here("Results.xlsx"))
# Data Cleaning/Checks ---------------------------------------------------------
##Reporting years
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  skimr::skim(GrantNumber)
AAPCHOMembers20212025 %>%
  group_by(GrantNumber) %>%
  skimr::skim(ReportingYear, 2021)

##AA, NH, PI
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  skimr::skim(T3b_L1_Cd)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear, GrantNumber) %>%
  skimr::skim(T3b_L1_Cd)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  skimr::skim(T3b_L2a_Cd)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  skimr::skim(T3b_L2b_Cd)


# Analysis ----------------------------------------------------------------
# General  ----------------------------------------------------------------
##Statesterritories, urban/rural, patient count groupings, 
###States organized most to least
AAAPCHOMembers20212025 %>%
  tabyl(ReportingYear, HealthCenterState) %>%
  adorn_totals(where = "col") %>%        # add Total FIRST before select
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  select(ReportingYear, CA, HI, NY, FM, MA, GU, MP, AR, IL, LA, OH, TX, WA, Total) %>%  # reorder AFTER all adorn steps
  flextable::flextable() %>%
  flextable::autofit()



AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L39_Ca + T3a_L39_Cb, na.rm = TRUE))



# AAPCHO Member Demographics (3A, 3B) -------------------------------------


#AAPCHO Members Count
#sum(AAPCHOMembers20212025$T3a_L39_Ca[AAPCHOMembers20212025$ReportingYear == "2024"], na.rm = TRUE)
##Checked against Excel spreadsheet. Both = 287,124
#run sum across all years and check against all years
###Patient Counts
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L39_Ca + T3a_L39_Cb, na.rm = TRUE))
###Gender 
##Male
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L39_Ca, na.rm = TRUE))
##Female
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L39_Cb, na.rm = TRUE))

###Age
##under 1 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L1_Ca + T3a_L1_Cb, na.rm = TRUE))
##1-4
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L2_Ca + T3a_L2_Cb + T3a_L3_Ca + T3a_L3_Cb + T3a_L4_Ca + T3a_L4_Cb + T3a_L5_Ca + T3a_L5_Cb, na.rm = TRUE))
##5-12 years 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L6_Ca+
                          T3a_L6_Cb+
                          T3a_L7_Ca+
                          T3a_L7_Cb+
                          T3a_L8_Ca+
                          T3a_L8_Cb+
                          T3a_L9_Ca+
                          T3a_L9_Cb+
                          T3a_L10_Ca+
                          T3a_L10_Cb+
                          T3a_L11_Ca+
                          T3a_L11_Cb+
                          T3a_L12_Ca+
                          T3a_L12_Cb+
                          T3a_L13_Ca+
                          T3a_L13_Cb, na.rm = TRUE))
##13-19
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L14_Ca+
                          T3a_L14_Cb+
                          T3a_L15_Ca+
                          T3a_L15_Cb+
                          T3a_L16_Ca+
                          T3a_L16_Cb+
                          T3a_L17_Ca+
                          T3a_L17_Cb+
                          T3a_L18_Ca+
                          T3a_L18_Cb+
                          T3a_L19_Ca+
                          T3a_L19_Cb+
                          T3a_L20_Ca+
                          T3a_L20_Cb, na.rm = TRUE))
##20-29
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L21_Ca+
                          T3a_L21_Cb+
                          T3a_L22_Ca+
                          T3a_L22_Cb+
                          T3a_L23_Ca+
                          T3a_L23_Cb+
                          T3a_L24_Ca+
                          T3a_L24_Cb+
                          T3a_L25_Ca+
                          T3a_L25_Cb+
                          T3a_L26_Ca+
                          T3a_L26_Cb, na.rm = TRUE))
##30-49
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L27_Ca+
                          T3a_L27_Cb+
                          T3a_L28_Ca+
                          T3a_L28_Cb, na.rm = TRUE))
##40-49
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L29_Ca+
                          T3a_L29_Cb+
                          T3a_L30_Ca+
                          T3a_L30_Cb, na.rm = TRUE))
##50-59
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L31_Ca+
                          T3a_L31_Cb+
                          T3a_L32_Ca+
                          T3a_L32_Cb, na.rm = TRUE))
##60-64
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L33_Ca+
                          T3a_L33_Cb, na.rm = TRUE))
##65+
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3a_L34_Ca+
                          T3a_L34_Cb+
                          T3a_L35_Ca+
                          T3a_L35_Cb+
                          T3a_L36_Ca+
                          T3a_L36_Cb+
                          T3a_L37_Ca+
                          T3a_L37_Cb+
                          T3a_L38_Ca+
                          T3a_L38_Cb, na.rm = TRUE))



#AAPCHO Members - Patient Demographics
##Total Asian 2021-2025 
Total_Asian <- AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L1_Cd, na.rm = TRUE))
##Total NH 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L2a_Cd, na.rm = TRUE))
##Total PI 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L2b_Cd, na.rm = TRUE))
##Total Black/AA 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L3_Cd, na.rm = TRUE))
##Total AIAN 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L4_Cd, na.rm = TRUE))
##Total White 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L5_Cd, na.rm = TRUE))
##Total More than one 2021-2025 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L6_Cd, na.rm = TRUE))
##Total Unreported 2021-2025
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(T3b_L7_Cd, na.rm = TRUE))


#Disaggregated AANHPI Race Summary
##AA
options(max.print = 100000)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(Asian_Indian = sum(T3b_L1a_Cd, na.rm = TRUE),
            Chinese = sum(T3b_L1b_Cd, na.rm = TRUE),
            Filipino = sum(T3b_L1c_Cd, na.rm = TRUE),
            Japanese = sum(T3b_L1d_Cd, na.rm = TRUE),
            Korean = sum(T3b_L1e_Cd, na.rm = TRUE),
            Vietnamese = sum(T3b_L1f_Cd, na.rm = TRUE),
            Other_Asian = sum(T3b_L1g_Cd, na.rm = TRUE), 
            Total_Asian = sum(T3b_L1_Cd, na.rm = TRUE))
##checking why Total Asian is more than each category added. Likely suppression. Testing:
#count of AAPCHO Members reporting NOT SUPPRESSED
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    Asian_Indian = sum(!is.na(T3b_L1a_Cd)),
    Chinese      = sum(!is.na(T3b_L1b_Cd)),
    Filipino     = sum(!is.na(T3b_L1c_Cd)),
    Japanese     = sum(!is.na(T3b_L1d_Cd)),
    Korean       = sum(!is.na(T3b_L1e_Cd)),
    Vietnamese   = sum(!is.na(T3b_L1f_Cd)),
    Other_Asian  = sum(!is.na(T3b_L1g_Cd)),
    Total_Asian  = sum(!is.na(T3b_L1_Cd))
  )

#count of SUPPRESSED cells 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    Asian_Indian = sum(is.na(T3b_L1a_Cd)),
    Chinese      = sum(is.na(T3b_L1b_Cd)),
    Filipino     = sum(is.na(T3b_L1c_Cd)),
    Japanese     = sum(is.na(T3b_L1d_Cd)),
    Korean       = sum(is.na(T3b_L1e_Cd)),
    Vietnamese   = sum(is.na(T3b_L1f_Cd)),
    Other_Asian  = sum(is.na(T3b_L1g_Cd)),
    Total_Asian  = sum(is.na(T3b_L1_Cd))
  )
##check complete. Suppressed and non-suppressed add to 28. 
##NHPI
options(max.print = 1000000)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(Native_Hawaiian = sum(T3b_L2a_Cd, na.rm = TRUE),
            Other_Pacific_Islander = sum(T3b_L2b_Cd, na.rm = TRUE),
            Guamanian_or_Chamorro = sum(T3b_L2c_Cd, na.rm = TRUE),
            Samoan = sum(T3b_L2d_Cd, na.rm = TRUE),
            Total_NH_and_Other_Pacific_islander = sum(T3b_L2_Cd, na.rm = TRUE))
#AANHPI Distribution by State
AAPCHOMembers20212025 %>%
  group_by(ReportingYear, HealthCenterState) %>%
  summarise(
    Asian_Indian = sum(T3b_L1a_Cd, na.rm = TRUE),
    Chinese      = sum(T3b_L1b_Cd, na.rm = TRUE),
    Filipino     = sum(T3b_L1c_Cd, na.rm = TRUE),
    Japanese     = sum(T3b_L1d_Cd, na.rm = TRUE),
    Korean       = sum(T3b_L1e_Cd, na.rm = TRUE),
    Vietnamese   = sum(T3b_L1f_Cd, na.rm = TRUE),
    Other_Asian  = sum(T3b_L1g_Cd, na.rm = TRUE),
    Native_Hawaiian = sum(T3b_L2a_Cd, na.rm = TRUE),
    Other_Pacific_Islander = sum(T3b_L2b_Cd, na.rm = TRUE),
    Guamanian_or_Chamorro = sum(T3b_L2c_Cd, na.rm = TRUE),
    Samoan = sum(T3b_L2d_Cd, na.rm = TRUE),
    .groups = "drop"
  )

#Best Served in a Language Other than English 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T3b_L12_Ca, na.rm = TRUE))


# Table 5 - FTEs, Visits --------------------------------------------------
##FTEs 
#widen tibble display
options(pillar.width = Inf,
        +         pillar.sigfig = 4,
        +         dplyr.width = Inf)
###Family Physicians T5_L1_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L1_Ca, na.rm = TRUE))
###GPs T5_L2_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L2_Ca, na.rm = TRUE))
###Internists T5_L3_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L3_Ca, na.rm = TRUE))
###OB/GYNs T5_L4_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L4_Ca, na.rm = TRUE))
###Peds T5_L5_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L5_Ca, na.rm = TRUE))
###Other Specialty Physicians T5_L7_Ca
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L7_Ca, na.rm = TRUE))
#CHECK# Total Physician Count
##Discrepancy by 4 FTEs - likely due to other physician type not listed. Lot of suppression in 2021. CHECK BACK.
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L8_Ca, na.rm = TRUE))
##Physician Averages#
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    FM        = mean(T5_L1_Ca, na.rm = TRUE),
    GP     = mean(T5_L2_Ca, na.rm = TRUE),
    IM    = mean(T5_L3_Ca, na.rm = TRUE),
    OBGYN = mean(T5_L4_Ca, na.rm = TRUE),
    Peds        = mean(T5_L5_Ca, na.rm = TRUE),
    OtherSpec        = mean(T5_L7_Ca, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

#Total NPs, PAs, CNMs FTEs
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L10a_Ca, na.rm = TRUE))
#Total Medical Care Services FTEs 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L15_Ca, na.rm = TRUE))
#Total Med Patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L15_Cc, na.rm = TRUE))
#Total Dental Services FTEs
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L19_Ca, na.rm = TRUE))
#Total Dental Patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L19_Cc, na.rm = TRUE))
#Total Mental Health Services FTEs
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L20_Ca, na.rm = TRUE))
#Total Mental Health Patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L20_Cc, na.rm = TRUE))
#SUD FTEs
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L21_Ca, na.rm = TRUE))
#SUD Patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L21_Cc, na.rm = TRUE))
#Total Vision Services FTEs
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L22d_Ca, na.rm = TRUE))
#Total Vision Patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L22d_Cc, na.rm = TRUE))
#Pharmacy Personnel - 2021-2022
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L23_Ca, na.rm = TRUE))
#Pharmacy Personnel - 2023-2025 #left off here 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
ClinicalPharm        = sum(T5_L1_Ca, na.rm = TRUE),
PharmTech     = sum(T5_L2_Ca, na.rm = TRUE),
OtherPharmPersonnel    = sum(T5_L3_Ca, na.rm = TRUE),
TotalPharmPersonnel = sum(T5_L4_Ca, na.rm = TRUE)) 
 

#Total Enabling Services 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L29_Ca, na.rm = TRUE))
##Case Managers
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L24_Ca, na.rm = TRUE))
##Health Education Specialists
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L25_Ca, na.rm = TRUE))
##Outreach Workers
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L26_Ca, na.rm = TRUE))
##Transportation Personnel
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L27_Ca, na.rm = TRUE))
##Eligibility Assistance Workers
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L27a_Ca, na.rm = TRUE))
##Interpretation Personnel
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L27b_Ca, na.rm = TRUE))
##Community Health Workers 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L27c_Ca, na.rm = TRUE))
##Other Enabling Services 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T5_L28_Ca, na.rm = TRUE))
#? Maybe some other services (QI, patient support personnel). Argument to be made that if clinical quality performance better than average (and services/staff-patient ratio), still cost effective if employing more FTEs/patient.
##Visit Counts

# Table 6A ----------------------------------------------------------------
##Tuberculosis ##need to check if this is new diagnosis or can include existing
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L3_Cb, na.rm = TRUE))


# Table 6B ----------------------------------------------------------------


# Table 7 -----------------------------------------------------------------



# Table 8 -----------------------------------------------------------------


# Table 9D ----------------------------------------------------------------


# HIT ---------------------------------------------------------------


# Addendum - WKFC ---------------------------------------------------------


# DATA VIZ ----------------------------------------------------------------
##Patient Counts

# #total AANHPI AAPCHO Members
# totalAANHPIAAPCHO <- sum(rowSums(AAPCHOMember[, c("T3b_L1_Cd", "T3b_L2_Cd")], na.rm = TRUE), na.rm = TRUE)
# AAPCHOMember[cols_to_convert] <- lapply(AAPCHOMember[cols_to_convert], as.numeric)
# AAPCHO_row_totals <- rowSums(AAPCHOMember[, c("T3b_L1_Cd", "T3b_L2_Cd")], na.rm = TRUE)
# summary(AAPCHO_row_totals)
# ggplot(data.frame(AAPCHO_row_totals), aes(x = AAPCHO_row_totals)) +
#   # IQR shaded band
#   annotate("rect", xmin = 3193, xmax = 15303, ymin = 0, ymax = Inf,
#            fill = "lavender", alpha = 0.15) +
#   geom_histogram(bins = 15, fill = "lavender", color = "white") +
#   scale_x_continuous(labels = scales::comma) +
#   # Vertical lines
#   geom_vline(xintercept = 5856,  color = "orange", linetype = "dashed", linewidth = 0.8) +
#   geom_vline(xintercept = 12885, color = "red",    linetype = "dashed", linewidth = 0.8) +
#   geom_vline(xintercept = 3193,   color = "gray40", linetype = "dotted", linewidth = 0.7) +
#   geom_vline(xintercept = 15303,  color = "gray40", linetype = "dotted", linewidth = 0.7) +
#   # Labels
#   annotate("text", x = 5856,  y = Inf, label = "Median\n5856",  
#            vjust = 1.5, hjust = -0.1, color = "orange", size = 3.5) +
#   annotate("text", x = 12885, y = Inf, label = "Mean\n12,885", 
#            vjust = 1.5, hjust = -0.1, color = "red",    size = 3.5) +
#   annotate("text", x = 3,193,   y = Inf, label = "Q1\n3193",      
#            vjust = 3.5, hjust = -0.1, color = "gray40", size = 3) +
#   annotate("text", x = 15303,  y = Inf, label = "Q3\n736",     
#            vjust = 3.5, hjust = -0.1, color = "gray40", size = 3) +
#   # IQR bracket label
#   annotate("text", x = sqrt(3193 * 15303), y = Inf,
#            label = "Middle 50% of orgs", vjust = 5, color = "lavender", 
#            size = 3, fontface = "italic") +
#   labs(title = "Distribution of AANHPI Patients per AAPCHO Member Health Center",
#        x = "Total AANHPI Patients",
#        y = "Number of AAPCHO Member Health Centers") +
#   theme_minimal()

# ##DATA VIZ## for map 
# #downloading urbnmapr for better viz of territories#
# gitcreds::gitcreds_set()
# #new token through personal gmail 
# remotes::install_github("UrbanInstitute/urbnmapr")
# #from https://urbaninstitute.github.io/urbnmapr/articles/introducing-urbnmapr.html
# library(tidyverse)
# library(urbnmapr)
# 
# states %>%
#   ggplot(aes(long, lat, group = group)) +
#   geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)
# 
# ccdf <- get_urbn_map(map = "territories_counties")
# 
# ccdf %>%
#   ggplot(aes(long, lat, group = group)) +
#   geom_polygon(fill = "grey", color = "#ffffff", size = 0.25) +
#   scale_x_continuous(limits = c(-141, -55)) +
#   scale_y_continuous(limits = c(24, 50)) +  
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)
# 
# 
# ccdf <- get_urbn_map(map = "ccdf")
# ccdf_labels <- get_urbn_labels(map = "ccdf")
# 
# ccdf %>%
#   ggplot() +
#   geom_polygon(aes(long, lat, group = group), 
#                fill = "grey", color = "#ffffff", size = 0.25) +
#   geom_text(data = ccdf_labels, aes(long, lat, label = state_abbv), size = 3) +  
#   scale_x_continuous(limits = c(-141, -55)) +
#   scale_y_continuous(limits = c(24, 50)) +  
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)

#AAPCHO Members - AANHPI Considerations 

#AAPCHO Members - Cost/Finance and Clinical Quality Outcomes 

#AAPCHO Members - Workforce

#AAPCHO Members - HIT

