

# Establish Git Connection ------------------------------------------------
gitcreds::gitcreds_set()
# paste token when prompted -- selection, enter token if needed
usethis::use_github()
# File Setup --------------------------------------------------------------


#install.packages(pacman)
#pacman::p_load()
#install.packages("devtools") #to get urbnmapr
library(here)
library(tidyverse)
library(readxl)
library(purrr)
library(dplyr)
library(tidygeocoder)
library(plotly)
library(urbnmapr)
library(gitcreds)
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

###Visit Counts
###States

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
##Total White 2021-2025 
sum(AAPCHOMembers20212025$T3b_L6_Cd[AAPCHOMembers20212025$ReportingYear == "2021"], na.rm = TRUE)
sum(AAPCHOMembers20212025$T3b_L6_Cd[AAPCHOMembers20212025$ReportingYear == "2022"], na.rm = TRUE)
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
T3b_L12_Ca
# DATA VIZ ----------------------------------------------------------------


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
#test
##test
