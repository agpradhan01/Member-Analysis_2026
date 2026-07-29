read_uds <- function(file, sheet, col_select = NULL, filter_ids = NULL) {
  df <- read_excel(file, sheet = sheet)
  
  if (!is.null(col_select)) {
    keep <- union(which(names(df) == "GrantNumber"), col_select)
  } else {
    keep <- union(which(names(df) == "GrantNumber"), setdiff(seq_along(df), 1))
  }
  
  df <- df[keep] %>%
    filter(grepl("^H80", GrantNumber))    # <-- this is the line, right here
  
  if (!is.null(filter_ids)) {
    df <- df %>% filter(GrantNumber %in% filter_ids)
  }
  
  df
}




load_year <- function(file, filter_ids = NULL) {
  
  hci <- read_excel(file, sheet = "HealthCenterInfo") |>
    select(GrantNumber, ReportingYear, HealthCenterName, HealthCenterCity,
           HealthCenterState, HealthCenterZIPCode, FundingCHC, FundingMHC,
           FundingHO, FundingPH, UrbanRuralFlag) |>
            filter(grepl("^H80", GrantNumber))
  
  if(!is.null(filter_ids)) {
    hci <- hci |> filter(GrantNumber %in% filter_ids)
  }
  
  tables <- list(
    read_UDS(file, "Table3A", filter_ids = filter_ids),
    read_UDS(file, "Table3B", filter_ids = filter_ids),
    read_UDS(file, "Table4", col_select = 2:30, filter_ids = filter_ids),
    read_UDS(file, "Table5", filter_ids = filter_ids),
    read_UDS(file, "Table6A", filter_ids = filter_ids),
    read_UDS(file, "Table6B", filter_ids = filter_ids),
    read_UDS(file, "Table6BClinicalmeasures", filter_ids = filter_ids),
    read_UDS(file, "Table7_1", filter_ids = filter_ids),
    read_UDS(file, "Table7_2", filter_ids = filter_ids),
    read_UDS(file, "Table8A", filter_ids = filter_ids),
    read_UDS(file, "Table9E", filter_ids = filter_ids),
    read_UDS(file, "HITInformation", filter_ids = filter_ids),
    read_UDS(file, "OtherDataElements", filter_ids = filter_ids),
    read_UDS(file, "Workforce", filter_ids = filter_ids)
  )
  
  lapply(c(list(hci), tables), function(df) {
    if (any(duplicated(df$GrantNumber))) {
      stop("Duplicate GrantNumbers found in one of the tables — check before joining.")
    }
  })
  merged <- c(list(hci), tables) |>
    reduce(full_join, by = "GrantNumber") |>
    filter(grepl("^H80", GrantNumber))
  
  keep_as_character <- c("GrantNumber", "HealthCenterName", "HealthCenterCity",
                         "HealthCenterState", "HealthCenterZIPCode",
                         "UrbanRuralFlag", "ReportingYear")
  merged[keep_as_character] <- lapply(merged[keep_as_character], as.character)
  
  cols_to_convert <- setdiff(names(merged), keep_as_character)
  merged[cols_to_convert] <- lapply(merged[cols_to_convert], as.numeric)
  
  merged
}




year_files <- list(
  here("H802021.xlsx"),
  here("H802022.xlsx"),
  here("H802023.xlsx"),
  here("H802024.xlsx")
)





National20212024 <- map(year_files, ~load_year(.x, filter_ids = NULL)) %>%
  bind_rows()

AAPCHOMembers <- c("H80CS02327", "H80CS29016", "H80CS26615", "H80CS00773",
                   "H80CS26574", "H80CS02326", "H80CS00358", "H80CS04290",
                   "H80CS00722", "H80CS28986", "H80CS02468", "H80CS06640",
                   "H80CS24153", "H80CS00437", "H80CS31624", "H80CS00814",
                   "H80CS00776", "H80CS33646", "H80CS35350", "H80CS08775",
                   "H80CS00397", "H80CS26582", "H80CS00221", "H80CS26623",
                   "H80CS00600", "H80CS06653", "H80CS00807", "H80CS00646")

AAPCHOMembers20212024 <- National20212024 %>%
  filter(GrantNumber %in% AAPCHOMembers)

##Checks
#should be: up to 1,360 orgs × 4 years
nrow(National20212024)
#should be close to 1,360 (or fewer, if some HCs did report every year)
n_distinct(National20212024$GrantNumber)  
##was 1373 
#investigating which additional 3 health centers added
orgs_2021 <- National20212024 %>%
  filter(ReportingYear == 2021) %>%
  pull(GrantNumber) %>%
  unique()

all_orgs <- unique(National20212024$GrantNumber)

#HCs that appear in the full dataset but NOT in 2021
setdiff(all_orgs, orgs_2021)
##"H80CS45850" "H80CS49339" "H80CS47461"

missing_from_2021 <- setdiff(all_orgs, orgs_2021)

National20212024 %>%
  filter(GrantNumber %in% missing_from_2021) %>%
  select(GrantNumber, ReportingYear, HealthCenterName) %>%
  arrange(GrantNumber, ReportingYear)

#Table showing which health centers dropped
# Get distinct GrantNumbers per year
orgs_by_year <- National20212024 %>%
  distinct(GrantNumber, ReportingYear) %>%
  split(.$ReportingYear) %>%
  lapply(function(df) df$GrantNumber)

# Organizations present in 2021 but gone by 2022
dropped_2021_to_2022 <- setdiff(orgs_by_year[["2021"]], orgs_by_year[["2022"]])

# Organizations present in 2022 but gone by 2023
dropped_2022_to_2023 <- setdiff(orgs_by_year[["2022"]], orgs_by_year[["2023"]])

# Organizations present in 2023 but gone by 2024
dropped_2023_to_2024 <- setdiff(orgs_by_year[["2023"]], orgs_by_year[["2024"]])

# See counts at a glance
length(dropped_2021_to_2022)
length(dropped_2022_to_2023)
length(dropped_2023_to_2024)
dropped_all <- bind_rows(
  National20212024 %>%
    filter(GrantNumber %in% dropped_2021_to_2022, ReportingYear == 2021) %>%
    mutate(dropped_after = "2021"),
  National20212024 %>%
    filter(GrantNumber %in% dropped_2022_to_2023, ReportingYear == 2022) %>%
    mutate(dropped_after = "2022"),
  National20212024 %>%
    filter(GrantNumber %in% dropped_2023_to_2024, ReportingYear == 2023) %>%
    mutate(dropped_after = "2023")
) %>%
  select(GrantNumber, HealthCenterName, HealthCenterCity, HealthCenterState, dropped_after)

dropped_all

#Confirm no cartesian product from merge
National20212024 %>%
  count(GrantNumber, ReportingYear) %>%
  filter(n > 1)
