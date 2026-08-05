load_year_raw <- function(file) {
  
  hci <- read_excel(file, sheet = "HealthCenterInfo", col_types = "text") |>
    select(GrantNumber, ReportingYear, HealthCenterName, HealthCenterCity,
           HealthCenterState, HealthCenterZIPCode, FundingCHC, FundingMHC,
           FundingHO, FundingPH, UrbanRuralFlag) |>
    filter(GrantNumber %in% AAPCHOMembers)
  
  tables <- list(
    read_aapcho(file, "Table3A"),
    read_aapcho(file, "Table3B"),
    read_aapcho(file, "Table4"),
    read_aapcho(file, "Table5"),
    read_aapcho(file, "Table6A"),
    read_aapcho(file, "Table6B"),
    read_aapcho(file, "Table6BClinicalmeasures"),
    read_aapcho(file, "Table7_1"),
    read_aapcho(file, "Table7_2"),
    read_aapcho(file, "Table8A"),
    read_aapcho(file, "Table9E"),
    read_aapcho(file, "HITInformation"),
    read_aapcho(file, "OtherDataElements"),
    read_aapcho(file, "Workforce")
  )
  
  merged <- c(list(hci), tables) |>
    reduce(full_join, by = "GrantNumber") |>
    filter(!is.na(GrantNumber), !grepl("^-,$", GrantNumber))
  
  merged   
}
read_aapcho <- function(file, sheet, col_select = NULL) {
  df <- read_excel(file, sheet = sheet, col_types = "text")
  
  if (!is.null(col_select)) {
    keep <- union(which(names(df) == "GrantNumber"), col_select)
  } else {
    keep <- union(which(names(df) == "GrantNumber"), setdiff(seq_along(df), 1))
  }
  
  df[keep] |> filter(GrantNumber %in% AAPCHOMembers)
}
all_years_raw <- map(year_files, load_year_raw)
AAPCHOMembers_raw <- bind_rows(all_years_raw)

suppressed_flags <- AAPCHOMembers_raw %>%
  mutate(across(-c(GrantNumber, ReportingYear, HealthCenterName, HealthCenterCity,
                   HealthCenterState, HealthCenterZIPCode, UrbanRuralFlag),
                ~ grepl("^-+$", .x),
                .names = "suppressed_{.col}"))
