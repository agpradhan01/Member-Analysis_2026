##Analysis notes
#Suppression applied to Table 5 (all years), Table 8A (2021-2023), Table 9D
#For AAPCHO Members (28) as of 2026, performed checks of health centers dropped and added 2021-2025
##0 **in this subset (duh)** dropped since 2021. Other Members have dropped, but analysis is based off of list of Members updated to 2026
##0 added since 2021** even though 6 CHCs were added to AAPCHO Membership, they were already FQHCs. For this analysis, will consider them "AAPCHO Members", even if they were not Members in 2021; rationale: they were still serving large population of AANHPIs. 
#For National, performed same checks (drop/add 2021-2025)
## 21 dropped since 2021
## 4 added since 2021
##NEED TO CHECK FOR 2021-2025 PCT CHANGES AND COUNTS, DID REMOVE HCS ADDED OR DROPPED FROM ANALYSES?
# Read files --------------------------------------------------------------
read_UDS <- function(file, sheet, col_select = NULL, filter_ids = NULL) {
  df <- read_excel(file, sheet = sheet)
  
  if (!is.null(col_select)) {
    keep <- union(which(names(df) == "GrantNumber"), col_select)
  } else {
    keep <- union(which(names(df) == "GrantNumber"), setdiff(seq_along(df), 1))
  }
  
  df <- df[keep] %>%
    filter(grepl("^H80", GrantNumber))   
  
  if (!is.null(filter_ids)) {
    df <- df %>% filter(GrantNumber %in% filter_ids)
  }
  
  df
}

load_year <- function(file, filter_ids = NULL) {
  
  hci <- read_excel(file, sheet = "HealthCenterInfo") |>
    select(GrantNumber, ReportingYear, HealthCenterName, HealthCenterCity,
           HealthCenterState, HealthCenterZIPCode, UrbanRuralFlag) |>
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
    read_UDS(file, "Table9D", filter_ids = filter_ids),
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
  here("H802024.xlsx"), 
  here("H802025.xlsx")
)

National20212025 <- map(year_files, ~load_year(.x, filter_ids = NULL)) %>%
  bind_rows()

library(openxlsx)
write.xlsx(National20212025, "National20212015.xlsx")
 ----
all_names <- names(National20212025)

# Cleaning: mismatched-case columns over years (Table 6B, 8A, 9D) ---------
##Duplicates when uppercase
dupes <- all_names[duplicated(toupper(all_names)) | duplicated(toupper(all_names), fromLast = TRUE)]
dupes <- sort(dupes)
print(dupes)

library(stringr)

#Uppercase version of every name, find case-duplicates
name_map <- tibble(original = all_names, upper = toupper(all_names)) %>%
  group_by(upper) %>%
  filter(n() == 2) %>%  
  ungroup()

##Coalesce duplicated uppercase name; two original columns into one
for (col_upper in unique(name_map$upper)) {
  pair <- name_map %>% filter(upper == col_upper) %>% pull(original)
  National20212025[[col_upper]] <- coalesce(
    National20212025[[pair[1]]],
    National20212025[[pair[2]]]
  )
}

##Keep only the new coalesced uppercase columns, drop rest
National20212025 <- National20212025 %>%
  select(-all_of(name_map$original))

##Verify no case duplicates
names(National20212025) %>% toupper() %>% duplicated() %>% sum()  # should be 0

##Confirm no cartesian product from merge
National20212025 %>%
  count(GrantNumber, ReportingYear) %>%
  filter(n > 1)


# Subset AAPCHO Members ---------------------------------------------------


AAPCHOMembers <- c("H80CS02327", "H80CS29016", "H80CS26615", "H80CS00773",
                   "H80CS26574", "H80CS02326", "H80CS00358", "H80CS04290",
                   "H80CS00722", "H80CS28986", "H80CS02468", "H80CS06640",
                   "H80CS24153", "H80CS00437", "H80CS31624", "H80CS00814",
                   "H80CS00776", "H80CS33646", "H80CS35350", "H80CS08775",
                   "H80CS00397", "H80CS26582", "H80CS00221", "H80CS26623",
                   "H80CS00600", "H80CS06653", "H80CS00807", "H80CS00646")

AAPCHOMembers20212025 <- National20212025%>%
  filter(GrantNumber %in% AAPCHOMembers)
library(openxlsx)
write.xlsx(AAPCHOMembers20212025, "AAPCHOMembers20212025.xlsx")

# Checks - AAPCHO subset --------------------------------------------------
##Should be: up to 1,360 orgs × 5 years
nrow(National20212025)
##should be close to 1,360 (or fewer, if some HCs did report every year)
n_distinct(National20212025$GrantNumber)  
##was 1373 
#investigating which additional 3 health centers added
orgs_2021 <- National20212025%>%
  filter(ReportingYear == 2021) %>%
  pull(GrantNumber) %>%
  unique()





# Checking dropped orgs National ------------------------------------------
all_orgs <- unique(National20212025$GrantNumber)

##HCs that appear in the full dataset but NOT in 2021
setdiff(all_orgs, orgs_2021)
##"H80CS45850" "H80CS49339" "H80CS47461" "H80CS54598"

missing_from_2021 <- setdiff(all_orgs, orgs_2021)

National20212025%>%
  filter(GrantNumber %in% missing_from_2021) %>%
  select(GrantNumber, ReportingYear, HealthCenterName) %>%
  arrange(GrantNumber, ReportingYear)

##Table showing which health centers dropped
###Distinct GrantNumbers per year
orgs_by_year <- National20212025 %>%
  distinct(GrantNumber, ReportingYear) %>%
  split(.$ReportingYear) %>%
  lapply(function(df) df$GrantNumber)


##HCs in 2021 but dropped by 2022
dropped_2021_to_2022 <- setdiff(orgs_by_year[["2021"]], orgs_by_year[["2022"]])
print(dropped_2021_to_2022)
##HCs added in 2022
added_2022 <- setdiff(orgs_by_year[["2022"]], orgs_by_year[["2021"]])
print(added_2022)
##HCs in 2022 but dropped by 2023
dropped_2022_to_2023 <- setdiff(orgs_by_year[["2022"]], orgs_by_year[["2023"]])
added_2023 <- setdiff(orgs_by_year[["2023"]], orgs_by_year[["2022"]])
##HCs in 2023 but dropped by 2024
dropped_2023_to_2024 <- setdiff(orgs_by_year[["2023"]], orgs_by_year[["2024"]])
added_2024 <- setdiff(orgs_by_year[["2024"]], orgs_by_year[["2023"]])
##HCs in 2024 but dropped by 2025
dropped_2024_to_2025 <- setdiff(orgs_by_year[["2024"]], orgs_by_year[["2025"]])
added_2025 <- setdiff(orgs_by_year[["2025"]], orgs_by_year[["2024"]])
##Counts
length(dropped_2021_to_2022)
length(added_2022)
length(dropped_2022_to_2023)
length(added_2023)
length(dropped_2023_to_2024)
length(added_2024)
length(dropped_2024_to_2025)
length(added_2025)
dropped_all <- bind_rows(
  National20212025 %>%
    filter(GrantNumber %in% dropped_2021_to_2022, ReportingYear == 2021) %>%
    mutate(dropped_after = "2021"),
  National20212025 %>%
    filter(GrantNumber %in% dropped_2022_to_2023, ReportingYear == 2022) %>%
    mutate(dropped_after = "2022"),
  National20212025 %>%
    filter(GrantNumber %in% dropped_2023_to_2024, ReportingYear == 2023) %>%
    mutate(dropped_after = "2023"),
  National20212025 %>%
   filter(GrantNumber %in% dropped_2024_to_2025, ReportingYear == 2024) %>%
    mutate(dropped_after = "2024")
) %>%
  select(GrantNumber, HealthCenterName, HealthCenterCity, HealthCenterState, dropped_after)

dropped_all
print(dropped_all, n = 25)

added_all <- bind_rows(
  National20212025 %>%
    filter(GrantNumber %in% added_2022, ReportingYear == 2022) %>%
    mutate(added_in = "2022"),
  National20212025 %>%
    filter(GrantNumber %in% added_2023, ReportingYear == 2023) %>%
    mutate(added_in = "2023"),
  National20212025 %>%
    filter(GrantNumber %in% added_2024, ReportingYear == 2024) %>%
    mutate(added_in = "2024"),
  National20212025 %>%
    filter(GrantNumber %in% added_2025, ReportingYear == 2025) %>%
    mutate(added_in = "2025")
) %>%
  select(GrantNumber, HealthCenterName, HealthCenterCity, HealthCenterState, added_in)

added_all
print(added_all, n = 25)


#subset that includes HCs present in both 2021 and 2025
HCs_both_years <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  distinct(GrantNumber, ReportingYear) %>%
  count(GrantNumber) %>%
  filter(n == 2) %>%
  pull(GrantNumber)

National_HCsbothyears <- National20212025 %>%
  filter(GrantNumber %in% HCs_both_years)

length(HCs_both_years)
length(unique(National_HCsbothyears$GrantNumber))

#Checking dropped AAPCHO Orgs ------------------------------------
AAPCHOorgs_by_year <- AAPCHOMembers20212025 %>%
  distinct(GrantNumber, ReportingYear) %>%
  split(.$ReportingYear) %>%
  lapply(function(df) df$GrantNumber)
print(AAPCHOorgs_by_year)

##HCs in 2021 but dropped by 2022
AAPCHOdropped_2021_to_2022 <- setdiff(AAPCHOorgs_by_year[["2021"]], AAPCHOorgs_by_year[["2022"]])
print(AAPCHOdropped_2021_to_2022)
##HCs added in 2022
AAPCHOadded_2022 <- setdiff(AAPCHOorgs_by_year[["2022"]], AAPCHOorgs_by_year[["2021"]])
print(AAPCHOadded_2022)
##HCs in 2022 but dropped by 2023
AAPCHOdropped_2022_to_2023 <- setdiff(AAPCHOorgs_by_year[["2022"]], AAPCHOorgs_by_year[["2023"]])
AAPCHOadded_2023 <- setdiff(AAPCHOorgs_by_year[["2023"]], AAPCHOorgs_by_year[["2022"]])
##HCs in 2023 but dropped by 2024
AAPCHOdropped_2023_to_2024 <- setdiff(AAPCHOorgs_by_year[["2023"]], AAPCHOorgs_by_year[["2024"]])
AAPCHOadded_2024 <- setdiff(AAPCHOorgs_by_year[["2024"]], AAPCHOorgs_by_year[["2023"]])
##HCs in 2024 but dropped by 2025
AAPCHOdropped_2024_to_2025 <- setdiff(AAPCHOorgs_by_year[["2024"]], AAPCHOorgs_by_year[["2025"]])
AAPCHOadded_2025 <- setdiff(AAPCHOorgs_by_year[["2025"]], AAPCHOorgs_by_year[["2024"]])
##Counts
length(AAPCHOdropped_2021_to_2022)
length(AAPCHOadded_2022)
length(AAPCHOdropped_2022_to_2023)
length(AAPCHOadded_2023)
length(AAPCHOdropped_2023_to_2024)
length(AAPCHOadded_2024)
length(AAPCHOdropped_2024_to_2025)
length(AAPCHOadded_2025)
AAPCHOdropped_all <- bind_rows(
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOdropped_2021_to_2022, ReportingYear == 2021) %>%
    mutate(AAPCHOdropped_after = "2021"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOdropped_2022_to_2023, ReportingYear == 2022) %>%
    mutate(AAPCHOdropped_after = "2022"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOdropped_2023_to_2024, ReportingYear == 2023) %>%
    mutate(AAPCHOdropped_after = "2023"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOdropped_2024_to_2025, ReportingYear == 2024) %>%
    mutate(AAPCHOdropped_after = "2024")
) %>%
  select(GrantNumber, HealthCenterName, HealthCenterCity, HealthCenterState, AAPCHOdropped_after)

AAPCHOdropped_all
print(AAPCHOdropped_all, n = 25)

AAPCHOadded_all <- bind_rows(
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOadded_2022, ReportingYear == 2022) %>%
    mutate(AAPCHOadded_in = "2022"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOadded_2023, ReportingYear == 2023) %>%
    mutate(AAPCHOadded_in = "2023"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOadded_2024, ReportingYear == 2024) %>%
    mutate(AAPCHOadded_in = "2024"),
  AAPCHOMembers20212025 %>%
    filter(GrantNumber %in% AAPCHOadded_2025, ReportingYear == 2025) %>%
    mutate(AAPCHOadded_in = "2025")
) %>%
  select(GrantNumber, HealthCenterName, HealthCenterCity, HealthCenterState, AAPCHOadded_in)

AAPCHOadded_all
print(AAPCHOadded_all, n = 25)



#Load fonts to match Canva design ----------------------------------------
library(ggplot2)
#install.packages("showtext")
library(showtext)

##Add fonts from Google Fonts
font_add_google("Montserrat", "montserrat")
showtext_auto()
font_add_google("Open Sans", "open_sans")
showtext_auto()


# Members by State --------------------------------------------------------
members_by_state <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  distinct(GrantNumber, HealthCenterState) %>%
  count(HealthCenterState, name = "member_count") %>%
  arrange(desc(member_count))

ggplot(members_by_state, aes(x = reorder(HealthCenterState, -member_count), y = member_count)) +
  geom_col(fill = "#B25833", width = 0.6) +
  geom_text(aes(label = member_count), vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "2025 AAPCHO Member Health Centers \n by State/Territory",
    x = "State/Territory",
    y = "Count of Health Centers"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )


# Map of Members ----------------------------------------------------------

library(tigris)
library(dplyr)
library(ggplot2)
library(sf)

options(tigris_use_cache = TRUE)

states_map <- states(cb = TRUE)

states_map_joined <- states_map %>%
  left_join(members_by_state, by = c("STUSPS" = "HealthCenterState"))

##Coordinates for label placement
state_centroids <- states_map_joined %>%
  filter(!is.na(member_count)) %>%   # only label states that actually have members
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

ggplot(states_map_joined) +
  geom_sf(aes(fill = member_count), color = "white", linewidth = 0.2) +
  geom_text(
    data = state_centroids,
    aes(x = lon, y = lat, label = member_count),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#e1c758", high = "#4d2716",
    na.value = "#EEEEEE",
    name = "Member\nCenters"
  ) +
  coord_sf(xlim = c(-125, -65), ylim = c(24, 50)) +
  labs(
    title = "2025 AAPCHO Member Health Centers by State/Territory"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA)
  )

ggsave("member_map.png", bg = "transparent", width = 8, height = 6, dpi = 300)
 #larger text
ggsave("member_map2.png", bg = "transparent", width = 10, height = 7.5, dpi = 300)
ggsave("member_map3.png", bg = "transparent", width = 6, height = 4.5, dpi = 300)

# OCONUS - minus PR, including HI. NH folks may disagree with Hawaii grouped in...tbd ------------------------------------------------------------------
OCONUSIslands2025 <- c("PW", "HI", "FM", "GU", "MH", "AS", "MP")
tibble <- National20212025 %>%
  filter(HealthCenterState %in% OCONUSIslands2025) %>%
  group_by(ReportingYear) %>%
  summarise(
    Total_Patients = sum(T3a_L39_Ca, T3a_L39_Cb, na.rm = TRUE),
    .groups = "drop"
  )
print(tibble, n = Inf) 

tibble <- AAPCHOMembers20212025 %>%
  filter(HealthCenterState %in% OCONUSIslands2025) %>%
  group_by(ReportingYear) %>%
  summarise(
    Total_Patients = sum(T3a_L39_Ca, T3a_L39_Cb, na.rm = TRUE),
    .groups = "drop"
  )
print(tibble, n = Inf) 

OCONUSIslands2025 <- c("PW", "HI", "FM", "GU", "MH", "AS", "MP")

##National totals by state/territory, by year
national_tibble <- National20212025 %>%
  filter(HealthCenterState %in% OCONUSIslands2025) %>%
  group_by(HealthCenterState, ReportingYear) %>%
  summarise(
    Total_Patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  )
##AAPCHO Member totals by state/territory, by year
member_tibble <- AAPCHOMembers20212025 %>%
  filter(HealthCenterState %in% OCONUSIslands2025) %>%
  group_by(HealthCenterState, ReportingYear) %>%
  summarise(
    Total_Patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  )

##Combine — Member vs Non-Member split of the National total
patients_by_state <- national_tibble %>%
  rename(National_Total = Total_Patients) %>%
  left_join(
    member_tibble %>% rename(Member_Total = Total_Patients),
    by = c("HealthCenterState", "ReportingYear")
  ) %>%
  mutate(
    Member_Total = coalesce(Member_Total, 0),
    NonMember_Total = National_Total - Member_Total
  )


##Labels with footnote markers (keeping separate from the raw HealthCenterState codes used for filtering/joins)
state_labels <- c(
  "PW" = "PW**",
  "HI" = "HI",
  "FM" = "FM**",
  "GU" = "GU*",
  "MH" = "MH**",
  "AS" = "AS*",
  "MP" = "MP*"
)
##Stacked bar data — 2025 only
bar_data_2025 <- patients_by_state %>%
  filter(ReportingYear == "2025") %>%
  select(HealthCenterState, `AAPCHO Member` = Member_Total, `Non-Member` = NonMember_Total) %>%
  pivot_longer(-HealthCenterState, names_to = "MemberStatus", values_to = "Patients") %>%
  mutate(StateLabel = recode(HealthCenterState, !!!state_labels))

##Growth data — National total, 2021 vs 2025, per state
growth_data <- patients_by_state %>%
  select(HealthCenterState, ReportingYear, National_Total) %>%
  pivot_wider(names_from = ReportingYear, values_from = National_Total, names_prefix = "Y") %>%
  mutate(
    Increase = Y2025 - Y2021,
    label = paste0("+", scales::comma(Increase)),
    StateLabel = recode(HealthCenterState, !!!state_labels)
  )

ggplot(bar_data_2025, aes(x = StateLabel, y = Patients, fill = MemberStatus)) +
  geom_col(position = "stack") +
  geom_text(
    data = growth_data,
    aes(x = StateLabel, y = Y2025, label = scales::comma(Y2025)),
    inherit.aes = FALSE,
    color = "black",
    fontface = "bold",
    size = 4,
    vjust = -0.8
  ) +
  scale_fill_manual(values = c(
    "AAPCHO Member" = "#3C6E8A",
    "Non-Member" = "#CBD5C6"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = scales::comma) +
  labs(
    title = "2025 Health Center Patients \nServed in Pacific Islands",
    x = "State/Territory/COFA State",
    y = "Number of Patients Served",
    fill = NULL,
    caption = "* Territory \n ** COFA State"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 10, b = 15, l = 10),
    legend.position = "top"
  )

ggsave("pacific_islands_patients.png", bg = "transparent", width = 10, height = 6, dpi = 300)

#did validate below in 2025 UDS which CHCs are in Pacific Islands. Had mistakenly included UDSVI, which is not in Pacific 
PacificHealthCenters2025 <- c("H80CS02467",
                             "H80CS00807",
                             "H80CS02449",
                             "H80CS00852",
                             "H80CS04302",
                             "H80CS00053",
                             "H80CS00451",
                             "H80CS00646",
                             "H80CS06640",
                             "H80CS06641",
                             "H80CS06653",
                             "H80CS08775",
                             "H80CS00776",
                             "H80CS02468",
                             "H80CS02472",
                             "H80CS00814",
                             "H80CS00722",
                             "H80CS00290",
                             "H80CS02470",
                             "H80CS31624",
                             "H80CS30720",
                             "H80CS35350")
##Initially included PR and USVI
# OCONUSHealthCenters2025 <- c("H80CS00325",
#                              "H80CS00598",
#                              "H80CS00354",
#                              "H80CS00323",
#                              "H80CS00063",
#                              "H80CS00008",
#                              "H80CS00695",
#                              "H80CS00379",
#                              "H80CS00747",
#                              "H80CS00489",
#                              "H80CS00356",
#                              "H80CS00620",
#                              "H80CS00712",
#                              "H80CS00334",
#                              "H80CS00162",
#                              "H80CS00474",
#                              "H80CS00656",
#                              "H80CS00353",
#                              "H80CS00382",
#                              "H80CS00373",
#                              "H80CS00372",
#                              "H80CS22687",
#                              "H80CS33662",
#                              "H80CS02467",
#                              "H80CS00807",
#                              "H80CS02449",
#                              "H80CS00852",
#                              "H80CS04302",
# #                              "H80CS00053",
# #                              "H80CS00451",
# #                              "H80CS00646",
# #                              "H80CS06640",
# #                              "H80CS06641",
# #                              "H80CS06653",
# #                              "H80CS08775",
# #                              "H80CS00776",
# #                              "H80CS02468",
# #                              "H80CS02472",
# #                              "H80CS00814",
#                              "H80CS00722",
#                              "H80CS00290",
#                              "H80CS02470",
#                              "H80CS31624",
#                              "H80CS30720",
#                              "H80CS35350")
National20212025 %>%
  filter(GrantNumber %in% PacificHealthCenters2025) %>%
  group_by(ReportingYear) %>%
  summarise(
    Total_Patients = sum(T3a_L39_Ca, T3a_L39_Cb, na.rm = TRUE),
    .groups = "drop"
  )
AAPCHOMembers20212025 %>%
  filter(GrantNumber %in% PacificHealthCenters2025) %>%
  group_by(ReportingYear) %>%
  summarise(
    Total_Patients = sum(T3a_L39_Ca, T3a_L39_Cb, na.rm = TRUE),
    .groups = "drop"
  )

# AANHPI, and NHPI groupings ----------------------------------------------
library(dplyr)

org_classification <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(GrantNumber) %>%
  summarise(
    NHPI_patients = sum(rowSums(across(c(T3b_L2a_Cd, T3b_L2b_Cd, T3B_L2C_CD, T3B_L2D_CD)), na.rm = TRUE)),
    Asian_patients = sum(T3b_L1_Cd, na.rm = TRUE),
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    AANHPI_patients = NHPI_patients + Asian_patients,
    pct_AANHPI = AANHPI_patients / total_patients * 100,
    NHPI_serving = NHPI_patients >= 1000,
    AANHPI_serving = pct_AANHPI >= 10
  ) %>%
  filter(is.finite(pct_AANHPI))


NHPI_serving_orgs <- org_classification %>% filter(NHPI_serving == TRUE) %>% pull(GrantNumber)
AANHPI_serving_orgs <- org_classification %>% filter(AANHPI_serving == TRUE) %>% pull(GrantNumber)


length(NHPI_serving_orgs)
length(AANHPI_serving_orgs)
length(intersect(NHPI_serving_orgs, AANHPI_serving_orgs))  # orgs meeting BOTH criteria

# Patient Counts - 2021,2025 Bar Chart ----------------------------------------------

#Members total patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))
# 1 2021          555569
# 2 2022          580675
# 3 2023          607872
# 4 2024          660946
#5 2025          695716
##validated against Excel
#National total patients 
National20212025%>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))
# 1 2021          30193278
# 2 2022          30517276
# 3 2023          31277341
# 4 2024          32387774
# 5 2025          32746392
##validated against HRSA website
#Members vs. National patient growth, 2021-2025
#((2025-2021)/2021)*100
###Grouped bar
library(dplyr)
library(ggplot2)
library(scales)


# Members — per-org % change, then average across orgs

members_by_org <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = ReportingYear, values_from = total_patients, names_prefix = "y") %>%
  mutate(pct_change = (y2025 - y2021) / y2021 * 100)

members_avg_growth <- members_by_org %>%
  summarise(median_pct_change = median(pct_change, na.rm = TRUE)) %>%
  mutate(Group = "AAPCHO Members")


# National — same, per-org, then averaged

national_by_org <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = ReportingYear, values_from = total_patients, names_prefix = "y") %>%
  mutate(pct_change = (y2025 - y2021) / y2021 * 100)

##NEED TO REMOVE THESE FOUR ORGS, added since 2021
national_by_org %>% filter(y2021 == 0 | is.na(y2021) | is.infinite(pct_change))
# A tibble: 4 × 4
# GrantNumber y2021 y2025 pct_change
# <chr>       <dbl> <dbl>      <dbl>
#   1 H80CS45850     NA  6259         NA
# 2 H80CS47461     NA  1922         NA
# 3 H80CS49339     NA 15752         NA
# 4 H80CS54598     NA 14384         NA

# **MAYBE NEED TO REMOVE THOSE DROPPED SINCE 2021 -------------------------


national_by_org_clean <- national_by_org %>%
  filter(!is.na(y2021), y2021 != 0, is.finite(pct_change))

national_avg_growth <- National_HCsbothyears %>%
  summarise(median_pct_change = median(pct_change, na.rm = TRUE)) %>%
  mutate(Group = "National")

#check to use mean vs median
# members_by_org %>%
#   summarise(
#     mean_pct_change = mean(pct_change, na.rm = TRUE),
#     median_pct_change = median(pct_change, na.rm = TRUE)
#   )
# national_by_org %>%
#   summarise(
#     mean_pct_change = mean(pct_change, na.rm = TRUE),
#     median_pct_change = median(pct_change, na.rm = TRUE)
#   )


avg_patient_growth_combined <- bind_rows(members_avg_growth, national_avg_growth)
avg_patient_growth_combined

avg_patient_growth_labels <- avg_patient_growth_combined%>%
  mutate(
    growth_pct = round(median_pct_change, 2),
    label = paste0(ifelse(growth_pct >= 0, "+", ""), growth_pct, "%")
  )


ggplot(combined_totals, aes(x = as.character(ReportingYear), y = total_patients, fill = ReportingYear)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(total_patients)), vjust = -0.5, size = 3.5) +
  geom_text(
    data = avg_patient_growth_labels,
    aes(x = "2025", y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 2,
    fontface = "bold",
    color = "#B2334F",
    size = 3.5
  ) +
  facet_wrap(~Group, scales = "free_y") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("2021" = "#B29633", "2025" = "#B25833")) +
  labs(
    title = "Total Patients Served",
    x = NULL,
    y = "Count of Patients",
    caption = "Percent label = median growth across health centers"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    plot.caption = element_text(color = "#B2334F", hjust = 0.5, size = 10, margin = margin(t = 10))
    )


# Visits - 2025 Bar Chart ------------------------------------------------------------------


#Total Visits - T5_L34_Cb, T5_L34_Cb2
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L34_Cb, T5_L34_Cb2)), na.rm = TRUE)))
##Medical Visits
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)))

##Dental Visits
AAPCHOMembers20212025 %>%
group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L19_Cb, T5_L19_Cb2)), na.rm = TRUE)))

##Mental Health Visits
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L20_Cb, T5_L20_Cb2)), na.rm = TRUE)))

##Vision Visits
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L22d_Cb, T5_L22d_Cb2)), na.rm = TRUE)))


##Enabling Services
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE)))
##Bar chart of services, 2024
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

Membervisits_2025 <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Medical Service Visits` = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),      
    `Dental Service Visits` = sum(rowSums(across(c(T5_L19_Cb, T5_L19_Cb2)), na.rm = TRUE)),     
    `Mental Health Visits` = sum(rowSums(across(c(T5_L20_Cb, T5_L20_Cb2)), na.rm = TRUE)),      
    `Vision Visits` = sum(rowSums(across(c(T5_L22d_Cb, T5_L22d_Cb2)), na.rm = TRUE)),                
    `Enabling Services` = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE))           
  ) %>%
  pivot_longer(everything(), names_to = "Service", values_to = "Visits")

ggplot(Membervisits_2025, aes(x = reorder(Service, -Visits), y = Visits, fill = Service)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(Visits)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(
    "Medical Service Visits" = "#B2334F",
    "Dental Service Visits" = "#B25833",
    "Mental Health Visits" = "#B29633",
    "Vision Visits" = "#5C7A5A",
    "Enabling Services" = "#3C6E8A"
  )) +
  labs(
    title = "2025 AAPCHO Member Visits by Service Type",
    x = NULL,
    y = "Visits"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1),
   legend.position = "none"
  )

##Medical visits/patient, 2025
members_2025medicalvisits_patients_ratio <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(GrantNumber) %>%
  summarise(
    medicalvisits = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),
    medicalpatients = sum(rowSums(across(T5_L15_Cc), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    medvisitsperpatient = medicalvisits / medicalpatients,
  ) %>%
  filter(is.finite(medvisitsperpatient)) %>%
  summarise(
    `Medical Visits per Patient` = mean(medvisitsperpatient, na.rm = TRUE),
  ) %>%
  mutate(Group = "AAPCHO Members")

##Medical visits/patient, 2021 
members_2021medicalvisits_patients_ratio <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2021) %>%
  group_by(GrantNumber) %>%
  summarise(
    medicalvisits = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),
    medicalpatients = sum(rowSums(across(T5_L15_Cc), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    medvisitsperpatient = medicalvisits / medicalpatients,
  ) %>%
  filter(is.finite(medvisitsperpatient)) %>%
  summarise(
    `Medical Visits per Patient` = mean(medvisitsperpatient, na.rm = TRUE),
  ) %>%
  mutate(Group = "AAPCHO Members")

##Medical visits/patient, 2025
national_2025medicalvisits_patients_ratio <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(GrantNumber) %>%
  summarise(
    medicalvisits = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),
    medicalpatients = sum(rowSums(across(T5_L15_Cc), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    medvisitsperpatient = medicalvisits / medicalpatients,
  ) %>%
  filter(is.finite(medvisitsperpatient)) %>%
  summarise(
    `Medical Visits per Patient` = mean(medvisitsperpatient, na.rm = TRUE),
  ) %>%
  mutate(Group = "National")

##Medical visits/patient, 2021 National
national_2021medicalvisits_patients_ratio <- National20212025 %>%
  filter(ReportingYear == 2021) %>%
  group_by(GrantNumber) %>%
  summarise(
    medicalvisits = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),
    medicalpatients = sum(rowSums(across(T5_L15_Cc), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    medvisitsperpatient = medicalvisits / medicalpatients,
  ) %>%
  filter(is.finite(medvisitsperpatient)) %>%
  summarise(
    `Medical Visits per Patient` = mean(medvisitsperpatient, na.rm = TRUE),
  ) %>%
  mutate(Group = "National")
##Enabling services proportion of total stacked bar 

# Enabling Visits 2021,2025 - Stacked Bar ---------------------------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
members_enablingvisits <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    enabling = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE)),
    total = sum(rowSums(across(c(T5_L34_Cb, T5_L34_Cb2)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_enabling = enabling / total * 100,
    pct_other = 100 - pct_enabling
  ) %>%
  filter(is.finite(pct_enabling)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Enabling Services` = median(pct_enabling, na.rm = TRUE),
    `All Other Services` = median(pct_other, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "AAPCHO Members")

national_enablingvisits <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    enabling = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE)),
    total = sum(rowSums(across(c(T5_L34_Cb, T5_L34_Cb2)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_enabling = enabling / total * 100,
    pct_other = 100 - pct_enabling
  ) %>%
  filter(is.finite(pct_enabling)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Enabling Services` = median(pct_enabling, na.rm = TRUE),
    `All Other Services` = median(pct_other, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "National")

combinedenabling <- bind_rows(members_enablingvisits, national_enablingvisits) %>%
  pivot_longer(-c(Group, ReportingYear), names_to = "Category", values_to = "pct")
ggplot(combinedenabling, aes(x = as.character(ReportingYear), y = pct, fill = Category)) +
  geom_col(width = 0.5) +
  geom_text(
    data = combinedenabling %>% filter(Category == "All Other Services"),
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  geom_text(
    data = combinedenabling %>% filter(Category == "Enabling Services"),
    aes(y = 105, label = paste0("Enabling: ", round(pct, 1), "%")),
    color = "#3C6E8A",
    fontface = "bold",
    size = 3.0
  ) +
  facet_wrap(~Group) +
  scale_y_continuous(limits = c(0, 115)) +
  scale_fill_manual(values = c(
    "Enabling Services" = "#3C6E8A",
    "All Other Services" = "#CBD5C6"
  )) +
  labs(
    title = "Enabling Services as Percent of Total Visits",
    x = NULL,
    y = "Average Proportion of Visits",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top"
 )

# Age - 2025 Bar ---------------------------------------------------------------------


##Demographics - Age
Memberages_2library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

Memberages_by_year <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `0-9` = sum(rowSums(across(c(T3a_L1_Ca, T3a_L1_Cb, T3a_L2_Ca, T3a_L2_Cb, T3a_L3_Ca, T3a_L3_Cb, T3a_L4_Ca, T3a_L4_Cb, T3a_L5_Ca, T3a_L5_Cb, T3a_L6_Ca, T3a_L6_Cb, T3a_L7_Ca, T3a_L7_Cb, T3a_L8_Ca, T3a_L8_Cb, T3a_L9_Ca, T3a_L9_Cb, T3a_L10_Ca, T3a_L10_Cb)), na.rm = TRUE)),
    `10-19` = sum(rowSums(across(c(T3a_L11_Ca, T3a_L11_Cb, T3a_L12_Ca, T3a_L12_Cb, T3a_L13_Ca, T3a_L13_Cb, T3a_L14_Ca, T3a_L14_Cb, T3a_L15_Ca, T3a_L15_Cb, T3a_L16_Ca, T3a_L16_Cb, T3a_L17_Ca, T3a_L17_Cb, T3a_L18_Ca, T3a_L18_Cb, T3a_L19_Ca, T3a_L19_Cb, T3a_L20_Ca, T3a_L20_Cb)), na.rm = TRUE)),
    `20-29` = sum(rowSums(across(c(T3a_L21_Ca, T3a_L21_Cb, T3a_L22_Ca, T3a_L22_Cb, T3a_L23_Ca, T3a_L23_Cb, T3a_L24_Ca, T3a_L24_Cb, T3a_L25_Ca, T3a_L25_Cb, T3a_L26_Ca, T3a_L26_Cb)), na.rm = TRUE)),
    `30-39` = sum(rowSums(across(c(T3a_L27_Ca, T3a_L27_Cb, T3a_L28_Ca, T3a_L28_Cb)), na.rm = TRUE)),
    `40-49` = sum(rowSums(across(c(T3a_L29_Ca, T3a_L29_Cb, T3a_L30_Ca, T3a_L30_Cb)), na.rm = TRUE)),
    `50-59` = sum(rowSums(across(c(T3a_L31_Ca, T3a_L31_Cb, T3a_L32_Ca, T3a_L32_Cb)), na.rm = TRUE)),
    `60-69` = sum(rowSums(across(c(T3a_L33_Ca, T3a_L33_Cb, T3a_L34_Ca, T3a_L34_Cb)), na.rm = TRUE)),
    `70-79` = sum(rowSums(across(c(T3a_L35_Ca, T3a_L35_Cb, T3a_L36_Ca, T3a_L36_Cb)), na.rm = TRUE)),
    `80-85+` = sum(rowSums(across(c(T3a_L37_Ca, T3a_L37_Cb, T3a_L38_Ca, T3a_L38_Cb)), na.rm = TRUE)),
     .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Age", values_to = "Patients")


age_increase <- Memberages_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


Memberages_2025 <- age_increase %>%
  select(Age, Patients = y2025)

age_order <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-85+")



max_patients <- max(Memberages_2025$Patients, na.rm = TRUE)

age_increase <- age_increase %>%
  mutate(label_y = y2025 + max_patients * 0.05)

ggplot(Memberages_2025, aes(x = factor(Age, levels = age_order), y = Patients, fill = Age)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 3.0) +
  geom_text(
    data = age_increase %>% mutate(Age = factor(Age, levels = age_order)),
    aes(x = Age, y = label_y, label = label_text),
    inherit.aes = FALSE,
    color = "#B2334F",
    fontface = "bold",
    size = 3.0
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "0-9" = "#B2334F", "10-19" = "#B25833", "20-29" = "#B29633",
    "30-39" = "#5C7A5A", "40-49" = "#3C6E8A", "50-59" = "#654d78",
    "60-69" = "#2f4858", "70-79" = "#eaeeff", "80-85+" = "#3b8035"
  )) +
  labs(
    title = "2025 AAPCHO Member Patients: Age",
    x = "Age (Years)",
    y = "Count of Patients",
    caption = "Percent label = percent change in patient count, 2021-2025"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    plot.caption = element_text(color = "#B2334F", hjust = 0.5, size = 10, margin = margin(t = 10)),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )

##AAPCHO Members: change in patient age groups from 2021
member_age_change_named <- member_age_change %>%
  inner_join(
    HealthCenterInfo %>% select(GrantNumber, HealthCenterName, HealthCenterState),
    by = "GrantNumber"
  ) %>%
  select(GrantNumber, HealthCenterName, HealthCenterState, y2021, y2025, member_age_change) %>%
  arrange(desc(member_age_change))

memberg_age_change_named


Nationalages_by_year <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `0-9` = sum(rowSums(across(c(T3a_L1_Ca, T3a_L1_Cb, T3a_L2_Ca, T3a_L2_Cb, T3a_L3_Ca, T3a_L3_Cb, T3a_L4_Ca, T3a_L4_Cb, T3a_L5_Ca, T3a_L5_Cb, T3a_L6_Ca, T3a_L6_Cb, T3a_L7_Ca, T3a_L7_Cb, T3a_L8_Ca, T3a_L8_Cb, T3a_L9_Ca, T3a_L9_Cb, T3a_L10_Ca, T3a_L10_Cb)), na.rm = TRUE)),
    `10-19` = sum(rowSums(across(c(T3a_L11_Ca, T3a_L11_Cb, T3a_L12_Ca, T3a_L12_Cb, T3a_L13_Ca, T3a_L13_Cb, T3a_L14_Cb, T3a_L15_Ca, T3a_L15_Cb, T3a_L16_Ca, T3a_L16_Cb, T3a_L17_Ca, T3a_L17_Cb, T3a_L18_Ca, T3a_L18_Cb, T3a_L19_Ca, T3a_L19_Cb, T3a_L20_Ca, T3a_L20_Cb)), na.rm = TRUE)),
    `20-29` = sum(rowSums(across(c(T3a_L21_Ca, T3a_L21_Cb, T3a_L22_Ca, T3a_L22_Cb, T3a_L23_Ca, T3a_L23_Cb, T3a_L24_Ca, T3a_L24_Cb, T3a_L25_Ca, T3a_L25_Cb, T3a_L26_Ca, T3a_L26_Cb)), na.rm = TRUE)),
    `30-39` = sum(rowSums(across(c(T3a_L27_Ca, T3a_L27_Cb, T3a_L28_Ca, T3a_L28_Cb)), na.rm = TRUE)),
    `40-49` = sum(rowSums(across(c(T3a_L29_Ca, T3a_L29_Cb, T3a_L30_Ca, T3a_L30_Cb)), na.rm = TRUE)),
    `50-59` = sum(rowSums(across(c(T3a_L31_Ca, T3a_L31_Cb, T3a_L32_Ca, T3a_L32_Cb)), na.rm = TRUE)),
    `60-69` = sum(rowSums(across(c(T3a_L33_Ca, T3a_L33_Cb, T3a_L34_Ca, T3a_L34_Cb)), na.rm = TRUE)),
    `70-79` = sum(rowSums(across(c(T3a_L35_Ca, T3a_L35_Cb, T3a_L36_Ca, T3a_L36_Cb)), na.rm = TRUE)),
    `80-85+` = sum(rowSums(across(c(T3a_L37_Ca, T3a_L37_Cb, T3a_L38_Ca, T3a_L38_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Age", values_to = "Patients")


age_increase <- Nationalages_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


Nationalages_2025 <- age_increase %>%
  select(Age, Patients = y2025)

age_order <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-85+")



max_patients <- max(Nationalages_2025$Patients, na.rm = TRUE)

age_increase <- age_increase %>%
  mutate(label_y = y2025 + max_patients * 0.1)

ggplot(Nationalages_2025, aes(x = factor(Age, levels = age_order), y = Patients, fill = Age)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 2.5) +
  geom_text(
    data = age_increase %>% mutate(Age = factor(Age, levels = age_order)),
    aes(x = Age, y = label_y, label = label_text),
    inherit.aes = FALSE,
    color = "#B2334F",
    fontface = "bold",
    size = 3.0
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "0-9" = "#B2334F", "10-19" = "#B25833", "20-29" = "#B29633",
    "30-39" = "#5C7A5A", "40-49" = "#3C6E8A", "50-59" = "#654d78",
    "60-69" = "#2f4858", "70-79" = "#eaeeff", "80-85+" = "#3b8035"
  )) +
  labs(
    title = "2025 National Health Center Patients: Age",
    subtitle = "Red text = percent change in patient count 2021-2025",
    x = "Age",
    y = "Patients"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#B2334F", size = 9),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )


# Race - 2025 Bar --------------------------------------------------------------------
MemberRace_by_year <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Asian` = sum(T3b_L1_Cd, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Pacific Islander` = sum(rowSums(across(c(T3b_L2b_Cd,T3B_L2C_CD,T3B_L2D_CD)), na.rm = TRUE)),
    `Black/African American` = sum(T3b_L3_Cd, na.rm = TRUE),
    `American Indian/Alaska Native` = sum(T3b_L4_Cd, na.rm = TRUE),
    `White` = sum(T3b_L5_Cd, na.rm = TRUE),
    `More than one race` = sum(T3b_L6_Cd, na.rm = TRUE),
    `Unreported` = sum(T3b_L7_Cd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Race Group", values_to = "Patients")


racegroup_increase <- MemberRace_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


MemberRace_2025 <- racegroup_increase %>%
  select('Race Group', Patients = y2025)


max_patients <- max(MemberRace_2025$Patients, na.rm = TRUE)

racegroup_increase <- racegroup_increase %>%
  mutate(label_y = y2025 + max_patients * 0.1)

ggplot(MemberRace_2025, aes(x = `Race Group`, y = Patients, fill = `Race Group`)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 3.0) +
  geom_text(
    data = racegroup_increase,
    aes(x = `Race Group`, y = label_y, label = label_text),
    inherit.aes = FALSE,
    color = "#B2334F",
    fontface = "bold",
    size = 3.0
  ) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "Asian" = "#B2334F", "Native Hawaiian" = "#B25833", "Pacific Islander" = "#B29633",
    "Black/African American" = "#5C7A5A", "American Indian/Alaska Native" = "#3C6E8A", "White" = "#654d78",
    "More than one race" = "#2f4858", "Unreported" = "#eaeeff"
  )) +
  labs(
    title = "2025 AAPCHO Member Patients: Race",
    x = "Race Group",
    y = "Count of Patients",
    caption = "Percent label = percent change in patient count since 2021"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    plot.caption = element_text(color = "#B2334F", hjust = 0.5, size = 10, margin = margin(t = 10)),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )


##AAPCHO Members: change in patient age groups from 2021
member_age_change_named <- member_age_change %>%
  inner_join(
    HealthCenterInfo %>% select(GrantNumber, HealthCenterName, HealthCenterState),
    by = "GrantNumber"
  ) %>%
  select(GrantNumber, HealthCenterName, HealthCenterState, y2021, y2025, member_age_change) %>%
  arrange(desc(member_age_change))

memberg_age_change_named



##Demographics - Race National
NationalRace_by_year <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Asian` = sum(T3b_L1_Cd, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Pacific Islander` = sum(rowSums(across(c(T3b_L2b_Cd,T3b_L2c_Cd,T3b_L2d_Cd)), na.rm = TRUE)),
    `Black/African American` = sum(T3b_L3_Cd, na.rm = TRUE),
    `American Indian/Alaska Native` = sum(T3b_L4_Cd, na.rm = TRUE),
    `White` = sum(T3b_L5_Cd, na.rm = TRUE),
    `More than one race` = sum(T3b_L6_Cd, na.rm = TRUE),
    `Unreported` = sum(T3b_L7_Cd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Race Group", values_to = "Patients")


racegroup_increase <- NationalRace_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


NationalRace_2025 <- racegroup_increase %>%
  select('Race Group', Patients = y2025)


max_patients <- max(NationalRace_2025$Patients, na.rm = TRUE)

racegroup_increase <- racegroup_increase %>%
  mutate(label_y = y2025 + max_patients * 0.1)

ggplot(NationalRace_2025, aes(x = `Race Group`, y = Patients, fill = `Race Group`)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 3.0) +
  geom_text(
    data = racegroup_increase,
    aes(x = `Race Group`, y = label_y, label = label_text),
    inherit.aes = FALSE,
    color = "#B2334F",
    fontface = "bold",
    size = 3.0
  ) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "Asian" = "#B2334F", "Native Hawaiian" = "#B25833", "Pacific Islander" = "#B29633",
    "Black/African American" = "#5C7A5A", "American Indian/Alaska Native" = "#3C6E8A", "White" = "#654d78",
    "More than one race" = "#2f4858", "Unreported" = "#eaeeff"
  )) +
  labs(
    title = "2025 National Health Center Patients: Race",
    subtitle = "Red text = percent change in patient count since 2021",
    x = "Race Group",
    y = "Patients"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#B2334F", size = 9),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )

# AA, NH, and PI stacked bars ---------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

Member_totals_2025 <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    Asian = sum(T3b_L1_Cd, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Pacific Islander` = sum(rowSums(across(c(T3b_L2b_Cd, T3B_L2C_CD, T3B_L2D_CD)), na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "RaceGroup", values_to = "Member_Patients")

National_totals_2025 <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    Asian = sum(T3b_L1_Cd, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Pacific Islander` = sum(rowSums(across(c(T3b_L2b_Cd, T3B_L2C_CD, T3B_L2D_CD)), na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "RaceGroup", values_to = "National_Patients")


race_2025 <- National_totals_2025 %>%
  left_join(Member_totals_2025, by = "RaceGroup") %>%
  mutate(
    Member_Patients = coalesce(Member_Patients, 0),
    NonMember_Patients = National_Patients - Member_Patients,
    Member_Pct = Member_Patients/National_Patients * 100,
    NonMember_Pct = NonMember_Patients/National_Patients *100
  ) %>%
  select(RaceGroup, `AAPCHO Members` = Member_Pct, `Non-Member` = NonMember_Pct) %>%
  pivot_longer(-RaceGroup, names_to = "MemberStatus", values_to = "Pct")

ggplot(race_2025, aes(x = RaceGroup, y = Pct, fill = MemberStatus)) +
  geom_col(width = 0.4) +
  geom_text(
    aes(label = paste0(round(Pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "AAPCHO Members" = "#3C6E8A",
    "Non-Member" = "#CBD5C6"
  )) +
  labs(
    title = "2025 Share of AA, NH, and PI Race \nGroups Served by AAPCHO Members",
    x = NULL,
    y = "Proportion of Total Patients in Race Group",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
# Disaggregated AANHPI ----------------------------------------------------

##AANHPI Disaggregated by State 
#Total Asian by state
AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(ReportingYear, HealthCenterState) %>%
  summarise(
    `Total Asian` = sum(T3b_L1_Cd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total Asian`))


#Total NH by state
AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(ReportingYear, HealthCenterState) %>%
  summarise(
    `Total NH` = sum(T3b_L2a_Cd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total NH`))
#Total PI by state 

AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(ReportingYear, HealthCenterState) %>%
  summarise(
    `Total PI` = sum(T3b_L2b_Cd, T3b_L2c_Cd, T3b_L2d_Cd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(`Total PI`))

##Disaggregated AANHPI - National
NationalAANHPI_by_year <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Asian Indian` = sum(T3B_L1A_CD, na.rm = TRUE),
    `Chinese` = sum(T3B_L1B_CD, na.rm = TRUE),
    `Filipino` = sum(T3B_L1C_CD, na.rm = TRUE),
    `Japanese` = sum(T3B_L1D_CD, na.rm = TRUE),
    `Korean` = sum(T3B_L1E_CD, na.rm = TRUE),
    `Vietnamese` = sum(T3B_L1F_CD, na.rm = TRUE),
    `Other Asian` = sum(T3B_L1G_CD, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Other Pacific Islander` = sum(T3b_L2b_Cd, na.rm = TRUE),
    `Guamanian or Chamorro` = sum(T3B_L2C_CD, na.rm = TRUE),
    `Samoan` = sum(T3B_L2D_CD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Disaggregated Race Group", values_to = "Patients")


racegroup_increase <- NationalAANHPI_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


NationalAANHPI_2025 <- racegroup_increase %>%
  select('Disaggregated Race Group', Patients = y2025)


max_patients <- max(NationalAANHPI_2025$Patients, na.rm = TRUE)

racegroup_increase <- racegroup_increase %>%
  mutate(label_y = y2025 + max_patients * 0.1)

AANHPIorder <- c("Asian Indian", 
                 "Chinese", 
                 "Filipino",
                 "Japanese","Korean", "Vietnamese", "Other Asian", 
                 "Native Hawaiian", 
                 "Guamanian or Chamorro", 
                 "Samoan",
                 "Other Pacific Islander")

ggplot(NationalAANHPI_2025, aes(factor(x = `Disaggregated Race Group`, levels = AANHPIorder),  y = Patients, fill = `Disaggregated Race Group`)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 3.0) +
   scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "Asian Indian" = "#B25833", 
    "Chinese" = "#B66F23", 
    "Filipino" = "#B98714",
    "Japanese" = "#BD9E04", 
    "Korean" = "#CEB84E", 
    "Vietnamese" = "#D6C673",
    "Other Asian" = "#E7E0BD",
    "Native Hawaiian" = "#546885",
    "Other Pacific Islander" = "#6883A8",
    "Guamanian or Chamorro" = "#AAB7C6",
    "Samoan" = "#D2D1C8"
  )) +
  labs(
    title = "2025 National Health Center Patients: \nAANHPI Disaggregated",
    x = "Disaggregated Race Group",
    y = "Patients"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#B2334F", size = 9),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )
##Disaggregated AANHPI - Members
AAPCHOMembersAANHPI_by_year <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    `Asian Indian` = sum(T3B_L1A_CD, na.rm = TRUE),
    `Chinese` = sum(T3B_L1B_CD, na.rm = TRUE),
    `Filipino` = sum(T3B_L1C_CD, na.rm = TRUE),
    `Japanese` = sum(T3B_L1D_CD, na.rm = TRUE),
    `Korean` = sum(T3B_L1E_CD, na.rm = TRUE),
    `Vietnamese` = sum(T3B_L1F_CD, na.rm = TRUE),
    `Other Asian` = sum(T3B_L1G_CD, na.rm = TRUE),
    `Native Hawaiian` = sum(T3b_L2a_Cd, na.rm = TRUE),
    `Other Pacific Islander` = sum(T3b_L2b_Cd, na.rm = TRUE),
    `Guamanian or Chamorro` = sum(T3B_L2C_CD, na.rm = TRUE),
    `Samoan` = sum(T3B_L2D_CD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-ReportingYear, names_to = "Disaggregated Race Group", values_to = "Patients")


racegroup_increase <- AAPCHOMembersAANHPI_by_year %>%
  pivot_wider(names_from = ReportingYear, values_from = Patients, names_prefix = "y") %>%
  mutate(
    increase = y2025 - y2021,
    pct_change = round((y2025 - y2021) / y2021 * 100, 1),
    label_text = paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
  )


AAPCHOMembersAANHPI_2025 <- racegroup_increase %>%
  select('Disaggregated Race Group', Patients = y2025)


max_patients <- max(AAPCHOMembersAANHPI_2025$Patients, na.rm = TRUE)

racegroup_increase <- racegroup_increase %>%
  mutate(label_y = y2025 + max_patients * 0.1)

AANHPIorder <- c("Asian Indian", 
                 "Chinese", 
                 "Filipino",
                 "Japanese","Korean", "Vietnamese", "Other Asian", 
                 "Native Hawaiian", 
                 "Guamanian or Chamorro", 
                 "Samoan",
                 "Other Pacific Islander")

ggplot(AAPCHOMembersAANHPI_2025, aes(factor(x = `Disaggregated Race Group`, levels = AANHPIorder),  y = Patients, fill = `Disaggregated Race Group`)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = comma(Patients)), vjust = -0.5, size = 3.0, fontface = "bold") +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(
    "Asian Indian" = "#B25833", 
    "Chinese" = "#B66F23", 
    "Filipino" = "#B98714",
    "Japanese" = "#BD9E04", 
    "Korean" = "#CEB84E", 
    "Vietnamese" = "#D6C673",
    "Other Asian" = "#E7E0BD",
    "Native Hawaiian" = "#546885",
    "Other Pacific Islander" = "#6883A8",
    "Guamanian or Chamorro" = "#AAB7C6",
    "Samoan" = "#D2D1C8"
  )) +
  labs(
    title = "2025 AAPCHO Member Patients: \nAA, NH/PI Disaggregated",
    x = "Disaggregated Race Group",
    y = "Patients"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#B2334F", size = 9),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )

# LOE - 2021,2025 Bar Chart ---------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#Deciding between median or mean for avg rate of growth
members_LOE %>% summarise(mean_val = mean(pct_change, na.rm = TRUE), median_val = median(pct_change, na.rm = TRUE))

LOEmembers_avg_growth <- members_LOE %>%
  summarise(median_pct_change = median(pct_change, na.rm = TRUE)) %>%
  mutate(Group = "AAPCHO Members")
National_LOE <- National_HCsbothyears %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    NationalLOE_patients = sum(T3b_L12_Ca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = ReportingYear, values_from = NationalLOE_patients, names_prefix = "y") %>%
  mutate(pct_change = (y2025 - y2021) / y2021 * 100) %>%
  filter(is.finite(pct_change))
LOEnational_avg_growth <- National_LOE %>%
  summarise(median_pct_change = median(pct_change, na.rm = TRUE)) %>%
  mutate(Group = "National")

avg_LOE_growth_combined <- bind_rows(LOEmembers_avg_growth, LOEnational_avg_growth)

avg_LOE_growth_combined_labels <- avg_LOE_growth_combined %>%
  mutate(
    growth_pct = round(median_pct_change, 2),
    label = paste0(ifelse(growth_pct >= 0, "+", ""), growth_pct, "%")
  )

members_LOE_totals <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(LOE_patients = sum(T3b_L12_Ca, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = "AAPCHO Members")

national_LOE_totals <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(LOE_patients = sum(T3b_L12_Ca, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = "National")

combined_LOE_totals <- bind_rows(members_LOE_totals, national_LOE_totals)


ggplot(combined_LOE_totals, aes(x = as.character(ReportingYear), y = LOE_patients, fill = as.character(ReportingYear))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(LOE_patients)), vjust = -0.5, size = 3.5) +
  geom_text(
    data = avg_LOE_growth_combined_labels,
    aes(x = "2025", y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 2,
    fontface = "bold",
    color = "#B2334F",
    size = 3.5
  ) +
    facet_wrap(~Group, scales = "free_y") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("2021" = "#D6C673", "2025" = "#546885")) +
  labs(
    title = "Patients Best Served \nin a Language Other than English",
    x = NULL,
    y = "Count of Patients",
    caption = "Percent label = median rate of growth across individual health centers"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    plot.caption = element_text(color = "#B2334F", hjust = 0.5, size = 10, margin = margin(t = 10))
  )
#excluded 
National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(LOE_patients = sum(T3b_L12_Ca, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = ReportingYear, values_from = LOE_patients, names_prefix = "y") %>%
  mutate(pct_change = (y2025 - y2021) / y2021 * 100) %>%
  filter(!is.finite(pct_change)) %>%
  nrow()

#median LOE proportion
library(dplyr)

members_LOE_pct <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(GrantNumber) %>%
  summarise(
    LOE_patients = sum(T3b_L12_Ca, na.rm = TRUE),
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(pct_LOE = LOE_patients / total_patients * 100) %>%
  filter(is.finite(pct_LOE)) %>%
  summarise(median_pct_LOE = median(pct_LOE, na.rm = TRUE)) %>%
  mutate(Group = "AAPCHO Members")


national_LOE_pct <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  group_by(GrantNumber) %>%
  summarise(
    LOE_patients = sum(T3b_L12_Ca, na.rm = TRUE),
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(pct_LOE = LOE_patients / total_patients * 100) %>%
  filter(is.finite(pct_LOE)) %>%
  summarise(median_pct_LOE = median(pct_LOE, na.rm = TRUE)) %>%
  mutate(Group = "National")

bind_rows(members_LOE_pct, national_LOE_pct)

#count enabling service virtual and clinic visits
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T5_L29_Cb,T5_L29_Cb2)), na.rm = TRUE)))

# Trainees - No Viz----------------------------------------------------------------
#Pre+post-grad

AAPCHOMembers20212025 %>%
  group_by(GrantNumber,ReportingYear) %>%
  summarise(AAPCHOtotal = sum(rowSums(across(c(Twfc_L2.1_Ca,
                                         Twfc_L2.1_Cb,
                                         Twfc_L2.1a_Cb,
                                         Twfc_L2.1b_Cb,
                                         Twfc_L2.1c_Cb,
                                         Twfc_L2.1d_Cb,
                                         Twfc_L2.1e_Cb,
                                         Twfc_L2.1f_Cb,
                                         Twfc_L2.2_Ca,
                                         Twfc_L2.2_Cb,
                                         Twfc_L2.3_Ca,
                                         Twfc_L2.3_Cb,
                                         Twfc_L2.4_Ca,
                                         Twfc_L2.4_Cb,
                                         Twfc_L2.5_Ca,
                                         Twfc_L2.5_Cb,
                                         Twfc_L2.6_Ca,
                                         Twfc_L2.6_Cb,
                                         Twfc_L2.7_Ca,
                                         Twfc_L2.7_Cb,
                                         Twfc_L2.8_Ca,
                                         Twfc_L2.8_Cb,
                                         Twfc_L2.9_Ca,
                                         Twfc_L2.9_Cb,
                                         Twfc_L2.10_Ca,
                                         Twfc_L2.10_Cb,
                                         Twfc_L2.10a_Ca,
                                         Twfc_L2.10a_Cb,
                                         Twfc_L2.11_Cb,
                                         Twfc_L2.12_Ca,
                                         Twfc_L2.12_Cb,
                                         Twfc_L2.13_Ca,
                                         Twfc_L2.13_Cb,
                                         Twfc_L2.14_Ca,
                                         Twfc_L2.14_Cb,
                                         Twfc_L2.15_Ca,
                                         Twfc_L2.15_Cb,
                                         Twfc_L2.16_Ca,
                                         Twfc_L2.16_Cb,
                                         Twfc_L2.17_Ca,
                                         Twfc_L2.17_Cb,
                                         Twfc_L2.18_Ca,
                                         Twfc_L2.18_Cb,
                                         Twfc_L2.19_Ca,
                                         Twfc_L2.19_Cb,
                                         Twfc_L2.20_Ca,
                                         Twfc_L2.20_Cb,
                                         Twfc_L2.21_Ca,
                                         Twfc_L2.21_Cb,
                                         Twfc_L2.22_Ca,
                                         Twfc_L2.22_Cb,
                                         Twfc_L2.23_Ca,
                                         Twfc_L2.23_Cb,
                                         Twfc_L2.24_Ca,
                                         Twfc_L2.24_Cb,
                                         Twfc_L2.25_Ca,
                                         Twfc_L2.25_Cb)), na.rm = TRUE)),
            .groups = "drop"
  ) %>%
  group_by(ReportingYear) %>%
  summarise(median_total = median(AAPCHOtotal, na.rm = TRUE))

National20212025 %>%
  group_by(GrantNumber,ReportingYear) %>%
  summarise(Nationaltotal = sum(rowSums(across(c(Twfc_L2.1_Ca,
                                         Twfc_L2.1_Cb,
                                         Twfc_L2.1a_Cb,
                                         Twfc_L2.1b_Cb,
                                         Twfc_L2.1c_Cb,
                                         Twfc_L2.1d_Cb,
                                         Twfc_L2.1e_Cb,
                                         Twfc_L2.1f_Cb,
                                         Twfc_L2.2_Ca,
                                         Twfc_L2.2_Cb,
                                         Twfc_L2.3_Ca,
                                         Twfc_L2.3_Cb,
                                         Twfc_L2.4_Ca,
                                         Twfc_L2.4_Cb,
                                         Twfc_L2.5_Ca,
                                         Twfc_L2.5_Cb,
                                         Twfc_L2.6_Ca,
                                         Twfc_L2.6_Cb,
                                         Twfc_L2.7_Ca,
                                         Twfc_L2.7_Cb,
                                         Twfc_L2.8_Ca,
                                         Twfc_L2.8_Cb,
                                         Twfc_L2.9_Ca,
                                         Twfc_L2.9_Cb,
                                         Twfc_L2.10_Ca,
                                         Twfc_L2.10_Cb,
                                         Twfc_L2.10a_Ca,
                                         Twfc_L2.10a_Cb,
                                         Twfc_L2.11_Cb,
                                         Twfc_L2.12_Ca,
                                         Twfc_L2.12_Cb,
                                         Twfc_L2.13_Ca,
                                         Twfc_L2.13_Cb,
                                         Twfc_L2.14_Ca,
                                         Twfc_L2.14_Cb,
                                         Twfc_L2.15_Ca,
                                         Twfc_L2.15_Cb,
                                         Twfc_L2.16_Ca,
                                         Twfc_L2.16_Cb,
                                         Twfc_L2.17_Ca,
                                         Twfc_L2.17_Cb,
                                         Twfc_L2.18_Ca,
                                         Twfc_L2.18_Cb,
                                         Twfc_L2.19_Ca,
                                         Twfc_L2.19_Cb,
                                         Twfc_L2.20_Ca,
                                         Twfc_L2.20_Cb,
                                         Twfc_L2.21_Ca,
                                         Twfc_L2.21_Cb,
                                         Twfc_L2.22_Ca,
                                         Twfc_L2.22_Cb,
                                         Twfc_L2.23_Ca,
                                         Twfc_L2.23_Cb,
                                         Twfc_L2.24_Ca,
                                         Twfc_L2.24_Cb,
                                         Twfc_L2.25_Ca,
                                         Twfc_L2.25_Cb)), na.rm = TRUE)),
            .groups = "drop"
  ) %>%
  group_by(ReportingYear) %>%
  summarise(median_total = median(Nationaltotal, na.rm = TRUE))


# FPL - Stacked Bar --------------------------------------------------------------------


## Could do custom grouping #101-200% FPL
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L2_Ca,T4_L3_Ca)), na.rm = TRUE)))

#Over 200% FPL
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T4_L4_Ca, na.rm = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# T4_L1_Ca - 100% and below
# T4_L2_Ca - 101–150% 
# T4_L3_Ca - 151–200%
# T4_L4_Ca - Over 200%
# T4_L5_Ca - Unknown
# T4_L6_Ca - Total
members_FPL <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber, ReportingYear) %>%
  summarise(
    `100% and below FPL` = sum(T4_L1_Ca, na.rm = TRUE),
    `101-150% FPL` = sum(T4_L2_Ca, na.rm = TRUE),
    `151-200% FPL` = sum(T4_L3_Ca, na.rm = TRUE),
    `Over 200% FPL` = sum(T4_L4_Ca, na.rm = TRUE),
    `Unknown FPL` = sum(T4_L5_Ca, na.rm = TRUE),
    `Total FPL` = sum(T4_L6_Ca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "AAPCHO Members")
 
national_FPL <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(GrantNumber,ReportingYear) %>%
  summarise(
    `100% and below FPL` = sum(T4_L1_Ca, na.rm = TRUE),
    `101-150% FPL` = sum(T4_L2_Ca, na.rm = TRUE),
    `151-200% FPL` = sum(T4_L3_Ca, na.rm = TRUE),
    `Over 200% FPL` = sum(T4_L4_Ca, na.rm = TRUE),
    `Unknown FPL` = sum(T4_L5_Ca, na.rm = TRUE),
    `Total FPL` = sum(T4_L6_Ca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "National")
 

    
#below combinedFPL from when did not group by GrantNumber
# combinedFPL <- bind_rows(members_FPL, national_FPL) %>%
#   pivot_longer(
#     cols = c(`100% and below FPL`, `101-150% FPL`, `151-200% FPL`, `Over 200% FPL`, `Unknown FPL`),
#     names_to = "Percent Federal Poverty Level (FPL)",
#     values_to = "Patients"
#   ) %>%
#   group_by(Group, ReportingYear) %>%
#   mutate(Pct = Patients / sum(Patients) * 100) %>%
#   ungroup() %>%
#   mutate(`Percent Federal Poverty Level (FPL)` = factor(
#     `Percent Federal Poverty Level (FPL)`,
#     levels = c("100% and below FPL", "101-150% FPL", "151-200% FPL", "Over 200% FPL", "Unknown FPL")
#   ))

combinedFPL <- bind_rows(members_FPL, national_FPL) %>%
  pivot_longer(
    cols = c(`100% and below FPL`, `101-150% FPL`, `151-200% FPL`, `Over 200% FPL`, `Unknown FPL`),
    names_to = "Percent Federal Poverty Level (FPL)",
    values_to = "Patients"
  ) %>%
  group_by(Group, ReportingYear, `Percent Federal Poverty Level (FPL)`) %>%
  summarise(Patients = sum(Patients, na.rm = TRUE), .groups = "drop") %>%   # <-- collapses across GrantNumber
  group_by(Group, ReportingYear) %>%
  mutate(Pct = Patients / sum(Patients) * 100) %>%
  ungroup() %>%
  mutate(`Percent Federal Poverty Level (FPL)` = factor(
    `Percent Federal Poverty Level (FPL)`,
    levels = c("100% and below FPL", "101-150% FPL", "151-200% FPL", "Over 200% FPL", "Unknown FPL")
  ))

ggplot(combinedFPL, aes(x = as.character(ReportingYear), y = Pct, fill = `Percent Federal Poverty Level (FPL)`)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse(Pct >= 3, paste0(round(Pct), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.0
  ) +
  facet_wrap(~Group) +
  scale_y_continuous(limits = c(0, 115)) +
  scale_fill_manual(values = c(
    "100% and below FPL" = "#3C6E8A",
    "101-150% FPL" = "#CBD5C6",
    "151-200% FPL" = "#A8C4B8",
    "Over 200% FPL" = "#E8B4A0",
    "Unknown FPL" = "#D9D9D9"
  )) +
  labs(
    title = "Patients - Percent Federal Poverty Level (FPL)",
    x = "Reporting Year",
    y = "Proportion of Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

# Insurance Status - Stacked Bar --------------------------------------------------------------------

##None/Uninsured 
###0-17
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T4_L7_Ca, na.rm = TRUE))
###18 and up
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T4_L7_Cb, na.rm = TRUE))

#Total Uninsured
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L7_Ca, T4_L7_Cb)), na.rm = TRUE)))


##Total Medicaid
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L8_Ca, T4_L8_Cb)), na.rm = TRUE)))

##Dually Eligible (Medicare and Medicaid)
###
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L9a_Ca, T4_L9a_Cb)), na.rm = TRUE)))
##Total Medicare*includes dually eligible - HRSA counts dually eligible towards Medicaid
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L9_Ca,T4_L9_Cb)), na.rm = TRUE)))
##Other Public Ins Non-Chip, Total
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L10A_OTHER,
                                 T4_L10a_Ca,
                                 T4_L10a_Cb,
                                 T4_L10b_Ca,
                                 T4_L10b_Cb)), na.rm = TRUE)))
# ##Total Public Insurance <- seems wrong
# AAPCHOMembers20212025 %>%
#   group_by(ReportingYear) %>%
#   summarise(
#     total = sum(rowSums(across(c(T4_L10_Ca,T4_L10_Cb)), na.rm = TRUE)))
##Private Insurance
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    total = sum(rowSums(across(c(T4_L11_Ca,T4_L11_Ca)), na.rm = TRUE)))
# [stacked bar using ggplot]
insurance_members <- AAPCHOMembers20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    Uninsured   = sum(rowSums(across(c(T4_L7_Ca, T4_L7_Cb)), na.rm = TRUE)),
    'Medicaid (incl. CHIP)'    = sum(rowSums(across(c(T4_L8_Ca, T4_L8_Cb)), na.rm = TRUE)),
    'Medicare*'    = sum(rowSums(across(c(T4_L9_Ca, T4_L9_Cb)), na.rm = TRUE)),
    'Other Public**' = sum(rowSums(across(c(T4_L10A_OTHER, T4_L10a_Ca, T4_L10a_Cb,
                                       T4_L10b_Ca, T4_L10b_Cb)), na.rm = TRUE)),
    Private     = sum(rowSums(across(c(T4_L11_Ca, T4_L11_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(Group = "AAPCHO Members")

insurance_national <- National20212025 %>%
  filter(ReportingYear %in% c(2021, 2025)) %>%
  group_by(ReportingYear) %>%
  summarise(
    Uninsured   = sum(rowSums(across(c(T4_L7_Ca, T4_L7_Cb)), na.rm = TRUE)),
    'Medicaid (incl. CHIP)'    = sum(rowSums(across(c(T4_L8_Ca, T4_L8_Cb)), na.rm = TRUE)),
   'Medicare*'    = sum(rowSums(across(c(T4_L9_Ca, T4_L9_Cb)), na.rm = TRUE)),
    'Other Public**' = sum(rowSums(across(c(T4_L10A_OTHER, T4_L10a_Ca, T4_L10a_Cb,
                                       T4_L10b_Ca, T4_L10b_Cb)), na.rm = TRUE)),
    Private     = sum(rowSums(across(c(T4_L11_Ca, T4_L11_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(Group = "National")

insurance_combined <- bind_rows(insurance_members, insurance_national) %>%
  pivot_longer(-c(Group, ReportingYear), names_to = "Insurance", values_to = "Patients") %>%
  group_by(Group, ReportingYear) %>%
  mutate(Pct = Patients / sum(Patients) * 100) %>%
  ungroup() %>%
  mutate(Insurance = factor(Insurance,
                            levels = c("Uninsured", "Medicaid (incl. CHIP)", "Medicare*", "Other Public**", "Private")))


ggplot(insurance_combined, aes(x = as.character(ReportingYear), y = Pct, fill = Insurance)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse(Pct >= 3, paste0(round(Pct), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.0
  ) +
  facet_wrap(~Group) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "Uninsured"   = "#E8B4A0",
    "Medicaid (incl. CHIP)"    = "#3C6E8A",
    "Medicare*"    = "#A8C4B8",
    "Other Public**" = "#D9D9D9",
    "Private"     = "#CBD5C6"
  )) +
  labs(
    title = "Patients - Insurance Type",
    x = "Reporting Year",
    y = "Proportion of Patients",
    fill = NULL,
    caption = "* Medicare total includes dually eligible (Medicare and Medicaid) patients, who are also counted under Medicaid, consistent with HRSA UDS reporting conventions.
    \n** Other Public Insurance accounts for less than 2% in each bar."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, size = 8, face = "italic")
  )


# Revenue -----------------------------------------------------------------
##Total Medicaid - Amt Collected T9D_L3_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L3_CB, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L3_CB, na.rm = TRUE))

National20212025 %>%
  group_by(ReportingYear) %>%
summarise(total = sum(rowSums(across(c(T9D_L1_CB, T9D_L2A_CB, T9D_L2B_CB)), na.rm = TRUE)))


##Total Medicare - Amt Collected T9D_L6_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L6_CB, na.rm = TRUE))
##Total Other Public - Amt Collected T9D_L9_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L9_CB, na.rm = TRUE))
##Total Private - Amt Collected T9D_L12_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L12_CB, na.rm = TRUE))
##Total Self-Pay - Amt Collected T9D_L13_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T9D_L13_CB, na.rm = TRUE))

# Capitated Member Months vs FFS - Stacked Bar ------------------------------------------
# T4_L13a_Ca		Medicaid (a)
# T4_L13a_Cb		Capitated Member Months	CMCb	Medicare (b)
# T4_L13a_Cc	Capitated Member Months	CMCc	Other Public Including Non-Medicaid CHIP (c)
# T4_L13a_Cd		Capitated Member Months	CMCd	Private (d)
# T4_L13a_Ce		Capitated Member Months	CMCe	TOTAL (e)


# T4_L13b_Ca		Fee-for-service Member Months	CMCa	Medicaid (a)
# T4_L13b_Cb	Fee-for-service Member Months	CMCb	Medicare (b)
# T4_L13b_Cc	Fee-for-service Member Months	CMCc	Other Public Including Non-Medicaid CHIP (c)
# T4_L13b_Cd		Fee-for-service Member Months	CMCd	Private (d)
# T4_L13b_Ce		Fee-for-service Member Months	CMCe	TOTAL (e)
# T4_L13c_Ca		Total Member Months (Sum of Lines 13a + 13b)	CMCa	Medicaid (a)
# T4_L13c_Cb		Total Member Months (Sum of Lines 13a + 13b)	CMCb	Medicare (b)
# T4_L13c_Cc		Total Member Months (Sum of Lines 13a + 13b)	CMCc	Other Public Including Non-Medicaid CHIP (c)
# T4_L13c_Cd	 	Total Member Months (Sum of Lines 13a + 13b)	CMCd	Private (d)
# T4_L13c_Ce		Total Member Months (Sum of Lines 13a + 13b)	CMCe	TOTAL (e)


# T6A - Select Diagnoses - Large bar chart, or separate --------------------------------------------------
##This uses median aross orgs; 12.3% AAPCHO; 11.1% national
members_diabetes <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == "2025") %>%
  group_by(GrantNumber) %>%
  summarise(
    diabetesdiagnosis = sum(T6a_L9_Cb, na.rm = TRUE),
    totpop = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_diabetes = diabetesdiagnosis / totpop * 100,
    pct_nondiabetic = 100 - pct_diabetes
  ) %>%
  filter(is.finite(pct_diabetes)) %>%
  summarise(
    `Patients with Diabetes Diagnosis` = median(pct_diabetes, na.rm = TRUE),
    `All Other Patients` = median(pct_nondiabetic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "AAPCHO Members")

national_diabetes <- National20212025 %>%
  filter(ReportingYear == "2025") %>%
  group_by(GrantNumber) %>%
  summarise(
    diabetesdiagnosis = sum(T6a_L9_Cb, na.rm = TRUE),
    totpop = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_diabetes = diabetesdiagnosis / totpop * 100,
    pct_nondiabetic = 100 - pct_diabetes
  ) %>%
  filter(is.finite(pct_diabetes)) %>%
  summarise(
    `Patients with Diabetes Diagnosis` = median(pct_diabetes, na.rm = TRUE),
    `All Other Patients` = median(pct_nondiabetic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Group = "National")

combineddiabetes <- bind_rows(members_diabetes, national_diabetes) %>%
  pivot_longer(-Group, names_to = "Category", values_to = "pct")

ggplot(combineddiabetes, aes(x = Group, y = pct, fill = Category)) +
  geom_col(width = 0.5) +
  geom_text(
    data = combineddiabetes %>% filter(Category == "All Other Patients"),
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  geom_text(
    data = combineddiabetes %>% filter(Category == "Patients with Diabetes Diagnosis"),
    aes(y = 105, label = paste0("Diabetes ", round(pct, 1), "%")),
    color = "#3C6E8A",
    fontface = "bold",
    size = 3.0
  ) +
  scale_y_continuous(limits = c(0, 115)) +
  scale_fill_manual(values = c(
    "Patients with Diabetes Diagnosis" = "#3C6E8A",
    "All Other Patients" = "#CBD5C6"
  )) +
  labs(
    title = "Share of Patients Served \nwith Diabetes Diagnosis",
    x = NULL,
    y = "Average Share of Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )
# 6a- Prevalent Chronic Conditions --------------------------------------------------
##Overweight and obesity T6a_L14a_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L14a_Cb, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L14a_Cb, na.rm = TRUE))
##Asthma
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L5_Cb, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L5_Cb, na.rm = TRUE))

##Diabetes mellitus 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L9_Cb, na.rm = TRUE))))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L9_Cb, na.rm = TRUE))


##Heart Disease(selected) 
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L10_Cb, na.rm = TRUE))

##Hypertension T6a_L11_Cb
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L11_Cb, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L11_Cb, na.rm = TRUE))

# 6a- AANHPI prevalent conditions -----------------------------------------
##All members vs national



library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


national_total_patients <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))

aanhpi_total_patients <- National20212025 %>%
  filter(ReportingYear == 2025, GrantNumber %in% AANHPI_serving_orgs) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))

nhpi_total_patients <- National20212025 %>%
  filter(ReportingYear == 2025, GrantNumber %in% NHPI_serving_orgs) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))

# ------------------------------------------------------------
# 2. Condition counts per group
# ------------------------------------------------------------
national_conditions <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Diabetes Mellitus` = sum(T6a_L9_Cb, na.rm = TRUE),
    `Hypertension` = sum(T6a_L11_Cb, na.rm = TRUE),
    `Hepatitis B` = sum(T6a_L4a_Cb, na.rm = TRUE),
    `Tuberculosis` = sum(T6a_L3_Cb, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "Patients") %>%
  mutate(
    total_patients = national_total_patients$total_patients,
    pct = Patients / total_patients * 100,
    Group = "National"
  )

aanhpi_conditions <- National20212025 %>%
  filter(ReportingYear == 2025, GrantNumber %in% AANHPI_serving_orgs) %>%
  summarise(
    `Diabetes Mellitus` = sum(T6a_L9_Cb, na.rm = TRUE),
    `Hypertension` = sum(T6a_L11_Cb, na.rm = TRUE),
    `Hepatitis B` = sum(T6a_L4a_Cb, na.rm = TRUE),
    `Tuberculosis` = sum(T6a_L3_Cb, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "Patients") %>%
  mutate(
    total_patients = aanhpi_total_patients$total_patients,
    pct = Patients / total_patients * 100,
    Group = "AANHPI-Serving"
  )

nhpi_conditions <- National20212025 %>%
  filter(ReportingYear == 2025, GrantNumber %in% NHPI_serving_orgs) %>%
  summarise(
    `Diabetes Mellitus` = sum(T6a_L9_Cb, na.rm = TRUE),
    `Hypertension` = sum(T6a_L11_Cb, na.rm = TRUE),
    `Hepatitis B` = sum(T6a_L4a_Cb, na.rm = TRUE),
    `Tuberculosis` = sum(T6a_L3_Cb, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "Patients") %>%
  mutate(
    total_patients = nhpi_total_patients$total_patients,
    pct = Patients / total_patients * 100,
    Group = "NHPI-Serving"
  )

# ------------------------------------------------------------
# 3. Combine and plot
# ------------------------------------------------------------
conditions_combined <- bind_rows(national_conditions, aanhpi_conditions, nhpi_conditions) %>%
  mutate(Group = factor(Group, levels = c("National", "AANHPI-Serving", "NHPI-Serving")))

ggplot(conditions_combined, aes(x = reorder(Condition, -pct), y = pct, fill = Group)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 2.8
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(
    "National" = "#CBD5C6",
    "AANHPI-Serving" = "#3C6E8A",
    "NHPI-Serving" = "#B25833"
  )) +
  labs(
    title = "2025 Prevalence of Selected Conditions",
    subtitle = "National vs AANHPI-Serving (\u226510% AANHPI patients) vs NHPI-Serving (\u22651,000 NHPI patients)",
    x = NULL,
    y = "Proportion of Total Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )
##This uses aggregate, not median; diabetes 11.3% and 10.7% respectively (see above for median approach)
##Tuberculosis ##need to check if this is new diagnosis or can include existing
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L3_Cb, na.rm = TRUE))

National20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T6a_L3_Cb, na.rm = TRUE))
##HepB
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(sum(T6a_L4a_Cb, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear) %>%
  summarise(sum(T6a_L4a_Cb, na.rm = TRUE))

# ##VISITS Exposure to Heat or Cold T6a_L14_Ca
# AAPCHOMembers20212025 %>%
#   group_by(ReportingYear)%>%
#   summarise(sum(T6a_L14_Ca, na.rm = TRUE))


# ##Perinatal and Neonatal conditions T6a_L16_Cb
# AAPCHOMembers20212025 %>%
#   group_by(ReportingYear)%>%
#   summarise(sum(T6a_L16_Cb, na.rm = TRUE))
# 
# ##Alcohol-related disorders
# AAPCHOMembers20212025 %>%
#   group_by(ReportingYear)%>%
#   summarise(sum(T6a_L18_Cb, na.rm = TRUE))
# 
# ##IPV  T6a_L20f_Cb
# AAPCHOMembers20212025 %>%
#   group_by(ReportingYear)%>%
#   summarise(sum(T6a_L20f_Cb, na.rm = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)



members_total_patients <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))

national_total_patients <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))


members_conditions <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Diabetes Mellitus` = sum(T6a_L9_Cb, na.rm = TRUE),
    `Hypertension` = sum(T6a_L11_Cb, na.rm = TRUE),
    `Hepatitis B` = sum(T6a_L4a_Cb, na.rm = TRUE),
  `Tuberculosis` = sum(T6a_L3_Cb, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "Patients") %>%
  mutate(
    total_patients = members_total_patients$total_patients,
    pct = Patients / total_patients * 100,
    Group = "AAPCHO Members"
  )

national_conditions <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Diabetes Mellitus` = sum(T6a_L9_Cb, na.rm = TRUE),
    `Hypertension` = sum(T6a_L11_Cb, na.rm = TRUE),
    `Hepatitis B` = sum(T6a_L4a_Cb, na.rm = TRUE),
    `Tuberculosis` = sum(T6a_L3_Cb, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Condition", values_to = "Patients") %>%
  mutate(
    total_patients = national_total_patients$total_patients,
    pct = Patients / total_patients * 100,
    Group = "National"
  )

conditions_combined <- bind_rows(members_conditions, national_conditions)

ggplot(conditions_combined, aes(x = reorder(Condition, -pct), y = pct, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.2
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("AAPCHO Members" = "#3C6E8A", "National" = "#A8C4B8")) +
  labs(
    title = "2025 Prevalence of Selected Conditions",
    x = NULL,
    y = "Proportion of Total Patients",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "open_sans"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )
# 6B- CQMs ----------------------------------------------------------------

##Cervical cancer %ofPatientstestedPap
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientstestedPap`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientstestedPap`, na.rm = TRUE))
##Mammogram
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientswithMammogram`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientswithMammogram`, na.rm = TRUE))
##BMI and counseling for nutrition (child)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ChildrenandAdolescentswithDocumentedCounselingandBMIPercentile`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ChildrenandAdolescentswithDocumentedCounselingandBMIPercentile`, na.rm = TRUE))
#Tobacco
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsAssessedforTobaccoUseandProvidedInterventionIfaTobaccoUser`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsAssessedforTobaccoUseandProvidedInterventionIfaTobaccoUser`, na.rm = TRUE))
##BMI and follow up (adult)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%AdultswithDocumentedBMIandFollow-upPlanIfWeightisOutsideParameters`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%AdultswithDocumentedBMIandFollow-upPlanIfWeightisOutsideParameters`, na.rm = TRUE))

##Statin therapy
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientsPrescribedOrOnStatinTherapy`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofPatientsPrescribedOrOnStatinTherapy`, na.rm = TRUE))
##IVD diagnosis
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofAdults18andolderwithIVDwithDocumentationOfAspirinOrOtherAntiplateletTherapy`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofAdults18andolderwithIVDwithDocumentationOfAspirinOrOtherAntiplateletTherapy`, na.rm = TRUE))
##CRC screening
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofAdultswithAppropriateScreeningforColorectalCancer`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%ofAdultswithAppropriateScreeningforColorectalCancer`, na.rm = TRUE))
#HIV treatment
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsSeenWithin30DaysofFirstDiagnosisofHIV`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsSeenWithin30DaysofFirstDiagnosisofHIV`, na.rm = TRUE))
##HIV testing
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsTestedforHIV`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsTestedforHIV`, na.rm = TRUE))
##Patients 12 and up screened for depression
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsScreenedforDepressionandFollowupPlanDocumentedasAppropriate`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsScreenedforDepressionandFollowupPlanDocumentedasAppropriate`, na.rm = TRUE))

##Dental sealant
AAPCHOMembers20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsAged6-9WithSealantsToFirstMolars`, na.rm = TRUE))
National20212025 %>%
  group_by(ReportingYear)%>%
  summarise(mean(`%PatientsAged6-9WithSealantsToFirstMolars`, na.rm = TRUE))
#early entry prenatal

#initiation and egagement of sud %PatientsAged13AndOlderWhoInitiatedNewSUDTreatment
#childhood immuniz  %ofPatientsImmunized 


# 7 - Disaggregated CQMs -----------------------------------------------------------------
##Hypertension
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    weighted_avg_ratio = sum(((T7_Li_C2c / T7_Li_C2b) * T7_Li_C2a)*100, na.rm = TRUE) /
      sum(T7_Li_C2a, na.rm = TRUE)
  )
National20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    weighted_avg_ratio = sum(((T7_Li_C2c / T7_Li_C2b) * T7_Li_C2a)*100, na.rm = TRUE) /
      sum(T7_Li_C2a, na.rm = TRUE)
  )
##Diabetes (uncontrolled)
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    weighted_avg_ratio = sum(((T7_Li_C3f / T7_Li_C3b) * T7_Li_C3a)*100, na.rm = TRUE) /
      sum(T7_Li_C3a, na.rm = TRUE)
  )
National20212025 %>%
  group_by(ReportingYear) %>%
  summarise(
    weighted_avg_ratio = sum(((T7_Li_C3f / T7_Li_C3b) * T7_Li_C3a)*100, na.rm = TRUE) /
      sum(T7_Li_C3a, na.rm = TRUE)
  )




members_total_patients <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))

national_total_patients <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))


members_6B <- AAPCHOMembers20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Cervical Cancer Screening` = mean(`%ofPatientstestedPap`, na.rm = TRUE),
    `Breast Cancer Screening` = mean(`%ofPatientswithMammogram`, na.rm = TRUE),
    `Colorectal Cancer Screening` = mean(`%ofAdultswithAppropriateScreeningforColorectalCancer`, na.rm = TRUE),
    `BMI & Nutrition \nCounseling (Child)` = mean(`%ChildrenandAdolescentswithDocumentedCounselingandBMIPercentile`, na.rm = TRUE),
    `BMI and Follow-Up (Adult)` = mean(`%AdultswithDocumentedBMIandFollow-upPlanIfWeightisOutsideParameters`, na.rm = TRUE),
    `Statin Therapy` = mean(`%ofPatientsPrescribedOrOnStatinTherapy`, na.rm = TRUE),
    `Ischemic Vascular Disease` = mean(`%ofAdults18andolderwithIVDwithDocumentationOfAspirinOrOtherAntiplateletTherapy`, na.rm = TRUE),
    `Patients Aged 12+ Screened for Depression` = mean(`%PatientsScreenedforDepressionandFollowupPlanDocumentedasAppropriate`, na.rm = TRUE),
    `Dental Sealants for Children 6-9 years` = mean(`%PatientsAged6-9WithSealantsToFirstMolars`, na.rm = TRUE),
    `Childhood Immunization Status` =  mean(`%ofPatientsImmunized`, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "CQM", values_to = "Outcome") %>%
  mutate(Group = "AAPCHO Members")

national_6B <- National20212025 %>%
  filter(ReportingYear == 2025) %>%
  summarise(
    `Cervical Cancer Screening` = mean(`%ofPatientstestedPap`, na.rm = TRUE),
    `Breast Cancer Screening` = mean(`%ofPatientswithMammogram`, na.rm = TRUE),
    `Colorectal Cancer Screening` = mean(`%ofAdultswithAppropriateScreeningforColorectalCancer`, na.rm = TRUE),
    `BMI & Nutrition \nCounseling (Child)` = mean(`%ChildrenandAdolescentswithDocumentedCounselingandBMIPercentile`, na.rm = TRUE),
    `BMI and Follow-Up (Adult)` = mean(`%AdultswithDocumentedBMIandFollow-upPlanIfWeightisOutsideParameters`, na.rm = TRUE),
    `Statin Therapy` = mean(`%ofPatientsPrescribedOrOnStatinTherapy`, na.rm = TRUE),
    `Ischemic Vascular Disease` = mean(`%ofAdults18andolderwithIVDwithDocumentationOfAspirinOrOtherAntiplateletTherapy`, na.rm = TRUE),
    `Patients Aged 12+ Screened for Depression` = mean(`%PatientsScreenedforDepressionandFollowupPlanDocumentedasAppropriate`, na.rm = TRUE),
    `Dental Sealants for Children 6-9 years` = mean(`%PatientsAged6-9WithSealantsToFirstMolars`, na.rm = TRUE),
    `Childhood Immunization Status` =  mean(`%ofPatientsImmunized`, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "CQM", values_to = "Outcome") %>%
  mutate(Group = "National")

combined_6B <- bind_rows(members_6B, national_6B)

ggplot(combined_6B, aes(x = CQM, y = Outcome, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(Outcome, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.0
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("AAPCHO Members" = "#3C6E8A", "National" = "#A8C4B8")) +
  labs(
    title = "2025 Select Quality of Care Measure Performance",
    x = "CQM",
    y = "Average Percentage of Patients that Met Quality Measure Definiton",
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )

AAPCHOMembers20212025
