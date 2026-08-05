##Analysis notes
#Suppression applied to Table 5 (all years), Table 8A (2021-2023), Table 9D


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


# Calculations for Narrative ----------------------------------------------

# A -----------------------------------------------------------------------
#Members total patients
AAPCHOMembers20212025 %>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))
# 1 2021          555569
# 2 2022          580675
# 3 2023          607872
# 4 2024          660946
##validated against Excel
#National total patients 
National20212024%>%
  group_by(ReportingYear) %>%
  summarise(total = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)))
# 1 2021          30193278
# 2 2022          30517276
# 3 2023          31277341
# 4 2024          32387774
##validated against HRSA website
#Members vs. National patient growth, 2021-2024
#((2025-2021)/2021)*100
###Grouped bar
library(dplyr)
library(ggplot2)
library(scales)

members_totalpatients <- AAPCHOMembers20212024 %>%
  filter(ReportingYear %in% c(2021, 2024)) %>%
  group_by(ReportingYear) %>%
  summarise(
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(Group = "AAPCHO Members")

national_totalpatients <- National20212024 %>%
  filter(ReportingYear %in% c(2021, 2024)) %>%
  group_by(ReportingYear) %>%
  summarise(
    total_patients = sum(rowSums(across(c(T3a_L39_Ca, T3a_L39_Cb)), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(Group = "National")


combined_totals <- bind_rows(members_totalpatients, national_totalpatients)

growth_labels <- combined_totals %>%
  tidyr::pivot_wider(names_from = ReportingYear, values_from = total_patients, names_prefix = "y") %>%
  mutate(
    growth_pct = round((y2024 - y2021) / y2021 * 100, 1),
    label = paste0(ifelse(growth_pct >= 0, "+", ""), growth_pct, "%")
  )


combined_totals$ReportingYear <- factor(combined_totals$ReportingYear, levels = c(2021, 2024))


ggplot(combined_totals, aes(x = as.character(ReportingYear), y = total_patients, fill = ReportingYear)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(total_patients)), vjust = -0.5, size = 3.5) +
  geom_text(
    data = growth_labels,
    aes(x = "2024", y = y2024, label = label),
    inherit.aes = FALSE,
    vjust = -2.2,
    fontface = "bold",
    color = "#B2334F",
    size = 4.5
  ) +
  facet_wrap(~Group, scales = "free_y") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("2021" = "#B29633", "2024" = "#B25833")) +
  labs(
    title = "Total Patients Served",
    x = NULL,
    y = "Total Patients"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Verdana"),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
    )
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

Membervisits_2024 <- AAPCHOMembers20212024 %>%
  filter(ReportingYear == 2024) %>%
  summarise(
    `Medical Service Visits` = sum(rowSums(across(c(T5_L15_Cb, T5_L15_Cb2)), na.rm = TRUE)),      # <- your column
    `Dental Service Visits` = sum(rowSums(across(c(T5_L19_Cb, T5_L19_Cb2)), na.rm = TRUE)),       # <- your column
    `Mental Health Visits` = sum(rowSums(across(c(T5_L20_Cb, T5_L20_Cb2)), na.rm = TRUE)),        # <- your column
    `Vision Visits` = sum(rowSums(across(c(T5_L22d_Cb, T5_L22d_Cb2)), na.rm = TRUE)),                # <- your column
    `Enabling Services` = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE))             # <- your column
  ) %>%
  pivot_longer(everything(), names_to = "Service", values_to = "Visits")

ggplot(Membervisits_2024, aes(x = reorder(Service, -Visits), y = Visits, fill = Service)) +
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
    title = "2024 Member Visits by Service Type",
    x = NULL,
    y = "Visits"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1),
   legend.position = "none"
  )
##Enabling services proportion of total stacked bar 
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

members_enablingvisits <- AAPCHOMembers20212024 %>%
  filter(ReportingYear == 2024) %>%
  summarise(
    enabling = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE)),   
    total = sum(rowSums(across(c(T5_L34_Cb, T5_L34_Cb2)), na.rm = TRUE))           
  ) %>%
  mutate(
    other = total - enabling,
    Group = "Members"
  )

national_enablingvisits <- National20212024 %>%
  filter(ReportingYear == 2024) %>%
  summarise(
    enabling = sum(rowSums(across(c(T5_L29_Cb, T5_L29_Cb2)), na.rm = TRUE)),   
    total = sum(rowSums(across(c(T5_L34_Cb, T5_L34_Cb2)), na.rm = TRUE)) 
  ) %>%
  mutate(
    other = total - enabling,
    Group = "National"
  )


combined <- bind_rows(members_enablingvisits, national_enablingvisits) %>%
  select(Group, `Enabling Services` = enabling, `All Other Services` = other) %>%
  pivot_longer(-Group, names_to = "Category", values_to = "Visits") %>%
  group_by(Group) %>%
  mutate(pct = Visits / sum(Visits)) %>%
  ungroup()


ggplot(combined, aes(x = Group, y = pct, fill = Category)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = percent(pct, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(
    "Enabling Services" = "#3C6E8A",
    "All Other Services" = "#CBD5C6"
  )) +
  labs(
    title = "Enabling Services as Percent of Total Visits (2024)",
    x = NULL,
    y = "Share of Total Visits",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Verdana"),
     plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )


