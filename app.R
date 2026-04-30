# ============================================================
# COSMI Hub V10 — Community Open Science Mapping Initiatives Hub
# ============================================================
#
# Purpose
# -------
# This Shiny application maps, explores and documents Open Science
# initiatives worldwide. It combines:
#   1. an overview dashboard with interactive maps and summary indicators;
#   2. analytical views by territory, typology, dimensions, economic role,
#      governance and diversity indicators;
#   3. a data/contribution area embedding Google Forms;
#   4. an about area embedding Google Docs for project information, FAQs
#      and methodological background.
#
# Data source
# -----------
# By default the app reads from a Google Sheet. To run offline, set
# use_google_sheets <- FALSE and provide a local Excel file path through
# the COSMI_LOCAL_DATA environment variable or the data/COSMIHUB.xlsx file.
#
# Code organisation
# -----------------
# 01. Packages and global options
# 02. Helper functions
# 03. Data loading and harmonisation
# 04. Theme, palette and UI helpers
# 05. User interface
# 06. Server: reusable renderers
# 07. Server: Overview
# 08. Server: Territories
# 09. Server: Typology / structure
# 10. Server: Open Science dimensions
# 11. Server: Economic role
# 12. Server: Governance and diversity
# 13. Server: Data export and app launch
#
# Notes for maintenance
# ---------------------
# - Keep all external URLs in the “UI helpers / external URLs” block.
# - Keep column-name harmonisation in one place so upstream spreadsheet
#   changes are easier to manage.
# - Plotly click interactions use explicit source names; avoid reusing the
#   same source identifier for different widgets.
# ============================================================


# ---- 01. Packages ----
# Dependencies are declared in one place. The app stops early with a clear message if one is missing.
packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "stringr", "ggplot2",
  "plotly", "DT", "leaflet", "googlesheets4", "readxl", "scales", "forcats", "readr", "purrr", "digest"
)

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(googlesheets4)
library(readxl)
library(scales)
library(forcats)
library(readr)
library(purrr)
library(digest)

# ---- 01b. Global options ----
# Raise the Shiny upload limit for potential local datasets.
options(shiny.maxRequestSize = 50 * 1024^2)

# ---- 02. Helper functions ----
# Reusable cleaning, normalisation and counting utilities.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

clean_names_local <- function(df) {
  names(df) <- names(df) |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_|_$", "") |>
    stringr::str_to_lower()
  df
}

normalise_empty <- function(x, unknown = "Unknown") {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  ifelse(is.na(x) | x == "" | x == "NA", unknown, x)
}

normalise_flag <- function(x) {
  x <- stringr::str_to_lower(normalise_empty(x, "unknown"))
  dplyr::case_when(
    x %in% c("yes", "y", "true", "1", "oui") ~ "Yes",
    x %in% c("no", "n", "false", "0", "non") ~ "No",
    stringr::str_detect(x, "possible|partial|maybe|partly") ~ "Possible",
    TRUE ~ "Unknown"
  )
}

first_existing <- function(df, candidates, default = NA_character_) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) > 0) df[[hit[1]]] else rep(default, nrow(df))
}


# ---- COSMI V2 taxonomy helpers ----
clean_text <- function(x) {
  x <- normalise_empty(x, "Unknown")
  x |> stringr::str_to_lower() |> stringr::str_replace_all("[_-]", " ") |> stringr::str_squish()
}

clean_yes_no_level <- function(x, na_as = "Unknown") {
  x <- clean_text(x)
  dplyr::case_when(
    x %in% c("yes", "y", "true", "1", "oui") ~ "Yes",
    x %in% c("possible", "partial", "partly", "maybe", "partiel") ~ "Partial / possible",
    x %in% c("no", "n", "false", "0", "non") ~ "No",
    x %in% c("not applicable", "n/a", "na") ~ "Not applicable",
    stringr::str_detect(x, "unknown|unclear|missing|to validate") ~ na_as,
    TRUE ~ stringr::str_to_sentence(x)
  )
}

clean_scope <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "international|global|world") ~ "International / global",
    stringr::str_detect(x, "regional|europe|africa|latin|asia|arab") ~ "Regional / supranational",
    stringr::str_detect(x, "national|country") ~ "National",
    stringr::str_detect(x, "local|city|institution") ~ "Local / institutional",
    TRUE ~ "Unknown / other"
  )
}

clean_actor <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "community|grassroots|volunteer|cooperative") ~ "Community-led",
    stringr::str_detect(x, "public|government|ministry|national|intergovernmental") ~ "Public sector",
    stringr::str_detect(x, "university|academic|research institution|learned society|scholarly society") ~ "Academic / scholarly",
    stringr::str_detect(x, "non profit|nonprofit|foundation|association|ngo|charity|consortium|membership") ~ "Non-profit / foundation",
    stringr::str_detect(x, "commercial|company|publisher|platform|vendor|for profit") ~ "Commercial / publisher",
    stringr::str_detect(x, "standard") ~ "Standards body",
    TRUE ~ "Other / mixed"
  )
}

clean_object_type <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "policy|guideline|declaration|recommendation|mandate") ~ "Policy / guidelines",
    stringr::str_detect(x, "infrastructure|repository|registry|archive|database|index|catalog|platform|tool|software") ~ "Infrastructure / tools",
    stringr::str_detect(x, "publishing|publication|journal|preprint|dissemination|book") ~ "Publishing / dissemination",
    stringr::str_detect(x, "network|community|association|consortium|membership|coalition") ~ "Network / community",
    stringr::str_detect(x, "funding|grant|finance|award") ~ "Funding / incentives",
    stringr::str_detect(x, "training|education|capacity|course|literacy") ~ "Training / capacity building",
    stringr::str_detect(x, "monitoring|observatory|evaluation|metrics|assessment") ~ "Monitoring / evaluation",
    stringr::str_detect(x, "research project|initiative|program|programme|project") ~ "Research / programme",
    TRUE ~ "Other / mixed"
  )
}

unesco_categories <- c(
  "Citizen science, open and participative",
  "Open Science infrastructure and tools",
  "Open access",
  "Open and responsible evaluation of science",
  "Open data",
  "Open dialogue with other knowledge systems",
  "Open education",
  "Open innovation",
  "Open reproducible research",
  "Policy, declarations and guidelines of open science"
)

clean_unesco_category <- function(x) {
  x_clean <- clean_text(x)
  out <- dplyr::case_when(
    stringr::str_detect(x_clean, "citizen|participative|participatory") ~ "Citizen science, open and participative",
    stringr::str_detect(x_clean, "infrastructure|tool|repository|platform|software") ~ "Open Science infrastructure and tools",
    stringr::str_detect(x_clean, "open access|access") ~ "Open access",
    stringr::str_detect(x_clean, "responsible evaluation|evaluation|metrics|assessment") ~ "Open and responsible evaluation of science",
    stringr::str_detect(x_clean, "open data|data") ~ "Open data",
    stringr::str_detect(x_clean, "dialogue|knowledge systems|indigenous|knowledge") ~ "Open dialogue with other knowledge systems",
    stringr::str_detect(x_clean, "education|training|capacity|course") ~ "Open education",
    stringr::str_detect(x_clean, "innovation") ~ "Open innovation",
    stringr::str_detect(x_clean, "reproducible|reproducibility|reproduc") ~ "Open reproducible research",
    stringr::str_detect(x_clean, "policy|declaration|guideline|guidelines") ~ "Policy, declarations and guidelines of open science",
    TRUE ~ "Unknown"
  )
  factor(out, levels = unesco_categories)
}

clean_governance_family <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "community") ~ "Community-led",
    stringr::str_detect(x, "public|government|national|intergovernmental") ~ "Public-led",
    stringr::str_detect(x, "non profit|nonprofit|foundation|association|consortium|membership") ~ "Non-profit-led",
    stringr::str_detect(x, "commercial|company|publisher|platform") ~ "Commercial-led",
    stringr::str_detect(x, "academic|university|scholarly|learned") ~ "Academic-led",
    stringr::str_detect(x, "standard") ~ "Standards body",
    TRUE ~ "Other / mixed"
  )
}

clean_funding_model <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "apc|article processing") ~ "APC / author fees",
    stringr::str_detect(x, "membership|member") ~ "Membership",
    stringr::str_detect(x, "public|government|institutional|university|state") ~ "Public / institutional",
    stringr::str_detect(x, "grant|philanthrop|foundation|donation|sponsor") ~ "Grant / philanthropic",
    stringr::str_detect(x, "commercial|subscription|client|freemium|service|sales|market") ~ "Commercial / service revenue",
    stringr::str_detect(x, "volunteer|community|in kind") ~ "Community / in-kind",
    stringr::str_detect(x, "unknown|unclear") ~ "Unknown",
    TRUE ~ "Other / mixed"
  )
}

clean_economic_logic <- function(x) {
  x <- clean_text(x)
  dplyr::case_when(
    stringr::str_detect(x, "market|commercial|service|client|vendor") ~ "Market / service logic",
    stringr::str_detect(x, "membership|association|community") ~ "Membership / community logic",
    stringr::str_detect(x, "public|institutional|infrastructure") ~ "Public / infrastructure logic",
    stringr::str_detect(x, "grant|philanthrop|foundation|donation") ~ "Grant / philanthropic logic",
    stringr::str_detect(x, "diamond|non commercial|no fee|free") ~ "Non-commercial / diamond logic",
    stringr::str_detect(x, "unknown|unclear") ~ "Unknown",
    TRUE ~ "Other / mixed"
  )
}

safe_distinct_values <- function(x) {
  sort(unique(normalise_empty(x, "Unknown")))
}

top_count <- function(df, var, n = 12) {
  df |>
    filter(!is.na({{ var }}), {{ var }} != "", {{ var }} != "Unknown") |>
    count({{ var }}, sort = TRUE) |>
    slice_head(n = n)
}

completion_share <- function(df, vars) {
  vars <- intersect(vars, names(df))
  if (length(vars) == 0 || nrow(df) == 0) return(NA_real_)
  vals <- df |> select(all_of(vars))
  mean(!is.na(vals) & vals != "" & vals != "Unknown" & vals != "unknown / to validate")
}

cosmi_dt_options <- function(page_length = 10, scroll_y = NULL, dom = "Bfrtip", buttons = c("copy", "csv", "excel")) {
  opts <- list(
    pageLength = page_length,
    lengthMenu = c(8, 10, 15, 25, 50, 100),
    scrollX = TRUE,
    autoWidth = TRUE,
    searchHighlight = TRUE,
    language = list(search = "Search:", lengthMenu = "Show _MENU_ records")
  )
  if (!is.null(dom)) opts$dom <- dom
  if (!is.null(buttons)) opts$buttons <- buttons
  if (!is.null(scroll_y)) {
    opts$scrollY <- scroll_y
    opts$scroller <- TRUE
  }
  opts
}

plotly_cosmi_config <- function(p) {
  plotly::config(
    p,
    displaylogo = FALSE,
    responsive = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
  )
}

empty_plotly <- function(message = "No records match the current selection") {
  plotly::plot_ly() |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(text = message, x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 15)))
    ) |>
    plotly_cosmi_config()
}

# ---- 03. Data loading ----
# Read COSMI tables from Google Sheets by default, or from a local Excel workbook.
# Set COSMI_USE_GOOGLE_SHEETS=false for local/offline deployment.
use_google_sheets <- tolower(Sys.getenv("COSMI_USE_GOOGLE_SHEETS", "true")) %in% c("true", "1", "yes", "y", "oui")

sheet_url <- Sys.getenv(
  "COSMI_SHEET_URL",
  unset = "https://docs.google.com/spreadsheets/d/1o8r97sTmS3JQgwJD9kqpVm5WAucodZZqYOat5h8j-3o"
)

local_data_path <- Sys.getenv(
  "COSMI_LOCAL_DATA",
  unset = file.path("data", "COSMIHUB.xlsx")
)

candidate_local_paths <- unique(c(
  local_data_path,
  file.path("data", "COSMIHUB.xlsx"),
  file.path("data", "COSMIHUB (2).xlsx"),
  "COSMIHUB.xlsx",
  "COSMIHUB (2).xlsx"
))

read_table_safe <- function(sheet) {
  read_local <- function() {
    path <- candidate_local_paths[file.exists(candidate_local_paths)][1]
    if (is.na(path)) {
      stop("Local data file not found. Tried: ", paste(candidate_local_paths, collapse = ", "))
    }
    out <- readxl::read_excel(path, sheet = sheet, guess_max = 5000)
    attr(out, "cosmi_source") <- paste0("Local workbook: ", basename(path))
    out
  }

  out <- if (isTRUE(use_google_sheets)) {
    googlesheets4::gs4_deauth()
    tryCatch(
      {
        x <- googlesheets4::read_sheet(sheet_url, sheet = sheet)
        attr(x, "cosmi_source") <- "Google Sheets"
        x
      },
      error = function(e) {
        message("Google Sheets read failed; falling back to local workbook. Details: ", conditionMessage(e))
        read_local()
      }
    )
  } else {
    read_local()
  }
  out <- clean_names_local(out)
  attr(out, "cosmi_source") <- attr(out, "cosmi_source") %||% ifelse(isTRUE(use_google_sheets), "Google Sheets", "Local workbook")
  out
}


master_raw <- read_table_safe("initiatives_master")

# ---- 03b. Single-source data model ----
# The dashboard uses only initiatives_master as source of truth.
# The analytical tables formerly stored in separate spreadsheet tabs are generated here.

build_open_science_dimensions <- function(master_df) {
  dim_labels <- c(
    dim_citizen_science_participative = "Citizen science, open and participative",
    dim_open_infrastructure_tools = "Open Science infrastructure and tools",
    dim_open_access = "Open access",
    dim_open_responsible_evaluation = "Open and responsible evaluation of science",
    dim_open_data = "Open data",
    dim_open_dialogue_knowledge_systems = "Open dialogue with other knowledge systems",
    dim_open_education = "Open education",
    dim_open_innovation = "Open innovation",
    dim_open_reproducible_research = "Open reproducible research",
    dim_policy_guidelines = "Policy, declarations and guidelines of open science"
  )

  available_dims <- intersect(names(dim_labels), names(master_df))

  if (length(available_dims) == 0) {
    text <- paste(
      first_existing(master_df, c("original_category", "category"), ""),
      first_existing(master_df, c("object_type_openit", "initiative_type"), ""),
      first_existing(master_df, c("description_short", "description"), ""),
      first_existing(master_df, c("name", "initiative_name"), "")
    ) |> stringr::str_to_lower()

    master_df <- master_df |>
      mutate(
        dim_open_access = ifelse(stringr::str_detect(text, "open access|journal|publication|publishing|preprint"), "Yes", "No"),
        dim_open_data = ifelse(stringr::str_detect(text, "open data|data repository|dataset|data initiative"), "Yes", "No"),
        dim_open_reproducible_research = ifelse(stringr::str_detect(text, "reproduc|protocol|method|workflow|software"), "Possible", "No"),
        dim_open_responsible_evaluation = ifelse(stringr::str_detect(text, "evaluation|assessment|metrics|responsible"), "Yes", "No"),
        dim_policy_guidelines = ifelse(stringr::str_detect(text, "policy|declaration|guideline|recommendation"), "Yes", "No"),
        dim_open_education = ifelse(stringr::str_detect(text, "training|education|course|learn|capacity"), "Yes", "No"),
        dim_open_innovation = ifelse(stringr::str_detect(text, "innovation|industry|technology transfer"), "Possible", "No"),
        dim_open_infrastructure_tools = ifelse(stringr::str_detect(text, "infrastructure|tool|platform|repository|registry|database"), "Yes", "No"),
        dim_citizen_science_participative = ifelse(stringr::str_detect(text, "citizen|participatory|community science"), "Yes", "No"),
        dim_open_dialogue_knowledge_systems = ifelse(stringr::str_detect(text, "dialogue|knowledge|society|public engagement|indigenous"), "Possible", "No")
      )
    available_dims <- names(dim_labels)
  }

  master_df |>
    transmute(
      initiative_id = normalise_empty(first_existing(cur_data_all(), c("initiative_id", "id"))),
      name = normalise_empty(first_existing(cur_data_all(), c("name", "initiative_name"))),
      evidence = normalise_empty(first_existing(cur_data_all(), c("dimension_evidence_summary", "dimension_evidence", "coding_notes")), "")
    ) |>
    bind_cols(master_df |> select(all_of(available_dims))) |>
    mutate(across(all_of(available_dims), normalise_flag)) |>
    pivot_longer(cols = all_of(available_dims), names_to = "dimension_code", values_to = "level") |>
    mutate(dimension = unname(dim_labels[dimension_code])) |>
    select(initiative_id, name, dimension, level, evidence) |>
    filter(initiative_id != "Unknown", dimension != "Unknown")
}

build_economic_role <- function(master_df) {
  master_df |>
    transmute(
      initiative_id = normalise_empty(first_existing(cur_data_all(), c("initiative_id", "id"))),
      name = normalise_empty(first_existing(cur_data_all(), c("name", "initiative_name"))),
      role_in_ecosystem = normalise_empty(first_existing(cur_data_all(), c("role_in_ecosystem", "economic_role", "role"))),
      funding_model = normalise_empty(first_existing(cur_data_all(), c("funding_model"))),
      funding_source = normalise_empty(first_existing(cur_data_all(), c("funding_source", "funding_source_hint", "source"))),
      economic_logic = normalise_empty(first_existing(cur_data_all(), c("economic_logic"))),
      notes = normalise_empty(first_existing(cur_data_all(), c("economic_notes", "notes", "coding_notes")), "")
    ) |>
    filter(initiative_id != "Unknown")
}

build_diversity_profile <- function(master_df) {
  master_df |>
    transmute(
      initiative_id = normalise_empty(first_existing(cur_data_all(), c("initiative_id", "id"))),
      name = normalise_empty(first_existing(cur_data_all(), c("name", "initiative_name"))),
      non_profit = normalise_flag(first_existing(cur_data_all(), c("non_profit", "nonprofit"))),
      community_led = normalise_flag(first_existing(cur_data_all(), c("community_led", "community_governance"))),
      open_source_tools = normalise_flag(first_existing(cur_data_all(), c("open_source_tools", "open_source", "opensource"))),
      multilingual = normalise_flag(first_existing(cur_data_all(), c("multilingual"))),
      global_south_presence = normalise_empty(first_existing(cur_data_all(), c("global_south_presence", "global_south_inclusion")), "Unknown")
    ) |>
    filter(initiative_id != "Unknown")
}

build_validation_queue <- function(master_df) {
  key_fields <- c("initiative_type", "lead_actor_type", "governance_type", "country", "region", "website", "role_in_ecosystem", "funding_model")

  rows <- lapply(seq_len(nrow(master_df)), function(i) {
    row <- master_df[i, , drop = FALSE]
    initiative_id <- normalise_empty(first_existing(row, c("initiative_id", "id")))
    data_quality <- normalise_empty(first_existing(row, c("data_quality", "coding_status")), "")
    needs_validation <- normalise_flag(first_existing(row, c("needs_manual_validation", "manual_validation")))

    missing_fields <- key_fields[vapply(key_fields, function(f) {
      if (!f %in% names(row)) return(FALSE)
      val <- normalise_empty(row[[f]], "Unknown")
      val %in% c("Unknown", "unknown / to validate", "to validate", "")
    }, logical(1))]

    general_flag <- needs_validation == "Yes" || stringr::str_detect(stringr::str_to_lower(data_quality), "validate|review|low")
    fields_to_report <- unique(c(if (general_flag) "record_review" else character(0), missing_fields))
    if (length(fields_to_report) == 0) return(NULL)

    tibble::tibble(
      initiative_id = initiative_id,
      field = fields_to_report,
      current_value = vapply(fields_to_report, function(f) {
        if (f == "record_review" || !f %in% names(row)) return(data_quality)
        normalise_empty(row[[f]], "Unknown")
      }, character(1)),
      validation_priority = ifelse(general_flag, "High", "Medium"),
      notes = normalise_empty(first_existing(row, c("coding_notes", "notes")), "")
    )
  })

  dplyr::bind_rows(rows)
}

# ---- 03c. Column harmonisation ----
prepare_data_bundle <- function(master_raw_input) {
  source_label <- attr(master_raw_input, "cosmi_source") %||% ifelse(isTRUE(use_google_sheets), "Google Sheets", "Local workbook")

  master <- master_raw_input |>
    mutate(
      initiative_id = normalise_empty(first_existing(cur_data_all(), c("initiative_id", "id"))),
      name = normalise_empty(first_existing(cur_data_all(), c("name", "name_2", "name_17", "orgname", "org_name", "organization_name", "initiative_name"))),
      short_name = normalise_empty(first_existing(cur_data_all(), c("short_name", "abbreviated_orgname", "abbreviated_org_name")), ""),
      category = normalise_empty(first_existing(cur_data_all(), c("original_category", "category"))),
      initiative_type = normalise_empty(first_existing(cur_data_all(), c("object_type_openit_v3", "object_type_openit", "object_type", "initiative_type"))),
      lead_actor_type = normalise_empty(first_existing(cur_data_all(), c("lead_actor_type_openit_v3", "lead_actor_type_openit", "lead_actor_type"))),
      governance_type = normalise_empty(first_existing(cur_data_all(), c("governance_type_openit_v3", "governance_type_openit", "governance_type"))),
      scope = normalise_empty(first_existing(cur_data_all(), c("scope_openit_v3", "scope_openit", "scope"))),
      country = normalise_empty(first_existing(cur_data_all(), c("country"))),
      region = normalise_empty(first_existing(cur_data_all(), c("region", "continent"))),
      website = normalise_empty(first_existing(cur_data_all(), c("website", "org_website", "orgwebsite")), ""),
      latitude = suppressWarnings(as.numeric(first_existing(cur_data_all(), c("latitude", "lat"), NA))),
      longitude = suppressWarnings(as.numeric(first_existing(cur_data_all(), c("longitude", "lon", "lng"), NA))),
      data_quality = normalise_empty(first_existing(cur_data_all(), c("data_quality", "coding_status"), "To validate"))
    )

  master_full <- master |>
    mutate(
      initiative_type_raw = initiative_type,
      lead_actor_type_raw = lead_actor_type,
      governance_type_raw = governance_type,
      scope_raw = scope,
      object_type = clean_object_type(initiative_type_raw),
      initiative_type = clean_unesco_category(category),
      lead_actor_type = clean_actor(lead_actor_type_raw),
      governance_family = clean_governance_family(governance_type_raw),
      governance_type = governance_family,
      scope = clean_scope(scope_raw),
      role_in_ecosystem = clean_object_type(first_existing(cur_data_all(), c("role_in_ecosystem", "economic_role", "role", "object_type_openit"))),
      funding_model_raw = normalise_empty(first_existing(cur_data_all(), c("funding_model"))),
      funding_model = clean_funding_model(funding_model_raw),
      funding_source = normalise_empty(first_existing(cur_data_all(), c("funding_source", "funding_source_hint", "source"))),
      economic_logic_raw = normalise_empty(first_existing(cur_data_all(), c("economic_logic"))),
      economic_logic = clean_economic_logic(economic_logic_raw),
      non_profit = clean_yes_no_level(first_existing(cur_data_all(), c("non_profit", "nonprofit"))),
      community_led = clean_yes_no_level(first_existing(cur_data_all(), c("community_led", "community_governance"))),
      open_source_tools = clean_yes_no_level(first_existing(cur_data_all(), c("open_source_tools", "open_source", "opensource"))),
      multilingual = clean_yes_no_level(first_existing(cur_data_all(), c("multilingual"))),
      global_south_presence = clean_yes_no_level(first_existing(cur_data_all(), c("global_south_presence", "global_south_inclusion"))),
      source_url = normalise_empty(first_existing(cur_data_all(), c("source", "source_url", "economic_source_url", "website")), "")
    ) |>
    filter(!is.na(initiative_type), initiative_type != "Unknown")

  dims_long <- build_open_science_dimensions(master_full) |>
    mutate(dimension = factor(dimension, levels = unesco_categories))

  list(
    master_full = master_full,
    dims_long = dims_long,
    diversity = build_diversity_profile(master_full),
    valid = build_validation_queue(master_full),
    loaded_at = Sys.time(),
    source = source_label,
    error = NULL
  )
}

initial_bundle <- prepare_data_bundle(master_raw)
master_full <- initial_bundle$master_full
dims_long <- initial_bundle$dims_long
diversity <- initial_bundle$diversity
valid <- initial_bundle$valid


linkify_website <- function(df) {
  if (!"website" %in% names(df)) return(df)
  df |>
    dplyr::mutate(
      website = dplyr::case_when(
        is.na(.data$website) | stringr::str_squish(as.character(.data$website)) == "" ~ "",
        stringr::str_detect(as.character(.data$website), "^https?://") ~ paste0(
          '<a href="', htmltools::htmlEscape(as.character(.data$website)),
          '" target="_blank" rel="noopener noreferrer">',
          htmltools::htmlEscape(as.character(.data$website)), '</a>'
        ),
        TRUE ~ paste0(
          '<a href="https://', htmltools::htmlEscape(as.character(.data$website)),
          '" target="_blank" rel="noopener noreferrer">',
          htmltools::htmlEscape(as.character(.data$website)), '</a>'
        )
      )
    )
}
# ---- 04. Visual palette ----
# Centralised colours for plots and cards.
cosmi_palette <- c(
  "#12395B", "#1E7890", "#2AAE9F", "#7FB7BE", "#B8D8D8",
  "#F2B880", "#D9825B", "#8C6BB1", "#6E8FB2", "#8FA3AD"
)

cosmi_flag_palette <- c(
  "Yes" = "#1E7890",
  "Possible" = "#7FB7BE",
  "No" = "#D9825B",
  "Unknown" = "#C8D2DC"
)

# ---- 04b. Theme ----
# Global bslib theme and typography.
cosmi_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#12395B",
  secondary = "#1E7890",
  success = "#2AAE9F",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ---- 04c. UI helpers and external URLs ----
# Reusable card constructors and centralised Google Docs/Forms links.
plot_card <- function(title, output) {
  card(
    class = "cosmi-card",
    card_header(title),
    card_body(output)
  )
}

filter_box <- function(...) {
  div(class = "filter-card", ...)
}

embed_card <- function(title, src, height = "760px", note = NULL) {
  card(
    class = "cosmi-card embed-card",
    card_header(div(class = "embed-card-title", title)),
    card_body(
      if (!is.null(note)) p(class = "small-note", note),
      tags$iframe(src = src, class = "embed-frame", height = height, loading = "lazy", allowfullscreen = NA)
    )
  )
}

external_link_card <- function(title, text, href, button = "Open") {
  div(
    class = "action-card",
    h3(title),
    p(text),
    tags$a(class = "btn btn-primary", href = href, target = "_blank", rel = "noopener", button)
  )
}

about_doc_url <- "https://docs.google.com/document/d/163di4K3TfQqM-zqEc7IrxB7SCnhiZPn0pK-fbQrIftQ/preview"
faq_doc_url <- "https://docs.google.com/document/d/1F0CrXoXABLvmHDQO3ChrjtR_u9g7Zz5K4tzziCTsqEQ/preview"
method_doc_url <- "https://docs.google.com/document/d/14G8cpONSY4E3XoEq3hpSvflndpH0-qLU47P-rmA7UFo/preview"
add_initiative_form_url <- "https://docs.google.com/forms/d/1gcIosEnk4qOR9BS4lFitiwj-onLNrSpVybyqSIQ_3Kc/viewform?embedded=true"
report_error_form_url <- "https://docs.google.com/forms/d/1iDJp9iMNSfG2ur47i6122QphXxssL34-my_m2vDusoI/viewform?embedded=true"
source_code_url <- "https://github.com/"

# External project and institutional links used by clickable logos.
openit_project_url <- "https://www.gemass.fr/contract/openit/"
anr_project_url <- "https://anr.fr/Projet-ANR-24-RESO-0001"
cnrs_url <- "https://www.cnrs.fr"
sorbonne_url <- "https://www.sorbonne-universite.fr/"
gemass_url <- "https://www.gemass.fr/"

# Clickable project logo strip.
#
# Image files must be placed in the app's www/ directory. The filenames below
# match the filenames used in the previous COSMI/OpenIT Shiny app. If you rename
# assets, update only this helper.
project_logo_strip <- function(compact = FALSE) {
  div(
    class = if (isTRUE(compact)) "project-logo-strip compact" else "project-logo-strip",
    tags$a(
      href = openit_project_url, target = "_blank", rel = "noopener",
      title = "OPENIT project",
      tags$img(src = "logo-transparent.png", alt = "OPENIT project", class = "project-logo logo-openit")
    ),
    tags$a(
      href = anr_project_url, target = "_blank", rel = "noopener",
      title = "French National Research Agency — ANR",
      tags$img(src = "logo_ANR_2022.jpg", alt = "ANR", class = "project-logo logo-anr")
    ),
    tags$a(
      href = cnrs_url, target = "_blank", rel = "noopener",
      title = "CNRS",
      tags$img(src = "logo_cnrs.png", alt = "CNRS", class = "project-logo logo-cnrs")
    ),
    tags$a(
      href = sorbonne_url, target = "_blank", rel = "noopener",
      title = "Sorbonne Université",
      tags$img(src = "Logo_of_Sorbonne_University.svg.png", alt = "Sorbonne Université", class = "project-logo logo-sorbonne")
    )
  )
}

# ---- 05. User interface ----
# Navbar layout, page structure, custom CSS and output placeholders.
ui <- page_navbar(
  id = "main_nav",
  title = div(class = "brand-title", "COSMI Hub"),
  theme = cosmi_theme,
  bg = "#12395B",
  inverse = TRUE,
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "hub5.png"),
    tags$link(rel = "shortcut icon", type = "image/png", href = "hub5.png"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        function cleanTabName(txt) { return (txt || '').trim().replace(/\\s+/g, ' '); }
        function activateFromHash() {
          var hash = decodeURIComponent(window.location.hash.replace(/^#/, ''));
          if (!hash) return;
          var links = document.querySelectorAll('.navbar-nav .nav-link');
          for (var i = 0; i < links.length; i++) {
            if (cleanTabName(links[i].textContent) === hash) {
              if (window.bootstrap && bootstrap.Tab) {
                bootstrap.Tab.getOrCreateInstance(links[i]).show();
              } else {
                links[i].click();
              }
              break;
            }
          }
        }
        document.addEventListener('shown.bs.tab', function(e) {
          var txt = cleanTabName(e.target.textContent);
          if (txt) history.replaceState(null, '', '#' + encodeURIComponent(txt));
        });
        window.addEventListener('hashchange', activateFromHash);
        setTimeout(activateFromHash, 250);
      });
    ")),
    tags$style(HTML("
      body {
        background: #F4F7FA;
        color: #112233;
        overflow-x: hidden;
      }
      .container-fluid, .bslib-page-navbar > .container-fluid { max-width: 100%; }
      .navbar > .container-fluid { display: flex; align-items: center; justify-content: center; gap: 22px; }

      .navbar {
        min-height: 72px;
        box-shadow: 0 2px 18px rgba(0,0,0,0.12);
      }

      .navbar-brand {
        margin-right: 34px !important;
        padding-left: 10px;
      }

      .navbar-collapse {
        flex-grow: 0;
      }

      .navbar-nav {
        align-items: center;
        gap: 16px;
      }

      .navbar-openit-logo {
        margin-left: auto;
        margin-right: 22px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        height: 54px;
        width: 54px;
        padding: 7px;
        border-radius: 18px;
        background: #FFFFFF;
        box-shadow: 0 6px 18px rgba(0,0,0,0.22);
        transition: transform 0.15s ease, box-shadow 0.15s ease;
      }

      .navbar-openit-logo:hover {
        transform: translateY(-1px);
        box-shadow: 0 8px 22px rgba(0,0,0,0.28);
      }

      .navbar-openit-logo img {
        max-height: 42px;
        max-width: 42px;
        object-fit: contain;
      }

      .brand-title {
        font-weight: 850;
        letter-spacing: -0.025em;
        font-size: 1.25rem;
      }

      .navbar-nav .nav-link {
        color: rgba(255,255,255,0.86) !important;
        font-weight: 650;
        margin: 0;
        padding: 24px 0 18px 0 !important;
        border-bottom: 3px solid transparent;
      }

      .navbar-nav .nav-link:hover {
        color: #FFFFFF !important;
        border-bottom-color: rgba(42,174,159,0.55);
      }

      .navbar-nav .nav-link.active,
      .navbar-nav .show > .nav-link {
        color: #2AAE9F !important;
        border-bottom-color: #2AAE9F;
      }

      .page-wrap {
        max-width: 1260px;
        margin: 0 auto;
        padding: 38px 28px 56px 28px;
      }

      .hero {
        background: linear-gradient(135deg, #102F4A 0%, #1C7890 100%);
        color: white;
        border-radius: 28px;
        padding: 38px 46px;
        margin: 0 auto 24px auto;
        box-shadow: 0 14px 34px rgba(18,57,91,0.18);
      }

      .hero h1 {
        font-size: 3.0rem;
        line-height: 1.05;
        font-weight: 850;
        letter-spacing: -0.055em;
        margin: 0 0 12px 0;
      }

      .hero .tagline {
        font-size: 1.13rem;
        line-height: 1.58;
        max-width: 980px;
        margin: 0;
        opacity: 0.96;
      }


      .project-logo-strip {
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 14px;
        width: fit-content;
        max-width: 100%;
        margin-top: 24px;
        padding: 12px 16px;
        border-radius: 18px;
        background: rgba(255,255,255,0.96);
        box-shadow: 0 10px 24px rgba(18,57,91,0.16);
      }

      .project-logo-strip.compact {
        gap: 10px;
        margin-top: 18px;
        padding: 9px 12px;
        border-radius: 16px;
      }

      .project-logo-strip a {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-height: 50px;
        padding: 5px 8px;
        border-radius: 12px;
        transition: transform 0.15s ease, box-shadow 0.15s ease, background 0.15s ease;
      }

      .project-logo-strip a:hover {
        transform: translateY(-1px);
        background: #F4F7FA;
        box-shadow: 0 6px 14px rgba(18,57,91,0.12);
      }

      .project-logo {
        max-height: 52px;
        max-width: 168px;
        object-fit: contain;
      }

      .project-logo-strip.compact .project-logo {
        max-height: 42px;
        max-width: 132px;
      }

      .project-logo.logo-cnrs {
        max-width: 72px;
      }

      .project-logo.logo-sorbonne {
        max-width: 185px;
      }

      @media (max-width: 900px) {
        .project-logo-strip {
          width: 100%;
          justify-content: center;
        }
        .project-logo {
          max-height: 42px;
          max-width: 138px;
        }
      }



      .project-partners-card {
        margin-top: 22px;
      }

      .project-partners-card h2 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102F4A;
      }

      .project-partners-card .small-note {
        margin-bottom: 14px;
      }

      .app-footer {
        margin: 34px auto 0 auto;
        padding: 18px 24px;
        max-width: 1440px;
        border-top: 1px solid #DCE6ED;
        color: #5F7383;
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 20px;
        flex-wrap: wrap;
      }

      .app-footer-text {
        font-size: 0.92rem;
      }

      .app-footer .project-logo-strip {
        margin-top: 0;
        padding: 8px 12px;
        border-radius: 14px;
        background: #FFFFFF;
        box-shadow: 0 6px 18px rgba(18,57,91,0.08);
      }

      .app-footer .project-logo-strip .project-logo {
        max-height: 30px;
        max-width: 118px;
      }

      .app-footer .project-logo-strip .logo-cnrs {
        max-width: 54px;
      }

      @media (max-width: 900px) {
        .app-footer {
          justify-content: center;
          text-align: center;
        }
      }


      .page-wrap > .cosmi-card,
      .page-wrap > .html-fill-container,
      .page-wrap > .bslib-grid,
      .page-wrap > .layout-column-wrap {
        margin-left: auto;
        margin-right: auto;
      }

      .cosmi-card {
        max-width: 100%;
      }

      .page-wrap .bslib-grid > * {
        min-width: 0;
      }

      .hero + .kpi-grid, .kpi-grid {
        max-width: 100%;
      }

      .hero .micro {
        margin-top: 14px;
        font-size: 0.95rem;
        opacity: 0.82;
      }

      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 16px;
        margin-bottom: 22px;
      }

      .kpi {
        background: white;
        border: 1px solid #DDE6EE;
        border-radius: 22px;
        padding: 20px 22px;
        min-height: 124px;
        box-shadow: 0 10px 28px rgba(18,57,91,0.07);
      }

      .kpi .label {
        text-transform: uppercase;
        font-size: 0.76rem;
        letter-spacing: 0.09em;
        font-weight: 850;
        color: #637387;
      }

      .kpi .value {
        font-size: 2.2rem;
        font-weight: 850;
        letter-spacing: -0.04em;
        color: #0F2E49;
        margin-top: 4px;
      }

      .kpi .note {
        font-size: 0.90rem;
        color: #617184;
        margin-top: 2px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .cosmi-card {
        border: 1px solid #DDE6EE;
        border-radius: 24px;
        box-shadow: 0 10px 28px rgba(18,57,91,0.07);
        overflow: hidden;
        margin-bottom: 22px;
      }

      .card-header {
        background: white;
        font-weight: 850;
        font-size: 1.04rem;
        border-bottom: 1px solid #E7EDF3;
      }

      .filter-card {
        background: white;
        border: 1px solid #DDE6EE;
        border-radius: 24px;
        padding: 18px;
        box-shadow: 0 10px 28px rgba(18,57,91,0.07);
        margin-bottom: 22px;
        position: sticky;
        top: 92px;
        z-index: 2;
        overflow: hidden;
      }
      .filter-card .form-group, .filter-card .shiny-input-container { width: 100% !important; }

      .filter-card h3 {
        font-size: 1.05rem;
        font-weight: 850;
        margin: 0 0 14px 0;
      }

      .insight {
        background: #EAF4F7;
        border-left: 5px solid #1E7890;
        color: #17364C;
        border-radius: 14px;
        padding: 15px 18px;
        margin-bottom: 22px;
        font-weight: 550;
      }

      .dataTables_wrapper {
        font-size: 0.88rem;
      }

      .leaflet-container {
        border-radius: 18px;
      }

      .country-popup { min-width: 520px; max-width: 680px; font-size: 0.88rem; }
      .country-popup h4 { margin: 0 0 8px 0; font-size: 1.05rem; font-weight: 850; color: #12395B; }
      .country-popup .popup-total { display: inline-block; margin-bottom: 8px; padding: 5px 9px; border-radius: 999px; background: #EAF4F7; color: #12395B; font-weight: 800; }
      .country-popup table { width: 100%; border-collapse: collapse; margin: 6px 0 10px 0; }
      .country-popup th, .country-popup td { border-bottom: 1px solid #E7EDF3; padding: 5px 6px; vertical-align: top; }
      .country-popup th { background: #F4F7FA; color: #12395B; font-weight: 800; }

      .country-detail-panel {
        margin: 16px 0 8px 0;
        padding: 18px 20px;
        border-radius: 20px;
        background: linear-gradient(135deg, #F8FCFE 0%, #EEF7FA 100%);
        border: 1px solid #D7E7EF;
        box-shadow: 0 12px 30px rgba(18, 57, 91, 0.08);
      }

      .country-detail-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 16px;
        margin-bottom: 12px;
      }

      .country-detail-header h4 {
        margin: 0;
        font-weight: 900;
        color: #12395B;
        font-size: 1.45rem;
      }

      .country-total-badge {
        white-space: nowrap;
        border-radius: 999px;
        background: #12395B;
        color: white;
        padding: 7px 13px;
        font-weight: 850;
      }

      .gov-pill-wrap { display: flex; flex-wrap: wrap; gap: 8px; margin: 8px 0 6px 0; }
      .gov-pill { border-radius: 999px; background: white; border: 1px solid #D7E7EF; color: #17364C; padding: 6px 10px; font-weight: 700; box-shadow: 0 4px 12px rgba(18, 57, 91, 0.05); }
      .country-detail-panel p { margin: 5px 0; }

      .plotly.html-widget {
        cursor: pointer;
      }

      .small-note {
        color: #617184;
        font-size: 0.92rem;
      }

      .section-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 16px;
        margin-bottom: 22px;
      }

      .action-card {
        background: white;
        border: 1px solid #DDE6EE;
        border-radius: 24px;
        padding: 20px;
        box-shadow: 0 10px 28px rgba(18,57,91,0.07);
      }

      .action-card h3 {
        font-size: 1.05rem;
        font-weight: 850;
        color: #0F2E49;
        margin-bottom: 8px;
      }

      .action-card p {
        min-height: 54px;
        color: #617184;
      }

      .embed-frame {
        width: 100%;
        border: 1px solid #DDE6EE;
        border-radius: 18px;
        background: white;
      }

      .embed-card .card-body {
        padding: 18px;
      }

      .pill-note {
        display: inline-flex;
        align-items: center;
        border-radius: 999px;
        background: #EAF4F7;
        color: #12395B;
        padding: 6px 10px;
        font-size: 0.84rem;
        font-weight: 750;
        margin-bottom: 12px;
      }

      .nav-tabs .nav-link {
        font-weight: 750;
      }


      .navbar-data-tools {
        margin-left: 14px;
        display: inline-flex;
        align-items: center;
        gap: 10px;
        color: rgba(255,255,255,0.88);
        font-size: 0.78rem;
        white-space: nowrap;
      }

      .navbar-data-tools .btn {
        border-radius: 999px;
        border: 1px solid rgba(255,255,255,0.32);
        color: white;
        background: rgba(255,255,255,0.08);
        font-weight: 800;
        padding: 7px 12px;
        box-shadow: none;
      }

      .navbar-data-tools .btn:hover {
        background: rgba(255,255,255,0.16);
        color: white;
      }

      .navbar-data-tools .btn:active {
        transform: translateY(1px);
      }

      .data-status-dot {
        width: 8px;
        height: 8px;
        border-radius: 999px;
        display: inline-block;
        background: #38D39F;
        box-shadow: 0 0 0 3px rgba(56,211,159,0.14);
      }

      .data-status-dot.warn {
        background: #FFB84D;
        box-shadow: 0 0 0 3px rgba(255,184,77,0.15);
      }

      .data-status-main {
        font-weight: 800;
      }

      .data-status-sub {
        opacity: 0.72;
        font-size: 0.72rem;
      }

      .data-status-chip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 7px 10px;
        border-radius: 999px;
        background: rgba(255,255,255,0.10);
        color: rgba(255,255,255,0.92);
        border: 1px solid rgba(255,255,255,0.18);
      }

      @media (max-width: 1180px) {
        .navbar-data-tools .data-status-chip { display: none; }
      }

      @media (max-width: 1100px) {
        .section-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 700px) {
        .section-grid {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 1100px) {
        .kpi-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .hero h1 {
          font-size: 2.35rem;
        }
      }

      @media (max-width: 700px) {
        .kpi-grid {
          grid-template-columns: 1fr;
        }
      }

      .bslib-sidebar-layout, .bslib-grid, .row, .layout-column-wrap { min-width: 0; }
      .card-body, .html-widget, .dataTables_wrapper, .shiny-plot-output { min-width: 0; }
      .plotly, .plot-container, .svg-container { max-width: 100% !important; }
      .selectize-control { max-width: 100%; }
      .shiny-bound-output { overflow-x: auto; }
      .btn-primary, .btn-outline-primary { border-radius: 999px; font-weight: 800; padding-inline: 1rem; }
      .form-control, .selectize-input, .selectize-dropdown { border-radius: 14px; }
      .selectize-input { border-color: #D4E0EA; box-shadow: none; }
      .selectize-input.focus { border-color: #1E7890; box-shadow: 0 0 0 0.2rem rgba(30,120,144,0.16); }
      table.dataTable thead th { color: #12395B; background: #F4F7FA; border-bottom: 1px solid #DDE6EE !important; }
      table.dataTable tbody td { vertical-align: top; }
      .dt-buttons .dt-button { border-radius: 999px !important; border: 1px solid #D4E0EA !important; background: #FFFFFF !important; color: #12395B !important; font-weight: 750 !important; box-shadow: none !important; }
      .quality-strip { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 14px; margin: 0 0 22px 0; }
      .quality-chip { background: #FFFFFF; border: 1px solid #DDE6EE; border-radius: 20px; padding: 16px 18px; box-shadow: 0 8px 22px rgba(18,57,91,0.06); }
      .quality-chip strong { display: block; color: #0F2E49; font-size: 1.35rem; letter-spacing: -0.03em; }
      .quality-chip span { display: block; color: #617184; font-weight: 650; margin-top: 2px; }
      @media (max-width: 992px) {
        .navbar > .container-fluid { justify-content: flex-start; gap: 10px; }
        .navbar-brand { margin-right: 8px !important; }
        .navbar-openit-logo {
          margin-left: 0;
          height: 46px;
          width: 46px;
          padding: 6px;
        }
        .navbar-openit-logo img {
          max-height: 34px;
          max-width: 34px;
        }
        .navbar-collapse { padding-bottom: 14px; flex-grow: 1; }
        .navbar-nav .nav-link { padding: 10px 0 !important; margin: 0; border-bottom-width: 0; }
        .navbar-data-tools { margin-left: 0; width: 100%; justify-content: flex-start; padding: 8px 0; }
        .page-wrap { padding: 22px 16px 42px 16px; }
        .hero { border-radius: 22px; padding: 28px 24px; }
        .filter-card { position: relative; top: auto; }
        .country-popup { min-width: 280px; max-width: min(92vw, 680px); }
        .quality-strip { grid-template-columns: 1fr; }
      }
      @media (max-width: 560px) {
        .hero h1 { font-size: 1.85rem; letter-spacing: -0.035em; }
        .hero .tagline { font-size: 1rem; }
        .cosmi-card, .filter-card, .action-card, .kpi { border-radius: 18px; }
        .kpi { min-height: auto; padding: 16px 18px; }
        .kpi .value { font-size: 1.75rem; }
        .card-body { padding: 14px; }
        .embed-frame { min-height: 620px; }
      }
    "))
  ),

  nav_panel(
    "Overview",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Community Open Science Mapping Initiatives Hub"),
        p(class = "tagline", "COSMI Hub supports the analysis of open science initiatives across territories, actor configurations, governance models and open science dimensions. It is designed as a working tool for researchers, institutions, funders, infrastructures and policy actors."),
        p(class = "micro", "Use the analytical views to compare countries and regions, identify dominant initiative types, explore governance profiles and map connections between open science dimensions.")
      ),

      div(
        class = "kpi-grid",
        uiOutput("kpi_initiatives"),
        uiOutput("kpi_countries"),
        uiOutput("kpi_regions"),
        uiOutput("kpi_types")
      ),

      plot_card("Global distribution", leafletOutput("overview_map", height = 520)),

      plot_card(
        "Country-level overview",
        tagList(
          div(class = "small-note", "Click a country to display its initiatives and governance profile. The colour scale is compressed so countries with fewer initiatives remain visible."),
          plotlyOutput("overview_country_map", height = 590),
          uiOutput("overview_country_summary"),
          DTOutput("overview_country_table")
        )
      ),

      layout_columns(
        col_widths = c(12, 12),
        plot_card("Dominant initiative types", plotlyOutput("overview_type_plot", height = 430)),
        plot_card("Selected initiatives", DTOutput("overview_selected_table"))
      ),

      uiOutput("overview_insight")
    )
  ),

  nav_panel(
    "Territories",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Territorial analysis"),
        p(class = "tagline", "Compare countries and regions by initiative type, governance profile and actor configuration."),
      ),

      layout_columns(
        col_widths = c(3, 9),
        filter_box(
          h3("Filters"),
          selectInput("territory_region", "Region", choices = c("All", safe_distinct_values(master_full$region)), selected = "All"),
          selectInput("territory_country", "Country", choices = c("All", safe_distinct_values(master_full$country)), selected = "All"),
          selectInput("territory_type", "Initiative type", choices = c("All", safe_distinct_values(master_full$initiative_type)), selected = "All"),
          selectInput("territory_actor", "Lead actor type", choices = c("All", safe_distinct_values(master_full$lead_actor_type)), selected = "All"),
          actionButton("reset_territory", "Reset filters", class = "btn-outline-primary")
        ),
        div(
          div(
            class = "kpi-grid",
            uiOutput("territory_kpi_n"),
            uiOutput("territory_kpi_countries"),
            uiOutput("territory_kpi_community"),
            uiOutput("territory_kpi_nonprofit")
          ),
          plot_card("Regional volume by initiative type", plotlyOutput("region_type_count_plot", height = 500)),
          plot_card("Country volume by initiative type", plotlyOutput("country_type_count_plot", height = 620)),
          plot_card("Regional profiles by initiative type", plotlyOutput("region_distribution_plot", height = 520)),
          plot_card("Country profiles by initiative type", plotlyOutput("country_type_share_plot", height = 640)),
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Regional profile matrix", plotlyOutput("region_type_share_matrix", height = 460)),
            plot_card("Governance profile", plotlyOutput("territory_governance_plot", height = 460))
          )
        )
      )
    )
  ),

  nav_panel(
    "Typology",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Typology of initiatives"),
        p(class = "tagline", "The dataset is organised around ten categories of open science initiatives. This view examines their distribution, territorial presence and associated actors."),
      ),

      layout_columns(
        col_widths = c(12, 12, 12),
        plot_card("Initiative types", plotlyOutput("type_distribution_plot", height = 420)),
        plot_card("Lead actors by type", plotlyOutput("type_actor_matrix", height = 420)),
        plot_card("Scope by type", plotlyOutput("type_scope_matrix", height = 420))
      ),

      layout_columns(
        col_widths = c(3, 9),
        filter_box(
          h3("Empirical typology"),
          sliderInput("cluster_k", "Number of clusters", min = 3, max = 8, value = 5, step = 1),
          checkboxInput("cluster_include_region", "Include region", value = FALSE),
          p(class = "small-note", "Clusters are computed from initiative type, actor type, scope, governance indicators and open science dimensions.")
        ),
        div(
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Cluster sizes", plotlyOutput("cluster_size_plot", height = 390)),
            plot_card("Cluster profile", plotlyOutput("cluster_profile_plot", height = 390))
          ),
          plot_card("Hierarchical structure", plotOutput("cluster_dendrogram", height = 430)),
          downloadButton("download_clusters", "Download cluster results", class = "btn-outline-primary"),
          br(), br(),
          plot_card("Clustered initiatives", DTOutput("cluster_table"))
        )
      ),

      plot_card("Initiatives", DTOutput("typology_table"))
    )
  ),

  nav_panel(
    "Dimensions & networks",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Dimensions and networks"),
        p(class = "tagline", "Open science initiatives often combine several dimensions. Co-occurrences reveal how domains such as access, data, infrastructure, education, evaluation and participation are connected."),
      ),

      layout_columns(
        col_widths = c(3, 9),
        filter_box(
          h3("Dimension explorer"),
          selectInput("dimension_filter", "Dimension", choices = c("All", unesco_categories), selected = "All"),
          checkboxGroupInput("level_filter", "Coding level", choices = c("Yes", "Possible", "No", "Unknown"), selected = c("Yes", "Possible")),
          sliderInput("min_cooccurrence", "Minimum co-occurrence", min = 1, max = 20, value = 3, step = 1),
          p(class = "small-note", "Select one dimension to inspect its associated initiatives. Keep several coding levels selected for a broader exploratory view.")
        ),
        div(
          div(
            class = "kpi-grid",
            uiOutput("dimension_kpi_records"),
            uiOutput("dimension_kpi_initiatives"),
            uiOutput("dimension_kpi_top"),
            uiOutput("dimension_kpi_pairs")
          ),
          plot_card("Dimension volume", plotlyOutput("dimension_bar", height = 440)),
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Dimension co-occurrence network", plotlyOutput("dimension_network", height = 520)),
            plot_card("Dimension co-occurrence matrix", plotlyOutput("dimension_co_matrix", height = 520))
          ),
          plot_card("Dimensions by initiative type", plotlyOutput("dimension_type_matrix", height = 560)),
          plot_card("Initiatives by dimension", DTOutput("dimension_initiatives_table"))
        )
      )
    )
  ),

  nav_panel(
    "Economic ecosystem",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Economic ecosystem"),
        p(class = "tagline", "This view compares the position of initiatives in the ecosystem: role, funding configuration, economic coordination logic and territorial distribution."),
      ),

      layout_columns(
        col_widths = c(3, 9),
        filter_box(
          h3("Economic explorer"),
          selectInput("econ_region", "Region", choices = c("All", safe_distinct_values(master_full$region)), selected = "All"),
          selectInput("econ_country", "Country", choices = c("All", safe_distinct_values(master_full$country)), selected = "All"),
          selectInput("econ_role", "Role in ecosystem", choices = c("All", safe_distinct_values(master_full$object_type)), selected = "All"),
          selectInput("econ_funding", "Funding model", choices = c("All", safe_distinct_values(master_full$funding_model)), selected = "All"),
          actionButton("reset_econ", "Reset filters", class = "btn-outline-primary")
        ),
        div(
          div(
            class = "kpi-grid",
            uiOutput("econ_kpi_initiatives"),
            uiOutput("econ_kpi_roles"),
            uiOutput("econ_kpi_funding"),
            uiOutput("econ_kpi_unknown")
          ),
          plot_card("Roles in the ecosystem", plotlyOutput("role_plot", height = 440)),
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Funding models", plotlyOutput("funding_model_plot", height = 430)),
            plot_card("Economic logics", plotlyOutput("economic_logic_plot", height = 430))
          ),
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Roles by region", plotlyOutput("econ_role_region_matrix", height = 500)),
            plot_card("Funding models by region", plotlyOutput("econ_funding_region_matrix", height = 500))
          ),
          plot_card("Country profiles by economic role", plotlyOutput("econ_country_role_plot", height = 620)),
          plot_card("Economic role records", DTOutput("economic_table"))
        )
      )
    )
  ),

  nav_panel(
    "Governance & inclusion",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Governance and inclusion"),
        p(class = "tagline", "This view compares community orientation, non-profit status, open-source tools, multilingualism and Global South presence across territories and initiative types."),
      ),

      layout_columns(
        col_widths = c(3, 9),
        filter_box(
          h3("Governance explorer"),
          selectInput("gov_region", "Region", choices = c("All", safe_distinct_values(master_full$region)), selected = "All"),
          selectInput("gov_country", "Country", choices = c("All", safe_distinct_values(master_full$country)), selected = "All"),
          selectInput("gov_type", "Initiative type", choices = c("All", safe_distinct_values(master_full$initiative_type)), selected = "All"),
          selectInput("gov_indicator", "Indicator", choices = c("All", "Community-led", "Non-profit", "Open-source tools", "Multilingual", "Global South presence"), selected = "All"),
          actionButton("reset_gov", "Reset filters", class = "btn-outline-primary")
        ),
        div(
          div(
            class = "kpi-grid",
            uiOutput("gov_kpi_initiatives"),
            uiOutput("gov_kpi_index"),
            uiOutput("gov_kpi_multilingual"),
            uiOutput("gov_kpi_global_south")
          ),

          layout_columns(
            col_widths = c(12, 12),
            plot_card("Governance indicators by region", plotlyOutput("inclusion_region_matrix", height = 500)),
            plot_card("Governance indicators by initiative type", plotlyOutput("inclusion_type_matrix", height = 500))
          ),

          layout_columns(
            col_widths = c(12, 12),
            plot_card("Multilingualism by region", plotlyOutput("multilingual_region_plot", height = 460)),
            plot_card("Global South presence by region", plotlyOutput("global_south_region_plot", height = 460))
          ),

          layout_columns(
            col_widths = c(12, 12),
            plot_card("Governance profiles", plotlyOutput("governance_profile_plot", height = 500)),
            plot_card("Country comparison", plotlyOutput("governance_country_plot", height = 500))
          ),

          plot_card("Governance and inclusion records", DTOutput("diversity_table"))
        )
      )
    )
  ),

  nav_panel(
    "Data",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("Data and contributions"),
        p(class = "tagline", "Explore the current COSMI Hub dataset, download reusable data, and contribute directly through embedded forms for new initiatives or corrections."),
      ),

      div(
        class = "section-grid",
        external_link_card("Suggest an initiative", "Submit a missing Open Science initiative to enrich the directory.", add_initiative_form_url, "Open form"),
        external_link_card("Report an error", "Flag a correction or missing information for an existing record.", report_error_form_url, "Open form"),
        div(class = "action-card", h3("Download dataset"), p("Download the harmonised master table currently used by the dashboard."), downloadButton("download_dataset_csv", "Download CSV", class = "btn-primary")),
        external_link_card("Source code", "Access the application repository or replace this URL with your GitHub repository.", source_code_url, "Open GitHub")
      ),

      uiOutput("data_quality_strip"),

      navset_tab(
        nav_panel(
          "Add initiative",
          embed_card("Suggest a new Open Science initiative", add_initiative_form_url, height = "900px", note = "The Google Form is embedded here so contributors can submit entries without leaving the dashboard.")
        ),
        nav_panel(
          "Report an error",
          embed_card("Report an error or correction", report_error_form_url, height = "900px", note = "Use this form to suggest corrections, missing values or updates for an existing initiative.")
        ),
        nav_panel(
          "Validation queue",
          layout_columns(
            col_widths = c(12, 12),
            plot_card("Validation priority", plotlyOutput("validation_priority_plot", height = 390)),
            plot_card("Fields to check", plotlyOutput("validation_field_plot", height = 390))
          ),
          plot_card("Validation queue", DTOutput("validation_table"))
        )
      )
    )
  ),

  nav_panel(
    "About",
    div(
      class = "page-wrap",
      div(
        class = "hero",
        h1("About COSMI Hub"),
        p(class = "tagline", "Learn more about the OPENIT project, the purpose of COSMI Hub, the underlying methodology and frequently asked questions."),
      ),

      navset_tab(
        nav_panel(
          "About the project",
          embed_card("About this project", about_doc_url, height = "820px", note = "This section presents the objectives, scope and institutional context of the COSMI Hub, developed as part of the OPENIT project.")
        ),
        nav_panel(
          "FAQs",
          embed_card("Frequently asked questions", faq_doc_url, height = "820px", note = "This section provides answers to frequently asked questions about the data, methodology and scope of the COSMI Hub.")
        ),
        nav_panel(
          "Methodological background",
          embed_card("Methodological background", method_doc_url, height = "820px", note = "This section documents the data collection process, coding framework and analytical choices underlying the COSMI Hub.")
        )
      ),

      div(
        class = "cosmi-card project-partners-card",
        card_body(
          h2("Project support"),
          p(class = "small-note", "Funded by ANR and developed at Sorbonne University and CNRS within the OPENIT project."),
          project_logo_strip(compact = FALSE)
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(
    div(
      class = "navbar-data-tools",
      actionButton("refresh_data", "Refresh", class = "btn-sm", icon = icon("rotate")),
      uiOutput("data_status_nav", inline = TRUE)
    )
  ),
  nav_item(
    tags$a(
      href = openit_project_url, target = "_blank", rel = "noopener",
      class = "navbar-openit-logo", title = "OPENIT project",
      tags$img(src = "logo-transparent.png", alt = "OPENIT project")
    )
  )
)
# ---- 06. Server ----
# Shared renderers first, followed by one block per app section.
server <- function(input, output, session) {

  # ---- Data refresh state ----
  data_bundle <- reactiveVal(initial_bundle)
  data_hash <- reactiveVal(digest::digest(initial_bundle$master_full))
  refresh_state <- reactiveVal("ready")
  makeActiveBinding("master_full", function() data_bundle()$master_full, environment())
  makeActiveBinding("dims_long", function() data_bundle()$dims_long, environment())
  makeActiveBinding("diversity", function() data_bundle()$diversity, environment())
  makeActiveBinding("valid", function() data_bundle()$valid, environment())


  refresh_data_from_source <- function(show_feedback = TRUE) {
    refresh_state("loading")
    if (isTRUE(show_feedback)) {
      showNotification("Refreshing COSMI Hub data…", type = "message", duration = 2)
    }

    tryCatch({
      raw <- read_table_safe("initiatives_master")
      new_bundle <- prepare_data_bundle(raw)
      new_hash <- digest::digest(new_bundle$master_full)
      previous_hash <- data_hash()

      if (identical(new_hash, previous_hash)) {
        old_bundle <- data_bundle()
        old_bundle$loaded_at <- Sys.time()
        old_bundle$error <- NULL
        data_bundle(old_bundle)
        refresh_state("unchanged")
        if (isTRUE(show_feedback)) {
          showNotification("No data changes detected. The displayed dataset is up to date.", type = "default", duration = 4)
        }
      } else {
        data_bundle(new_bundle)
        data_hash(new_hash)
        refresh_state("updated")
        if (isTRUE(show_feedback)) {
          showNotification(
            paste0("Data updated from ", new_bundle$source, " — ", nrow(new_bundle$master_full), " initiatives loaded."),
            type = "message", duration = 5
          )
        }
      }
    }, error = function(e) {
      old_bundle <- data_bundle()
      old_bundle$error <- conditionMessage(e)
      data_bundle(old_bundle)
      refresh_state("error")
      showNotification(
        paste0("Data refresh failed. The previous dataset is still displayed. ", conditionMessage(e)),
        type = "error", duration = 8
      )
    })
  }

  observeEvent(input$refresh_data, {
    refresh_data_from_source(show_feedback = TRUE)
  }, ignoreInit = TRUE)

  output$data_status_nav <- renderUI({
    b <- data_bundle()
    state <- refresh_state()
    has_error <- !is.null(b$error)
    time_label <- if (is.null(b$loaded_at)) "loaded" else format(b$loaded_at, "%H:%M")
    main_label <- if (has_error) "Refresh failed" else if (identical(state, "unchanged")) "Up to date" else "Data live"
    dot_class <- if (has_error) "data-status-dot warn" else "data-status-dot"

    span(
      class = "data-status-chip",
      title = paste0("Source: ", b$source %||% "Unknown", if (has_error) paste0(" | Last error: ", b$error) else ""),
      span(class = dot_class),
      span(class = "data-status-main", main_label),
      span(class = "data-status-sub", paste0("· ", time_label))
    )
  })

  # ---- Shared KPI ----
  render_kpi <- function(value, label, note = NULL) {
    div(
      class = "kpi",
      div(class = "label", label),
      div(class = "value", value),
      if (!is.null(note)) div(class = "note", note)
    )
  }

  # ---- Reusable plots ----
  bar_plotly <- function(df, xvar, top_n = 12, horizontal = TRUE, y_title = "Initiatives") {
    plot_df <- df |>
      filter(!is.na({{ xvar }}), {{ xvar }} != "", {{ xvar }} != "Unknown") |>
      count({{ xvar }}, sort = TRUE) |>
      slice_head(n = top_n) |>
      mutate(label = forcats::fct_reorder(as.factor({{ xvar }}), n))

    p <- ggplot(plot_df, aes(x = label, y = n, text = paste0({{ xvar }}, "<br>", n))) +
      geom_col(width = 0.72, fill = "#1E7890") +
      labs(x = NULL, y = y_title) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(8, 12, 8, 8)
      )

    if (horizontal) p <- p + coord_flip()

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  }

  matrix_plotly <- function(df, x, y, value = n, fill_title = "Initiatives", percent = FALSE) {
    p <- ggplot(df, aes(x = {{ x }}, y = {{ y }}, fill = {{ value }},
                        text = if (percent) {
                          paste0({{ y }}, "<br>", {{ x }}, "<br>", scales::percent({{ value }}, accuracy = 1))
                        } else {
                          paste0({{ y }}, "<br>", {{ x }}, "<br>", {{ value }})
                        })) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B",
                          labels = if (percent) scales::percent_format(accuracy = 1) else waiver()) +
      labs(x = NULL, y = NULL, fill = fill_title) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        plot.margin = margin(8, 8, 8, 8)
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 115))
  }

  flag_share <- function(df, var) {
    mean(df[[var]] == "Yes", na.rm = TRUE)
  }

  # ---- Overview ----
  output$kpi_initiatives <- renderUI({
    render_kpi(comma(nrow(master_full)), "Initiatives", "Mapped records")
  })

  output$kpi_countries <- renderUI({
    render_kpi(comma(n_distinct(master_full$country[master_full$country != "Unknown"])), "Countries", "Geographical coverage")
  })

  output$kpi_regions <- renderUI({
    render_kpi(comma(n_distinct(master_full$region[master_full$region != "Unknown"])), "Regions", "World regions")
  })

  output$kpi_types <- renderUI({
    render_kpi(comma(n_distinct(master_full$initiative_type[master_full$initiative_type != "Unknown"])), "Types", "COSMI categories")
  })

  output$overview_map <- renderLeaflet({
    map_df <- master_full |>
      filter(!is.na(latitude), !is.na(longitude)) |>
      filter(latitude != 0 | longitude != 0)

    leaflet(
      map_df,
      options = leafletOptions(
        minZoom = 2,
        maxZoom = 8,
        maxBounds = list(list(-85, -180), list(85, 180)),
        maxBoundsViscosity = 1.0,
        worldCopyJump = FALSE
      )
    ) |>
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      setView(lng = 10, lat = 25, zoom = 2) |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        stroke = TRUE,
        weight = 0.7,
        color = "#FFFFFF",
        fillColor = "#1E7890",
        fillOpacity = 0.70,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE
        ),
        popup = ~paste0(
          "<strong>", name, "</strong><br/>",
          country, "<br/>",
          initiative_type
        )
      )
  })

  output$overview_country_map <- renderPlotly({
    country_totals <- master_full |>
      filter(country != "Unknown", !is.na(country), country != "") |>
      count(country, sort = TRUE, name = "total") |>
      mutate(
        z_scaled = log10(total + 1),
        hover_label = paste0(
          "<b>", country, "</b><br>",
          scales::comma(total), " initiatives<br>",
          "Click to show the detailed table"
        )
      )

    validate(need(nrow(country_totals) > 0, "No country data available."))

    tick_values <- c(1, 5, 10, 25, 50, 100, 250, 500)
    tick_values <- tick_values[tick_values <= max(country_totals$total, na.rm = TRUE)]
    if (length(tick_values) == 0) tick_values <- max(country_totals$total, na.rm = TRUE)

    plot_ly(
      country_totals,
      source = "overview_country_map",
      type = "choropleth",
      locations = ~country,
      locationmode = "country names",
      z = ~z_scaled,
      key = ~country,
      text = ~hover_label,
      colorscale = list(
        c(0, "#E6F2F5"), c(0.25, "#B8DDE3"), c(0.50, "#68AEBB"),
        c(0.75, "#237D96"), c(1, "#12395B")
      ),
      marker = list(line = list(color = "#FFFFFF", width = 0.7)),
      hoverinfo = "text",
      colorbar = list(
        title = list(text = "Initiatives"),
        tickmode = "array",
        tickvals = log10(tick_values + 1),
        ticktext = scales::comma(tick_values),
        len = 0.55,
        thickness = 14
      )
    ) |>
      layout(
        geo = list(
          projection = list(type = "natural earth"),
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#B8C4CC",
          showcountries = TRUE,
          countrycolor = "#FFFFFF",
          showland = TRUE,
          landcolor = "#F4F8FA",
          showocean = TRUE,
          oceancolor = "#EAF2F6",
          bgcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 0, r = 0, t = 10, b = 0)
      ) |>
      config(displayModeBar = FALSE)
  })

  selected_country <- reactive({
    click <- plotly::event_data("plotly_click", source = "overview_country_map")
    country_totals <- master_full |>
      filter(country != "Unknown", !is.na(country), country != "") |>
      count(country, sort = TRUE, name = "n")

    if (nrow(country_totals) == 0) return(NULL)

    if (!is.null(click) && nrow(click) > 0) {
      candidate <- NULL
      if ("location" %in% names(click) && !is.na(click$location[[1]])) candidate <- as.character(click$location[[1]])
      if ((is.null(candidate) || !candidate %in% country_totals$country) && "key" %in% names(click) && !is.na(click$key[[1]])) candidate <- as.character(click$key[[1]])
      if (!is.null(candidate) && candidate %in% country_totals$country) return(candidate)
    }

    country_totals$country[[1]]
  })

  output$overview_country_summary <- renderUI({
    country <- selected_country()
    if (is.null(country)) return(NULL)

    df <- master_full |> filter(.data$country == .env$country)
    gov <- df |> count(governance_type, sort = TRUE, name = "n") |>
      mutate(label = paste0(governance_type, " · ", scales::comma(n)))

    div(
      class = "country-detail-panel",
      div(class = "country-detail-header", h4(country), span(class = "country-total-badge", paste0(scales::comma(nrow(df)), " initiatives"))),
      div(class = "gov-pill-wrap", lapply(seq_len(nrow(gov)), function(i) span(class = "gov-pill", gov$label[[i]]))),
      p(class = "micro", "The table below is filtered dynamically from the clicked country.")
    )
  })

  output$overview_country_table <- renderDT({
    country <- selected_country()
    if (is.null(country)) return(NULL)

    df <- master_full |>
      filter(.data$country == .env$country) |>
      arrange(name) |>
      select(name, initiative_type, governance_type, lead_actor_type, region, website)

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      colnames = c("Initiative", "Type", "Governance", "Lead actor", "Region", "Website"),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: 800; padding: 10px 0; color: #12395B;",
        paste0("Initiatives in ", country)
      ),
      options = cosmi_dt_options(page_length = 8, dom = "tip", buttons = NULL)
    )
  })

  output$overview_type_plot <- renderPlotly({
    plot_df <- master_full |>
      filter(!is.na(initiative_type), initiative_type != "", initiative_type != "Unknown") |>
      count(initiative_type, sort = TRUE) |>
      slice_head(n = 10) |>
      mutate(label = forcats::fct_reorder(as.factor(initiative_type), n))

    p <- ggplot(
      plot_df,
      aes(
        x = label,
        y = n,
        customdata = initiative_type,
        text = paste0(initiative_type, "<br>", n, " initiatives<br>Click to list records")
      )
    ) +
      geom_col(width = 0.72, fill = "#1E7890") +
      coord_flip() +
      labs(x = NULL, y = "Initiatives") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(8, 12, 8, 8)
      )

    ggplotly(p, tooltip = "text", source = "overview_type") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })


  overview_selected_type <- reactive({
    click <- plotly::event_data("plotly_click", source = "overview_type")
    if (is.null(click) || is.null(click$customdata)) return(NULL)
    as.character(click$customdata[[1]])
  })

  output$overview_selected_table <- renderDT({
    selected <- overview_selected_type()

    df <- master_full |>
      select(initiative_id, name, initiative_type, lead_actor_type, governance_type, country, region, website)

    if (!is.null(selected)) {
      df <- df |> filter(initiative_type == selected)
      caption_txt <- paste0("Selected type: ", selected)
    } else {
      df <- df |>
        arrange(name) |>
        slice_head(n = 25)
      caption_txt <- "Click a bar to list initiatives by type"
    }

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: 700; padding-bottom: 8px;",
        caption_txt
      ),
      options = cosmi_dt_options(page_length = 8, dom = "tip", buttons = NULL)
    )
  })

  output$overview_insight <- renderUI({
    top_type <- master_full |>
      filter(initiative_type != "Unknown") |>
      count(initiative_type, sort = TRUE) |>
      slice_head(n = 1)

    top_region <- master_full |>
      filter(region != "Unknown") |>
      count(region, sort = TRUE) |>
      slice_head(n = 1)

    txt <- paste0(
      "The most frequent category is “", top_type$initiative_type %||% "Unknown",
      "”. The strongest regional concentration is “", top_region$region %||% "Unknown",
      "”. Click a bar in the typology chart to inspect the corresponding initiatives."
    )

    div(class = "insight", txt)
  })

  # ---- Territories ----
  territory_data <- reactive({
    df <- master_full
    if (!is.null(input$territory_region) && input$territory_region != "All") {
      df <- df |> filter(region == input$territory_region)
    }
    if (!is.null(input$territory_country) && input$territory_country != "All") {
      df <- df |> filter(country == input$territory_country)
    }
    if (!is.null(input$territory_type) && input$territory_type != "All") {
      df <- df |> filter(initiative_type == input$territory_type)
    }
    if (!is.null(input$territory_actor) && input$territory_actor != "All") {
      df <- df |> filter(lead_actor_type == input$territory_actor)
    }
    df
  })

  observeEvent(input$reset_territory, {
    updateSelectInput(session, "territory_region", selected = "All")
    updateSelectInput(session, "territory_country", selected = "All")
    updateSelectInput(session, "territory_type", selected = "All")
    updateSelectInput(session, "territory_actor", selected = "All")
  })

  observeEvent(input$territory_region, {
    df <- master_full
    if (!is.null(input$territory_region) && input$territory_region != "All") {
      df <- df |> filter(region == input$territory_region)
    }
    updateSelectInput(session, "territory_country", choices = c("All", safe_distinct_values(df$country)), selected = "All")
  })

  output$territory_kpi_n <- renderUI({
    render_kpi(comma(nrow(territory_data())), "Initiatives", "Current selection")
  })

  output$territory_kpi_countries <- renderUI({
    render_kpi(comma(n_distinct(territory_data()$country[territory_data()$country != "Unknown"])), "Countries", "Current selection")
  })

  output$territory_kpi_community <- renderUI({
    render_kpi(percent(flag_share(territory_data(), "community_led"), accuracy = 1), "Community-led", "Share coded Yes")
  })

  output$territory_kpi_nonprofit <- renderUI({
    render_kpi(percent(flag_share(territory_data(), "non_profit"), accuracy = 1), "Non-profit", "Share coded Yes")
  })

  output$region_type_share_matrix <- renderPlotly({
    df <- territory_data() |>
      filter(region != "Unknown", initiative_type != "Unknown") |>
      count(region, initiative_type) |>
      group_by(region) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(region = forcats::fct_reorder(region, total))

    matrix_plotly(
      df,
      initiative_type,
      region,
      share,
      fill_title = "Share",
      percent = TRUE
    )
  })

  output$territory_governance_plot <- renderPlotly({
    gov_long <- territory_data() |>
      select(initiative_id, non_profit, community_led, open_source_tools, multilingual) |>
      pivot_longer(-initiative_id, names_to = "indicator", values_to = "value") |>
      mutate(
        indicator = dplyr::recode(
          indicator,
          "non_profit" = "Non-profit",
          "community_led" = "Community-led",
          "open_source_tools" = "Open-source tools",
          "multilingual" = "Multilingual"
        )
      ) |>
      filter(value %in% c("Yes", "No", "Possible", "Unknown")) |>
      count(indicator, value)

    p <- ggplot(gov_long, aes(x = indicator, y = n, fill = value,
                              text = paste0(indicator, "<br>", value, "<br>", n))) +
      geom_col(position = "fill", width = 0.72) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = cosmi_flag_palette, drop = FALSE) +
      labs(x = NULL, y = "Share", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.major.x = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 90))
  })

  output$region_type_count_plot <- renderPlotly({
    df <- territory_data() |>
      filter(region != "Unknown", initiative_type != "Unknown") |>
      count(region, initiative_type) |>
      group_by(region) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(region = forcats::fct_reorder(region, total))

    p <- ggplot(df, aes(x = region, y = n, fill = initiative_type,
                        text = paste0(region, "<br>", initiative_type, "<br>", n, " initiatives"))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = cosmi_palette) +
      labs(x = NULL, y = "Initiatives", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.45, "cm")
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 120),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  output$country_type_count_plot <- renderPlotly({
    df <- territory_data() |>
      filter(country != "Unknown", initiative_type != "Unknown") |>
      count(country, initiative_type) |>
      group_by(country) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(country = forcats::fct_reorder(country, total))

    p <- ggplot(df, aes(x = country, y = n, fill = initiative_type,
                        text = paste0(country, "<br>", initiative_type, "<br>", n, " initiatives"))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = cosmi_palette) +
      labs(x = NULL, y = "Initiatives", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.45, "cm")
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 120),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  output$country_type_share_plot <- renderPlotly({
    df <- territory_data() |>
      filter(country != "Unknown", initiative_type != "Unknown") |>
      count(country, initiative_type) |>
      group_by(country) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(country = forcats::fct_reorder(country, total))

    p <- ggplot(df, aes(x = country, y = share, fill = initiative_type,
                        text = paste0(country, "<br>", initiative_type, "<br>",
                                      scales::percent(share, accuracy = 1),
                                      " (", n, " initiatives)"))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = cosmi_palette) +
      labs(x = NULL, y = "Within-country distribution", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.45, "cm")
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 120),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  output$region_distribution_plot <- renderPlotly({
    df <- territory_data() |>
      filter(region != "Unknown", initiative_type != "Unknown") |>
      count(region, initiative_type) |>
      group_by(region) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(region = forcats::fct_reorder(region, total))

    p <- ggplot(df, aes(x = region, y = share, fill = initiative_type,
                        text = paste0(region, "<br>", initiative_type, "<br>",
                                      scales::percent(share, accuracy = 1),
                                      " (", n, " initiatives)"))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = cosmi_palette) +
      labs(x = NULL, y = "Within-region distribution", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.45, "cm")
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 120),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  # ---- Typology ----
  output$type_distribution_plot <- renderPlotly({
    bar_plotly(master_full, initiative_type, top_n = 10)
  })

  output$type_actor_matrix <- renderPlotly({
    df <- master_full |>
      filter(initiative_type != "Unknown", lead_actor_type != "Unknown") |>
      count(initiative_type, lead_actor_type) |>
      group_by(initiative_type) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      filter(total >= 3)

    matrix_plotly(df, lead_actor_type, initiative_type, n)
  })

  output$type_scope_matrix <- renderPlotly({
    df <- master_full |>
      filter(initiative_type != "Unknown", scope != "Unknown") |>
      count(initiative_type, scope)

    matrix_plotly(df, scope, initiative_type, n)
  })


  cluster_model <- reactive({
    # Main categorical variables
    base <- master_full |>
      select(
        initiative_id, name, country, initiative_type, lead_actor_type, scope,
        governance_type, non_profit, community_led, open_source_tools,
        multilingual, global_south_presence, region
      )

    # Region is always kept for display in tables.
    # It is included in the clustering features only when requested.

    # Open science dimensions as binary variables
    dims_bin <- dims_long |>
      mutate(value = ifelse(level %in% c("Yes", "Possible"), 1, 0)) |>
      group_by(initiative_id, dimension) |>
      summarise(value = max(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = dimension,
        values_from = value,
        values_fill = 0,
        names_prefix = "dim_"
      )

    dat <- base |>
      left_join(dims_bin, by = "initiative_id")

    dim_cols <- names(dat)[stringr::str_starts(names(dat), "dim_")]
    if (length(dim_cols) > 0) {
      dat[dim_cols] <- lapply(dat[dim_cols], function(x) ifelse(is.na(x), 0, x))
    }

    # Features used for clustering
    feature_df <- dat |>
      select(-name) |>
      mutate(across(where(is.character), ~ ifelse(is.na(.x) | .x == "", "Unknown", .x)))

    if (!isTRUE(input$cluster_include_region) && "region" %in% names(feature_df)) {
      feature_df <- feature_df |> select(-region)
    }

    # Remove variables with no analytical variation BEFORE model.matrix
    variable_names <- setdiff(names(feature_df), "initiative_id")
    variable_names <- variable_names[
      vapply(feature_df[variable_names], function(x) dplyr::n_distinct(x, na.rm = TRUE) > 1, logical(1))
    ]

    if (length(variable_names) < 2 || nrow(feature_df) < 3) {
      return(list(
        data = dat |> mutate(cluster = "Not enough variability"),
        hc = NULL,
        features = NULL,
        status = "Not enough variation to compute clusters."
      ))
    }

    feature_df_clean <- feature_df |>
      select(initiative_id, all_of(variable_names))

    # model.matrix can still fail if a factor has one level after internal handling, so keep it safe
    feature_matrix <- tryCatch(
      model.matrix(~ . - initiative_id - 1, data = feature_df_clean),
      error = function(e) NULL
    )

    if (is.null(feature_matrix) || ncol(feature_matrix) < 2) {
      return(list(
        data = dat |> mutate(cluster = "Not enough variability"),
        hc = NULL,
        features = NULL,
        status = "Not enough variation to compute clusters."
      ))
    }

    # Remove constant dummy columns
    keep <- apply(feature_matrix, 2, function(x) stats::sd(x, na.rm = TRUE) > 0)
    feature_matrix <- feature_matrix[, keep, drop = FALSE]

    if (ncol(feature_matrix) < 2 || nrow(feature_matrix) < 3) {
      return(list(
        data = dat |> mutate(cluster = "Not enough variability"),
        hc = NULL,
        features = NULL,
        status = "Not enough variation to compute clusters."
      ))
    }

    scaled_matrix <- scale(feature_matrix)
    scaled_matrix[is.na(scaled_matrix)] <- 0

    dist_mat <- dist(scaled_matrix, method = "euclidean")
    hc <- hclust(dist_mat, method = "ward.D2")

    k <- input$cluster_k %||% 5
    k <- min(k, nrow(dat) - 1)
    clusters <- cutree(hc, k = k)

    list(
      data = dat |> mutate(cluster = paste0("Cluster ", clusters)),
      hc = hc,
      features = colnames(feature_matrix),
      status = "ok"
    )
  })

  cluster_data <- reactive({
    cluster_model()$data
  })

  output$cluster_size_plot <- renderPlotly({
    validate(need(cluster_model()$status == "ok", cluster_model()$status))

    df <- cluster_data() |>
      count(cluster, sort = TRUE) |>
      mutate(cluster_label = forcats::fct_reorder(cluster, n))

    p <- ggplot(
      df,
      aes(
        x = cluster_label,
        y = n,
        customdata = cluster,
        text = paste0(cluster, "<br>", n, " initiatives<br>Click to filter the table")
      )
    ) +
      geom_col(fill = "#1E7890", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Initiatives") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text", source = "cluster_size") |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$cluster_profile_plot <- renderPlotly({
    validate(need(cluster_model()$status == "ok", cluster_model()$status))
    dat <- cluster_data()

    profile <- dat |>
      select(cluster, non_profit, community_led, open_source_tools, multilingual) |>
      pivot_longer(-cluster, names_to = "indicator", values_to = "value") |>
      mutate(
        indicator = dplyr::recode(
          indicator,
          "non_profit" = "Non-profit",
          "community_led" = "Community-led",
          "open_source_tools" = "Open-source tools",
          "multilingual" = "Multilingual"
        ),
        yes = value == "Yes"
      ) |>
      group_by(cluster, indicator) |>
      summarise(share = mean(yes, na.rm = TRUE), .groups = "drop")

    p <- ggplot(profile, aes(x = indicator, y = cluster, fill = share,
                             text = paste0(cluster, "<br>", indicator, "<br>", scales::percent(share, accuracy = 1)))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Share Yes") +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 90))
  })


  output$cluster_dendrogram <- renderPlot({
    validate(need(cluster_model()$status == "ok", cluster_model()$status))

    hc <- cluster_model()$hc
    k <- input$cluster_k %||% 5

    plot(
      hc,
      labels = FALSE,
      hang = -1,
      main = "",
      xlab = "Initiatives",
      ylab = "Height",
      sub = ""
    )
    rect.hclust(hc, k = k, border = "#1E7890")
  })

  output$download_clusters <- downloadHandler(
    filename = function() {
      paste0("cosmi_hub_clusters_k", input$cluster_k, ".csv")
    },
    content = function(file) {
      readr::write_csv(
        cluster_data() |>
          select(cluster, initiative_id, name, country, initiative_type, lead_actor_type,
                 governance_type, scope, region, non_profit,
                 community_led, open_source_tools, multilingual),
        file
      )
    }
  )


  selected_cluster <- reactive({
    click <- plotly::event_data("plotly_click", source = "cluster_size")
    if (is.null(click) || is.null(click$customdata)) return(NULL)
    as.character(click$customdata[[1]])
  })

  output$cluster_table <- renderDT({
    validate(need(cluster_model()$status == "ok", cluster_model()$status))

    selected <- selected_cluster()

    df <- cluster_data() |>
      select(
        cluster, initiative_id, name, country, initiative_type, lead_actor_type,
        governance_type, scope, region, non_profit,
        community_led, open_source_tools, multilingual
      ) |>
      arrange(cluster, name)

    caption_txt <- "All clustered initiatives"
    if (!is.null(selected)) {
      df <- df |> filter(cluster == selected)
      caption_txt <- paste0("Selected cluster: ", selected)
    }

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: 700; padding-bottom: 8px;",
        caption_txt
      ),
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 360,
        scroller = TRUE
      )
    )
  })

  output$typology_table <- renderDT({
    master_full |>
      select(initiative_id, name, short_name, category, initiative_type, lead_actor_type, governance_type, scope, country, region, website) |>
      linkify_website() |>
      datatable(
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 12,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 420,
          scroller = TRUE
        )
      )
  })

  # ---- Dimensions & networks ----
  dims_filtered <- reactive({
    df <- dims_long
    if (!is.null(input$dimension_filter) && input$dimension_filter != "All") {
      df <- df |> filter(dimension == input$dimension_filter)
    }
    if (!is.null(input$level_filter) && length(input$level_filter) > 0) {
      df <- df |> filter(level %in% input$level_filter)
    }
    df
  })

  dimension_pairs <- reactive({
    records <- dims_filtered() |>
      filter(level %in% c("Yes", "Possible")) |>
      distinct(initiative_id, dimension)

    pair_source <- records |>
      group_by(initiative_id) |>
      summarise(dims = list(sort(unique(dimension))), .groups = "drop") |>
      filter(lengths(dims) >= 2)

    if (nrow(pair_source) == 0) {
      return(tibble(from = character(), to = character(), weight = numeric()))
    }

    purrr::map_dfr(pair_source$dims, function(x) {
      cmb <- t(combn(x, 2))
      tibble(from = cmb[, 1], to = cmb[, 2])
    }) |>
      count(from, to, name = "weight") |>
      filter(weight >= (input$min_cooccurrence %||% 1))
  })

  output$dimension_kpi_records <- renderUI({
    render_kpi(comma(nrow(dims_filtered())), "Records", "Current selection")
  })

  output$dimension_kpi_initiatives <- renderUI({
    render_kpi(comma(n_distinct(dims_filtered()$initiative_id)), "Initiatives", "With selected dimensions")
  })

  output$dimension_kpi_top <- renderUI({
    top_dim <- dims_filtered() |>
      filter(dimension != "Unknown") |>
      count(dimension, sort = TRUE) |>
      slice_head(n = 1)
    render_kpi(top_dim$n %||% 0, "Top dimension", top_dim$dimension %||% "None")
  })

  output$dimension_kpi_pairs <- renderUI({
    render_kpi(comma(nrow(dimension_pairs())), "Dimension pairs", "Above threshold")
  })

  output$dimension_bar <- renderPlotly({
    df <- dims_filtered() |>
      filter(dimension != "Unknown") |>
      distinct(initiative_id, dimension, level) |>
      count(dimension, level, sort = TRUE) |>
      group_by(dimension) |>
      mutate(total = sum(n)) |>
      ungroup() |>
      filter(total > 0) |>
      mutate(
        dimension_full = dimension,
        dimension_label = stringr::str_wrap(dimension, 36),
        dimension_label = forcats::fct_reorder(as.factor(dimension_label), total)
      )

    validate(need(nrow(df) > 0, "No dimension records for current selection."))

    p <- ggplot(
      df,
      aes(
        x = dimension_label,
        y = n,
        fill = level,
        customdata = dimension_full,
        text = paste0(dimension_full, "<br>", level, "<br>", n, " records<br>Click to list initiatives")
      )
    ) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = cosmi_flag_palette, drop = FALSE) +
      labs(x = NULL, y = "Records", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9)
      )

    ggplotly(p, tooltip = "text", source = "dimension_bar") |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 80),
        legend = list(orientation = "h", x = 0, y = -0.15)
      )
  })

  output$dimension_type_matrix <- renderPlotly({
    df <- dims_filtered() |>
      left_join(master_full |> select(initiative_id, initiative_type), by = "initiative_id") |>
      filter(dimension != "Unknown", initiative_type != "Unknown") |>
      distinct(initiative_id, dimension, initiative_type) |>
      count(dimension, initiative_type) |>
      group_by(initiative_type) |>
      mutate(type_total = sum(n), share = n / type_total) |>
      ungroup() |>
      filter(type_total >= 3)

    validate(need(nrow(df) > 0, "No dimension/type combination for current selection."))

    p <- ggplot(df, aes(x = dimension, y = initiative_type, fill = share,
                        text = paste0(
                          initiative_type, "<br>", dimension, "<br>",
                          scales::percent(share, accuracy = 1), " (", n, " records)"
                        ))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Within type") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        plot.margin = margin(8, 8, 8, 8)
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 130))
  })

  output$dimension_network <- renderPlotly({
    edges <- dimension_pairs()

    records <- dims_filtered() |>
      filter(level %in% c("Yes", "Possible")) |>
      distinct(initiative_id, dimension)

    nodes <- records |>
      count(dimension, name = "size") |>
      filter(dimension %in% unique(c(edges$from, edges$to))) |>
      arrange(desc(size))

    if (nrow(edges) == 0 || nrow(nodes) < 2) {
      return(
        plot_ly() |>
          layout(
            annotations = list(
              text = "No co-occurrences for current selection",
              x = 0.5, y = 0.5, showarrow = FALSE,
              font = list(size = 16, color = "#617184")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            margin = list(l = 10, r = 10, t = 10, b = 10)
          )
      )
    }

    n_nodes <- nrow(nodes)
    theta <- seq(0, 2*pi, length.out = n_nodes + 1)[-(n_nodes + 1)]
    nodes <- nodes |>
      mutate(
        x = cos(theta),
        y = sin(theta),
        label = stringr::str_wrap(dimension, 22)
      )

    edges <- edges |>
      left_join(nodes |> select(from = dimension, x_from = x, y_from = y), by = "from") |>
      left_join(nodes |> select(to = dimension, x_to = x, y_to = y), by = "to")

    p <- plot_ly()

    for (i in seq_len(nrow(edges))) {
      p <- add_trace(
        p,
        x = c(edges$x_from[i], edges$x_to[i]),
        y = c(edges$y_from[i], edges$y_to[i]),
        type = "scatter",
        mode = "lines",
        line = list(width = max(1.2, min(9, edges$weight[i] / 2)), color = "rgba(18,57,91,0.25)"),
        hoverinfo = "text",
        text = paste0(edges$from[i], " – ", edges$to[i], "<br>", edges$weight[i], " shared initiatives"),
        showlegend = FALSE
      )
    }

    p <- add_trace(
      p,
      data = nodes,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers+text",
      text = ~label,
      textposition = "top center",
      textfont = list(size = 10, color = "#596773"),
      marker = list(
        size = ~scales::rescale(size, to = c(16, 38)),
        color = "#1E7890",
        opacity = 0.92,
        line = list(color = "white", width = 2)
      ),
      hoverinfo = "text",
      hovertext = ~paste0(dimension, "<br>", size, " records"),
      showlegend = FALSE
    )

    p |>
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(l = 10, r = 10, t = 10, b = 10)
      )
  })

  output$dimension_co_matrix <- renderPlotly({
    edges <- dimension_pairs()

    validate(need(nrow(edges) > 0, "No co-occurrences for current selection."))

    df <- edges |>
      bind_rows(edges |> transmute(from = to, to = from, weight = weight))

    df <- df |>
      mutate(
        from_label = stringr::str_wrap(from, 28),
        to_label = stringr::str_wrap(to, 28)
      )

    p <- ggplot(df, aes(x = from_label, y = to_label, fill = weight,
                        text = paste0(from, " – ", to, "<br>", weight, " shared initiatives"))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B") +
      labs(x = NULL, y = NULL, fill = "Shared") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 150))
  })


  selected_dimension_from_plot <- reactive({
    click <- plotly::event_data("plotly_click", source = "dimension_bar")
    if (is.null(click) || is.null(click$customdata)) return(NULL)
    as.character(click$customdata[[1]])
  })

  output$dimension_initiatives_table <- renderDT({
    selected <- selected_dimension_from_plot()

    df <- dims_filtered() |>
      select(initiative_id, dimension, level, evidence) |>
      left_join(
        master_full |>
          select(initiative_id, name, initiative_type, lead_actor_type, governance_type, country, region, website),
        by = "initiative_id"
      ) |>
      mutate(
        name = normalise_empty(name),
        initiative_type = normalise_empty(initiative_type),
        lead_actor_type = normalise_empty(lead_actor_type),
        country = normalise_empty(country),
        region = normalise_empty(region)
      )

    if (!is.null(selected)) {
      df <- df |> filter(dimension == selected)
      caption_txt <- paste0("Selected dimension: ", selected)
    } else if (!is.null(input$dimension_filter) && input$dimension_filter != "All") {
      caption_txt <- paste0("Selected dimension: ", input$dimension_filter)
    } else {
      caption_txt <- "All selected dimension records. Click a bar above to filter this table."
    }

    df <- df |>
      arrange(dimension, name) |>
      select(initiative_id, name, dimension, level, initiative_type, lead_actor_type, governance_type, country, region, website, evidence)

    validate(need(nrow(df) > 0, "No initiatives for current dimension selection."))

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-weight: 700; padding-bottom: 8px;",
        caption_txt
      ),
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 420,
        scroller = TRUE
      )
    )
  })

  # ---- Economic ecosystem ----
  econ_full <- reactive({
    master_full |>
      transmute(
        initiative_id, name,
        role_in_ecosystem = normalise_empty(object_type),
        funding_model = normalise_empty(funding_model),
        funding_source = normalise_empty(funding_source),
        economic_logic = normalise_empty(economic_logic),
        notes = normalise_empty(first_existing(cur_data_all(), c("economic_notes", "notes")), ""),
        initiative_type = as.character(initiative_type),
        lead_actor_type, governance_type, country, region, website
      )
  })

  econ_filtered <- reactive({
    df <- econ_full()

    if (!is.null(input$econ_region) && input$econ_region != "All") {
      df <- df |> filter(region == input$econ_region)
    }
    if (!is.null(input$econ_country) && input$econ_country != "All") {
      df <- df |> filter(country == input$econ_country)
    }
    if (!is.null(input$econ_role) && input$econ_role != "All") {
      df <- df |> filter(role_in_ecosystem == input$econ_role)
    }
    if (!is.null(input$econ_funding) && input$econ_funding != "All") {
      df <- df |> filter(funding_model == input$econ_funding)
    }

    df
  })

  observeEvent(input$econ_region, {
    df <- econ_full()
    if (!is.null(input$econ_region) && input$econ_region != "All") {
      df <- df |> filter(region == input$econ_region)
    }
    updateSelectInput(session, "econ_country", choices = c("All", safe_distinct_values(df$country)), selected = "All")
  })

  observeEvent(input$reset_econ, {
    updateSelectInput(session, "econ_region", selected = "All")
    updateSelectInput(session, "econ_country", selected = "All")
    updateSelectInput(session, "econ_role", selected = "All")
    updateSelectInput(session, "econ_funding", selected = "All")
  })

  output$econ_kpi_initiatives <- renderUI({
    render_kpi(comma(n_distinct(econ_filtered()$initiative_id)), "Initiatives", "Current selection")
  })

  output$econ_kpi_roles <- renderUI({
    render_kpi(comma(n_distinct(econ_filtered()$role_in_ecosystem[econ_filtered()$role_in_ecosystem != "Unknown"])), "Roles", "Economic functions")
  })

  output$econ_kpi_funding <- renderUI({
    render_kpi(comma(n_distinct(econ_filtered()$funding_model[econ_filtered()$funding_model != "Unknown"])), "Funding models", "Current selection")
  })

  output$econ_kpi_unknown <- renderUI({
    df <- econ_filtered()
    pct <- if (nrow(df) == 0) NA_real_ else mean(df$funding_model %in% c("Unknown", "unknown / to validate", "to validate"), na.rm = TRUE)
    render_kpi(ifelse(is.na(pct), "—", percent(pct, accuracy = 1)), "Funding to validate", "Share of records")
  })

  output$role_plot <- renderPlotly({
    validate(need(nrow(econ_filtered()) > 0, "No economic records for current selection."))
    bar_plotly(econ_filtered(), role_in_ecosystem, top_n = 12)
  })

  output$funding_model_plot <- renderPlotly({
    validate(need(nrow(econ_filtered()) > 0, "No economic records for current selection."))
    bar_plotly(econ_filtered(), funding_model, top_n = 10)
  })

  output$economic_logic_plot <- renderPlotly({
    validate(need(nrow(econ_filtered()) > 0, "No economic records for current selection."))
    bar_plotly(econ_filtered(), economic_logic, top_n = 10)
  })

  output$econ_role_region_matrix <- renderPlotly({
    df <- econ_filtered() |>
      filter(region != "Unknown", role_in_ecosystem != "Unknown") |>
      count(region, role_in_ecosystem) |>
      group_by(region) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3)

    validate(need(nrow(df) > 0, "No role/region combination for current selection."))

    p <- ggplot(df, aes(x = role_in_ecosystem, y = forcats::fct_reorder(region, total), fill = share,
                        text = paste0(region, "<br>", role_in_ecosystem, "<br>",
                                      scales::percent(share, accuracy = 1), " (", n, " initiatives)"))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Within region") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 140))
  })

  output$econ_funding_region_matrix <- renderPlotly({
    df <- econ_filtered() |>
      filter(region != "Unknown", funding_model != "Unknown") |>
      count(region, funding_model) |>
      group_by(region) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3)

    validate(need(nrow(df) > 0, "No funding/region combination for current selection."))

    p <- ggplot(df, aes(x = funding_model, y = forcats::fct_reorder(region, total), fill = share,
                        text = paste0(region, "<br>", funding_model, "<br>",
                                      scales::percent(share, accuracy = 1), " (", n, " initiatives)"))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Within region") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 140))
  })

  output$econ_country_role_plot <- renderPlotly({
    df <- econ_filtered() |>
      filter(country != "Unknown", role_in_ecosystem != "Unknown") |>
      count(country, role_in_ecosystem) |>
      group_by(country) |>
      mutate(total = sum(n), share = n / total) |>
      ungroup() |>
      filter(total >= 3) |>
      mutate(country = forcats::fct_reorder(country, total))

    validate(need(nrow(df) > 0, "No country/role combination for current selection."))

    p <- ggplot(df, aes(x = country, y = share, fill = role_in_ecosystem,
                        text = paste0(country, "<br>", role_in_ecosystem, "<br>",
                                      scales::percent(share, accuracy = 1), " (", n, " initiatives)"))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = cosmi_palette) +
      labs(x = NULL, y = "Within-country distribution", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.45, "cm")
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 125),
        legend = list(orientation = "h", x = 0, y = -0.18)
      )
  })

  output$economic_table <- renderDT({
    df <- econ_filtered() |>
      select(
        initiative_id, name, role_in_ecosystem, funding_model, funding_source,
        economic_logic, initiative_type, lead_actor_type, governance_type,
        country, region, website, notes
      ) |>
      arrange(role_in_ecosystem, name)

    validate(need(nrow(df) > 0, "No economic records for current selection."))

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 420,
        scroller = TRUE
      )
    )
  })

  # ---- Governance & inclusion ----
  governance_full <- reactive({
    diversity |>
      select(initiative_id, non_profit, community_led, open_source_tools, multilingual, global_south_presence) |>
      left_join(
        master_full |>
          select(initiative_id, name, initiative_type, lead_actor_type, governance_type, country, region, website),
        by = "initiative_id"
      ) |>
      mutate(
        name = normalise_empty(name),
        initiative_type = normalise_empty(initiative_type),
        lead_actor_type = normalise_empty(lead_actor_type),
        governance_type = normalise_empty(governance_type),
        country = normalise_empty(country),
        region = normalise_empty(region),
        non_profit = normalise_flag(non_profit),
        community_led = normalise_flag(community_led),
        open_source_tools = normalise_flag(open_source_tools),
        multilingual = normalise_flag(multilingual),
        global_south_presence = normalise_empty(global_south_presence),
        global_south_yes = stringr::str_to_lower(global_south_presence) %in% c("yes", "yes / apparent", "global south", "present"),
        inclusion_index = rowMeans(
          cbind(
            community_led == "Yes",
            non_profit == "Yes",
            open_source_tools == "Yes",
            multilingual == "Yes",
            global_south_yes
          ),
          na.rm = TRUE
        ),
        governance_profile = paste(
          ifelse(community_led == "Yes", "community-led", "not community-led"),
          ifelse(non_profit == "Yes", "non-profit", "not non-profit"),
          ifelse(open_source_tools == "Yes", "open-source", "not open-source"),
          ifelse(multilingual == "Yes", "multilingual", "not multilingual"),
          sep = " | "
        )
      )
  })

  governance_filtered <- reactive({
    df <- governance_full()

    if (!is.null(input$gov_region) && input$gov_region != "All") {
      df <- df |> filter(region == input$gov_region)
    }
    if (!is.null(input$gov_country) && input$gov_country != "All") {
      df <- df |> filter(country == input$gov_country)
    }
    if (!is.null(input$gov_type) && input$gov_type != "All") {
      df <- df |> filter(initiative_type == input$gov_type)
    }

    df
  })

  observeEvent(input$gov_region, {
    df <- governance_full()
    if (!is.null(input$gov_region) && input$gov_region != "All") {
      df <- df |> filter(region == input$gov_region)
    }
    updateSelectInput(session, "gov_country", choices = c("All", safe_distinct_values(df$country)), selected = "All")
  })

  observeEvent(input$reset_gov, {
    updateSelectInput(session, "gov_region", selected = "All")
    updateSelectInput(session, "gov_country", selected = "All")
    updateSelectInput(session, "gov_type", selected = "All")
    updateSelectInput(session, "gov_indicator", selected = "All")
  })

  governance_long <- reactive({
    governance_filtered() |>
      select(
        initiative_id, region, country, initiative_type,
        community_led, non_profit, open_source_tools, multilingual, global_south_yes
      ) |>
      mutate(
        global_south_presence = ifelse(global_south_yes, "Yes", "No")
      ) |>
      select(-global_south_yes) |>
      pivot_longer(
        c(community_led, non_profit, open_source_tools, multilingual, global_south_presence),
        names_to = "indicator",
        values_to = "value"
      ) |>
      mutate(
        indicator = dplyr::recode(
          indicator,
          "community_led" = "Community-led",
          "non_profit" = "Non-profit",
          "open_source_tools" = "Open-source tools",
          "multilingual" = "Multilingual",
          "global_south_presence" = "Global South presence"
        )
      ) |>
      filter(input$gov_indicator == "All" | indicator == input$gov_indicator)
  })

  output$gov_kpi_initiatives <- renderUI({
    render_kpi(comma(n_distinct(governance_filtered()$initiative_id)), "Initiatives", "Current selection")
  })

  output$gov_kpi_index <- renderUI({
    idx <- mean(governance_filtered()$inclusion_index, na.rm = TRUE)
    render_kpi(ifelse(is.nan(idx), "—", percent(idx, accuracy = 1)), "Inclusion index", "Mean of five indicators")
  })

  output$gov_kpi_multilingual <- renderUI({
    pct <- mean(governance_filtered()$multilingual == "Yes", na.rm = TRUE)
    render_kpi(ifelse(is.nan(pct), "—", percent(pct, accuracy = 1)), "Multilingual", "Share coded Yes")
  })

  output$gov_kpi_global_south <- renderUI({
    pct <- mean(governance_filtered()$global_south_yes, na.rm = TRUE)
    render_kpi(ifelse(is.nan(pct), "—", percent(pct, accuracy = 1)), "Global South", "Presence / inclusion")
  })

  output$inclusion_region_matrix <- renderPlotly({
    df <- governance_long() |>
      filter(region != "Unknown") |>
      mutate(is_yes = value == "Yes") |>
      group_by(region, indicator) |>
      summarise(
        n = sum(is_yes, na.rm = TRUE),
        total = n_distinct(initiative_id),
        share = ifelse(total > 0, n / total, NA_real_),
        .groups = "drop"
      ) |>
      filter(total >= 3)

    validate(need(nrow(df) > 0, "No governance/region combination for current selection."))

    p <- ggplot(df, aes(x = indicator, y = forcats::fct_reorder(region, total), fill = share,
                        text = paste0(region, "<br>", indicator, "<br>",
                                      scales::percent(share, accuracy = 1), " (", n, "/", total, ")"))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Share Yes") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 120))
  })

  output$inclusion_type_matrix <- renderPlotly({
    df <- governance_long() |>
      filter(initiative_type != "Unknown") |>
      mutate(is_yes = value == "Yes") |>
      group_by(initiative_type, indicator) |>
      summarise(
        n = sum(is_yes, na.rm = TRUE),
        total = n_distinct(initiative_id),
        share = ifelse(total > 0, n / total, NA_real_),
        .groups = "drop"
      ) |>
      filter(total >= 3)

    validate(need(nrow(df) > 0, "No governance/type combination for current selection."))

    p <- ggplot(df, aes(x = indicator, y = initiative_type, fill = share,
                        text = paste0(initiative_type, "<br>", indicator, "<br>",
                                      scales::percent(share, accuracy = 1), " (", n, "/", total, ")"))) +
      geom_tile(color = "white", linewidth = 0.45) +
      scale_fill_gradient(low = "#EEF3F7", high = "#12395B", labels = scales::percent_format()) +
      labs(x = NULL, y = NULL, fill = "Share Yes") +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 120))
  })

  output$multilingual_region_plot <- renderPlotly({
    df <- governance_filtered() |>
      filter(region != "Unknown") |>
      group_by(region) |>
      summarise(
        total = n(),
        yes = sum(multilingual == "Yes", na.rm = TRUE),
        share = yes / total,
        .groups = "drop"
      ) |>
      filter(total >= 3) |>
      mutate(region = forcats::fct_reorder(region, share))

    validate(need(nrow(df) > 0, "No multilingualism data for current selection."))

    p <- ggplot(df, aes(x = region, y = share,
                        text = paste0(region, "<br>", scales::percent(share, accuracy = 1), " (", yes, "/", total, ")"))) +
      geom_col(fill = "#1E7890", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Share multilingual") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$global_south_region_plot <- renderPlotly({
    df <- governance_filtered() |>
      filter(region != "Unknown") |>
      group_by(region) |>
      summarise(
        total = n(),
        yes = sum(global_south_yes, na.rm = TRUE),
        share = yes / total,
        .groups = "drop"
      ) |>
      filter(total >= 3) |>
      mutate(region = forcats::fct_reorder(region, share))

    validate(need(nrow(df) > 0, "No Global South data for current selection."))

    p <- ggplot(df, aes(x = region, y = share,
                        text = paste0(region, "<br>", scales::percent(share, accuracy = 1), " (", yes, "/", total, ")"))) +
      geom_col(fill = "#1E7890", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Share with Global South presence") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$governance_profile_plot <- renderPlotly({
    df <- governance_filtered() |>
      filter(governance_profile != "Unknown") |>
      count(governance_profile, sort = TRUE) |>
      slice_head(n = 12) |>
      mutate(governance_profile = forcats::fct_reorder(stringr::str_wrap(governance_profile, 42), n))

    validate(need(nrow(df) > 0, "No governance profile for current selection."))

    p <- ggplot(df, aes(x = governance_profile, y = n, text = paste0(governance_profile, "<br>", n, " initiatives"))) +
      geom_col(fill = "#1E7890", width = 0.72) +
      coord_flip() +
      labs(x = NULL, y = "Initiatives") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$governance_country_plot <- renderPlotly({
    df <- governance_filtered() |>
      filter(country != "Unknown") |>
      group_by(country) |>
      summarise(
        total = n(),
        inclusion_index = mean(inclusion_index, na.rm = TRUE),
        community_led = mean(community_led == "Yes", na.rm = TRUE),
        non_profit = mean(non_profit == "Yes", na.rm = TRUE),
        open_source_tools = mean(open_source_tools == "Yes", na.rm = TRUE),
        multilingual = mean(multilingual == "Yes", na.rm = TRUE),
        global_south = mean(global_south_yes, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(total >= 3) |>
      arrange(desc(inclusion_index)) |>
      slice_head(n = 25) |>
      mutate(country = forcats::fct_reorder(country, inclusion_index))

    validate(need(nrow(df) > 0, "No country comparison for current selection."))

    p <- ggplot(df, aes(x = country, y = inclusion_index,
                        text = paste0(
                          country, "<br>Index: ", scales::percent(inclusion_index, accuracy = 1),
                          "<br>Community-led: ", scales::percent(community_led, accuracy = 1),
                          "<br>Non-profit: ", scales::percent(non_profit, accuracy = 1),
                          "<br>Open-source: ", scales::percent(open_source_tools, accuracy = 1),
                          "<br>Multilingual: ", scales::percent(multilingual, accuracy = 1),
                          "<br>Global South: ", scales::percent(global_south, accuracy = 1)
                        ))) +
      geom_col(fill = "#1E7890", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Inclusion index") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())

    ggplotly(p, tooltip = "text") |> plotly_cosmi_config() |>
      layout(margin = list(l = 20, r = 20, t = 10, b = 40))
  })

  output$diversity_table <- renderDT({
    df <- governance_filtered() |>
      select(
        initiative_id, name, initiative_type, lead_actor_type, governance_type,
        community_led, non_profit, open_source_tools, multilingual,
        global_south_presence, inclusion_index, country, region, website
      ) |>
      arrange(desc(inclusion_index), name)

    validate(need(nrow(df) > 0, "No governance records for current selection."))

    datatable(
      linkify_website(df),
      rownames = FALSE,
      escape = FALSE,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 420,
        scroller = TRUE
      )
    ) |>
      formatPercentage("inclusion_index", 0)
  })

  # ---- Data quality strip ----
  output$data_quality_strip <- renderUI({
    essential_fields <- c("name", "initiative_type", "lead_actor_type", "governance_type", "country", "region", "website")
    pct_complete <- completion_share(master_full, essential_fields)
    source_share <- mean(master_full$source_url != "" & master_full$source_url != "Unknown", na.rm = TRUE)
    validation_records <- if (nrow(valid) > 0) n_distinct(valid$initiative_id) else 0

    div(
      class = "quality-strip",
      div(class = "quality-chip", strong(scales::percent(pct_complete, accuracy = 1)), span("Essential-field completeness")),
      div(class = "quality-chip", strong(scales::percent(source_share, accuracy = 1)), span("Records with a source URL")),
      div(class = "quality-chip", strong(scales::comma(validation_records)), span("Initiatives flagged for review"))
    )
  })

  # ---- Validation ----
  output$download_dataset_csv <- downloadHandler(
    filename = function() {
      paste0("cosmihub_master_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(master_full, file)
    }
  )

  output$validation_priority_plot <- renderPlotly({
    bar_plotly(valid, validation_priority, top_n = 10)
  })

  output$validation_field_plot <- renderPlotly({
    bar_plotly(valid, field, top_n = 12)
  })

  output$validation_table <- renderDT({
    valid |>
      left_join(master |> select(initiative_id, name, initiative_type, country, region), by = "initiative_id") |>
      select(initiative_id, name, field, current_value, validation_priority, initiative_type, country, region, notes) |>
      datatable(
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        options = cosmi_dt_options(page_length = 12, dom = "Bfrtip")
      )
  })
}

shinyApp(ui, server)

# rsconnect::deployApp(appDir = "C:/Users/amaddi/Documents/Projets financés/OPENIT/openit/osinit_app/osinit", appName = "openit")
