library(arrow)
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(svglite)            # in-memory SVG rendering for list-view per-row charts
library(forcats)            # factor ordering in chart
library(bslib)              # Bootstrap 5 base theme
library(shinycssloaders)    # spinner overlays for slow outputs

# ===== Constants ==========================================================

sim_dirs <- c("coast", 'density', "diversity", "employment", "housing", "landcover",
              "people", 'remoteness', "socioeconomic", "terrain", "vegetation",
              "voting", "water",
              "weather",
              "dining", "transport", "food", "culture", "communityinfra",
              "tertiary", "health", "kinder", "schools")

all_dim_codes    <- sim_dirs
default_settings <- setNames(rep("similar", length(all_dim_codes)), all_dim_codes)

# Target-similarity values for the four-state toggle. Each is the raw_sim
# value that scores 1.0 (anything else scores progressively less by linear
# distance, clamped to >= 0). 0.5 isn't an option — use "similar" or
# "different" rather than something exactly in between.
target_values <- c(
  similar          = 1.0,
  mostly_similar   = 0.7,
  mostly_different = 0.3,
  different        = 0.0
)

# Human-readable labels for the 4 settings (used in summaries / chips).
target_labels <- c(
  similar          = "Similar",
  mostly_similar   = "Mostly similar",
  mostly_different = "Mostly different",
  different        = "Contrasting"
)

# People vs Place grouping for the focus presets. 4 people-ish dims (people,
# socioeconomic, voting, diversity) and 10 place-ish dims (the rest).
people_dims <- c("people", "socioeconomic", "voting", "diversity")
place_dims  <- setdiff(all_dim_codes, people_dims)

# Dream-suburb mode: which dims belong to each of the three user-facing
# themes. Different to the people/place split used elsewhere — employment
# moves into "urban" here because the user-facing question is about urban
# fabric/economy combined ("a busy place to work and live") rather than
# household socioeconomics.
dream_theme_dims <- list(
  people    = c("voting", "people", "diversity", "socioeconomic"),
  urban     = c("remoteness", "employment", "housing", "landcover", "density"),
  nature    = c("water", "weather", "terrain", "vegetation", "coast"),
  amenities = c("dining", "transport", "food", "culture", "communityinfra",
                "tertiary", "health", "kinder", "schools")
)
dream_theme_labels <- c(
  people    = "People & culture",
  urban     = "Urban fabric & economy",
  nature    = "Nature & climate",
  amenities = "Amenities")

# Preset focus weights — (w_people, w_place) applied to the group means
# when computing the match. Balanced = plain rowMeans across selected
# dims (no group weighting), as before.
focus_weights <- list(
  balanced       = NULL,            # signal: use plain rowMeans
  people_focused = c(people = 0.75, place = 0.25),
  place_focused  = c(people = 0.25, place = 0.75)
)

focus_labels <- c(
  balanced       = "Balanced",
  people_focused = "People-focused",
  place_focused  = "Place-focused"
)

dim_labels <- c(
  coast = "Coast access",                  
  density = 'Density',
  diversity = "Diversity",
  employment = "Employment",        housing = "Housing",
  landcover = "Land use",         people = "Age, sex and family",
  remoteness = 'Remoteness',
  socioeconomic = "Socioeconomic",  terrain = "Terrain",
  vegetation = "Vegetation",        voting = "Voting",
  water = "Water presence",
  weather = "Weather",
  dining = "Dining",
  transport = "Public transport",
  food = "Fresh food",
  culture = "Cultural amenities",
  communityinfra = "Community infrastructure",
  tertiary = "Tertiary education",
  health = "Health infrastructure",
  kinder = "Kinder",
  schools = "Schools"
)

# Cohesive 23-colour palette: Polychrome-derived, designed for high-cardinality
# categorical encodings with strong perceptual separation between adjacent
# hues. Reasonably colourblind-friendly across types. The original 14 (geo/
# people/place dims) keep their existing colours; the 9 amenity dims added
# below use a distinct sub-palette so the new "Amenities" group reads as its
# own visual family in charts that mix both sets.
dim_colors <- c(
  coast         = "#5A5156",   # graphite (geographic)
  density       = "#E4761B",   # orange (settlement intensity)
  diversity     = "#1CBE4F",   # green (people / mixing)
  employment    = "#FE00FA",   # magenta (jobs)
  housing       = "#F8A19F",   # salmon (dwelling)
  landcover     = "#822E1C",   # brown (terrain category)
  people        = "#1C8356",   # forest green (demographics)
  remoteness    = "#16FF32",   # lime (settlement category)
  socioeconomic = "#3283FE",   # blue (income / occupation)
  terrain       = "#FEAF16",   # amber (relief)
  vegetation    = "#2ED9FF",   # cyan (botany)
  voting        = "#B00068",   # crimson (political)
  water         = "#325A9B",   # navy (hydrology)
  weather       = "#85660D",   # olive (climate)
  dining        = "#C4451C",   # burnt orange (hospitality)
  transport     = "#90AD1C",   # olive-lime (mobility)
  food          = "#FBE426",   # yellow (fresh food)
  culture       = "#AA0DFE",   # violet (arts)
  communityinfra= "#F6222E",   # red (civic)
  tertiary      = "#B10DA1",   # magenta-purple (education)
  health        = "#FF6E54",   # coral (wellbeing)
  kinder        = "#7ED7D1",   # teal (early years)
  schools       = "#00B5F7"    # sky blue (education)
)

state_abbr <- c("New South Wales" = "NSW", "Victoria" = "VIC", "Queensland" = "QLD",
                "South Australia" = "SA", "Western Australia" = "WA",
                "Tasmania" = "TAS", "Northern Territory" = "NT",
                "Australian Capital Territory" = "ACT",
                "Other Territories" = "OT")

default_tree_selection <- character(0)  # empty = all states
default_ref_code       <- "20002"   # Abbotsford VIC

# ===== Load data ==========================================================

# Cloudflare R2 connection. Credentials come from .Renviron locally and
# from environment variables in Posit Connect Cloud.
r2 <- arrow::S3FileSystem$create(
  access_key = Sys.getenv("R2_ACCESS_KEY"),
  secret_key = Sys.getenv("R2_SECRET_KEY"),
  endpoint_override = Sys.getenv("R2_ENDPOINT"),
  scheme = "https"
)
# Helper: build R2 paths rooted at the bucket so call sites stay short.
r2_path <- function(...) {
  r2$path(paste(Sys.getenv("R2_BUCKET"), ..., sep = "/"))
}

ref        <- read_parquet(r2_path("other/ref_suburb_sa4.parquet")) |>
  as_tibble() |>
  mutate(suburb_code_2021 = as.character(suburb_code_2021))
# Shapefiles stay as local-repo reads — they're committed alongside the
# code in data/other/ since they're small and static. Only the larger,
# more volatile parquet data lives on R2.
shp_suburb <- readRDS("data/other/shp_suburb") |> st_transform(4326)
shp_state  <- readRDS("data/other/shp_state")  |> st_transform(4326)
shp_gcc    <- readRDS("data/other/shp_gcc")    |> st_transform(4326)
shp_sa4    <- readRDS("data/other/shp_sa4")    |> st_transform(4326)

# Suburb centroids — used to build "Show on map" links that open
# OpenStreetMap in a new tab. Computed once at startup so the link can be
# assembled inline without server reactivity.
local({
  cc <- shp_suburb |> sf::st_centroid() |> sf::st_coordinates()
  suburb_centroid_lng <<- setNames(cc[, 1], shp_suburb$suburb_code_2021)
  suburb_centroid_lat <<- setNames(cc[, 2], shp_suburb$suburb_code_2021)
})

# Per-suburb categorical labels used for inline filters in the title row.
# Has columns: suburb_code_2021, cat_remote, cat_terrain, cat_coast.
suburb_filters <- read_parquet(r2_path("other/suburb_filters.parquet")) |>
  as_tibble() |>
  mutate(across(c(cat_remote, suburb_code_2021, cat_terrain, cat_coast), as.character)) 

# Semantic orderings for each filter characteristic. Anything in the data that
# doesn't match these orderings is appended at the end so the dropdown
# always reflects the data, never silently empties.
cat_coast_order   <- c("On the coast", "Near the coast", "Short drive",
                       "Day trip", "Inland")
cat_terrain_order <- c("Flat", "Gently undulating", "Rolling/hilly",
                       "Steep/mountainous")
cat_remote_order  <- c("Major city", "Peri-urban", "Regional",
                       "Remote", "Very remote")

ordered_choices <- function(values, semantic_order) {
  values <- unique(values[!is.na(values)])
  matched   <- intersect(semantic_order, values)        # known, in order
  unmatched <- sort(setdiff(values, semantic_order))    # unknown, alpha
  c(matched, unmatched)
}

cat_coast_choices   <- ordered_choices(suburb_filters$cat_coast,   cat_coast_order)
cat_terrain_choices <- ordered_choices(suburb_filters$cat_terrain, cat_terrain_order)
cat_remote_choices  <- ordered_choices(suburb_filters$cat_remote,  cat_remote_order)

# Diagnostic: print what got loaded so it's visible in the R console at startup
cat("Filter choices loaded:\n")
cat("  coast:  ", paste(cat_coast_choices,   collapse = ", "), "\n")
cat("  terrain:", paste(cat_terrain_choices, collapse = ", "), "\n")
cat("  remote: ", paste(cat_remote_choices,  collapse = ", "), "\n")

# Maps each filter to the characteristic code it relates to (for auto-deselect)
filter_to_dim <- c(cat_coast = "coast", cat_terrain = "terrain",
                   cat_remote = "remoteness")

# Suburbs flagged TRUE in `allowed` are the only ones that can be picked,
# ranked, or used as comparison targets. Low-population suburbs missing
# census coverage are flagged FALSE.
allowed_codes_all <- ref |> filter(allowed) |> pull(suburb_code_2021)

state_lookup_full <- setNames(ref$state_name_2021, ref$suburb_code_2021)
gcc_lookup_full   <- setNames(ref$gcc_name_2021,   ref$suburb_code_2021)

# ===== Explanatory data (suburb info panel + list comparison) =============
# Pre-joined wide lookup of all 14 summary themes, keyed by
# suburb_code_2021. Built once offline and saved as a single parquet file —
# faster startup than fourteen separate qreads + full_joins, and ready for
# direct read from object storage (R2 / S3) when the app is deployed there.
suburb_info <- arrow::read_parquet(r2_path("explanatory/suburb_info.parquet")) |>
  as_tibble() |>
  mutate(suburb_code_2021 = as.character(suburb_code_2021))

cat("Suburb info lookup:", nrow(suburb_info), "suburbs,",
    ncol(suburb_info), "columns\n")

# Fast row fetch; returns NULL when the suburb has no explanatory data.
get_info <- function(code) {
  i <- match(code, suburb_info$suburb_code_2021)
  if (is.na(i)) NULL else suburb_info[i, , drop = FALSE]
}

# ===== Lookups ============================================================

suburb_code_to_abbr <- setNames(state_abbr[ref$state_name_2021], ref$suburb_code_2021)
suburb_code_to_name <- setNames(ref$display_name, ref$suburb_code_2021)

# state-grouped suburb picker choices: "Name (STATE)" -> code (allowed only)
ref_choices <- ref |>
  filter(allowed) |>
  mutate(label = display_name,
         value = suburb_code_2021,
         grp   = state_abbr[state_name_2021]) |>
  arrange(grp, label) |>
  split(~ grp) |>
  map(~ setNames(.x$value, .x$label))

region_tree <- ref |>
  distinct(state_name_2021, gcc_name_2021, sa4_name_2021) |>
  arrange(state_name_2021, gcc_name_2021, sa4_name_2021) |>
  transmute(State = state_name_2021, GCC = gcc_name_2021, SA4 = sa4_name_2021) |>
  create_tree(levels = c("State", "GCC", "SA4"))

# ===== Helpers ============================================================

# Load all raw_* for one reference suburb 
load_raw <- function(code) {
  read_parquet(r2_path("similarity/by_source", paste0(code, ".parquet"))) |>
    as_tibble()
}

# Dream-suburb variant: takes a list with elements `people`, `urban`,
# `nature`, `amenities` each holding a vector of reference suburb codes. 
compute_dream_raw <- function(theme_refs) {
  all_refs <- unique(unlist(theme_refs))
  if (!length(all_refs)) return(NULL)
  
  raw_per_ref <- setNames(lapply(all_refs, load_raw), all_refs)
  
  dim_tables <- list()
  for (theme in names(theme_refs)) {
    refs <- theme_refs[[theme]]
    if (!length(refs)) next
    for (d in dream_theme_dims[[theme]]) {
      dim_col <- paste0("raw_", d)
      per_ref <- lapply(refs, function(r) {
        raw_per_ref[[r]] |> select(suburb_b, all_of(dim_col))
      })
      joined <- reduce(per_ref, full_join, by = "suburb_b")
      avg_df <- joined |>
        rowwise() |>
        mutate(!!dim_col := mean(c_across(starts_with(dim_col)), na.rm = TRUE)) |>
        ungroup() |>
        select(suburb_b, all_of(dim_col))
      dim_tables[[d]] <- avg_df
    }
  }
  if (!length(dim_tables)) return(NULL)
  reduce(dim_tables, full_join, by = "suburb_b")
}

# Transform raw scores per dim + target, then aggregate to a match.
# Each `target` is one of the four states: similar / mostly_similar /
# mostly_different / different (target values 1.0, 0.7, 0.3, 0.0).
#
# Per-dim score formulas:
#   similar          : score = x       (identity — match is mean of raw)
#   different        : score = 1 - x   (natural complement)
#   mostly_similar   : score = max(0, 1 - 2 * |x - 0.7|)   (triangular peak)
#   mostly_different : score = max(0, 1 - 2 * |x - 0.3|)   (triangular peak)
# All scores are clamped to [0, 1] defensively.
#
# `focus` is one of: "balanced" (rowMeans across selected dims), or
# "people_focused" / "place_focused" (weighted mean of group means). The
# medians argument is retained for back-compat but no longer used.
compute_match <- function(raw_wide, settings, medians = NULL,
                              focus = "balanced") {
  selected <- names(settings)
  if (length(selected) == 0) return(NULL)

  scored <- raw_wide
  for (d in selected) {
    col <- paste0("raw_", d)
    if (!col %in% names(scored)) next
    target     <- settings[[d]]
    target_val <- target_values[[target]] %||% target_values[["similar"]]
    x <- scored[[col]]
    scored[[paste0("score_", d)]] <- switch(target,
      similar   = pmax(0, pmin(1, x)),
      different = pmax(0, pmin(1, 1 - x)),
      # mostly_similar / mostly_different (or any unexpected target) use
      # the triangular peak formula centred on target_val.
      pmax(0, pmin(1, 1 - 2 * abs(x - target_val)))
    )
  }

  score_cols <- paste0("score_", selected)
  score_cols <- score_cols[score_cols %in% names(scored)]
  if (length(score_cols) == 0) return(NULL)

  if (is.null(focus) || focus == "balanced" || !focus %in% names(focus_weights) ||
      is.null(focus_weights[[focus]])) {
    # Plain mean across all selected dims (unchanged behaviour).
    scored$match <- rowMeans(
      as.matrix(as.data.frame(scored)[, score_cols, drop = FALSE]),
      na.rm = TRUE)
  } else {
    # Group-weighted: split selected dims into people / place groups,
    # take each group's mean, then weight by the preset.
    selected_people <- intersect(selected, people_dims)
    selected_place  <- intersect(selected, place_dims)
    p_cols <- paste0("score_", selected_people)
    q_cols <- paste0("score_", selected_place)
    p_cols <- p_cols[p_cols %in% names(scored)]
    q_cols <- q_cols[q_cols %in% names(scored)]

    score_df <- as.data.frame(scored)
    people_mean <- if (length(p_cols)) {
      rowMeans(as.matrix(score_df[, p_cols, drop = FALSE]), na.rm = TRUE)
    } else NA_real_
    place_mean <- if (length(q_cols)) {
      rowMeans(as.matrix(score_df[, q_cols, drop = FALSE]), na.rm = TRUE)
    } else NA_real_

    w <- focus_weights[[focus]]
    # If only one group has any selected dims, fall back to that group's mean
    if (length(p_cols) == 0) {
      scored$match <- place_mean
    } else if (length(q_cols) == 0) {
      scored$match <- people_mean
    } else {
      scored$match <- w[["people"]] * people_mean + w[["place"]] * place_mean
    }
  }

  scored |> filter(!is.nan(match))
}

# Translate tree labels (any level) into a set of allowed suburb codes.
allowed_codes_from_tree <- function(selected_labels) {
  if (length(selected_labels) == 0) return(allowed_codes_all)
  states <- intersect(selected_labels, unique(ref$state_name_2021))
  gccs   <- intersect(selected_labels, unique(ref$gcc_name_2021))
  sa4s   <- intersect(selected_labels, unique(ref$sa4_name_2021))
  ref |>
    filter(allowed,
           state_name_2021 %in% states |
           gcc_name_2021   %in% gccs   |
           sa4_name_2021   %in% sa4s) |>
    pull(suburb_code_2021)
}

# Intersect the tree-filter codes with the cat_* picker selections.
# Each `selected` argument is either NULL/empty (no filter on that dim) or a
# character vector of category labels — suburbs matching any selected
# category in that dim are kept.
codes_from_cat_filters <- function(tree_codes, coast = NULL,
                                   terrain = NULL, remote = NULL) {
  out <- tree_codes
  apply_filter <- function(codes, col, picks) {
    if (length(picks) == 0) return(codes)
    keep <- suburb_filters |>
      filter(.data[[col]] %in% picks) |>
      pull(suburb_code_2021)
    intersect(codes, keep)
  }
  out <- apply_filter(out, "cat_coast",   coast)
  out <- apply_filter(out, "cat_terrain", terrain)
  out <- apply_filter(out, "cat_remote",  remote)
  out
}

# States visible in the current tree filter (drives the zoom dropdown).
states_in_filter <- function(selected_labels) {
  if (length(selected_labels) == 0) return(names(state_abbr))
  direct  <- intersect(selected_labels, names(state_abbr))
  implied <- ref |>
    filter(gcc_name_2021 %in% selected_labels |
           sa4_name_2021 %in% selected_labels) |>
    pull(state_name_2021) |> unique()
  sort(unique(c(direct, implied)))
}

fmt_pct <- function(pct) {
  if (is.na(pct)) return("")
  if (pct < 0.1)  return("top <0.1%")
  if (pct < 1)    return(sprintf("top %.1f%%", pct))
  sprintf("top %.0f%%", pct)
}

# Build a realestate.com.au "buy" search URL for a given suburb. The site
# accepts a slug like "in-{name},+{state}" without a postcode; ambiguous
# suburb names typically resolve to the largest/most active listing.
build_realestate_url <- function(suburb_name, state_abbr_value) {
  # Strip any parenthetical suffix defensively (most names are already clean)
  name_clean <- sub("\\s*\\([^()]*\\)\\s*$", "", suburb_name)
  # Build the slug with sequential assignments rather than the R 4.2+
  # placeholder-pipe syntax (`x = _`) since the deployment target runs R 4.1.
  slug <- tolower(name_clean)
  slug <- gsub("[^a-z0-9 ]", "", slug)
  slug <- gsub("\\s+",       "+", slug)
  slug <- trimws(slug)
  sprintf("https://www.realestate.com.au/buy/in-%s,+%s/list-1?activeSort=list-date&includeSurrounding=false",
          slug, tolower(state_abbr_value))
}

# Build the "why this match" sentence shown on list cards and map popups.
# Separates exact-match characteristics (>= 0.99) from informative top-3 so the
# user isn't told that two suburbs are "most similar in terms of remoteness"
# when remoteness happens to be a structural near-tie for thousands of
# suburbs. Returns plain text — caller wraps in the appropriate HTML.
format_why_sentence <- function(raw_vals) {
  vals <- unlist(raw_vals)
  if (length(vals) == 0) return("")

  # Comma-list joiner: "a, b and c"; "a and b"; "a"
  join_and <- function(parts) {
    if (length(parts) == 0) return("")
    if (length(parts) == 1) return(parts[1])
    paste(paste(parts[-length(parts)], collapse = ", "), "and",
          parts[length(parts)])
  }

  exact_idx <- which(vals >= 0.99)
  non_exact_idx <- setdiff(seq_along(vals), exact_idx)

  # Top 3 informative dims = highest raw similarities among the non-exact ones
  ord    <- non_exact_idx[order(vals[non_exact_idx], decreasing = TRUE)]
  top3   <- head(ord, 3)
  top_parts <- if (length(top3) > 0) {
    sprintf("%s (%.0f%%)", tolower(dim_labels[names(vals)[top3]]),
            vals[top3] * 100)
  } else character(0)

  if (length(exact_idx) == 0) {
    # No structural matches — original phrasing
    if (length(top_parts) == 0) return("")
    sprintf("Most similar in terms of %s", join_and(top_parts))
  } else {
    exact_names <- tolower(dim_labels[names(vals)[exact_idx]])
    besides_phrase <- if (length(exact_names) > 4) {
      sprintf("%d characteristics matching exactly (%s)",
              length(exact_names), join_and(exact_names))
    } else {
      join_and(exact_names)
    }
    if (length(top_parts) == 0) {
      # Edge case: every characteristic is a structural match
      sprintf("All characteristics match (%s)", join_and(exact_names))
    } else {
      sprintf("Shares an identical profile for %s, and is highly similar in %s",
              besides_phrase, join_and(top_parts))
    }
  }
}

# Per-characteristic contribution bars (showing RAW similarity, not transformed)
# plus the "set as reference" link. Rank info is local + national.
# `winning_ref_label` is shown only when more than one reference is selected
# — it identifies which reference produced this match. Pass NULL for
# single-reference mode (no annotation rendered).
build_popup <- function(name, abbr, match, rank, national_rank, national_pct,
                        raw_vals, code, winning_ref_label = NULL) {
  ord       <- order(unlist(raw_vals), decreasing = TRUE)
  dims_ord  <- names(raw_vals)[ord]
  vals_ord  <- unlist(raw_vals)[ord]
  bars <- paste0(sprintf(
    "<div style='display:flex;align-items:center;margin:1px 0;'>
       <span style='width:96px;font-size:10px;'>%s</span>
       <div style='flex:1;background:#eee;height:8px;border-radius:2px;min-width:60px;'>
         <div style='background:%s;height:8px;width:%.0f%%;border-radius:2px;'></div></div>
       <span style='width:32px;text-align:right;font-size:10px;'>%s</span></div>",
    dim_labels[dims_ord], dim_colors[dims_ord], vals_ord * 100, scales::percent(vals_ord, accuracy = 1)),
    collapse = "")

  # "Why this match" — separates structurally-similar dims (>=99%) from
  # the informative top-3 so users don't see "most similar in terms of
  # remoteness" when remoteness is exact for many suburbs.
  why_text <- format_why_sentence(raw_vals)
  why_html <- if (nchar(why_text) > 0) sprintf(
    "<div style='font-size:11px;color:#555;font-style:italic;margin:4px 0 2px 0;'>%s</div>",
    why_text) else ""

  winning_html <- if (!is.null(winning_ref_label)) {
    sprintf("<div style='color:#1f77b4;font-size:11px;margin-top:2px;'>most similar to %s</div>",
            winning_ref_label)
  } else ""
  rea_url <- build_realestate_url(name, abbr)
  sprintf(
    "<b>%s, %s</b><br>Match %s &middot; rank %d here
     <div style='color:#555;font-size:11px;margin-top:2px;'>rank %d nationally &middot; %s</div>
     %s
     %s
     <hr style='margin:4px 0;'>%s
     <div style='margin-top:6px;'>
       <a href='#' onclick=\"Shiny.setInputValue(
         'set_ref','%s',{priority:'event'});return false;\">&#8594; Set as reference</a>
       &nbsp;&middot;&nbsp;
       <a href='#' onclick=\"Shiny.setInputValue(
         'show_info','%s',{priority:'event'});return false;\">&#8594; About</a>
       &nbsp;&middot;&nbsp;
       <a href='%s' target='_blank' rel='noopener noreferrer'>&#8594; View homes for sale</a>
     </div>",
    name, abbr, scales::percent(match, .1), rank, national_rank, fmt_pct(national_pct),
    winning_html, why_html, bars, code, code, rea_url)
}

# Popup for clicked suburbs not in current map results (but allowed).
build_popup_unranked <- function(name, abbr, code) {
  rea_url <- build_realestate_url(name, abbr)
  sprintf(
    "<b>%s, %s</b><br><i>Very low or no match score</i>
     <div style='margin-top:6px;'>
       <a href='#' onclick=\"Shiny.setInputValue(
         'set_ref','%s',{priority:'event'});return false;\">&#8594; Set as reference</a>
       &nbsp;&middot;&nbsp;
       <a href='%s' target='_blank' rel='noopener noreferrer'>&#8594; View homes for sale</a>
     </div>",
    name, abbr, code, rea_url)
}

# Popup for low-population suburbs that can't be ranked at all.
build_popup_low_pop <- function(name, abbr) sprintf(
  "<b>%s, %s</b><br><i>Not enough data to compare this suburb.</i>",
  name, abbr)

# ===== Suburb info panel ==================================================

# NA-safe formatters for the info panel
fmt_na   <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "\u2014" else as.character(x)
fmt_num  <- function(x, digits = 0, suffix = "") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
  paste0(formatC(round(x, digits), format = "f", digits = digits,
                 big.mark = ","), suffix)
}
fmt_dollar <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
  paste0("$", formatC(round(x), format = "d", big.mark = ","))
}

# One labelled fact line inside a group
info_fact <- function(label, value) {
  tags$div(style = "display:flex; justify-content:space-between; gap:8px;
                    font-size:12px; padding:1px 0;",
    tags$span(style = "color:#777;", label),
    tags$span(style = "text-align:right; font-weight:500;", value))
}

# A titled group of facts (renders as a card in the panel grid)
info_group <- function(title, ..., note = NULL) {
  tags$div(style = "flex: 1 1 150px; min-width: 140px; max-width: 240px;
                    border: 1px solid #e8e8e8; border-radius: 6px;
                    padding: 6px 8px; background: #fcfcfc;",
    tags$div(style = "font-size:10px; font-weight:600; color:#999;
                      text-transform:uppercase; letter-spacing:0.5px;
                      margin-bottom:3px;", title),
    ...,
    if (!is.null(note))
      tags$div(style = "font-size:10px; color:#aaa; margin-top:4px;
                        font-style:italic;", note))
}

# Full info panel for one suburb. When `show_header = FALSE` the suburb
# name / hint is omitted (e.g. used inline in list cards where the card
# header already carries that information).
# `groups` defaults to all 9. Pass a subset (e.g. c("Place","People",
# "Housing","Work")) to render a slimmer panel suitable for inline use in
# list cards.
build_info_panel <- function(code, ref_code = NULL, show_header = TRUE,
                              groups = c("Place", "Climate", "Landscape",
                                         "People", "Diversity", "Housing",
                                         "Socioeconomic", "Work", "Voting")) {
  d <- get_info(code)
  name <- suburb_code_to_name[[code]] %||% code
  abbr <- suburb_code_to_abbr[[code]] %||% ""

  if (is.null(d)) {
    return(tags$div(style = "padding: 12px; color: #888; font-size: 13px;",
      sprintf("No detailed information available for %s, %s.", name, abbr)))
  }

  is_ref <- !is.null(ref_code) && length(ref_code) > 0 && code == ref_code[1]

  header <- if (show_header) {
    tags$div(style = "display:flex; align-items:baseline; gap:10px;
                      flex-wrap:wrap; margin-bottom:8px;",
      tags$span(style = "font-size:16px; font-weight:bold;",
                sprintf("%s, %s", name, abbr)),
      tags$span(style = "font-size:11px; color:#999;",
                if (is_ref) "Current reference suburb"
                else "click any suburb on the map to update"))
  } else NULL

  # Build groups as a named list, then keep only those requested. This keeps
  # the source of truth (what each group contains) in one place while letting
  # callers choose which ones to render.
  all_groups <- list(
    Place = info_group("Place",
      info_fact("Remoteness",  fmt_na(d$remoteness)),
      info_fact("Density",     fmt_num(d$density, 0, " /km\u00b2")),
      info_fact("Coast",       fmt_na(d$coast_class)),
      info_fact("To coast",    fmt_num(d$dist_to_coast_km, 0, " km")),
      info_fact("Terrain",     fmt_na(d$dominant_slope_class)),
      info_fact("Elevation",   fmt_na(d$dominant_elev_class))),

    Climate = info_group("Climate",
      info_fact("Avg max temp",  fmt_num(d$tmax_annual, 1, "\u00b0C")),
      info_fact("Avg min temp",  fmt_num(d$tmin_annual, 1, "\u00b0C")),
      info_fact("Avg monthly rain", fmt_num(d$annual_rain, 0, " mm")),
      info_fact("Wettest month", fmt_na(d$peak_rain_month)),
      info_fact("Driest month",  fmt_na(d$driest_month))),

    Landscape = info_group("Landscape",
      info_fact("Setting",        fmt_na(d$landscape_type)),
      info_fact("Natural veg",    fmt_num(d$pct_natural_veg_landcover, 0, "%")),
      info_fact("Dominant veg",   fmt_na(d$dominant_vegetation)),
      info_fact("Water",          fmt_na(d$water_character))),

    People = info_group("People",
      info_fact("Population",  fmt_num(d$population, 0)),
      info_fact("Median age",  fmt_num(d$median_age, 0)),
      info_fact("Age profile", fmt_na(d$age_archetype)),
      info_fact("Households",  fmt_na(d$household_archetype))),

    Diversity = info_group("Diversity",
      info_fact("Birthplace",   fmt_na(d$diversity_archetype)),
      info_fact("Top region",   sprintf("%s (%s)",
                  fmt_na(d$dominant_region),
                  fmt_num(d$pct_dominant_region, 0, "%"))),
      info_fact("Indigenous",   fmt_num(d$pct_indigenous, 1, "%")),
      info_fact("Religion",     fmt_na(d$religion_archetype))),

    Housing = info_group("Housing",
      info_fact("Tenure",          fmt_na(d$tenure_archetype)),
      info_fact("Dwellings",       fmt_na(d$dwelling_archetype)),
      info_fact("Typical size",    fmt_na(d$dominant_bedrooms)),
      info_fact("Median mortgage", paste0(fmt_dollar(d$median_mortgage_monthly), "/mo")),
      info_fact("Median rent",     paste0(fmt_dollar(d$median_rent_weekly), "/wk"))),

    Socioeconomic = info_group("Socioeconomic",
      info_fact("Income",      fmt_na(d$income_archetype)),
      info_fact("Education",   fmt_na(d$education_archetype)),
      info_fact("Labour force", fmt_na(d$lfs_archetype))),

    Work = info_group("Work",
      info_fact("Economy",     fmt_na(d$economy_type)),
      info_fact("Workforce",   fmt_na(d$workforce_character)),
      tags$div(style = "font-size:11px; color:#777; margin-top:3px;",
               tags$span(style = "color:#999;", "Top industries: "),
               fmt_na(d$top3_industries))),

    Voting = info_group("Voting",
      info_fact("Lean",        fmt_na(d$lean_category)),
      info_fact("Leading party '25", fmt_na(d$dominant_party_2025)),
      note = "Booth-level where available, otherwise electorate-level")
  )

  chosen <- all_groups[intersect(groups, names(all_groups))]

  tags$div(
    style = if (show_header) {
      "border: 1px solid #ddd; border-radius: 8px; padding: 12px 14px; background: white;"
    } else "",
    header,
    tags$div(style = "display:flex; flex-wrap:wrap; gap:8px;", chosen)
  )
}

# Compact comparison strip for list cards: the match's headline archetypes,
# each ticked green when identical to the reference suburb's.
comparison_fields <- c(
  "Age"     = "age_archetype",
  "Income"  = "income_archetype",
  "Housing" = "tenure_archetype",
  "Economy" = "economy_type"
)

build_comparison_strip <- function(ref_info, match_info) {
  if (is.null(ref_info) || is.null(match_info)) return(NULL)
  cells <- lapply(names(comparison_fields), function(lbl) {
    col <- comparison_fields[[lbl]]
    rv  <- ref_info[[col]];  mv <- match_info[[col]]
    if (is.null(mv) || is.na(mv)) return(NULL)
    same <- !is.null(rv) && !is.na(rv) && identical(rv, mv)
    tags$span(
      style = sprintf(
        "font-size:11px; padding:2px 8px; border-radius:10px; border:1px solid %s;
         background:%s; color:%s; white-space:nowrap;",
        if (same) "#a5d6a7" else "#e0e0e0",
        if (same) "#f1f8e9" else "#fafafa",
        if (same) "#33691e" else "#666"),
      title = if (same) sprintf("%s: same as reference (%s)", lbl, mv)
              else      sprintf("%s: %s (reference: %s)", lbl, mv,
                                if (is.null(rv) || is.na(rv)) "\u2014" else rv),
      paste0(lbl, ": ", mv, if (same) " \u2713" else ""))
  })
  cells <- cells[!vapply(cells, is.null, logical(1))]
  if (!length(cells)) return(NULL)
  tags$div(style = "display:flex; flex-wrap:wrap; gap:4px; margin:2px 0 8px 0;",
           cells)
}

# ===== UI =================================================================

ui <- function(request) fluidPage(
  theme = bs_theme(version = 5,
                   primary = "#2c7fb8",
                   base_font = font_google("Inter")),
  tags$head(tags$style(HTML("
  /* Compact the inline numericInput */
  .inline-topn .form-group { margin-bottom: 0; }
  .inline-topn input.form-control { padding: 4px 6px; height: 34px; }
  body {
  background-color: #ffffff;
}
  /* Common 34px row height */
  .row-align { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 6px 0 14px; }
  .row-align > * { vertical-align: middle; }

  /* virtualSelect picker — three things need fixing:
     1. Kill the form-group's bottom margin so vertical centering works
     2. Force the visible button to 34px and lay it out as a flex row so
        its inner content (placeholder text and selected-suburb tags) is
        centered, not top-aligned.
     3. Bring the tags' own padding down a touch so a tag-filled picker
        doesn't push the row taller. */
  .row-align .form-group { margin-bottom: 0; }
  .row-align .vscomp-ele { margin-bottom: 0; }
  .row-align .vscomp-wrapper {
    min-height: 34px;
    display: flex;
    align-items: center;
  }
  .row-align .vscomp-toggle-button {
    min-height: 34px;
    display: flex;
    align-items: center;
    padding: 0 8px;
  }
  .row-align .vscomp-value {
    line-height: 1.3;
    padding-top: 0;
    padding-bottom: 0;
  }
  .row-align .vscomp-value-tag {
    margin-top: 1px;
    margin-bottom: 1px;
  }
  
  /* Regions and Filters dropdownButtons — match the 34px row height.
   These render as plain Bootstrap btn-default action-button dropdown-toggle. */
.row-align .btn.dropdown-toggle,
.row-align .btn.action-button {
  height: 34px;
  min-height: 34px;
  padding: 4px 12px;
  line-height: 1.2;
  display: inline-flex;
  align-items: center;
  gap: 6px;
}
.row-align .btn.dropdown-toggle .caret,
.row-align .btn.action-button .caret {
  margin-left: 4px;
")),
  tags$script(HTML("
    // Update the Filters dropdown button label with the active count
    Shiny.addCustomMessageHandler('update_filters_label', function(n) {
      var btn = $('button[data-toggle=\"dropdown\"]').filter(function(){
        return $(this).text().trim().indexOf('Filters') === 0;
      });
      if (btn.length) {
        var baseHtml = btn.data('base-html');
        if (!baseHtml) {
          baseHtml = btn.html();
          btn.data('base-html', baseHtml);
        }
        if (n > 0) {
          btn.html(baseHtml.replace(/Filters( \\(\\d+\\))?/, 'Filters (' + n + ')'));
        } else {
          btn.html(baseHtml);
        }
      }
    });
    // When the user dismisses the popup via the leaflet X close button,
    // notify the server so its popup-state tracker stays accurate (the
    // next basemap click should then open a fresh popup rather than being
    // eaten as 'close the open one').
    $(document).on('click', '.leaflet-popup-close-button', function() {
      Shiny.setInputValue('__popup_x_close', Math.random(), {priority:'event'});
    });
  "))
  ),
  # --- Header Layout Block ---
  # Row 1: title centred + buttons right-aligned, on the same row.
  # 3-column flexbox keeps the title in true centre regardless of how wide
  # the buttons cluster ends up being.
  fluidRow(
    column(width = 12,
      tags$div(
        style = "display: flex; align-items: center; margin-top: 10px;
                 margin-bottom: 6px;",
        # Left spacer to balance the right cluster's width
        tags$div(style = "flex: 1;"),
        # Centred title
        tags$h3(
          "Suburb similarity explorer",
          style = "margin: 0; font-size: 13px; font-weight: 600; color: #666;
                   text-transform: uppercase; letter-spacing: 0.5px;
                   text-align: center;"
        ),
        # Right-aligned button cluster
        tags$div(
          style = "flex: 1; display: flex; justify-content: flex-end;
                   align-items: center;",
          actionButton("open_methodology", label = NULL,
                       icon = icon("circle-info"),
                       class = "btn-outline-secondary",
                       style = "margin-right: 4px;",
                       title = "How does this work?"),
          actionButton("open_dream", label = NULL,
                       icon = icon("wand-magic-sparkles"),
                       class = "btn-outline-secondary",
                       style = "margin-right: 4px;",
                       title = "Build your dream suburb"),
          downloadButton("export_csv", label = NULL,
                         icon = icon("file-arrow-down"),
                         class = "btn-outline-secondary",
                         style = "margin-right: 4px;",
                         title = "Export results as CSV"),
          bookmarkButton(label = NULL,
                         icon  = icon("link"),
                         style = "margin-right: 4px;",
                         title = "Share current dashboard view"),
          uiOutput("settings_btn", inline = TRUE)
        )
      )
    )
  ),

  # Row 2: centred input controls + right-aligned Map/List toggle.
  # Same 3-column flexbox trick — selectors sit in the true centre.
  fluidRow(
    column(width = 12,
      tags$div(
        style = "display: flex; align-items: flex-start; margin: 6px 0 4px;",
        # Left spacer
        tags$div(style = "flex: 1;"),
        # Centred selectors (existing .row-align block)
        div(class = "row-align",
            style = "justify-content: center; margin: 0;",
            tags$span("Show ", style = "font-size:14px;"),
          div(class = "inline-topn", style = "width:60px;",
              numericInput("top_n_input_inline", NULL,
                           value = 20, min = 1, step = 1, width = "100%")),
          tags$span("suburbs similar to", style = "font-size:14px;"),
          div(style = "min-width:240px; flex:0 1 auto; max-width:460px;",
              virtualSelectInput("ref_sub", NULL,
                                 choices = ref_choices, selected = default_ref_code,
                                 multiple = TRUE, width = "100%", search = TRUE,
                                 placeholder = "Pick one or more suburbs…", optionsCount = 8,
                                 showValueAsTags = TRUE, autoSelectFirstOption = FALSE,
                                 keepAlwaysOpen = FALSE)),
          conditionalPanel(
            condition = "input.ref_sub && input.ref_sub.length > 1",
            div(style = "display:flex; align-items:center; gap:4px;",
                tags$span("matching", style = "font-size:13px; color:#555;"),
                radioButtons("multi_mode", NULL,
                             choices  = c("Closest to any individual reference" = "max",
                                          "Closest to all references on average" = "mean"),
                             selected = "max", inline = TRUE),
                tags$span(
                  icon("circle-info"),
                  style = "cursor: help; color: #888; font-size: 13px;",
                  title = paste(
                    "Match any: find suburbs strongly similar to any one of your references",
                    "(best individual fit).",
                    "Balanced across all: find suburbs similar across all your references",
                    "(mean across all)."
                  )))),
          tags$span("in", style = "font-size:14px;"),
          dropdownButton(
            inputId = "region_dd",
            label   = "Regions",
            icon    = icon("globe"),
            status  = "default",
            width   = "340px",
            circle  = FALSE,
            div(style = "max-height: 380px; overflow-y: auto; padding: 4px;",
                treeInput("region_pick", label = NULL, choices = region_tree,
                          selected = default_tree_selection,
                          returnValue = "text", closeDepth = 0))
          ),
          dropdownButton(
            inputId = "filters_dd",
            label   = "Filters",
            icon    = icon("filter"),
            status  = "default",
            width   = "300px",
            right   = TRUE,
            circle  = FALSE,
            div(style = "padding: 6px;",
                tags$div(style = "font-size: 11px; color: #666; margin-bottom: 8px;",
                         tags$div(tags$b("How filters work:")),
                         tags$div("Selecting a category restricts matches to that specific group and automatically removes ",
                                  "that characteristic from the match score. For example, filtering to coastal ",
                                  "suburbs means the remaining 13 characteristics will determine the final similarity rank. ",
                                  "Leave a filter blank to score across all characteristics"),
                         tags$div(style = "margin-top: 4px;",
                                  "Leave a filter empty (",
                                  tags$em("any"), ") to keep its characteristic in the match.")),
                pickerInput("cat_coast_pick", "Coast",
                            choices  = cat_coast_choices, selected = character(0),
                            multiple = TRUE, width = "100%",
                            options  = pickerOptions(actionsBox = TRUE,
                                                     selectedTextFormat = "count > 2",
                                                     countSelectedText  = "{0} selected",
                                                     noneSelectedText   = "any")),
                pickerInput("cat_terrain_pick", "Terrain",
                            choices  = cat_terrain_choices, selected = character(0),
                            multiple = TRUE, width = "100%",
                            options  = pickerOptions(actionsBox = TRUE,
                                                     selectedTextFormat = "count > 2",
                                                     countSelectedText  = "{0} selected",
                                                     noneSelectedText   = "any")),
                pickerInput("cat_remote_pick", "Remoteness",
                            choices  = cat_remote_choices, selected = character(0),
                            multiple = TRUE, width = "100%",
                            options  = pickerOptions(actionsBox = TRUE,
                                                     selectedTextFormat = "count > 2",
                                                     countSelectedText  = "{0} selected",
                                                     noneSelectedText   = "any"))
            )
          ),
          tags$span(textOutput("region_summary", inline = TRUE),
                    style = "font-size:13px; color:#444; margin-left:4px;")
        ),
        # Right-aligned Map/List toggle
        tags$div(
          style = "flex: 1; display: flex; justify-content: flex-end;
                   align-items: center;",
          radioGroupButtons("view_mode", label = NULL,
                            choices = c("Map" = "map", "List" = "list"),
                            selected = "map", size = "sm",
                            individual = FALSE)
        )
      )
    )
  ),

  # Click-to-set-reference hint, centred just under the inputs
  tags$div(style = "text-align: center; font-size: 11px; color: #666;
                    margin: 0 0 8px 0;",
    icon("circle-info"),
    " Click any suburb on the map to set it as the reference."),
  # View mode toggle now lives in the inputs row above. Banner here only
  # when dream-suburb mode is active, sitting above the main view.
  uiOutput("dream_banner"),

  # "What's this suburb like" overview — one bullet per active characteristic
  # describing how close the top matches are. Auto-hidden when no results.
  uiOutput("similarity_overview"),

  conditionalPanel(condition = "input.view_mode == 'map'",
    div(style = "display: flex; gap: 12px; align-items: stretch;",
      # ----- Map (flexes to fill remaining width) -----
      div(style = "flex: 1 1 auto; min-width: 0; position: relative;",
        withSpinner(leafletOutput("map", height = 680),
                    type = 4, color = "#2c7fb8", size = 0.8,
                    caption = "Finding matches"),
        # bottom-left floating panel: top-10 contribution bars
        absolutePanel(
          bottom = 24, left = 12, width = 380, draggable = TRUE,
          style = paste("background: rgba(255,255,255,0.92); padding: 8px;",
                        "border-radius: 6px; box-shadow: 0 1px 4px rgba(0,0,0,0.3); z-index: 1000;",
                        "overflow: visible;"),
          uiOutput("top10"))
      ),
      # ----- Side panel: suburb info, fixed 360px width -----
      div(style = "flex: 0 0 360px; max-height: 680px; overflow-y: auto;
                   border: 1px solid #e0e0e0; border-radius: 6px;
                   padding: 8px 10px; background: white;",
        uiOutput("map_side_panel"))
    )
  ),

  conditionalPanel(condition = "input.view_mode == 'list'",
    div(style = "max-height: 720px; overflow-y: auto; padding: 4px 8px;
                 border: 1px solid #eee; border-radius: 4px;",
      withSpinner(uiOutput("list_view"),
                  type = 4, color = "#2c7fb8", size = 0.8,
                  caption = "Finding matches"))
  )
)

# ===== Server =============================================================

server <- function(input, output, session) {
  # add a cache
  raw_cache <- new.env(parent = emptyenv())
  
  # Active state — commits via Apply (modal) or picker/map-click (ref).
  current_ref     <- reactiveVal(default_ref_code)
  settings_active <- reactiveVal(default_settings)
  focus_active    <- reactiveVal("balanced")   # preset focus weighting
  top_n_active    <- reactiveVal(50)
  region_filter   <- reactiveVal(default_tree_selection)

  # Suburb whose facts are shown in the info panel (NULL = panel hidden)
  clicked_suburb  <- reactiveVal(NULL)

  # Categorical filters (NULL/character(0) = no filter on that dim)
  cat_coast_filter   <- reactiveVal(character(0))
  cat_terrain_filter <- reactiveVal(character(0))
  cat_remote_filter  <- reactiveVal(character(0))

  # Dream-suburb mode. Holds a list:
  #   theme_refs   = list(people = chr[], urban = chr[], nature = chr[],
  #                       amenities = chr[])
  #   filter_state = "" or a state name
  #   filter_coast = TRUE / FALSE
  # NULL when not active (default single-reference mode).
  dream_refs_active <- reactiveVal(NULL)

  # Update the Filters button label whenever any cat filter changes
  observe({
    n <- sum(
      length(cat_coast_filter())   > 0,
      length(cat_terrain_filter()) > 0,
      length(cat_remote_filter())  > 0
    )
    session$sendCustomMessage("update_filters_label", n)
  })

  # --- CSV export --------------------------------------------------------
  # Produces a CSV of the current top-N results with per-characteristic raw
  # similarity, the match, and the suburb summary facts for each match.
  # One row per (reference, match) pair so the multi-reference case is
  # represented explicitly. The first rows are the reference suburb(s)
  # themselves (with match = 1 against themselves). Active settings are
  # appended as commented footer lines so the CSV carries its own provenance.
  output$export_csv <- downloadHandler(
    filename = function() {
      codes <- current_ref()
      slugs <- vapply(codes, function(code) {
        nm <- unname(suburb_code_to_name[code]) %||% code
        # Strip the state-abbr suffix shown in display names like "Richmond (Vic.)"
        nm <- sub("\\s*\\([^)]*\\)\\s*$", "", nm)
        # Lower-case, drop apostrophes, replace non-alnum runs with nothing
        nm <- tolower(nm)
        nm <- gsub("['\u2019]", "",  nm)
        nm <- gsub("[^a-z0-9]+", "", nm)
        nm
      }, character(1))
      slugs <- slugs[nzchar(slugs)]
      if (!length(slugs)) slugs <- "results"
      paste0(paste(slugs, collapse = "_"), ".csv")
    },
    content = function(file) {
      cd <- match_df()
      if (is.null(cd) || nrow(cd) == 0) {
        writeLines("# No results to export.", file); return()
      }
      n_top <- top_n_active()
      ref_codes <- current_ref()
      raw_cols  <- grep("^raw_", names(cd), value = TRUE)
      raw_clean <- sub("^raw_", "", raw_cols)

      ref_name <- function(code) unname(suburb_code_to_name[code]) %||% code
      ref_abbr <- function(code) unname(suburb_code_to_abbr[code]) %||% ""
      label    <- function(code) sprintf("%s, %s", ref_name(code), ref_abbr(code))

      # Result rows: top-N matches in rank order. winning_ref tells us which
      # reference produced the surfaced match (for single-ref runs it's
      # the only ref; for multi-ref MAX it's the best-fit ref; for multi-ref
      # MEAN it's the ref with the strongest individual fit).
      top <- head(cd, n_top) |>
        mutate(
          `reference suburb`       = vapply(winning_ref, label, character(1)),
          `most similar suburb`    = vapply(suburb_b,    label, character(1)),
          most_similar_suburb_code = suburb_b,
          match                = round(match, 4)
        ) |>
        mutate(across(all_of(raw_cols), \(x) round(x, 4))) |>
        rename_with(\(nm) paste0("sim_", sub("^raw_", "", nm)),
                    all_of(raw_cols)) |>
        select(`reference suburb`, `most similar suburb`,
               most_similar_suburb_code, rank, match,
               all_of(paste0("sim_", raw_clean)))

      # Reference rows: one per reference suburb, against itself.
      ref_rows <- tibble(
        `reference suburb`       = vapply(ref_codes, label, character(1)),
        `most similar suburb`    = vapply(ref_codes, label, character(1)),
        most_similar_suburb_code = as.character(ref_codes),
        rank                     = 0L,
        match                = 1.0
      )
      for (c in paste0("sim_", raw_clean)) ref_rows[[c]] <- 1.0

      out <- bind_rows(ref_rows, top)

      # Enrich with summary info + REA URL (keyed off the most-similar code)
      info_cols_to_keep <- intersect(names(suburb_info), c(
        "remoteness", "density", "coast_class", "dist_to_coast_km",
        "dominant_slope_class", "dominant_elev_class",
        "tmax_annual", "tmin_annual", "annual_rain",
        "peak_rain_month", "driest_month",
        "landscape_type", "pct_natural_veg_landcover",
        "dominant_vegetation", "water_character",
        "population", "median_age", "age_archetype", "household_archetype",
        "diversity_archetype", "dominant_region", "pct_dominant_region",
        "pct_indigenous", "religion_archetype",
        "tenure_archetype", "dwelling_archetype", "dominant_bedrooms",
        "median_mortgage_monthly", "median_rent_weekly",
        "income_archetype", "education_archetype", "lfs_archetype",
        "economy_type", "workforce_character", "top3_industries",
        "lean_category", "dominant_party_2025"
      ))
      info_slice <- suburb_info |>
        select(suburb_code_2021, all_of(info_cols_to_keep))

      out <- out |>
        left_join(info_slice,
                  by = c("most_similar_suburb_code" = "suburb_code_2021")) |>
        mutate(
          state          = unname(state_lookup_full[most_similar_suburb_code]),
          realestate_url = mapply(
            build_realestate_url,
            unname(suburb_code_to_name[most_similar_suburb_code]),
            unname(suburb_code_to_abbr[most_similar_suburb_code]),
            USE.NAMES = FALSE),
          .after = most_similar_suburb_code
        )

      # Settings footer — written as commented lines so the CSV opens cleanly
      # in Excel/R/Python and the provenance survives editing.
      setts <- settings_active()
      foc   <- focus_active()
      mode  <- input$multi_mode %||% "max"

      settings_lines <- c(
        "",
        sprintf("# Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        sprintf("# Reference suburb(s): %s",
                paste(vapply(ref_codes, label, character(1)), collapse = "; ")),
        sprintf("# Top N requested: %d", n_top),
        sprintf("# Focus preset: %s", focus_labels[[foc]] %||% foc),
        if (length(ref_codes) > 1)
          sprintf("# Multi-reference aggregation: %s",
                  if (mode == "max") "match any (best fit)"
                  else "balanced across all (mean fit)") else NULL,
        "# Per-characteristic target settings:",
        paste0("#   ", names(setts), " = ",
               unname(target_labels[unlist(setts)] %||% unlist(setts))),
        "# Region filter:",
        sprintf("#   %s",
                if (length(region_filter()) == 0) "all states"
                else paste(region_filter(), collapse = "; ")),
        if (length(cat_coast_filter()))
          sprintf("# Coast filter: %s",   paste(cat_coast_filter(),   collapse = "; ")) else NULL,
        if (length(cat_terrain_filter()))
          sprintf("# Terrain filter: %s", paste(cat_terrain_filter(), collapse = "; ")) else NULL,
        if (length(cat_remote_filter()))
          sprintf("# Remoteness filter: %s", paste(cat_remote_filter(), collapse = "; ")) else NULL
      )
      settings_lines <- Filter(Negate(is.null), settings_lines)

      write.csv(out, file, row.names = FALSE, na = "")
      cat(paste(settings_lines, collapse = "\n"), file = file, append = TRUE)
    }
  )

  # --- Bookmarking (URL state for shareable links) -----------------------
  # Capture our reactiveVals into the bookmark state, then restore them when
  # the app loads from a bookmarked URL. Excludes noisy/ephemeral inputs.
  setBookmarkExclude(c(
    "map_click", "map_marker_click", "map_shape_click",
    "map_bounds", "map_center", "map_zoom_event",
    "set_ref", "open_settings", "apply_settings", "reset_settings",
    "open_methodology",
    "clear_filters_empty", "show_info",
    "region_dd", "region_dd_state", "filters_dd"
  ))

  onBookmark(function(state) {
    state$values$current_ref        <- current_ref()
    state$values$settings_active    <- settings_active()
    state$values$focus_active       <- focus_active()
    state$values$top_n_active       <- top_n_active()
    state$values$region_filter      <- region_filter()
    state$values$cat_coast_filter   <- cat_coast_filter()
    state$values$cat_terrain_filter <- cat_terrain_filter()
    state$values$cat_remote_filter  <- cat_remote_filter()
  })

  onRestore(function(state) {
    if (!is.null(state$values$current_ref))
      current_ref(state$values$current_ref)
    if (!is.null(state$values$settings_active))
      settings_active(state$values$settings_active)
    if (!is.null(state$values$focus_active))
      focus_active(state$values$focus_active)
    if (!is.null(state$values$top_n_active))
      top_n_active(state$values$top_n_active)
    if (!is.null(state$values$region_filter))
      region_filter(state$values$region_filter)
    if (!is.null(state$values$cat_coast_filter))
      cat_coast_filter(state$values$cat_coast_filter)
    if (!is.null(state$values$cat_terrain_filter))
      cat_terrain_filter(state$values$cat_terrain_filter)
    if (!is.null(state$values$cat_remote_filter))
      cat_remote_filter(state$values$cat_remote_filter)
  })

  onRestored(function(state) {
    # After UI is rebuilt, sync inputs back to match the restored state
    # (some widgets need an explicit update to display the right value).
    updateVirtualSelect(session, "ref_sub", selected = current_ref())
    updateNumericInput(session, "top_n_input_inline", value = top_n_active())
    updatePickerInput(session, "cat_coast_pick",   selected = cat_coast_filter())
    updatePickerInput(session, "cat_terrain_pick", selected = cat_terrain_filter())
    updatePickerInput(session, "cat_remote_pick",  selected = cat_remote_filter())
    updateTreeInput(inputId = "region_pick", selected = region_filter())
  })

  # Inline Top-N input -> top_n_active (debounced 600ms to avoid thrashing
  # the match_df reactive while the user is still typing/clicking arrows).
  top_n_debounced <- reactive(input$top_n_input_inline) |>
    debounce(600)
  observe({
    v <- suppressWarnings(as.integer(top_n_debounced()))
    if (!is.na(v) && v > 0) top_n_active(v)
  })

  # Picker -> current_ref (now a vector, multi-select)
  observeEvent(input$ref_sub, {
    codes <- input$ref_sub
    codes <- codes[!is.na(codes) & nzchar(codes)]
    if (length(codes) > 0 && !identical(sort(codes), sort(current_ref()))) {
      current_ref(codes)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Inline region picker -> region_filter (updates live, not via modal)
  observeEvent(input$region_pick, {
    region_filter(input$region_pick %||% character(0))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Categorical-filter pickers -> their reactiveVals.
  # When a filter becomes non-empty, also drop the corresponding characteristic
  # from settings_active so the match doesn't double-count it.
  # (One-way: clearing the filter does NOT auto-restore the characteristic —
  # the user can re-add it via the settings modal.)
  apply_cat_filter <- function(picks, dim_code, filter_rv) {
    picks <- picks %||% character(0)
    filter_rv(picks)
    if (length(picks) > 0) {
      setts <- settings_active()
      if (dim_code %in% names(setts)) {
        # Works whether setts is a named character vector OR a list
        setts <- setts[names(setts) != dim_code]
        settings_active(setts)
      }
    }
  }

  observeEvent(input$cat_coast_pick, {
    apply_cat_filter(input$cat_coast_pick, "coast", cat_coast_filter)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$cat_terrain_pick, {
    apply_cat_filter(input$cat_terrain_pick, "terrain", cat_terrain_filter)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$cat_remote_pick, {
    apply_cat_filter(input$cat_remote_pick, "remoteness", cat_remote_filter)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # "Clear all filters" button (shown in the empty-state card on the list view)
  observeEvent(input$clear_filters_empty, {
    updatePickerInput(session, "cat_coast_pick",   selected = character(0))
    updatePickerInput(session, "cat_terrain_pick", selected = character(0))
    updatePickerInput(session, "cat_remote_pick",  selected = character(0))
    cat_coast_filter(character(0))
    cat_terrain_filter(character(0))
    cat_remote_filter(character(0))
  })

  # Compact summary text shown next to the Regions button, including any
  # active categorical filters.
  output$region_summary <- renderText({
    sel <- region_filter()
    # When nothing is restricted, hide the qualifier entirely — no need to
    # tell the user "(all of Australia)" since that's the default.
    region_text <- if (length(sel) == 0) {
      ""
    } else if (length(sel) <= 3) {
      paste0("(", paste(sel, collapse = ", "), ")")
    } else {
      sprintf("(%d regions)", length(sel))
    }

    filt_parts <- c()
    if (length(cat_coast_filter()) > 0)
      filt_parts <- c(filt_parts, paste(cat_coast_filter(), collapse = "/"))
    if (length(cat_terrain_filter()) > 0)
      filt_parts <- c(filt_parts, paste(cat_terrain_filter(), collapse = "/"))
    if (length(cat_remote_filter()) > 0)
      filt_parts <- c(filt_parts, paste(cat_remote_filter(), collapse = "/"))

    if (length(filt_parts) == 0) {
      region_text
    } else if (nzchar(region_text)) {
      paste0(region_text, " · ", paste(filt_parts, collapse = "; "))
    } else {
      paste0("(", paste(filt_parts, collapse = "; "), ")")
    }
  })

  # --- Raw scores (parquet read; refreshes when ref changes) -------------
  # cached version
  # With multi-ref support, returns a NAMED LIST of wide tables: one per
  # reference suburb. Each table has raw_* columns for every characteristic.
  raw_scores <- reactive({
    codes <- current_ref(); req(length(codes) > 0)
    setNames(lapply(codes, function(code) {
      cached <- raw_cache[[code]]
      if (is.null(cached)) {
        cached <- load_raw(code)
        raw_cache[[code]] <- cached
      }
      cached
    }), codes)
  })
  
  # Per-reference medians: a named list keyed by reference code, each holding
  # a named vector of per-dim medians for that reference's raw_score table.
  # Used as the anchor for "Somewhat different" scoring — each reference has
  # its own typical-similarity baseline.
  medians <- reactive({
    rs_list <- raw_scores()
    map(rs_list, function(rs) {
      raw_cols <- grep("^raw_", names(rs), value = TRUE)
      setNames(
        sapply(raw_cols, function(col) median(rs[[col]], na.rm = TRUE)),
        sub("^raw_", "", raw_cols)
      )
    })
  })

  # match ranking across multiple references.
  # Logic: compute each reference's full match table independently, then
  # for each suburb_b take the MAX match across references (so a suburb
  # surfaces if it's strongly similar to ANY selected reference). Track which
  # reference contributed the winning match via `winning_ref` column.
  # The raw_* values displayed downstream are from the winning pair.
  match_df <- reactive({
    dream <- dream_refs_active()
    setts <- settings_active()
    focus <- focus_active()

    # ----- Dream-suburb mode --------------------------------------------
    # The synthetic raw_wide already averages within themes, so we feed it
    # straight through compute_match like a single ref. No MAX/MEAN
    # aggregation needed since there's effectively one "ref" — the blend.
    if (!is.null(dream)) {
      raw_wide <- compute_dream_raw(dream$theme_refs)
      if (is.null(raw_wide) || nrow(raw_wide) == 0) return(NULL)

      df <- compute_match(raw_wide, setts, NULL, focus = focus)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$winning_ref <- "dream"

      # Exclude all dream references from results
      dream_refs <- unique(unlist(dream$theme_refs))

      df_national <- df |>
        filter(!suburb_b %in% dream_refs,
               suburb_b %in% allowed_codes_all) |>
        arrange(desc(match)) |>
        mutate(national_rank = row_number(),
               national_pct  = 100 * national_rank / n())

      # Apply dream's own hard filters + standard region/cat filters
      keep_codes <- codes_from_cat_filters(
        tree_codes = allowed_codes_from_tree(region_filter()),
        coast      = cat_coast_filter(),
        terrain    = cat_terrain_filter(),
        remote     = cat_remote_filter()
      )
      if (!is.null(dream$filter_state) && nzchar(dream$filter_state)) {
        state_codes <- ref$suburb_code_2021[
          ref$state_name_2021 == dream$filter_state]
        keep_codes <- intersect(keep_codes, state_codes)
      }
      if (isTRUE(dream$filter_coast)) {
        coast_codes <- suburb_filters$suburb_code_2021[
          suburb_filters$cat_coast %in% c("On the coast", "Near the coast")]
        keep_codes <- intersect(keep_codes, coast_codes)
      }

      return(df_national |>
        filter(suburb_b %in% keep_codes) |>
        arrange(desc(match)) |>
        mutate(rank = row_number()) |>
        as_tibble())
    }

    # ----- Single/multi reference mode ----------------------------------
    rs_list   <- raw_scores()
    med_list  <- medians()
    ref_codes <- current_ref()
    mode      <- input$multi_mode %||% "max"

    # Per-reference match tables
    per_ref <- imap(rs_list, function(rs, ref_code) {
      meds <- med_list[[ref_code]]
      df   <- compute_match(rs, setts, meds, focus = focus)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df |> mutate(winning_ref = ref_code, .before = 1)
    })
    per_ref <- compact(per_ref)
    if (length(per_ref) == 0) return(NULL)

    # Aggregate across references. In MAX mode we pick the winning row per
    # suburb_b. In MEAN mode we average match + raw_* across refs and
    # surface suburbs that are similar to ALL refs on average. In single-ref
    # cases this branch is a no-op (one ref -> max == mean).
    if (length(per_ref) == 1 || mode == "max") {
      combined <- bind_rows(per_ref) |>
        group_by(suburb_b) |>
        slice_max(match, n = 1, with_ties = FALSE) |>
        ungroup()
    } else {
      stacked  <- bind_rows(per_ref)
      raw_cols <- grep("^raw_", names(stacked), value = TRUE)
      # Track which ref produced the *single highest* match for display,
      # even though the surfaced match is the mean. This way the popup
      # still tells the user where the strongest individual fit came from.
      win_per <- stacked |>
        group_by(suburb_b) |>
        slice_max(match, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(suburb_b, winning_ref)

      combined <- stacked |>
        group_by(suburb_b) |>
        summarise(match = mean(match, na.rm = TRUE),
                  across(all_of(raw_cols), \(x) mean(x, na.rm = TRUE)),
                  .groups = "drop") |>
        left_join(win_per, by = "suburb_b") |>
        filter(!is.nan(match))
    }

    # National pass: exclude all selected references, keep allowed only
    df_national <- combined |>
      filter(!suburb_b %in% ref_codes,
             suburb_b %in% allowed_codes_all) |>
      arrange(desc(match)) |>
      mutate(national_rank = row_number(),
             national_pct  = 100 * national_rank / n())

    # Apply region filter + cat filters, then re-rank within the filtered set
    filtered_codes <- codes_from_cat_filters(
      tree_codes = allowed_codes_from_tree(region_filter()),
      coast      = cat_coast_filter(),
      terrain    = cat_terrain_filter(),
      remote     = cat_remote_filter()
    )

    df_national |>
      filter(suburb_b %in% filtered_codes) |>
      arrange(desc(match)) |>
      mutate(rank = row_number()) |>
      as_tibble()
  })

  map_data <- reactive({
    cd <- match_df()
    if (is.null(cd) || nrow(cd) == 0) return(NULL)
    head(cd, top_n_active())
  })

  # --- Similarity settings modal ----------------------------------------
  settings_modal <- function() {
    setts <- settings_active()
    cur_focus <- focus_active()
    # Which dims are currently being filtered above via cat_* pickers?
    filtered_dims <- c(
      if (length(cat_coast_filter())   > 0) "coast"      else NULL,
      if (length(cat_terrain_filter()) > 0) "terrain"    else NULL,
      if (length(cat_remote_filter())  > 0) "remoteness" else NULL
    )

    # Sort: all place dims first (alphabetised), then all people dims.
    # One group header chip at the top of each section.
    place_ordered  <- intersect(sort(place_dims),  all_dim_codes)
    people_ordered <- intersect(sort(people_dims), all_dim_codes)
    ordered_dims   <- c(place_ordered, people_ordered)

    group_header <- function(label, colour) {
      tags$div(
        style = sprintf(
          "font-size:11px; font-weight:600; letter-spacing:0.5px;
           color:white; background:%s; display:inline-block;
           padding:2px 10px; border-radius:10px;
           margin: 4px 0 4px 0;", colour),
        toupper(label))
    }

    build_row <- function(d) {
      is_checked <- d %in% names(setts)
      cur_target <- if (is_checked) setts[[d]] else "similar"
      cur_target <- switch(cur_target,
        between    = "mostly_similar",
        dissimilar = "different",
        cur_target)
      is_filtered <- d %in% filtered_dims

      div(class = "dim-row",
          style = "display:flex; align-items:center; gap:6px;
                   margin:0; padding:2px 0;",
        div(style = "width: 190px; flex-shrink: 0;",
          # right = TRUE puts the switch on the left, label on the right
          materialSwitch(paste0("dim_check_", d),
                         label = dim_labels[d],
                         value = is_checked,
                         status = "primary", inline = TRUE, right = TRUE)),
        if (is_filtered) {
          tags$span(
            style = "font-size: 11px; color: #888; font-style: italic;",
            "filtered above — disabled in match")
        } else {
          conditionalPanel(
            condition = sprintf("input.dim_check_%s == true", d),
            div(style = "flex: 1; min-width: 0;",
              radioButtons(paste0("dim_target_", d), NULL,
                           choices = c("Similar"          = "similar",
                                       "Mostly similar"   = "mostly_similar",
                                       "Mostly different" = "mostly_different",
                                       "Different"        = "different"),
                           selected = cur_target, inline = TRUE)))
        })
    }

    place_block <- tagList(
      group_header("place",  "#5A5156"),
      lapply(place_ordered,  build_row))
    people_block <- tagList(
      group_header("people", "#1C8356"),
      lapply(people_ordered, build_row))

    modalDialog(
      title = "Match settings", easyClose = FALSE, size = "xl",
      tags$style(HTML(
        ".dim-row .radio-inline { margin: 0 6px 0 0; padding-top: 0;
                                   font-size: 11.5px; white-space: nowrap; }
         .dim-row .form-group   { margin-bottom: 0; }
         .dim-row .shiny-options-group { margin-top: 0; display: flex;
                                          flex-wrap: nowrap; white-space: nowrap; }
         .dim-row .bootstrap-switch { margin-right: 4px; }
         .dim-row label { font-size: 12.5px; white-space: nowrap;
                           overflow: hidden; text-overflow: ellipsis; }
        "
      )),
      tags$label("Focus:"),
      div(style = "margin-top: 4px; margin-bottom: 12px;",
        radioButtons("focus_choice", NULL,
                     choices  = c("Balanced (all characteristics equal)" = "balanced",
                                  "People-first (people group 75%)" = "people_focused",
                                  "Place-first (place group 75%)"  = "place_focused"),
                     selected = cur_focus, inline = FALSE),
        tags$div(style = "font-size: 11px; color: #666; margin-left: 24px;",
          "Adjust how much weight is given to people vs place characteristics. ",
          "People characteristics: age, sex, family composition, socioeconomic, voting, diversity. ",
          "Place characteristics: everything else.")),
      tags$hr(style = "margin: 8px 0;"),
      tags$label("characteristics and how to score them:"),
      div(style = "font-size: 11px; color: #666; margin-bottom: 4px;",
          "Similar = match the reference closely. Mostly similar = somewhat closer than average. ",
          "Mostly different = somewhat further. Different = match as little as possible."),
      div(style = "margin-top: 6px;", place_block, people_block),
      footer = tagList(
        actionButton("reset_settings", "Reset"),
        actionButton("apply_settings", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      )
    )
  }

  observeEvent(input$open_settings, {
    showModal(settings_modal())
    for (d in all_dim_codes) {
      is_active <- d %in% names(settings_active())
      updateMaterialSwitch(session, paste0("dim_check_", d), value = is_active)
      cur_target <- if (is_active) settings_active()[[d]] else "similar"
      cur_target <- switch(cur_target,
        between    = "mostly_similar",
        dissimilar = "different",
        cur_target)
      updateRadioButtons(session, paste0("dim_target_", d), selected = cur_target)
    }
    updateRadioButtons(session, "focus_choice", selected = focus_active())
  })

  observeEvent(input$apply_settings, {
    new_settings <- list()
    for (d in all_dim_codes) {
      if (isTRUE(input[[paste0("dim_check_", d)]])) {
        new_settings[[d]] <- input[[paste0("dim_target_", d)]] %||% "similar"
      }
    }
    settings_active(new_settings)
    focus_active(input$focus_choice %||% "balanced")
    removeModal()
  })

  observeEvent(input$reset_settings, {
    for (d in all_dim_codes) {
      updateMaterialSwitch(session, paste0("dim_check_", d), value = TRUE)
      updateRadioButtons(session, paste0("dim_target_", d), selected = "similar")
    }
    updateRadioButtons(session, "focus_choice", selected = "balanced")
  })

  # The Similarity Settings button doubles as a live summary of active
  # settings: a chip per target bucket (only buckets with at least one dim
  # are shown), plus a focus chip. Clicking opens the settings modal.
  output$settings_btn <- renderUI({
    setts <- settings_active()
    foc   <- focus_active()
    n_active <- length(setts)

    # Count dims per target (treat missing/unknown as "similar")
    targets <- vapply(setts, function(t) {
      t <- switch(t %||% "similar",
                  between    = "mostly_similar",
                  dissimilar = "different",
                  t)
      t
    }, character(1))

    counts <- c(
      similar          = sum(targets == "similar"),
      mostly_similar   = sum(targets == "mostly_similar"),
      mostly_different = sum(targets == "mostly_different"),
      different        = sum(targets == "different")
    )
    chip_colours <- c(
      similar          = "#1C8356",
      mostly_similar   = "#6FBA8E",
      mostly_different = "#E4A872",
      different        = "#D55E00"
    )

    chips <- lapply(names(counts), function(t) {
      if (counts[[t]] == 0) return(NULL)
      tags$span(
        style = sprintf(
          "background:%s; color:white; padding:1px 6px; border-radius:8px;
           font-weight:500; font-size:10px; white-space:nowrap;",
          chip_colours[[t]]),
        sprintf("%s %d", target_labels[[t]], counts[[t]]))
    })
    chips <- Filter(Negate(is.null), chips)

    focus_chip <- tags$span(
      style = "background:rgba(255,255,255,0.25); color:white;
               padding:1px 6px; border-radius:8px; font-weight:500;
               font-size:10px; white-space:nowrap;",
      focus_labels[[foc]] %||% foc)

    all_chips <- c(chips, list(focus_chip))
    n <- length(all_chips)

    # Row layout rule:
    #   1 badge     -> single row
    #   2 badges    -> two rows, one each (vertical stack)
    #   3-4 badges  -> two rows (split in half, top row gets the larger half)
    #   5 badges    -> 3 on top, 2 on bottom
    rows <- if (n <= 1) {
      list(all_chips)
    } else if (n == 2) {
      list(all_chips[1], all_chips[2])
    } else if (n <= 4) {
      cut <- ceiling(n / 2)
      list(all_chips[1:cut], all_chips[(cut + 1):n])
    } else {
      list(all_chips[1:3], all_chips[4:n])
    }

    row_divs <- lapply(rows, function(items) {
      tags$div(style = "display:flex; gap:3px; justify-content:flex-end;",
               items)
    })

    actionButton("open_settings",
      label = tagList(
        tags$span(style = "display:flex; align-items:center; gap:8px;",
          icon("sliders"),
          tags$div(style = "display:flex; flex-direction:column; gap:2px;",
                   row_divs))
      ),
      class = "btn-primary",
      title = "Refine match",
      style = "margin-left: 4px; padding: 4px 8px;")
  })

  # ===== Dream-suburb mode ================================================
  # Modal lets the user pick reference suburbs for each of four themes
  # (people, urban, nature, amenities) plus optional hard filters. On Apply, the
  # dream_refs_active reactiveVal is set and match_df switches branch to
  # the dream computation. A banner above the view shows what blend is
  # currently active and offers a "back to single reference" link.
  observeEvent(input$open_dream, {
    cur <- dream_refs_active()
    pre <- cur$theme_refs %||% list()

    theme_picker <- function(theme_id, label, hint, selected) {
      tags$div(style = "margin-bottom: 14px;",
        tags$label(style = "font-size: 11px; font-weight: 600;
                            letter-spacing: 0.5px; color: #555;
                            text-transform: uppercase; display: block;
                            margin-bottom: 2px;", label),
        tags$div(style = "font-size: 11px; color: #888; margin-bottom: 4px;",
                 hint),
        virtualSelectInput(
          paste0("dream_", theme_id, "_refs"), label = NULL,
          choices = ref_choices, selected = selected,
          multiple = TRUE, search = TRUE, width = "100%",
          showValueAsTags = TRUE, optionsCount = 12))
    }

    showModal(modalDialog(
      title = tagList(icon("wand-magic-sparkles"), " Build your dream suburb"),
      size = "l", easyClose = TRUE,
      tags$p(style = "font-size: 12px; color: #666;",
        "Pick one or more reference suburbs for each theme. Each theme can ",
        "draw on different references — e.g. ", tags$em("people like Tecoma"),
        ", ", tags$em("urban feel like Hawthorn"), ", ",
        tags$em("nature like Sorrento"), ". The results are ranked by how ",
        "well each suburb matches the blend across all themes."),
      theme_picker("people", "People & culture",
                   "voting, people, diversity, socioeconomic",
                   pre$people),
      theme_picker("urban",  "Urban fabric & economy",
                   "remoteness, employment, housing, land use, density",
                   pre$urban),
      theme_picker("nature", "Nature & climate",
                   "water, weather, terrain, vegetation, coast",
                   pre$nature),
      theme_picker("amenities", "Amenities",
                   "dining, public transport, fresh food, cultural amenities, community infrastructure, tertiary education, health infrastructure, kinder, schools",
                   pre$amenities),
      tags$hr(style = "margin: 8px 0;"),
      tags$label(style = "font-size: 11px; font-weight: 600;
                          letter-spacing: 0.5px; color: #555;
                          text-transform: uppercase;",
                 "Hard filters (optional)"),
      tags$div(style = "display: flex; gap: 16px; align-items: center;
                        flex-wrap: wrap; margin-top: 6px;",
        pickerInput("dream_filter_state", "Limit to state:",
                    choices = c("Any" = "",
                                setNames(names(state_abbr), names(state_abbr))),
                    selected = cur$filter_state %||% "",
                    width = "200px",
                    options = pickerOptions(style = "btn-default")),
        materialSwitch("dream_filter_coast", "Coast or near-coast only",
                       value = isTRUE(cur$filter_coast),
                       status = "primary", inline = TRUE, right = TRUE)),
      footer = tagList(
        if (!is.null(cur)) actionButton("clear_dream_modal",
                                        "Clear & exit",
                                        class = "btn-outline-secondary"),
        modalButton("Cancel"),
        actionButton("apply_dream", "Build", class = "btn-primary"))
    ))
  })

  observeEvent(input$apply_dream, {
    theme_refs <- list(
      people    = input$dream_people_refs    %||% character(0),
      urban     = input$dream_urban_refs     %||% character(0),
      nature    = input$dream_nature_refs    %||% character(0),
      amenities = input$dream_amenities_refs %||% character(0))

    if (!length(unlist(theme_refs))) {
      showNotification("Pick at least one reference suburb in any theme.",
                       type = "warning", duration = 4)
      return()
    }

    dream_refs_active(list(
      theme_refs   = theme_refs,
      filter_state = input$dream_filter_state %||% "",
      filter_coast = isTRUE(input$dream_filter_coast)))

    removeModal()
  })

  observeEvent(input$clear_dream_modal, {
    dream_refs_active(NULL)
    removeModal()
  })

  observeEvent(input$clear_dream, {
    dream_refs_active(NULL)
  })

  output$dream_banner <- renderUI({
    dream <- dream_refs_active()
    if (is.null(dream)) return(NULL)

    label_refs <- function(codes) {
      if (!length(codes)) return(NULL)
      paste(vapply(codes,
        \(c) suburb_code_to_name[[c]] %||% c,
        character(1)), collapse = " + ")
    }

    bits <- list(
      people    = label_refs(dream$theme_refs$people),
      urban     = label_refs(dream$theme_refs$urban),
      nature    = label_refs(dream$theme_refs$nature),
      amenities = label_refs(dream$theme_refs$amenities))
    bits <- compact(bits)

    pieces <- imap(bits, function(refs, theme) {
      tagList(tags$b(dream_theme_labels[[theme]], ": "),
              tags$span(refs))
    })
    # Join with " · " separators
    joined <- list()
    for (i in seq_along(pieces)) {
      joined[[length(joined) + 1]] <- pieces[[i]]
      if (i < length(pieces))
        joined[[length(joined) + 1]] <-
          tags$span(style = "color:#999; margin: 0 6px;", "·")
    }

    tags$div(
      style = "background: #fff8e6; border: 1px solid #f0c14b;
               border-radius: 4px; padding: 6px 12px; margin-bottom: 8px;
               font-size: 12px; display: flex; align-items: center; gap: 10px;
               flex-wrap: wrap;",
      tags$span(style = "color: #b8860b;",
                icon("wand-magic-sparkles")),
      tags$b("Dream suburb:"),
      do.call(tagList, joined),
      tags$a(href = "#", style = "margin-left: auto; font-size: 11px;",
        onclick = "Shiny.setInputValue('clear_dream', Math.random(),
                   {priority:'event'});return false;",
        "← Back to single-reference mode"))
  })

  observeEvent(input$open_methodology, {
    showModal(modalDialog(
      title = "How does this work?",
      easyClose = TRUE, size = "l",
      tags$h5("What is a match score?"),
      tags$p("Each pair of suburbs is compared across 23 characteristics covering ",
             "geography, environment, demographics, politics, and local amenities. ",
             "A per-characteristic match score is computed from the underlying data ",
             "(using Bray-Curtis or scaled-Euclidean distance, depending on the ",
             "characteristic). A score of 1 means identical on that ",
             "characteristic; 0 means as different as possible."),
      tags$h5("The 23 characteristics"),
      tags$div(style = "font-size: 12px;",
        tags$p(tags$b("People-group: "),
          "people (age, household composition), socioeconomic (income, ",
          "occupation, education), voting (federal first-preference vote shares), ",
          "diversity (country-of-birth, language, religion mixing)."),
        tags$p(tags$b("Place-group: "),
          "remoteness (ABS RA class), density (population per km²), ",
          "housing (dwelling structure, tenure), employment (industry mix), ",
          "coast (distance bands to coastline), terrain (slope, elevation), ",
          "vegetation (cover types from Digital Earth Australia), ",
          "water (rivers, lakes, hydrology), weather (temperature, rainfall ",
          "grids), landcover (mesh-block land use composition), and nine ",
          "amenity-access characteristics — public transport, health ",
          "infrastructure, tertiary education, community infrastructure, ",
          "cultural amenities, fresh food, dining, schools, and kinder — ",
          "each measured by average distance to and count of nearby ",
          "facilities (from OpenStreetMap).")),
      tags$h5("How the match is calculated"),
      tags$p("Each characteristic's raw similarity is transformed by your chosen ",
             "target: ", tags$b("Similar"), " = peak at 1.0 (highest similarity), ",
             tags$b("Mostly similar"), " = peak at 0.7, ",
             tags$b("Mostly different"), " = peak at 0.3, ",
             tags$b("Different"), " = peak at 0.0. ",
             "Scores are computed using a triangular decay function: a perfect ",
             "target match scores 1.0, while similarity drops off linearly the",
             "the further a suburb deviates from your target settings."
             ),
      tags$p("The transformed characteristic scores are then aggregated:"),
      tags$ul(
        tags$li(tags$b("Balanced"), " — simple mean across all selected characteristics."),
        tags$li(tags$b("People-focused"), " — people-group mean × 0.75 + place-group mean × 0.25."),
        tags$li(tags$b("Place-focused"), " — opposite weighting (place × 0.75, people × 0.25).")
      ),
      tags$h5("Multiple reference suburbs"),
      tags$p("When more than one reference is selected: ",
             tags$b("Match any"), " surfaces suburbs strongly similar to any one ",
             "of your references (best individual fit per pair). ",
             tags$b("Balanced across all"), " surfaces suburbs moderately similar ",
             "to every reference (mean across pairs)."),
      tags$h5("Filters"),
      tags$p("Category filters (Coast, Terrain, Remoteness) restrict results to ",
             "the selected categories AND remove that characteristic from the match. ",
             "Region filters restrict by state, GCCSA, or SA4 without affecting scoring."),
      footer = modalButton("Close")
    ))
  })



  # --- Map base ----------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 134, lat = -28, zoom = 4)
  })

  output$map_zoom_dropdown <- renderUI({
    states <- states_in_filter(region_filter())
    selectInput("map_zoom", "Zoom to:",
      choices  = c("Auto (fit results)" = "auto",
                   "Australia"          = "australia",
                   setNames(states, states)),
      selected = isolate(input$map_zoom) %||% "auto")
  })

  observeEvent(input$map_zoom, {
    #req(input$map_bounds)
    z <- input$map_zoom %||% "auto"
    if (z == "auto") {
      d <- map_data()
      if (!is.null(d) && nrow(d) > 0) {
        shp_sub <- shp_suburb |> filter(suburb_code_2021 %in% d$suburb_b)
        if (nrow(shp_sub) > 0) {
          bb <- as.numeric(st_bbox(shp_sub))
          leafletProxy("map") |> fitBounds(bb[1], bb[2], bb[3], bb[4])
        }
      }
    } else if (z == "australia") {
      leafletProxy("map") |> setView(lng = 134, lat = -28, zoom = 4)
    } else {
      # Fit to the subset of top-N results that fall within this state.
      d <- map_data()
      codes_in_state <- ref |>
        filter(state_name_2021 == z) |>
        pull(suburb_code_2021)
      if (!is.null(d) && nrow(d) > 0) {
        shp_sub <- shp_suburb |>
          filter(suburb_code_2021 %in% d$suburb_b,
                 suburb_code_2021 %in% codes_in_state)
        if (nrow(shp_sub) > 0) {
          bb <- as.numeric(st_bbox(shp_sub))
          leafletProxy("map") |> fitBounds(bb[1], bb[2], bb[3], bb[4])
          return()
        }
      }
      # Fallback: no results in that state -> fit to the state polygon
      st_poly <- shp_state |> filter(state_name_2021 == z)
      if (nrow(st_poly) > 0) {
        bb <- as.numeric(st_bbox(st_poly))
        leafletProxy("map") |> fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # --- Redraw data layers when map_data changes --------------------------
  observeEvent(map_data(), {
    d <- map_data()
    #req(input$map_bounds)
    proxy <- leafletProxy("map") |>
      clearGroup("matches") |> clearGroup("ref") |>
      clearGroup("sa4") |> clearGroup("clickpop") |> clearControls()
    
    if (is.null(d) || nrow(d) == 0) {
      showNotification("No suburbs to show. Trying widening the region selection or changing settings.", type = "warning")
      return()
    }

    raw_cols   <- grep("^raw_",   names(d), value = TRUE)
    join_cols  <- c("suburb_b", "match", "rank", "national_rank",
                    "national_pct", "winning_ref", raw_cols)

    shp_sub <- shp_suburb |>
      inner_join(d |> select(all_of(join_cols)),
                 by = c("suburb_code_2021" = "suburb_b")) |>
      mutate(suburb_name_2021 = coalesce(unname(suburb_code_to_name[suburb_code_2021]),
                                          suburb_name_2021))
    
    ref_poly <- shp_suburb |>
      filter(suburb_code_2021 %in% current_ref()) |>
      mutate(suburb_name_2021 = coalesce(unname(suburb_code_to_name[suburb_code_2021]),
                                          suburb_name_2021))

    shown_sa4 <- ref |>
      filter(suburb_code_2021 %in% d$suburb_b) |>
      pull(sa4_name_2021) |> unique()
    sa4_lines <- shp_sa4 |> filter(sa4_name_2021 %in% shown_sa4)

    # Build a "winning ref" label only when more than one reference is selected
    multi_ref       <- length(current_ref()) > 1
    ref_name_lookup <- if (multi_ref) {
      ref |>
        filter(suburb_code_2021 %in% current_ref()) |>
        transmute(code = suburb_code_2021,
                  label = paste0(display_name, ", ",
                                 state_abbr[state_name_2021])) |>
        { \(d) setNames(d$label, d$code) }()
    } else NULL

    shp_sub$popup_html <- vapply(seq_len(nrow(shp_sub)), function(i) {
      raw_vals <- as.list(as.data.frame(st_drop_geometry(shp_sub))[i, raw_cols, drop = FALSE])
      names(raw_vals) <- sub("^raw_", "", names(raw_vals))
      winning_lbl <- if (multi_ref) {
        ref_name_lookup[[ shp_sub$winning_ref[i] ]]
      } else NULL
      code <- shp_sub$suburb_code_2021[i]
      build_popup(suburb_code_to_name[[code]] %||% shp_sub$suburb_name_2021[i],
                  suburb_code_to_abbr[[code]],
                  shp_sub$match[i], shp_sub$rank[i],
                  shp_sub$national_rank[i], shp_sub$national_pct[i],
                  raw_vals, code,
                  winning_ref_label = winning_lbl)
    }, character(1))

    # Rank-based Blues palette: rank 1 = darkest blue, rank N = lightest.
    # Visually emphasises rank position, which scales cleanly even when
    # match scores cluster in a narrow numeric range.
    n_results <- nrow(shp_sub)
    pal_fn <- colorNumeric(
      palette = "Blues",
      domain  = c(1, max(n_results, 2)),
      reverse = TRUE      # invert so rank 1 (smallest int) -> darkest blue
    )

    # Legend ticks: 1, ~N/4, ~N/2, ~3N/4, N (or just 1..N for small N)
    legend_ranks <- if (n_results <= 5) {
      seq_len(n_results)
    } else {
      unique(round(c(1, n_results * c(0.25, 0.5, 0.75), n_results)))
    }

    proxy |>
      addPolygons(
        data = shp_sub, group = "matches", layerId = ~suburb_code_2021,
        fillColor = ~pal_fn(rank), fillOpacity = 0.65,
        color = "white", weight = 2,
        label = ~lapply(
          sprintf("%s: rank %d (match %.1f%%)<br><i>Click for more info</i>",
                  suburb_name_2021, rank, match * 100),
          htmltools::HTML),
        popup = ~popup_html,
        popupOptions = popupOptions(minWidth = 260, maxWidth = 300),
        highlightOptions = highlightOptions(weight = 2, color = "#444",
                                            bringToFront = TRUE)) |>
      addPolygons(data = ref_poly, group = "ref",
                  fillColor = "#111", fillOpacity = 0.65,
                  color = "black", weight = 1.4,
                  label = ~paste0(suburb_name_2021, " (reference)")) |>
      addPolygons(data = sa4_lines, group = "sa4",
                  fill = FALSE, color = "black",
                  weight = 0.8, opacity = 0.6,
                  label = ~sa4_name_2021) |>
      addLegend("bottomright",
                colors = pal_fn(legend_ranks),
                labels = paste0("#", legend_ranks),
                title  = "Rank", opacity = 0.9)

    # if zoom is in Auto mode, fit to the GCCSA holding the most matches
    # (single dominant cluster usually tells the most useful story).
    # Falls back to the full top-N bbox if no GCC info is available.
    if ((isolate(input$map_zoom) %||% "auto") == "auto" && nrow(shp_sub) > 0) {
      top_gcc <- shp_sub$suburb_code_2021 |>
        (\(codes) gcc_lookup_full[codes])() |>
        unname() |>
        (\(v) v[!is.na(v) & nzchar(v)])() |>
        table()
      if (length(top_gcc) > 0) {
        winner_gcc <- names(top_gcc)[which.max(top_gcc)]
        winner_codes <- shp_sub$suburb_code_2021[
          gcc_lookup_full[shp_sub$suburb_code_2021] == winner_gcc]
        shp_winner <- shp_sub |> filter(suburb_code_2021 %in% winner_codes)
        if (nrow(shp_winner) > 0) {
          bb <- as.numeric(st_bbox(shp_winner))
          proxy |> fitBounds(bb[1], bb[2], bb[3], bb[4])
        } else {
          bb <- as.numeric(st_bbox(shp_sub))
          proxy |> fitBounds(bb[1], bb[2], bb[3], bb[4])
        }
      } else {
        bb <- as.numeric(st_bbox(shp_sub))
        proxy |> fitBounds(bb[1], bb[2], bb[3], bb[4])
      }
    }
  })

  # --- "What's this suburb like" overview --------------------------------
  # Two-part summary:
  #   1. Header sentence — names the suburbs that dominate the strongest
  #      bucket across characteristics (the suburbs that show up most often
  #      among the closest matches).
  #   2. "Where things differ" — characteristics where the best bucket reached
  #      is weaker than the strongest, with the closest suburbs there.
  # Goal: surface the dominant story in one line, then call out outliers.
  output$similarity_overview <- renderUI({
    cd <- match_df()
    if (is.null(cd) || nrow(cd) == 0) return(NULL)
    ref_codes <- current_ref()
    if (!length(ref_codes)) return(NULL)

    raw_cols <- grep("^raw_", names(cd), value = TRUE)
    # Structural characteristics that score near-perfectly between any two
    # nearby suburbs (all metro Melbourne suburbs share the same coast /
    # remoteness / weather profile) — they'd dominate the "almost
    # identical" header without telling the user anything informative.
    # Excluded from the overview only; still in the underlying match.
    raw_cols <- setdiff(raw_cols,
                        c("raw_coast", "raw_remoteness", "raw_weather"))
    if (!length(raw_cols)) return(NULL)

    n_top <- top_n_active()
    top <- head(cd, n_top)
    if (!nrow(top)) return(NULL)

    bucket_for <- function(v) {
      if (is.na(v))      NA_integer_
      else if (v >= 0.95) 1L
      else if (v >= 0.90) 2L
      else if (v >= 0.80) 3L
      else                NA_integer_
    }
    bucket_label  <- c("almost identical to", "a lot like", "somewhat like")
    bucket_colour <- c("#1C8356", "#6FBA8E", "#D9893C")

    dim_summaries <- lapply(raw_cols, function(col) {
      dim_code <- sub("^raw_", "", col)
      values   <- top[[col]]
      buckets  <- vapply(values, bucket_for, integer(1))
      if (all(is.na(buckets))) return(NULL)

      best <- min(buckets, na.rm = TRUE)
      idx  <- which(buckets == best)
      idx  <- idx[order(values[idx], decreasing = TRUE)]
      idx  <- idx[seq_len(min(3, length(idx)))]

      names_in_bucket <- vapply(top$suburb_b[idx],
        function(c) suburb_code_to_name[[c]] %||% c,
        character(1))

      list(dim_code  = dim_code,
           dim_label = unname(dim_labels[dim_code]) %||% dim_code,
           bucket    = best,
           suburbs   = unname(names_in_bucket))
    })
    dim_summaries <- Filter(Negate(is.null), dim_summaries)
    if (!length(dim_summaries)) return(NULL)

    join_names <- function(xs) {
      if (length(xs) == 1) return(xs)
      if (length(xs) == 2) return(paste(xs, collapse = " and "))
      paste0(paste(xs[-length(xs)], collapse = ", "),
             " and ", xs[length(xs)])
    }

    # Name -> code lookup for the top-N (so links in the overview can
    # drop a marker at the right place on the map).
    name_to_code <- setNames(
      top$suburb_b,
      vapply(top$suburb_b,
        function(c) suburb_code_to_name[[c]] %||% c,
        character(1))
    )

    # Build a clickable suburb-name span. Clicking fires overview_pin
    # which the server uses to add a single marker on the map.
    suburb_link <- function(nm) {
      code <- unname(name_to_code[nm])
      if (is.null(code) || is.na(code)) {
        return(tags$span(style = "font-weight:500;", nm))
      }
      tags$a(
        href = "#",
        onclick = sprintf(
          "Shiny.setInputValue('overview_pin','%s',{priority:'event'});return false;",
          code),
        style = "font-weight:500; color:#2c7fb8; text-decoration:none;
                 cursor:pointer; border-bottom:1px dashed #2c7fb8;",
        title = "Click to drop a marker on the map",
        nm)
    }

    # Join a list of tag elements with commas and a final "and".
    join_tags <- function(items) {
      if (length(items) == 0) return(NULL)
      if (length(items) == 1) return(items[[1]])
      if (length(items) == 2) return(tagList(items[[1]], " and ", items[[2]]))
      pieces <- list()
      for (i in seq_along(items)) {
        pieces[[length(pieces) + 1]] <- items[[i]]
        if (i < length(items) - 1)        pieces[[length(pieces) + 1]] <- ", "
        else if (i == length(items) - 1)  pieces[[length(pieces) + 1]] <- " and "
      }
      do.call(tagList, pieces)
    }

    # Group characteristics by bucket strength. Each bucket becomes one bullet
    # naming the top 3 suburbs (by frequency across characteristics in that
    # bucket, with ties broken alphabetically) and listing the characteristics
    # in alphabetical order. This trades suburb-per-characteristic precision
    # (still available in the per-card breakdown below) for a compact,
    # scannable overview that scales gracefully whether the strongest
    # bucket covers all 11 characteristics or just 1.
    buckets_vec <- vapply(dim_summaries, `[[`, integer(1), "bucket")
    buckets_present <- sort(unique(buckets_vec))

    primary_ref <- ref_codes[1]
    ref_label <- sprintf("%s, %s",
      suburb_code_to_name[[primary_ref]] %||% primary_ref,
      suburb_code_to_abbr[[primary_ref]] %||% "")
    if (length(ref_codes) > 1) {
      ref_label <- paste0(ref_label, sprintf(" (and %d other%s)",
        length(ref_codes) - 1,
        if (length(ref_codes) > 2) "s" else ""))
    }

    bullets <- lapply(buckets_present, function(b) {
      in_bucket <- dim_summaries[buckets_vec == b]

      # Top 3 suburbs by frequency across this bucket's characteristics
      all_suburbs <- unlist(lapply(in_bucket, `[[`, "suburbs"))
      freq <- sort(table(all_suburbs), decreasing = TRUE)
      named_suburbs <- head(names(freq), 3)
      suburb_link_tags <- lapply(named_suburbs, suburb_link)

      # characteristics in this bucket, alphabetical
      dim_lbls <- sort(vapply(in_bucket, `[[`, character(1), "dim_label"))

      tags$li(style = "margin: 2px 0;",
        tags$span(style = sprintf("color:%s; font-weight:600;",
                                  bucket_colour[b]),
                  bucket_label[b]),
        " ",
        join_tags(suburb_link_tags),
        tags$span(style = "color:#666;", " across "),
        tags$span(tolower(paste(dim_lbls, collapse = ", "))))
    })

    # Overall top-5: simple ranked list with match percentages. Drawn
    # independently of the bucket logic (top-5 by match, no thresholds)
    # so users get a quick "who's the closest match overall" answer in
    # addition to the per-characteristic breakdown.
    top5 <- head(cd, 5)
    top5_html <- if (nrow(top5) >= 1) {
      parts <- lapply(seq_len(nrow(top5)), function(i) {
        code <- top5$suburb_b[i]
        nm   <- suburb_code_to_name[[code]] %||% code
        pct  <- round(top5$match[i] * 100)
        tagList(
          suburb_link(nm),
          tags$span(style = "color:#888; margin-left:2px;",
                    sprintf("(%d%%)", pct)))
      })
      join_tags(parts)
    } else NULL

    tags$div(
      style = "border: 1px solid #e0e0e0; border-radius: 6px;
               padding: 8px 14px 10px; background: #fafbfc;
               margin-bottom: 10px; font-size: 13px;",
      tags$div(style = "margin-bottom: 6px; line-height: 1.45;",
        tags$b(ref_label),
        " is overall most similar to ",
        top5_html,
        "."),
      # Native HTML <details>/<summary> gives us an accordion with zero JS.
      # Collapsed by default. The triangle is the browser's default marker
      # which works across browsers. Hover/cursor styled for affordance.
      tags$details(
        tags$summary(
          style = "cursor: pointer; color: #666; font-size: 12px;
                   margin-bottom: 2px; outline: none; user-select: none;",
          "By characteristic"),
        tags$ul(style = "margin: 4px 0 0 0; padding-left: 20px;
                          line-height: 1.45;",
                bullets)
      )
    )
  })

  # --- Top-right: matches by GCC -----------------------------------------
  output$gcc_bar <- renderUI({
    d <- map_data(); if (is.null(d) || nrow(d) == 0) return(NULL)
    joined <- d |>
      left_join(ref |> select(suburb_code_2021, gcc_name_2021),
                by = c("suburb_b" = "suburb_code_2021"))
    if (!"gcc_name_2021" %in% names(joined)) return(NULL)
    # Coerce defensively — if a prior reactive ever ends up storing this
    # column as a list (seen intermittently after rapid filter changes),
    # count() would blow up. Flattening to character is safe either way.
    gcc <- as.character(unlist(joined$gcc_name_2021))
    gcc <- gcc[!is.na(gcc) & nzchar(gcc)]
    if (!length(gcc)) return(NULL)
    counts <- tibble(gcc_name_2021 = gcc) |>
      dplyr::count(gcc_name_2021, name = "n") |>
      arrange(desc(n))
    mx <- max(counts$n)
    rows <- lapply(seq_len(nrow(counts)), function(i) {
      pct <- counts$n[i] / mx * 100
      tags$div(style = "margin-bottom: 4px;",
        tags$div(style = "font-size: 10px; color: #333; margin-bottom: 1px;",
                 sprintf("%s (%d)", counts$gcc_name_2021[i], counts$n[i])),
        tags$div(style = "background: #e6e6e6; border-radius: 2px; height: 10px; width: 100%;",
          tags$div(style = sprintf(
            "background: #2c7fb8; height: 10px; width: %.1f%%; border-radius: 2px;", pct))))
    })
    tagList(tags$div(style = "font-weight: bold; font-size: 11px; margin-bottom: 4px;",
                     "Matches by GCC"),
            rows)
  })

  # --- Bottom-left: top-10 horizontal bar chart of match scores.
  # Each row is one suburb_b — bar width = match %. Hovering a row
  # reveals an absolutely-positioned tooltip showing the per-dim raw
  # similarity bars + winning-ref label (when multi-ref).
  output$top10 <- renderUI({
    d <- map_data(); if (is.null(d) || nrow(d) == 0) return(NULL)

    raw_cols <- grep("^raw_", names(d), value = TRUE)
    if (!length(raw_cols)) return(NULL)
    dims_ordered <- sub("^raw_", "", raw_cols)

    top <- d |> arrange(rank) |> head(10)
    if (!nrow(top)) return(NULL)

    multi_ref       <- length(current_ref()) > 1
    ref_name_lookup <- if (multi_ref) {
      ref |>
        filter(suburb_code_2021 %in% current_ref()) |>
        transmute(code = suburb_code_2021,
                  label = paste0(display_name, ", ",
                                 state_abbr[state_name_2021])) |>
        { \(d) setNames(d$label, d$code) }()
    } else NULL

    # Use the largest match as the bar-scale max (so the top suburb's
    # bar fills the row). Visual spread is more informative than absolute
    # percentage. Keep the % label as the true match value.
    max_v <- max(top$match, na.rm = TRUE)
    if (!is.finite(max_v) || max_v <= 0) max_v <- 1

    render_row <- function(i) {
      r <- as.data.frame(top)[i, , drop = FALSE]
      vals <- as.numeric(r[, raw_cols])
      match_pct <- r$match * 100
      bar_width_pct <- pmin(100, (r$match / max_v) * 100)

      # Hover tooltip: per-dim raw-similarity bars + winning-ref label.
      dim_bars <- map2(vals, dims_ordered, function(v, dc) {
        tags$div(style = "display:flex; align-items:center; margin:2px 0; gap:6px;",
          tags$span(style = "width:90px; font-size:10px; color:#444;", dim_labels[dc]),
          tags$div(style = "flex:1; background:#eee; height:8px; border-radius:2px; position:relative;",
            tags$div(style = sprintf(
              "background:%s; height:8px; width:%.0f%%; border-radius:2px;",
              dim_colors[dc], v * 100))),
          tags$span(style = "width:36px; text-align:right; font-size:10px;",
                    sprintf("%.1f%%", v * 100)))
      })
      winning_line <- if (multi_ref && !is.null(r$winning_ref) &&
                          !is.na(r$winning_ref) &&
                          r$winning_ref %in% names(ref_name_lookup)) {
        tags$div(style = "font-size: 10px; color: #1f77b4; margin-bottom: 4px;",
                 sprintf("most similar to %s", ref_name_lookup[[r$winning_ref]]))
      } else NULL

      tooltip <- tags$div(
        class = "top10-tooltip",
        style = paste(
          "position:absolute; left:100%; top:0; margin-left:10px;",
          "background:white; border:1px solid #ccc; border-radius:4px;",
          "box-shadow:0 2px 8px rgba(0,0,0,0.15);",
          "padding:8px 10px; min-width:260px; z-index:2000;",
          "display:none; pointer-events:none;"),
        tags$div(style = "font-weight:bold; font-size:11px; margin-bottom:4px;",
                 sprintf("%s, %s",
                         suburb_code_to_name[[r$suburb_b]],
                         suburb_code_to_abbr[[r$suburb_b]])),
        tags$div(style = "font-size:10px; color:#666; margin-bottom:6px;",
                 sprintf("nat. rank %d · %s",
                         r$national_rank, fmt_pct(r$national_pct))),
        winning_line,
        dim_bars)

      # The row: rank label, suburb name, bar, % label. Tooltip on hover.
      tags$div(
        class = "top10-row",
        style = paste(
          "position:relative;",
          "display:grid; grid-template-columns: 28px 130px 1fr 50px; gap:6px;",
          "align-items:center; padding:3px 4px; border-radius:3px;",
          "cursor:default;"),
        tags$span(style = "font-size:10px; font-weight:bold; color:#666;",
                  sprintf("#%d", r$rank)),
        tags$span(style = "font-size:11px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                  sprintf("%s, %s",
                          suburb_code_to_name[[r$suburb_b]],
                          suburb_code_to_abbr[[r$suburb_b]])),
        tags$div(style = "background:#eee; height:14px; border-radius:3px; position:relative;",
          tags$div(style = sprintf(
            "background:#2c7fb8; height:14px; width:%.1f%%; border-radius:3px;",
            bar_width_pct))),
        tags$span(style = "font-size:10px; text-align:right; color:#333;",
                  sprintf("%.1f%%", match_pct)),
        tooltip)
    }

    rows <- lapply(seq_len(nrow(top)), render_row)

    # CSS for hover-reveal: show tooltip when mouse enters the row.
    hover_css <- tags$style(HTML("
      .top10-row:hover { background: #f3f3f3; }
      .top10-row:hover .top10-tooltip { display: block !important; }
    "))

    tagList(
      hover_css,
      tags$div(style = "font-weight: bold; font-size: 11px; margin-bottom: 4px;",
               "Top 10 by match score"),
      tags$div(style = "font-size: 9px; color: #888; margin-bottom: 6px;",
               "Hover for per-characteristic breakdown"),
      rows
    )
  })

  # --- List view: per-suburb cards with dim chart + mini-map -------------
  # Each card has a header (rank, name, match, action buttons), a
  # ggplot of all characteristics with match as a dashed reference line,
  # and the pre-rendered SA4-context SVG mini-map.
  list_chart_svg <- function(raw_vals, match_val) {
    df <- tibble::tibble(dim = names(raw_vals),
                         val = unlist(raw_vals)) |>
      mutate(label = dim_labels[dim],
             colour = dim_colors[dim]) |>
      arrange(desc(val)) |>
      mutate(label = forcats::fct_inorder(label))

    p <- ggplot(df, aes(val, forcats::fct_rev(label), fill = label)) +
      geom_col(width = 0.75) +
      geom_vline(xintercept = match_val, linetype = "dashed",
                 colour = "#333", linewidth = 0.5) +
      scale_fill_manual(values = setNames(df$colour, df$label)) +
      scale_x_continuous(limits = c(0, 1.05),
                         breaks = c(0, 0.5, 1),
                         labels = c("0%", "50%", "100%"),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 8) +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text.y = element_text(size = 8),
            plot.margin = margin(8, 8, 4, 4))

    # Render to in-memory SVG string (no temp files, parallel-safe)
    tmp <- tempfile(fileext = ".svg")
    on.exit(unlink(tmp), add = TRUE)
    ggsave(tmp, plot = p, device = svglite::svglite,
           width = 3.4, height = 2.4, units = "in", bg = "white")
    chart_svg <- HTML(paste(readLines(tmp, warn = FALSE), collapse = "\n"))

    # Wrap the chart with an HTML label above it (more reliable than an
    # in-plot annotation, which can clip at the top of the SVG).
    tags$div(
      tags$div(style = "font-size: 11px; color: #333; margin-bottom: 2px;",
               sprintf("Composite match: %.0f%%", match_val * 100)),
      chart_svg)
  }

  output$list_view <- renderUI({
    d <- match_df()
    if (is.null(d) || nrow(d) == 0) {
      # Build a list of currently active filters for the empty-state card
      active_filters <- list()
      if (length(cat_coast_filter()) > 0)
        active_filters$Coast <- cat_coast_filter()
      if (length(cat_terrain_filter()) > 0)
        active_filters$Terrain <- cat_terrain_filter()
      if (length(cat_remote_filter()) > 0)
        active_filters$Remoteness <- cat_remote_filter()

      active_chips <- if (length(active_filters) > 0) {
        tags$div(style = "margin-bottom: 16px;",
          tags$div(style = "font-size: 11px; color: #666; margin-bottom: 6px;
                            text-transform: uppercase; letter-spacing: 0.5px;",
                   "Currently filtered by"),
          tags$div(style = "display: flex; flex-wrap: wrap; gap: 6px;
                            justify-content: center;",
            lapply(names(active_filters), function(filter_name) {
              tags$span(
                style = "background: #fff3e0; border: 1px solid #ffb74d;
                         padding: 3px 10px; border-radius: 12px;
                         font-size: 11px; color: #6d4400;",
                sprintf("%s: %s", filter_name,
                        paste(active_filters[[filter_name]], collapse = ", ")))
            })
          ))
      } else NULL

      return(tags$div(
        style = paste(
          "max-width: 480px; margin: 60px auto; padding: 24px;",
          "border: 1px solid #e6e6e6; border-radius: 8px;",
          "background: #fafafa; text-align: center;"),
        tags$div(icon("filter", class = "fa-2x"),
                 style = "color: #bbb; margin-bottom: 12px;"),
        tags$div(style = "font-size: 16px; font-weight: 600; margin-bottom: 6px;",
                 "No matches with current filters"),
        tags$div(style = "font-size: 13px; color: #666; margin-bottom: 16px;",
                 if (length(active_filters) > 0) {
                   "Your category filters are excluding every suburb. Try removing one or widening the region selection."
                 } else {
                   "Try widening your region selection — the current region tree has no allowed suburbs."
                 }),
        active_chips,
        actionButton("clear_filters_empty", "Clear all filters",
                     class = "btn btn-primary btn-sm",
                     icon = icon("rotate-left"))
      ))
    }
    n <- top_n_active()
    top <- d |> arrange(rank) |> head(n)
    if (!nrow(top)) return(NULL)

    raw_cols <- grep("^raw_", names(top), value = TRUE)

    multi_ref       <- length(current_ref()) > 1
    ref_name_lookup <- if (multi_ref) {
      ref |>
        filter(suburb_code_2021 %in% current_ref()) |>
        transmute(code = suburb_code_2021,
                  label = paste0(display_name, ", ",
                                 state_abbr[state_name_2021])) |>
        { \(d) setNames(d$label, d$code) }()
    } else NULL

    # Reference info row for the comparison strip (primary ref only)
    ref_info_row <- get_info(current_ref()[1])

    rows <- lapply(seq_len(nrow(top)), function(i) {
      r <- as.data.frame(top)[i, , drop = FALSE]
      code <- r$suburb_b
      raw_vals <- as.list(r[, raw_cols, drop = FALSE])
      names(raw_vals) <- sub("^raw_", "", names(raw_vals))

      name <- suburb_code_to_name[[code]] %||% code
      abbr <- suburb_code_to_abbr[[code]] %||% ""

      winning_html <- if (multi_ref && !is.null(r$winning_ref) &&
                         !is.na(r$winning_ref) &&
                         r$winning_ref %in% names(ref_name_lookup)) {
        tags$span(style = "font-size: 11px; color: #1f77b4; margin-left: 8px;",
                  sprintf("· most similar to %s", ref_name_lookup[[r$winning_ref]]))
      } else NULL

      rea_url <- build_realestate_url(name, abbr)

      tags$div(style = paste(
        "border: 1px solid #e0e0e0; border-radius: 6px;",
        "margin-bottom: 10px; padding: 8px 12px;",
        "background: white;"),
        # Header row
        tags$div(style = "display: flex; align-items: baseline; gap: 8px;
                          margin-bottom: 6px; flex-wrap: wrap;",
          tags$span(style = "font-weight: bold; font-size: 13px; color: #666;
                            min-width: 32px;",
                    sprintf("#%d", r$rank)),
          tags$span(style = "font-weight: bold; font-size: 16px;",
                    sprintf("%s, %s", name, abbr)),
          tags$span(style = "font-size: 15px; color: #1f77b4; font-weight: 600;",
                    sprintf("%.0f%%", r$match * 100)),
          tags$span(style = "font-size: 11px; color: #888;",
                    sprintf("nat. rank %d · %s",
                            r$national_rank, fmt_pct(r$national_pct))),
          winning_html,
          tags$div(style = "margin-left: auto; display: flex; gap: 8px;",
            tags$a(href = sprintf(
                     "https://www.google.com/maps?q=%f,%f&z=11",
                     unname(suburb_centroid_lat[code]),
                     unname(suburb_centroid_lng[code])),
                   target = "_blank", rel = "noopener noreferrer",
                   style = "font-size: 11px;",
                   title = "Open this suburb in Google Maps",
                   "→ Show on map"),
            tags$a(href = "#", style = "font-size: 11px;",
                   onclick = sprintf(
                     "Shiny.setInputValue('set_ref','%s',{priority:'event'});return false;",
                     code),
                   "→ Set as reference"),
            tags$a(href = rea_url, target = "_blank",
                   rel = "noopener noreferrer", style = "font-size: 11px;",
                   "→ realestate.com.au")
          )
        ),
        # "Why this match" — top 3 informative characteristics (with structural
        # ~100% matches called out separately so they don't dominate)
        tags$div(style = "font-size: 12px; color: #555; margin: 2px 0 8px 0;
                          font-style: italic;",
          format_why_sentence(raw_vals)
        ),
        # Comparison strip — match's headline archetypes, ticked when they
        # equal the reference suburb's (data: suburb_info summaries)
        build_comparison_strip(ref_info_row, get_info(code)),
        # Body: scoring chart (left) + inline info (right). The mini-map has
        # been retired; clicking "Show on map" opens a leaflet popup that
        # shows the suburb in regional context.
        tags$div(style = "display: flex; gap: 12px; align-items: flex-start;",
          tags$div(style = "flex: 2 1 0; min-width: 0;",
                   list_chart_svg(raw_vals, r$match)),
          tags$div(style = "flex: 4 1 0; min-width: 0;",
                   build_info_panel(code, ref_code = current_ref(),
                                    show_header = FALSE,
                                    groups = c("Place", "People", "Diversity",
                                               "Housing", "Socioeconomic",
                                               "Work")))
        )
      )
    })

    tagList(
      tags$div(style = "font-size: 11px; color: #666; margin-bottom: 8px;",
               sprintf("Showing top %d matches. Dashed line = match score.",
                       nrow(top))),
      rows
    )
  })


  # --- "Set as reference" from a popup link ------------------------------
  observeEvent(input$set_ref, {
    id <- input$set_ref; req(id)
    updateVirtualSelect(session = session, inputId = "ref_sub", selected = id)
  })

  # --- Suburb info side panel ---------------------------------------------
  # The map-view side panel updates from three sources:
  #   * polygon click on the matches layer (input$map_shape_click)
  #   * empty-map click that resolved to a suburb (handled below)
  #   * "About" link in a map popup (input$show_info)
  # When nothing has been clicked yet, the panel defaults to showing the
  # current reference suburb so the panel is never empty.

  # Popup tracking — the next click on the basemap (not on a top-N polygon)
  # closes the popup rather than opening a new one. The "X" close button
  # on Leaflet's popup is wired via JS below so the state stays in sync if
  # the user closes manually.
  popup_open      <- reactiveVal(FALSE)
  last_shape_clk  <- reactiveVal(0)  # epoch seconds

  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!is.null(id) && id %in% suburb_info$suburb_code_2021) clicked_suburb(id)
    last_shape_clk(as.numeric(Sys.time()))
    popup_open(TRUE)
  })

  observeEvent(input$show_info, {
    req(input$show_info)
    clicked_suburb(input$show_info)
  })

  observeEvent(input$`__popup_x_close`, {
    popup_open(FALSE)
  })

  # --- Overview "pin a suburb on the map" ---------------------------------
  # Clicking a suburb name in the similarity overview drops a single marker
  # at that suburb's centroid. Only one marker exists at a time (subsequent
  # clicks replace it). Clicking the marker itself removes it.
  observeEvent(input$overview_pin, {
    code <- input$overview_pin; req(code)
    hit <- shp_suburb |> dplyr::filter(suburb_code_2021 == code)
    if (!nrow(hit)) return()
    co <- sf::st_coordinates(sf::st_centroid(hit))[1, ]
    nm  <- suburb_code_to_name[[code]] %||% code
    abbr <- suburb_code_to_abbr[[code]] %||% ""
    leafletProxy("map") |>
      clearGroup("overview_pin") |>
      addMarkers(
        lng = co[1], lat = co[2],
        layerId = "overview_pin_marker", group = "overview_pin",
        label = sprintf("%s, %s  \u2715  (click marker to remove)",
                        nm, abbr),
        labelOptions = labelOptions(
          permanent = TRUE, direction = "right",
          offset = c(12, 0),
          style = list("background-color" = "white",
                       "border" = "1px solid #888",
                       "padding" = "2px 6px",
                       "font-size" = "12px",
                       "cursor" = "pointer")))
  })

  observeEvent(input$map_marker_click, {
    if (isTRUE(input$map_marker_click$id == "overview_pin_marker")) {
      leafletProxy("map") |> clearGroup("overview_pin")
    }
  })

  output$map_side_panel <- renderUI({
    code <- clicked_suburb() %||% current_ref()[1]
    if (is.null(code)) {
      return(tags$div(style = "padding: 12px; color: #888; font-size: 13px;",
        "Click any suburb on the map to see its details here."))
    }
    build_info_panel(code, ref_code = current_ref(), show_header = TRUE)
  })

  # --- Click on empty map: find suburb, show popup with "set as reference"
  observeEvent(input$map_click, {
    cl <- input$map_click; req(cl$lng, cl$lat)

    # If the user just clicked a top-N polygon, map_click fires alongside
    # map_shape_click — let the polygon's own popup show, don't double up.
    if (as.numeric(Sys.time()) - last_shape_clk() < 0.2) return()

    # If a popup is already open and this click is on the basemap rather
    # than another mapped polygon, just close it. The next click can then
    # open something new — standard "click outside to dismiss" expectation.
    if (popup_open()) {
      leafletProxy("map") |> clearGroup("clickpop") |> clearPopups()
      popup_open(FALSE)
      return()
    }

    pt  <- st_sfc(st_point(c(cl$lng, cl$lat)), crs = 4326)
    idx <- which(st_within(pt, shp_suburb, sparse = FALSE)[1, ])
    if (!length(idx)) return()

    hit  <- shp_suburb[idx[1], ]
    code <- hit$suburb_code_2021
    nm   <- suburb_code_to_name[[code]] %||% hit$suburb_name_2021
    abbr <- suburb_code_to_abbr[[code]] %||% ""

    # Feed the info panel regardless of whether a popup can be shown
    if (code %in% suburb_info$suburb_code_2021) clicked_suburb(code)

    cd <- map_data()
    if (!is.null(cd) && code %in% cd$suburb_b) return()  # mapped polygon handles its own popup

    # Low-population suburb -> no comparison possible
    if (!(code %in% allowed_codes_all)) {
      leafletProxy("map") |>
        clearGroup("clickpop") |>
        addPopups(lng = cl$lng, lat = cl$lat,
                  popup = build_popup_low_pop(nm, abbr),
                  group = "clickpop")
      popup_open(TRUE)
      return()
    }

    cdf <- match_df()
    row <- if (!is.null(cdf)) cdf[cdf$suburb_b == code, , drop = FALSE] else NULL

    html <- if (!is.null(row) && nrow(row) == 1) {
      raw_cols <- grep("^raw_", names(row), value = TRUE)
      vals <- as.list(as.data.frame(row)[, raw_cols, drop = FALSE])
      names(vals) <- sub("^raw_", "", names(vals))
      win_label <- if (length(current_ref()) > 1) {
        r <- ref |> filter(suburb_code_2021 == row$winning_ref)
        if (nrow(r) == 1) paste0(r$display_name, ", ",
                                 state_abbr[r$state_name_2021]) else NULL
      } else NULL
      build_popup(nm, abbr, row$match, row$rank,
                  row$national_rank, row$national_pct, vals, code,
                  winning_ref_label = win_label)
    } else build_popup_unranked(nm, abbr, code)

    leafletProxy("map") |>
      clearGroup("clickpop") |>
      addPopups(lng = cl$lng, lat = cl$lat, popup = html, group = "clickpop")
    popup_open(TRUE)
  })
}

shinyApp(ui, server, enableBookmarking = "url")
