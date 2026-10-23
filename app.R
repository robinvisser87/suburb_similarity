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
# distance, clamped to >= 0). 0.5 isn't an option. Use "similar" or
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

# Dream-suburb mode: which dims belong to each of the four user-facing
# themes. This is also now the grouping used for match scoring itself
# (see compute_match): the overall match is the average of whichever of
# these four themes have at least one selected indicator. Employment
# sits in "urban" because the user-facing question is about urban
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

# Same 4-theme colour chips used in the Match Settings modal, hoisted to
# top level so the new suburb-comparison table (below) can reuse them too.
theme_group_colours <- c(
  people    = "#1C8356",
  urban     = "#5A5156",
  nature    = "#325A9B",
  amenities = "#B10DA1"
)

# Similarity is now always the average of each active theme's own mean
# (people / urban / nature / amenities, per dream_theme_dims above), not a
# flat mean of every individual indicator. A theme with zero selected
# indicators is skipped entirely rather than pulling the score toward 0.
# There is no more people/place "Focus" preset. All active themes are
# weighted equally.

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

# Colours are generated as tints WITHIN each theme's own colour (rather
# than 23 unrelated hues), so a chart mixing dimensions from different
# themes reads first by theme family, then by individual dimension -
# e.g. every Amenities dim is a shade of the same magenta/purple, every
# Nature & climate dim a shade of the same navy blue.
make_theme_shades <- function(base_hex, n) {
  if (n <= 1) return(rep(base_hex, max(n, 1)))
  # Interpolate from the theme's own colour toward a light tint of itself.
  # Capped at 70 (not 100 = white) so even the lightest shade in a large
  # theme (Amenities has 9 dims) stays a recognisable tint rather than
  # washing out - same lesson as the earlier fix to the map's rank-colour
  # palette, which had the same near-white problem at its light end.
  ramp <- grDevices::colorRampPalette(c(base_hex, "#FFFFFF"))(100)
  idx  <- round(seq(1, 70, length.out = n))
  ramp[idx]
}

dim_colors <- unlist(lapply(names(dream_theme_dims), function(theme_id) {
  dims   <- sort(dream_theme_dims[[theme_id]])
  shades <- make_theme_shades(theme_group_colours[[theme_id]], length(dims))
  setNames(shades, dims)
}))

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
# Shapefiles stay as local-repo reads. They're committed alongside the
# code in data/other/ since they're small and static. Only the larger,
# more volatile parquet data lives on R2.
shp_suburb <- readRDS("data/other/shp_suburb") |> st_transform(4326)
shp_state  <- readRDS("data/other/shp_state")  |> st_transform(4326)
shp_gcc    <- readRDS("data/other/shp_gcc")    |> st_transform(4326)
shp_sa4    <- readRDS("data/other/shp_sa4")    |> st_transform(4326)

# Suburb centroids, used to build "Show on map" links that open
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
# suburb_code_2021. Built once offline and saved as a single parquet file,
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

# ===== Suburb profile panel: sentence + charts ============================
# Replaces the old text-box info panel in the map side panel and list view.
# Also drives the Compare page (build_compare_charts). build_info_panel()
# above is no longer called anywhere and can be deleted.
#
# Data:
#   explanatory/suburb_summary.parquet  one summary_highlight sentence per suburb
#   explanatory/chart_data.parquet      long format: suburb x subdimension x
#                                       category (x weather_month) -> value
#
# Everything is plain HTML/CSS plus inline SVG for the two monthly charts, so
# there are no charting dependencies. Styles live in the .st-* CSS block in
# the UI head; tab switching is the small stApplyTabs() script there too.

suburb_summary <- arrow::read_parquet(r2_path("explanatory/suburb_summary.parquet")) |>
  as_tibble() |>
  mutate(suburb_code_2021 = as.character(suburb_code_2021))
summary_lookup <- setNames(suburb_summary$summary_highlight,
                           suburb_summary$suburb_code_2021)

chart_data <- arrow::read_parquet(r2_path("explanatory/chart_data.parquet")) |>
  as_tibble() |>
  transmute(code     = as.character(suburb_code_2021),
            sub      = as.character(subdimension),
            category = as.character(category),
            month    = as.character(weather_month),
            valtype  = as.character(valtype),
            value    = as.numeric(value)) |>
  filter(!is.na(code), !is.na(value))

# Rain is stored as average mm per day for each month; show mm per month.
# Guarded on valtype so a future chart_data that already stores monthly
# totals isn't converted twice.
local({
  month_days <- c(Jan = 31, Feb = 28.25, Mar = 31, Apr = 30, May = 31, Jun = 30,
                  Jul = 31, Aug = 31, Sep = 30, Oct = 31, Nov = 30, Dec = 31)
  i <- chart_data$sub == "Rain" & chart_data$valtype == "monthly_median"
  chart_data$value[i] <<- chart_data$value[i] * month_days[chart_data$month[i]]
})

# Percentile rows have no category; use the subdimension name so every row
# can be looked up the same way.
chart_data$category <- coalesce(chart_data$category, chart_data$sub)
pct_subs  <- unique(chart_data$sub[chart_data$valtype == "percentile"])
chart_data$valtype <- NULL

# Which share subdimensions hold a suburb's full distribution (missing
# category = 0) rather than only its top N. Industry / Occupation /
# Vegetation are stored as top N in the current chart_data; once the
# generator stores them in full this flips automatically and the selected
# suburb gets true values for the reference suburb's top categories.
topn_n <- c(Industry = 5, Occupation = 5, Vegetation = 3)
full_dist_subs <- local({
  share_subs <- setdiff(unique(chart_data$sub[is.na(chart_data$month)]), pct_subs)
  per <- chart_data |>
    filter(sub %in% names(topn_n)) |>
    count(sub, code) |>
    group_by(sub) |> summarise(med = median(n), .groups = "drop")
  full_topn <- per$sub[per$med > topn_n[per$sub]]
  c(setdiff(share_subs, names(topn_n)), full_topn)
})
all_cats <- chart_data |>
  filter(sub %in% full_dist_subs) |>
  distinct(sub, category) |>
  { \(d) split(d$category, d$sub) }()

# Row index per suburb: fast lookup without filtering 1.2M rows each time.
chart_idx <- split(seq_len(nrow(chart_data)), chart_data$code)

suburb_series <- function(code) {
  i <- chart_idx[[code]]
  if (is.null(i)) return(NULL)
  chart_data[i, c("sub", "category", "month", "value")]
}

# Add 0 rows for categories a suburb doesn't have (full-distribution
# subdimensions only, and only where the suburb has some data for it).
zero_fill <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  have <- intersect(unique(d$sub), full_dist_subs)
  add <- lapply(have, function(s) {
    miss <- setdiff(all_cats[[s]], d$category[d$sub == s])
    if (length(miss)) tibble(sub = s, category = miss, month = NA_character_, value = 0)
  })
  bind_rows(d, add)
}

# Australian benchmark: median across suburbs for each category (and month).
# Zero-filled for full distributions so suburbs without a category count.
# Percentiles need no lookup: the national median is the 50th by definition.
aus_median <- local({
  monthly <- chart_data |>
    filter(!is.na(month)) |>
    group_by(sub, category, month) |>
    summarise(value = median(value), .groups = "drop")
  shares <- chart_data |> filter(is.na(month), !sub %in% pct_subs)
  n_sub  <- shares |> distinct(sub, code) |> count(sub, name = "n_sub")
  share_med <- shares |>
    group_by(sub, category) |>
    summarise(vals = list(value), .groups = "drop") |>
    left_join(n_sub, by = "sub") |>
    mutate(value = pmap_dbl(list(vals, n_sub, sub), function(v, n, s) {
      if (s %in% full_dist_subs) median(c(v, rep(0, max(0, n - length(v)))))
      else median(v)
    }), month = NA_character_) |>
    select(sub, category, month, value)
  bind_rows(monthly, share_med)
})

# Reference data, cached per reference set: mean across the reference
# suburbs (lo/hi = range, used for the temperature band) plus each
# reference's own rows (used for the percentile dots).
profile_ref_cache <- new.env(parent = emptyenv())
profile_ref_data <- function(ref_codes) {
  ref_codes <- unique(ref_codes[!is.na(ref_codes)])
  key <- paste(sort(ref_codes), collapse = ",")
  if (!is.null(profile_ref_cache[[key]])) return(profile_ref_cache[[key]])
  each <- bind_rows(lapply(ref_codes, function(cd) {
    d <- zero_fill(suburb_series(cd))
    if (is.null(d) || !nrow(d)) NULL else mutate(d, code = cd)
  }))
  agg <- if (nrow(each)) {
    each |>
      group_by(sub, category, month) |>
      summarise(lo = min(value), hi = max(value), value = mean(value),
                .groups = "drop")
  } else NULL
  out <- list(agg = agg, each = each, codes = ref_codes,
              n = length(unique(each$code)))
  profile_ref_cache[[key]] <- out
  out
}

# ---- Chart definitions ----------------------------------------------------
# Tabs and the charts in each follow the theme / subdimension structure of
# chart_data. order: explicit category order, or "desc" (by the reference's
# value). top_n: show the reference's top N categories.
profile_tabs <- list(
  people = list(label = "People & culture",       colour = "#1C8356",
                charts = c("age", "household", "birth", "education", "lfs",
                           "voting")),
  urban  = list(label = "Urban fabric & economy", colour = "#5A5156",
                charts = c("dwelling", "bedrooms", "housing_cost",
                           "industry", "occupation")),
  nature = list(label = "Nature & climate",       colour = "#325A9B",
                charts = c("temp", "rain", "veg"))
)

profile_charts <- list(
  age = list(title = "Age", type = "bar", subs = "Age",
             order = c("Children 0-14", "Youth 15-24", "Young adults 25-34",
                       "Adults 35-54", "Mature 55-64", "Seniors 65-84",
                       "Elderly 85+")),
  household = list(title = "People by household role", type = "bar",
                   subs = "Household composition", order = "desc"),
  birth = list(title = "Region of birth", type = "bar",
               subs = "Region of birth", order = "desc"),
  education = list(title = "Highest qualification", type = "bar",
                   subs = "Educational attainment",
                   order = c("Postgraduate degree", "Bachelor degree",
                             "Diploma / Adv Diploma", "Certificate III/IV",
                             "Certificate I/II", "No post-school qualification")),
  # Household income sits in this card (same Socioeconomic dimension) so the
  # People tab is 6 cards / 3 rows and fits the side panel without scrolling.
  lfs = list(title = "Work & income", type = "bar",
             subs = "Labour force status",
             order = c("Employed full-time", "Employed part-time",
                       "Employed away", "Unemployed", "Not in labour force"),
             extra = "income"),
  income = list(title = "Household income", type = "pct",
                subs = "Household income",
                labels = c("Household income" = "Household income")),
  voting = list(title = "Primary vote, 2025 federal election", type = "bar",
                subs = "Voting",
                order = c("Labor", "Coalition", "Greens", "Independent", "Other")),
  dwelling = list(title = "Dwelling type", type = "bar", subs = "Dwelling type",
                  order = c("Separate house", "Townhouse / terrace",
                            "Flat / apartment", "Other dwelling")),
  bedrooms = list(title = "Number of bedrooms", type = "bar",
                  subs = "Number of bedrooms",
                  order = c("Studio / no bedroom", "1 bedroom", "2 bedrooms",
                            "3 bedrooms", "4 bedrooms", "5+ bedrooms")),
  housing_cost = list(title = "Housing costs", type = "pct",
                      subs = c("Monthly mortgage", "Weekly rent"),
                      labels = c("Monthly mortgage" = "Mortgage",
                                 "Weekly rent" = "Rent")),
  industry = list(title = "Top industries", type = "bar", subs = "Industry",
                  top_n = 5),
  occupation = list(title = "Top occupations", type = "bar", subs = "Occupation",
                    top_n = 5),
  temp = list(title = "Temperature (°C, avg daily max / min)", type = "temp",
              subs = "Temperature"),
  rain = list(title = "Rainfall (mm per month)", type = "rain", subs = "Rain"),
  veg  = list(title = "Top native vegetation", type = "bar", subs = "Vegetation",
              top_n = 3, no_aus = TRUE)
)

# ---- Chart renderers (return HTML strings) ----------------------------------

st_col_ref <- "#333333"
st_col_sel <- "#2C7FB8"
st_col_aus <- "#E07B39"

st_esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

st_fmt_share <- function(v) {
  ifelse(is.na(v), "–",
         ifelse(v > 0 & v < 0.005, "<1%", sprintf("%.0f%%", v * 100)))
}

st_ordinal <- function(p) {
  if (is.na(p)) return("–")
  n <- round(p * 100)
  suf <- if (n %% 100 %in% 11:13) "th" else
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suf)
}

st_pick <- function(d, subs) {
  if (is.null(d) || !nrow(d)) return(d[0, , drop = FALSE])
  d[d$sub %in% subs, , drop = FALSE]
}

st_val <- function(d, cats, month = NULL) {
  if (is.null(d) || !nrow(d)) return(rep(NA_real_, length(cats)))
  if (is.null(month)) d$value[match(cats, d$category)]
  else d$value[match(paste(cats, month), paste(d$category, d$month))]
}

st_card <- function(title, body, note = NULL) {
  paste0('<div class="st-card"><div class="st-card-title">', st_esc(title), '</div>',
         if (!is.null(note)) paste0('<div class="st-card-note">', st_esc(note), '</div>'),
         body, '</div>')
}

st_nodata <- '<div class="st-empty">No data</div>'


# A "series" is one coloured thing drawn on every chart:
#   list(label, col, d = rows (sub, category, month, value [, lo, hi]),
#        each = per-reference rows for percentile dots (optional),
#        band = TRUE to shade the lo-hi range on the temperature chart)
# The side panel / list view pass reference + selected; the Compare page
# passes one series per suburb. val_label names the series whose value is
# printed at the right of each row (NULL = no value column).

st_ser_pick <- function(ser, subs) {
  lapply(ser, function(x) {
    x$d <- st_pick(x$d, subs)
    if (!is.null(x$each)) x$each <- st_pick(x$each, subs)
    x
  })
}
st_has <- function(ser) Filter(function(x) !is.null(x$d) && nrow(x$d) > 0, ser)

# Horizontal bars, one per series, with an orange tick for the Australian
# median. basis = rows used to order categories / pick the top N
# (the reference, or mean of references).
st_bar_chart <- function(spec, ser, a, basis = NULL, val_label = NULL) {
  ser <- st_has(ser)
  if (!length(ser)) return(st_nodata)
  if (is.null(basis) || !nrow(basis)) basis <- ser[[1]]$d
  pool <- unique(unlist(lapply(ser, function(x) x$d$category)))
  cats <- if (!is.null(spec$top_n)) {
    head(basis$category[order(-basis$value)], spec$top_n)
  } else if (identical(spec$order, "desc")) {
    pool[order(-coalesce(st_val(basis, pool), 0))]
  } else {
    c(intersect(spec$order, pool), setdiff(pool, spec$order))
  }
  vals <- lapply(ser, function(x) st_val(x$d, cats))
  av   <- if (isTRUE(spec$no_aus)) rep(NA_real_, length(cats)) else st_val(a, cats)
  xmax <- max(c(unlist(vals), av, 0.01), na.rm = TRUE)
  w    <- function(v) sprintf("%.1f", 100 * pmin(v, xmax) / xmax)
  n    <- length(ser)
  h    <- if (n <= 2) 6 else if (n <= 4) 5 else if (n <= 6) 4 else 3
  vi   <- if (is.null(val_label)) NA else match(val_label, vapply(ser, `[[`, "", "label"))
  row_class <- if (is.null(val_label)) "st-row st-row-nov" else "st-row"
  rows <- vapply(seq_along(cats), function(i) {
    tip <- paste(c(
      vapply(seq_len(n), function(k) paste0(ser[[k]]$label, ": ", st_fmt_share(vals[[k]][i])), ""),
      if (!is.na(av[i])) paste("Australia (median):", st_fmt_share(av[i]))),
      collapse = "\n")
    bars <- paste(vapply(seq_len(n), function(k) {
      v <- vals[[k]][i]
      sprintf('<div class="st-bar" style="width:%s%%; height:%dpx; background:%s;"></div>',
              if (is.na(v)) "0" else w(v), h, ser[[k]]$col)
    }, ""), collapse = "")
    paste0(
      '<div class="', row_class, '"', if (n > 2) ' style="padding:3px 0;"',
      ' title="', st_esc(paste0(cats[i], "\n", tip)), '">',
      '<div class="st-lab">', st_esc(cats[i]), '</div>',
      '<div class="st-track" style="gap:', if (n > 4) 1 else 2, 'px;">', bars,
      if (!is.na(av[i])) paste0('<div class="st-aus" style="left:', w(av[i]), '%"></div>'),
      '</div>',
      if (!is.null(val_label))
        paste0('<div class="st-v">', st_fmt_share(if (is.na(vi)) NA else vals[[vi]][i]), '</div>'),
      '</div>')
  }, character(1))
  paste(rows, collapse = "")
}

# National percentile strips: a dot per series (one per reference suburb
# where a series carries several), orange tick at the 50th = Australian median.
st_pct_chart <- function(spec, ser, val_label = NULL) {
  pv <- function(x, sb) {
    if (!is.null(x$each) && nrow(x$each)) x$each$value[x$each$sub == sb]
    else { v <- st_val(x$d, sb); v[!is.na(v)] }
  }
  row_class <- if (is.null(val_label)) "st-row st-row-nov" else "st-row"
  rows <- vapply(spec$subs, function(sb) {
    vals <- lapply(ser, pv, sb = sb)
    if (!length(unlist(vals))) return("")
    dots <- paste(unlist(lapply(seq_along(ser), function(k) vapply(vals[[k]], function(v) sprintf(
      '<div class="st-dot" style="left:%.1f%%; background:%s;"></div>', 100 * v, ser[[k]]$col), ""))),
      collapse = "")
    tip <- paste(vapply(seq_along(ser), function(k) if (!length(vals[[k]])) "" else
      paste0(ser[[k]]$label, ": ", paste(vapply(vals[[k]], st_ordinal, ""), collapse = ", ")), ""),
      collapse = "\n")
    shown <- if (is.null(val_label)) NULL else {
      k <- match(val_label, vapply(ser, `[[`, "", "label"))
      v <- if (is.na(k)) numeric(0) else vals[[k]]
      if (length(v) == 1) st_ordinal(v) else "–"
    }
    paste0(
      '<div class="', row_class, '" title="', st_esc(paste0(spec$labels[[sb]], " (national percentile)\n", tip)), '">',
      '<div class="st-lab">', st_esc(spec$labels[[sb]]), '</div>',
      '<div class="st-ptrack"><div class="st-pmid"></div>', dots, '</div>',
      if (!is.null(shown)) paste0('<div class="st-v">', shown, '</div>'),
      '</div>')
  }, character(1))
  rows <- rows[nzchar(rows)]
  if (!length(rows)) return(st_nodata)
  paste0(paste(rows, collapse = ""),
         '<div class="', if (is.null(val_label)) "st-axis st-axis-nov" else "st-axis",
         '"><span></span><span>national percentile</span></div>')
}

# Shared SVG frame for the two monthly charts.
st_svg_frame <- function(ymin, ymax, ticks, W, H, ml, mr, mt, mb, body) {
  yy <- function(v) mt + (ymax - v) / (ymax - ymin) * (H - mt - mb)
  grid <- paste(vapply(ticks, function(t) sprintf(
    '<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#eee"/><text x="%d" y="%.1f" class="st-svgt" text-anchor="end">%s</text>',
    ml, W - mr, yy(t), yy(t), ml - 3, yy(t) + 3, format(t)), ""), collapse = "")
  xs <- ml + (seq_len(12) - 0.5) * (W - ml - mr) / 12
  mlab <- paste(sprintf('<text x="%.1f" y="%d" class="st-svgt" text-anchor="middle">%s</text>',
                        xs, H - 3, substr(month.abb, 1, 1)), collapse = "")
  sprintf('<svg viewBox="0 0 %d %d" class="st-svg" preserveAspectRatio="xMidYMid meet">%s%s%s</svg>',
          W, H, grid, mlab, body)
}

st_month_vals <- function(d, v = NULL, col = "value") {
  if (is.null(d) || !nrow(d)) return(rep(NA_real_, 12))
  if (is.null(v)) d[[col]][match(month.abb, d$month)]
  else d[[col]][match(paste(v, month.abb), paste(d$category, d$month))]
}

st_polyline <- function(xs, ys, col, extra = "", sw = 1.8, title = NULL) {
  ok <- !is.na(ys)
  if (!any(ok)) return("")
  sprintf('<polyline fill="none" stroke="%s" stroke-width="%.1f" %s points="%s">%s</polyline>',
          col, sw, extra, paste(sprintf("%.1f,%.1f", xs[ok], ys[ok]), collapse = " "),
          if (is.null(title)) "" else paste0("<title>", st_esc(title), "</title>"))
}

st_temp_chart <- function(ser, a, val_label = NULL) {
  ser <- st_has(ser)
  if (!length(ser)) return(st_nodata)
  W <- 300; H <- 132; ml <- 24; mr <- 26; mt <- 6; mb <- 16
  all_v <- c(unlist(lapply(ser, function(x) c(
               st_month_vals(x$d, "tmax"), st_month_vals(x$d, "tmin"),
               if (isTRUE(x$band)) c(st_month_vals(x$d, "tmax", "hi"), st_month_vals(x$d, "tmin", "lo"))))),
             st_month_vals(a, "tmax"), st_month_vals(a, "tmin"))
  ymin <- floor(min(all_v, na.rm = TRUE) / 5) * 5
  ymax <- ceiling(max(all_v, na.rm = TRUE) / 5) * 5
  if (ymax - ymin < 10) ymax <- ymin + 10
  xs <- ml + (seq_len(12) - 0.5) * (W - ml - mr) / 12
  yy <- function(v) mt + (ymax - v) / (ymax - ymin) * (H - mt - mb)
  sw <- if (length(ser) > 4) 1.4 else 1.8
  band <- function(x, v) {
    lo <- st_month_vals(x$d, v, "lo"); hi <- st_month_vals(x$d, v, "hi")
    if (!isTRUE(x$band) || any(is.na(c(lo, hi)))) return("")
    pts <- c(sprintf("%.1f,%.1f", xs, yy(hi)), rev(sprintf("%.1f,%.1f", xs, yy(lo))))
    sprintf('<polygon fill="%s" fill-opacity="0.13" stroke="none" points="%s"/>',
            x$col, paste(pts, collapse = " "))
  }
  lab_ser <- ser[[max(1, match(val_label %||% "", vapply(ser, `[[`, "", "label")), na.rm = TRUE)]]
  lab <- function(v, txt) {
    y <- st_month_vals(lab_ser$d, v)[12]
    if (is.na(y)) "" else sprintf('<text x="%.1f" y="%.1f" class="st-svgt">%s</text>',
                                  xs[12] + 4, yy(y) + 3, txt)
  }
  body <- paste0(
    paste(vapply(ser, function(x) paste0(band(x, "tmax"), band(x, "tmin")), ""), collapse = ""),
    st_polyline(xs, yy(st_month_vals(a, "tmax")), st_col_aus, 'stroke-dasharray="3,2"', 1.6, "Australia (median) max"),
    st_polyline(xs, yy(st_month_vals(a, "tmin")), st_col_aus, 'stroke-dasharray="3,2"', 1.6, "Australia (median) min"),
    paste(vapply(ser, function(x) paste0(
      st_polyline(xs, yy(st_month_vals(x$d, "tmax")), x$col, sw = sw, title = paste(x$label, "max")),
      st_polyline(xs, yy(st_month_vals(x$d, "tmin")), x$col, sw = sw, title = paste(x$label, "min"))), ""),
      collapse = ""),
    lab("tmax", "max"), lab("tmin", "min"))
  ticks <- seq(ymin, ymax, by = if (ymax - ymin > 20) 10 else 5)
  st_svg_frame(ymin, ymax, ticks, W, H, ml, mr, mt, mb, body)
}

# Columns side by side for up to 3 series; lines beyond that so a Compare
# page with many suburbs stays readable.
st_rain_chart <- function(ser, a) {
  ser <- st_has(ser)
  if (!length(ser)) return(st_nodata)
  W <- 300; H <- 132; ml <- 24; mr <- 6; mt <- 6; mb <- 16
  vals <- lapply(ser, function(x) st_month_vals(x$d))
  av   <- st_month_vals(a)
  top  <- max(c(unlist(vals), av), na.rm = TRUE)
  step <- if (top > 200) 100 else if (top > 80) 50 else if (top > 40) 20 else 10
  ymax <- ceiling(top / step) * step
  gw <- (W - ml - mr) / 12
  xs <- ml + (seq_len(12) - 0.5) * gw
  yy <- function(v) mt + (ymax - v) / ymax * (H - mt - mb)
  k  <- length(ser)
  series_svg <- if (k <= 3) {
    bw <- gw * 0.72 / k
    paste(vapply(seq_len(k), function(j) paste(vapply(seq_len(12), function(i) {
      v <- vals[[j]][i]
      if (is.na(v)) return("")
      sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s, %s: %.0f mm</title></rect>',
              ml + (i - 1) * gw + gw * 0.14 + (j - 1) * bw, yy(v), bw, yy(0) - yy(v),
              ser[[j]]$col, st_esc(ser[[j]]$label), month.abb[i], v)
    }, ""), collapse = ""), ""), collapse = "")
  } else {
    paste(vapply(seq_len(k), function(j)
      st_polyline(xs, yy(vals[[j]]), ser[[j]]$col, sw = 1.4, title = ser[[j]]$label), ""),
      collapse = "")
  }
  aus <- if (all(is.na(av))) "" else paste0(
    st_polyline(xs, yy(av), st_col_aus, 'stroke-dasharray="3,2"', 1.6, "Australia (median)"),
    paste(sprintf('<circle cx="%.1f" cy="%.1f" r="1.8" fill="%s"/>', xs, yy(av), st_col_aus),
          collapse = ""))
  st_svg_frame(0, ymax, seq(0, ymax, by = step), W, H, ml, mr, mt, mb, paste0(series_svg, aus))
}

# One card's body for a chart spec. ser must already be subset to spec$subs.
st_body <- function(spec, ser, basis, val_label) {
  a <- st_pick(aus_median, spec$subs)
  switch(spec$type,
    bar  = st_bar_chart(spec, ser, a, st_pick(basis, spec$subs), val_label),
    pct  = st_pct_chart(spec, ser, val_label),
    temp = st_temp_chart(ser, a, val_label),
    rain = st_rain_chart(ser, a))
}

# Tabs + panes. card_fn(id) returns a card's HTML ("" to skip it);
# note_fn(tab_id) returns an optional note shown above that tab's grid.
st_tabs_html <- function(card_fn, note_fn = function(tab, any_cards) "", grid_class = "st-grid") {
  tab_ids <- names(profile_tabs)
  tabs <- paste0('<div class="st-tabs">', paste(vapply(seq_along(tab_ids), function(i) {
    t <- profile_tabs[[tab_ids[i]]]
    sprintf('<button type="button" class="st-tab%s" data-tab="%s" style="--tabc:%s;">%s</button>',
            if (i == 1) " active" else "", tab_ids[i], t$colour, st_esc(t$label))
  }, ""), collapse = ""), '</div>')
  panes <- paste(vapply(seq_along(tab_ids), function(i) {
    cards <- vapply(profile_tabs[[tab_ids[i]]]$charts, card_fn, "")
    sprintf('<div class="st-pane" data-tab="%s"%s>%s<div class="%s">%s</div></div>',
            tab_ids[i], if (i == 1) "" else ' style="display:none;"',
            note_fn(tab_ids[i], any(nzchar(cards))), grid_class,
            paste(cards, collapse = ""))
  }, ""), collapse = "")
  HTML(paste0(tabs, panes))
}

st_card_for <- function(id, ser, basis, val_label) {
  spec <- profile_charts[[id]]
  body <- st_body(spec, st_ser_pick(ser, spec$subs), basis, val_label)
  if (!is.null(spec$extra)) {
    x <- profile_charts[[spec$extra]]
    body <- paste0(body, '<div class="st-divider"></div>',
                   st_body(x, st_ser_pick(ser, x$subs), basis, val_label))
  }
  st_card(spec$title, body)
}

st_empty_rows <- tibble(sub = character(), category = character(),
                        month = character(), value = numeric())

# ---- Side panel / list view ---------------------------------------------------
# code:       the suburb being described (reference by default, or the
#             clicked / listed suburb)
# ref_codes:  the active reference suburb(s)
# The displayed suburb is drawn as the blue "selected" series unless it is
# the only reference, in which case the charts show reference vs Australia.
build_profile_panel <- function(code, ref_codes, show_header = TRUE,
                                show_hint = TRUE, anchor_text = NULL) {
  ref_codes <- unique(ref_codes[!is.na(ref_codes)])
  name   <- suburb_code_to_name[[code]] %||% code
  abbr   <- suburb_code_to_abbr[[code]] %||% ""
  is_ref <- code %in% ref_codes
  rd     <- profile_ref_data(ref_codes)
  show_sel <- !(is_ref && length(ref_codes) == 1)
  sel    <- (if (show_sel) zero_fill(suburb_series(code))) %||% st_empty_rows
  ref_agg <- rd$agg %||% st_empty_rows

  ref_name  <- function(cd) sprintf("%s, %s", suburb_code_to_name[[cd]] %||% cd,
                                    suburb_code_to_abbr[[cd]] %||% "")
  ref_label <- if (rd$n == 1) paste(ref_name(rd$codes[1]), "(reference)")
               else sprintf("Reference (mean of %d)", rd$n)
  sel_label <- sprintf("%s, %s", name, abbr)
  sel_has_census <- show_sel && any(sel$sub %in% c("Age", "Dwelling type"))

  ser <- c(
    if (rd$n > 0) list(list(label = ref_label, col = st_col_ref, d = ref_agg,
                            each = rd$each, band = rd$n > 1)),
    if (show_sel) list(list(label = sel_label, col = st_col_sel, d = sel)))
  val_label <- if (show_sel) sel_label else ref_label

  header <- if (show_header) {
    tags$div(class = "st-head",
      tags$span(class = "st-name", sel_label),
      if (is_ref) tags$span(class = "st-badge", "Reference suburb")
      else tags$span(class = "st-badge st-badge-sel", "Selected"),
      if (!is.null(anchor_text)) tags$span(class = "anchor-note", anchor_text),
      if (show_hint) tags$span(class = "st-hint",
                               "click any suburb on the map to update"))
  }
  sentence <- summary_lookup[[code]] %||% NULL
  legend <- tags$div(class = "st-legend",
    lapply(ser, function(x) tags$span(
      tags$span(class = "st-sw", style = sprintf("background:%s;", x$col)), x$label)),
    tags$span(tags$span(class = "st-sw st-sw-aus"), "Australia (median)"))

  # The panel describes the displayed suburb: when it has no data for a
  # chart (e.g. no census below 50 residents) the card is left out rather
  # than showing the reference alone.
  card_fn <- function(id) {
    spec <- profile_charts[[id]]
    if (show_sel && !nrow(st_pick(sel, spec$subs))) return("")
    st_card_for(id, ser, ref_agg, val_label)
  }
  note_fn <- function(tab, any_cards) {
    if (show_sel && !sel_has_census && tab != "nature")
      sprintf('<div class="st-card-note">No census data for %s (fewer than 50 residents)%s.</div>',
              st_esc(name), if (any_cards) "; showing what is available" else "")
    else ""
  }

  tags$div(class = "st-panel",
    header,
    if (!is.null(sentence)) tags$div(class = "st-sentence", sentence),
    legend,
    st_tabs_html(card_fn, note_fn))
}

# ---- Compare page -------------------------------------------------------------
# Every suburb picked on the Compare page is its own series in the same
# charts. References are drawn in greys (matching the reference colour
# elsewhere in the app), compared suburbs in distinct colours. Top-N and
# "desc" category orders follow the mean of the references.
compare_ref_greys <- c("#222222", "#5F5F5F", "#8F8F8F", "#B5B5B5")
compare_palette   <- c("#2C7FB8", "#1C8356", "#B10DA1", "#D62728", "#17BECF",
                       "#8C564B", "#9467BD", "#BCBD22", "#E377C2", "#7F7F7F")

build_compare_charts <- function(ref_codes, other_codes) {
  ref_codes   <- unique(ref_codes[!is.na(ref_codes)])
  other_codes <- setdiff(unique(other_codes[!is.na(other_codes)]), ref_codes)
  codes <- c(ref_codes, other_codes)
  if (!length(codes)) return(NULL)
  is_ref <- codes %in% ref_codes
  n_ref  <- sum(is_ref)
  cols <- c(if (n_ref <= length(compare_ref_greys)) compare_ref_greys[seq_len(n_ref)]
            else compare_palette[seq_len(n_ref)],
            setdiff(compare_palette, if (n_ref > length(compare_ref_greys))
              compare_palette[seq_len(n_ref)])[seq_along(other_codes)])
  label <- function(cd) sprintf("%s, %s", suburb_code_to_name[[cd]] %||% cd,
                                suburb_code_to_abbr[[cd]] %||% "")
  ser <- lapply(seq_along(codes), function(i) list(
    label = label(codes[i]), col = cols[i],
    d = zero_fill(suburb_series(codes[i])) %||% st_empty_rows))
  basis <- profile_ref_data(ref_codes)$agg %||% st_empty_rows

  key <- tags$div(class = "st-compare-key",
    lapply(seq_along(codes), function(i) tags$div(class = "st-compare-item",
      tags$div(class = "st-compare-name",
        tags$span(class = "st-sw", style = sprintf("background:%s;", cols[i])),
        ser[[i]]$label,
        if (is_ref[i]) tags$span(class = "st-badge", "Reference")),
      if (!is.null(summary_lookup[[codes[i]]]))
        tags$div(class = "st-compare-sentence", summary_lookup[[codes[i]]]))),
    tags$div(class = "st-compare-item st-compare-aus",
      tags$div(class = "st-compare-name",
        tags$span(class = "st-sw st-sw-aus"), "Australia (median)")))

  card_fn <- function(id) {
    spec <- profile_charts[[id]]
    if (!length(st_has(st_ser_pick(ser, spec$subs)))) return("")
    st_card_for(id, ser, basis, NULL)
  }

  tags$div(class = "st-panel st-compare",
    tags$div(class = "st-compare-intro",
             "Hover over a bar, dot or line for exact values."),
    key,
    st_tabs_html(card_fn, grid_class = "st-grid st-grid-wide"))
}

# ===== Anchor: "near a place" =============================================
# Optional location preference. Users pick up to 3 anchor suburbs and a
# radius; each candidate's similarity is multiplied by a proximity factor:
#   prox  = 0.5 ^ ((distance_km / radius_km) ^ 2)   1 at the anchor, 0.5 at
#                                                   the radius, ~0.06 at 2x
#   score = similarity * prox ^ beta                beta = strength setting
# Results are ranked by score, but the match % shown everywhere stays pure
# similarity, with the straight-line distance shown next to it. With several
# anchors, "near any" uses the best proximity and "near all" multiplies them.
# Off unless the user opens "near a place" and picks a suburb; hidden in
# dream-suburb mode.

anchor_radius_choices   <- c("10 km" = "10", "20 km" = "20",
                             "40 km" = "40", "80 km" = "80")
anchor_strength_choices <- c("A little" = "little", "Normal" = "normal",
                             "Strong" = "strong")
anchor_strength_beta    <- c(little = 0.5, normal = 1, strong = 2)
anchor_max              <- 3
anchor_colour           <- "#D6336C"

# Any suburb can be an anchor, including low-population ones (a workplace
# suburb such as a CBD or industrial area may have few residents).
anchor_choices <- ref |>
  filter(suburb_code_2021 %in% names(suburb_centroid_lat)) |>
  mutate(grp = state_abbr[state_name_2021]) |>
  arrange(grp, display_name) |>
  split(~ grp) |>
  map(~ setNames(.x$suburb_code_2021, .x$display_name))

haversine_km <- function(lat1, lng1, lat2, lng2) {
  rad  <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlng <- (lng2 - lng1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlng / 2)^2
  6371 * 2 * asin(pmin(1, sqrt(a)))
}

# Straight-line km from each of `codes` (rows) to each anchor (columns),
# centroid to centroid.
anchor_dist_matrix <- function(codes, anchor_codes) {
  lat <- unname(suburb_centroid_lat[codes])
  lng <- unname(suburb_centroid_lng[codes])
  m <- vapply(anchor_codes, function(a)
         haversine_km(lat, lng, suburb_centroid_lat[[a]], suburb_centroid_lng[[a]]),
       numeric(length(codes)))
  m <- matrix(m, nrow = length(codes))
  colnames(m) <- anchor_codes
  m
}

# Adds anchor_km (nearest anchor), proximity and score columns. With no
# anchor, score = match so the ranking is unchanged.
apply_anchor <- function(df, anchor) {
  if (is.null(df)) return(df)
  if (is.null(anchor) || !nrow(df)) {
    df$anchor_km <- rep(NA_real_, nrow(df))
    df$proximity <- rep(NA_real_, nrow(df))
    df$score     <- df$match
    return(df)
  }
  d <- anchor_dist_matrix(df$suburb_b, anchor$codes)
  p <- 0.5 ^ ((d / anchor$radius)^2)
  prox <- if (identical(anchor$mode, "all")) apply(p, 1, prod) else apply(p, 1, max)
  df$anchor_km <- apply(d, 1, min)
  df$proximity <- coalesce(prox, 0)
  df$score     <- df$match * df$proximity ^ anchor$beta
  df
}

fmt_km <- function(x) {
  ifelse(is.na(x), "–",
         ifelse(x < 10, sprintf("%.1f km", x), sprintf("%.0f km", x)))
}

anchor_name <- function(code) {
  nm <- suburb_code_to_name[[code]] %||% code
  sub("\\s*\\([^)]*\\)\\s*$", "", nm)   # drop "(Vic.)"-style suffixes
}

# "6.2 km from Kew · 11 km from Carlton". NULL when no anchor is active.
anchor_note <- function(code, anchor, straight_line = FALSE) {
  if (is.null(anchor) || is.null(code)) return(NULL)
  if (code %in% anchor$codes) return("Location anchor")
  d <- anchor_dist_matrix(code, anchor$codes)[1, ]
  parts <- paste(fmt_km(d), "from", vapply(anchor$codes, anchor_name, ""))
  paste0(paste(parts, collapse = " · "),
         if (straight_line) " (straight line)" else "")
}

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
#   similar          : score = x       (identity, match is mean of raw)
#   different        : score = 1 - x   (natural complement)
#   mostly_similar   : score = max(0, 1 - 2 * |x - 0.7|)   (triangular peak)
#   mostly_different : score = max(0, 1 - 2 * |x - 0.3|)   (triangular peak)
# All scores are clamped to [0, 1] defensively.
#
# Aggregation: the match is the mean of each active theme's own mean
# (people / urban / nature / amenities, per dream_theme_dims), not a flat
# mean across every selected indicator. A theme with none of its
# indicators selected is skipped entirely and does not pull the score
# down; the match is simply the average of whichever themes have at
# least one active indicator. All active themes are weighted equally.
compute_match <- function(raw_wide, settings, medians = NULL) {
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

  score_df <- as.data.frame(scored)

  # One mean per theme, using only that theme's selected+scored dims.
  # Themes with zero selected dims contribute NA and are dropped from
  # the final average below (rowMeans(..., na.rm = TRUE) over the
  # theme-means matrix).
  theme_means <- lapply(dream_theme_dims, function(dims) {
    cols <- paste0("score_", intersect(selected, dims))
    cols <- cols[cols %in% names(score_df)]
    if (length(cols) == 0) return(rep(NA_real_, nrow(score_df)))
    rowMeans(as.matrix(score_df[, cols, drop = FALSE]), na.rm = TRUE)
  })
  theme_mean_mat <- do.call(cbind, theme_means)

  scored$match <- rowMeans(theme_mean_mat, na.rm = TRUE)

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
# character vector of category labels; suburbs matching any selected
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
# suburbs. Returns plain text; caller wraps in the appropriate HTML.
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
    # No structural matches: original phrasing
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
# `winning_ref_label` is shown only when more than one reference is selected.
# It identifies which reference produced this match. Pass NULL for
# single-reference mode (no annotation rendered).
build_popup <- function(name, abbr, match, rank, national_rank, national_pct,
                        raw_vals, code, winning_ref_label = NULL,
                        anchor_text = NULL) {
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

  # "Why this match": separates structurally-similar dims (>=99%) from
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
  anchor_html <- if (!is.null(anchor_text)) {
    sprintf("<div style='color:%s;font-size:11px;margin-top:2px;'>&#9679; %s</div>",
            anchor_colour, htmltools::htmlEscape(anchor_text))
  } else ""
  rea_url <- build_realestate_url(name, abbr)
  sprintf(
    "<b>%s, %s</b><br>Match %s &middot; rank %d here
     <div style='color:#555;font-size:11px;margin-top:2px;'>rank %d nationally &middot; %s</div>
     %s%s
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
    anchor_html, winning_html, why_html, bars, code, code, rea_url)
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
fmt_percentile <- function(x) {
  # x is 0-100. 2021 Census dollar figures are stale by 2026, but a
  # suburb's national rank is not, since every suburb was measured in the
  # same 2021 dollars, so percentile is shown instead of a $ amount.
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2014")
  n <- round(x)
  suffix <- if (n %% 10 == 1 && n %% 100 != 11) "st"
    else if (n %% 10 == 2 && n %% 100 != 12) "nd"
    else if (n %% 10 == 3 && n %% 100 != 13) "rd"
    else "th"
  paste0(n, suffix, " percentile (national)")
}

# One labelled fact line inside a group
info_fact <- function(label, value) {
  tags$div(style = "display:flex; justify-content:space-between; gap:8px;
                    font-size:12px; padding:1.5px 0;",
    tags$span(style = "color:#9CA3AF; font-size:11px;", label),
    tags$span(style = "text-align:right; font-weight:600; color:#333333;",
              value))
}

# A titled group of facts (renders as a card in the panel grid)
info_group <- function(title, ..., note = NULL, collapsible = FALSE, group_id = NULL) {
  content <- tagList(...,
    if (!is.null(note))
      tags$div(style = "font-size:10px; color:#aaa; margin-top:4px;
                        font-style:italic;", note))

  card_style <- "flex: 1 1 150px; min-width: 140px; max-width: 240px;
                border: 1px solid #e8e8e8; border-radius: 6px;
                padding: 6px 8px; background: #fcfcfc;"

  if (!collapsible) {
    tags$div(style = card_style,
      tags$div(style = "font-size:10px; font-weight:600; color:#999;
                        text-transform:uppercase; letter-spacing:0.5px;
                        margin-bottom:3px;", title),
      content)
  } else {
    tid <- paste0("infogrp-", group_id %||% gsub("[^a-zA-Z0-9]", "", title))
    tags$div(style = card_style,
      tags$div(
        onclick = sprintf(
          "var el=document.getElementById('%s');
           var open = el.style.display !== 'none';
           el.style.display = open ? 'none' : 'block';
           this.querySelector('.infogrp-caret').textContent = open ? '\u25b8' : '\u25be';",
          tid),
        style = "cursor:pointer; display:flex; align-items:center; gap:5px;
                 font-size:10px; font-weight:600; color:#999;
                 text-transform:uppercase; letter-spacing:0.5px;
                 margin-bottom:3px;",
        tags$span(class = "infogrp-caret",
                  style = "font-size:8px; width:8px;", "\u25be"),
        title),
      tags$div(id = tid, style = "display:block;", content))
  }
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
                                         "Socioeconomic", "Work", "Voting"),
                              collapsible = FALSE) {
  d <- get_info(code)
  name <- suburb_code_to_name[[code]] %||% code
  abbr <- suburb_code_to_abbr[[code]] %||% ""

  if (is.null(d)) {
    return(tags$div(style = "padding: 12px; color: #888; font-size: 13px;",
      sprintf("No detailed information available for %s, %s.", name, abbr)))
  }

  is_ref <- !is.null(ref_code) && length(ref_code) > 0 && code %in% ref_code

  header <- if (show_header) {
    tags$div(style = "display:flex; align-items:baseline; gap:10px;
                      flex-wrap:wrap; margin-bottom:8px;",
      tags$span(style = "font-size:16px; font-weight:bold;",
                sprintf("%s, %s", name, abbr)),
      tags$span(style = "font-size:11px; color:#999;",
                if (is_ref) "Current reference suburb"
                else "click any suburb on the map to update"))
  } else NULL

  # group_id is prefixed with the suburb code so collapse/expand toggles
  # stay independent when multiple suburbs' panels are on the same page
  # at once (e.g. the Compare page), rather than colliding on the group
  # name alone.
  gid <- function(name) paste0(code, "_", name)

  # Build groups as a named list, then keep only those requested. This keeps
  # the source of truth (what each group contains) in one place while letting
  # callers choose which ones to render.
  all_groups <- list(
    Place = info_group("Place", collapsible = collapsible, group_id = gid("place"),
      info_fact("Remoteness",  fmt_na(d$remoteness)),
      info_fact("Density",     fmt_num(d$density, 0, " /km\u00b2")),
      info_fact("Coast",       fmt_na(d$coast_class)),
      info_fact("To coast",    fmt_num(d$dist_to_coast_km, 0, " km")),
      info_fact("Terrain",     fmt_na(d$dominant_slope_class)),
      info_fact("Elevation",   fmt_na(d$dominant_elev_class))),

    Climate = info_group("Climate", collapsible = collapsible, group_id = gid("climate"),
      info_fact("Avg max temp",  fmt_num(d$tmax_annual, 1, "\u00b0C")),
      info_fact("Avg min temp",  fmt_num(d$tmin_annual, 1, "\u00b0C")),
      info_fact("Avg monthly rain", fmt_num(d$annual_rain, 0, " mm")),
      info_fact("Wettest month", fmt_na(d$peak_rain_month)),
      info_fact("Driest month",  fmt_na(d$driest_month))),

    Landscape = info_group("Landscape", collapsible = collapsible, group_id = gid("landscape"),
      info_fact("Setting",        fmt_na(d$landscape_type)),
      info_fact("Natural veg",    fmt_num(d$pct_natural_veg_landcover, 0, "%")),
      info_fact("Dominant veg",   fmt_na(d$dominant_vegetation)),
      info_fact("Water",          fmt_na(d$water_character))),

    People = info_group("People", collapsible = collapsible, group_id = gid("people"),
      info_fact("Population",  fmt_num(d$population, 0)),
      info_fact("Median age",  fmt_num(d$median_age, 0)),
      info_fact("Age profile", fmt_na(d$age_archetype)),
      info_fact("Households",  fmt_na(d$household_archetype))),

    Diversity = info_group("Diversity", collapsible = collapsible, group_id = gid("diversity"),
      info_fact("Birthplace",   fmt_na(d$diversity_archetype)),
      info_fact("Top region",   sprintf("%s (%s)",
                  fmt_na(d$dominant_region),
                  fmt_num(d$pct_dominant_region, 0, "%"))),
      info_fact("Indigenous",   fmt_num(d$pct_indigenous, 1, "%")),
      info_fact("Religion",     fmt_na(d$religion_archetype))),

    Housing = info_group("Housing", collapsible = collapsible, group_id = gid("housing"),
      info_fact("Tenure",          fmt_na(d$tenure_archetype)),
      info_fact("Dwellings",       fmt_na(d$dwelling_archetype)),
      info_fact("Typical size",    fmt_na(d$dominant_bedrooms)),
      info_fact("Mortgage repayments", fmt_percentile(d$mortgage_percentile)),
      info_fact("Rent",                fmt_percentile(d$rent_percentile))),

    Socioeconomic = info_group("Socioeconomic", collapsible = collapsible, group_id = gid("socioeconomic"),
      info_fact("Income",      fmt_na(d$income_archetype)),
      info_fact("Household income", fmt_percentile(d$income_percentile)),
      info_fact("Education",   fmt_na(d$education_archetype)),
      info_fact("Labour force", fmt_na(d$lfs_archetype))),

    Work = info_group("Work", collapsible = collapsible, group_id = gid("work"),
      info_fact("Economy",     fmt_na(d$economy_type)),
      info_fact("Workforce",   fmt_na(d$workforce_character)),
      tags$div(style = "font-size:11px; color:#777; margin-top:3px;",
               tags$span(style = "color:#999;", "Top industries: "),
               fmt_na(d$top3_industries))),

    Voting = info_group("Voting", collapsible = collapsible, group_id = gid("voting"),
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

# ===== Suburb comparison table (Compare page) =============================
# Builds the Overall/theme/dimension grid for the comparison page. Rows:
# "Overall match", then one row per theme (each expandable to reveal its
# individual dimensions), one column per suburb. Reference suburbs show as
# a labelled anchor column with no score (matching yourself is meaningless);
# compared suburbs show their actual computed match, sourced from
# `compare_df` (the output of the compare_data() reactive).
build_compare_table <- function(ref_codes, other_codes, compare_df, selected_dims,
                                anchor = NULL) {
  all_codes <- c(ref_codes, other_codes)
  n_cols    <- length(all_codes)
  if (n_cols == 0) return(NULL)

  col_w <- sprintf("minmax(90px, 1fr)")
  grid_style <- sprintf(
    "display:grid; grid-template-columns: 170px repeat(%d, %s);
     gap:1px; align-items:center; background:#e8e8e8;", n_cols, col_w)

  cell <- function(content, header = FALSE, is_ref = FALSE, align = "center") {
    tags$div(style = sprintf(
      "background:%s; padding:5px 8px; font-size:11px; text-align:%s;
       %s", if (is_ref) "#f5f5f5" else "white", align,
      if (header) "font-weight:600;" else ""),
      content)
  }

  row_of <- function(label_cell, value_fn, extra_style = "") {
    tagList(
      tags$div(style = paste("background:white; padding:5px 8px;
                              font-size:11px; font-weight:500;", extra_style),
               label_cell),
      lapply(all_codes, function(code) {
        is_ref <- code %in% ref_codes
        cell(value_fn(code, is_ref), is_ref = is_ref)
      }))
  }

  get_row <- function(code) {
    if (code %in% ref_codes || is.null(compare_df)) return(NULL)
    r <- compare_df[compare_df$suburb_b == code, , drop = FALSE]
    if (nrow(r) != 1) return(NULL)
    r
  }

  # Header row: suburb names, reference columns marked.
  header_row <- row_of(
    tags$span("Suburb"),
    function(code, is_ref) {
      nm   <- suburb_code_to_name[[code]] %||% code
      abbr <- suburb_code_to_abbr[[code]] %||% ""
      tags$div(
        tags$div(style = "font-weight:600;", sprintf("%s, %s", nm, abbr)),
        if (is_ref) tags$div(style = "font-size:9px; color:#888;
                                      text-transform:uppercase;
                                      letter-spacing:0.4px;", "Reference"))
    })

  # Overall match row.
  overall_row <- row_of(
    tags$b("Overall match"),
    function(code, is_ref) {
      if (is_ref) return(tags$span(style = "color:#aaa;", "\u2014"))
      r <- get_row(code)
      if (is.null(r)) tags$span(style = "color:#aaa;", "\u2014")
      else tags$b(sprintf("%.1f%%", r$match * 100))
    },
    extra_style = "font-size:12px;")

  # Distance to the main search's location anchor(s), when one is set.
  # Left blank for the reference columns.
  anchor_row <- if (!is.null(anchor)) row_of(
    tags$span(style = sprintf("color:%s;", anchor_colour),
              sprintf("Distance to %s",
                      paste(vapply(anchor$codes, anchor_name, ""), collapse = " / "))),
    function(code, is_ref) {
      if (is_ref) return(tags$span(style = "color:#aaa;", "\u2014"))
      if (code %in% anchor$codes) return(tags$span(style = "color:#555;", "anchor"))
      d <- anchor_dist_matrix(code, anchor$codes)[1, ]
      tags$span(style = "color:#555;", paste(fmt_km(d), collapse = " / "))
    }) else NULL

  # One block per theme: a toggleable header row + hidden detail rows.
  theme_blocks <- lapply(names(dream_theme_dims), function(theme_id) {
    dims <- intersect(sort(dream_theme_dims[[theme_id]]), selected_dims)
    theme_score <- function(code) {
      r <- get_row(code)
      if (is.null(r)) return(NA_real_)
      cols <- paste0("score_", dims)
      cols <- cols[cols %in% names(r)]
      if (!length(cols)) return(NA_real_)
      mean(as.numeric(unlist(r[1, cols])), na.rm = TRUE)
    }

    toggle_id <- paste0("cmp-theme-", theme_id)
    theme_row <- row_of(
      tags$div(
        onclick = sprintf(
          "var el=document.getElementById('%s');
           var open = el.style.display !== 'none';
           el.style.display = open ? 'none' : 'contents';
           this.querySelector('.cmp-caret').textContent = open ? '\u25b8' : '\u25be';",
          toggle_id),
        style = "cursor:pointer; display:flex; align-items:center; gap:6px;",
        tags$span(class = "cmp-caret",
                  style = "font-size:9px; color:#888; width:8px;", "\u25b8"),
        tags$span(
          style = sprintf(
            "font-size:10.5px; font-weight:700; color:white; background:%s;
             padding:2px 9px; border-radius:8px; letter-spacing:0.3px;",
            theme_group_colours[[theme_id]]),
          dream_theme_labels[[theme_id]])),
      function(code, is_ref) {
        if (is_ref) return(tags$span(style = "color:#aaa;", "\u2014"))
        if (!length(dims)) return(tags$span(style = "color:#ccc;", "off"))
        v <- theme_score(code)
        if (is.na(v)) tags$span(style = "color:#aaa;", "\u2014")
        else tags$b(style = "font-size:12.5px; color:#222222;",
                    sprintf("%.1f%%", v * 100))
      },
      extra_style = "font-size:11.5px; padding-top:7px; padding-bottom:7px;")

    dim_rows <- tags$div(
      id = toggle_id, style = "display:none;",
      lapply(dims, function(d) {
        row_of(
          tags$div(style = "padding-left:16px; font-size:10px; color:#9CA3AF;",
                   tags$span(style = sprintf(
                     "display:inline-block; width:6px; height:6px;
                      border-radius:2px; background:%s; margin-right:5px;",
                     dim_colors[[d]]), NULL),
                   dim_labels[[d]]),
          function(code, is_ref) {
            if (is_ref) return(tags$span(style = "color:#ccc;", "\u2014"))
            r <- get_row(code)
            col <- paste0("score_", d)
            if (is.null(r) || !col %in% names(r) || is.na(r[[col]][1])) {
              tags$span(style = "color:#ccc;", "\u2014")
            } else tags$span(style = "color:#9CA3AF; font-weight:400;",
                             sprintf("%.1f%%", r[[col]][1] * 100))
          })
      }))

    tagList(theme_row, dim_rows)
  })

  tags$div(style = grid_style,
    header_row, overall_row, anchor_row, theme_blocks)
}

# Compact comparison strip for list cards: the match's headline archetypes,
# each ticked green when identical to the reference suburb's.
comparison_fields <- c(
  "Age"     = "age_archetype",
  "Income"  = "income_archetype",
  "Housing" = "tenure_archetype",
  "Economy" = "economy_type"
)

# Which dream theme supplies the reference value for each strip field:
# age & income are "People & culture", tenure & economy are "Urban fabric".
comparison_field_theme <- c(
  age_archetype    = "people",
  income_archetype = "people",
  tenure_archetype = "urban",
  economy_type     = "urban"
)

# `ref_vals` is a named list: column -> character vector of reference
# values (one per reference suburb). A single-reference run passes one
# value per field; dream mode passes the values of every suburb picked
# for the field's theme, and an empty vector when that theme was left
# blank. The chip is ticked when the match equals any reference value.
build_comparison_strip <- function(ref_vals, match_info) {
  if (is.null(ref_vals) || is.null(match_info)) return(NULL)
  cells <- lapply(names(comparison_fields), function(lbl) {
    col <- comparison_fields[[lbl]]
    rv  <- ref_vals[[col]] %||% character(0)
    rv  <- unique(as.character(rv[!is.na(rv)]))
    mv  <- match_info[[col]]
    if (is.null(mv) || is.na(mv)) return(NULL)
    same <- length(rv) > 0 && mv %in% rv
    tags$span(
      style = sprintf(
        "font-size:11px; padding:2px 8px; border-radius:10px; border:1px solid %s;
         background:%s; color:%s; white-space:nowrap;",
        if (same) "#a5d6a7" else "#e0e0e0",
        if (same) "#f1f8e9" else "#fafafa",
        if (same) "#33691e" else "#666"),
      title = if (same) sprintf("%s: same as reference (%s)", lbl, mv)
              else      sprintf("%s: %s (reference: %s)", lbl, mv,
                                if (!length(rv)) "\u2014"
                                else paste(rv, collapse = " / ")),
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
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
  /* Compact the inline numericInput */
  .inline-topn .form-group { margin-bottom: 0; }
  .inline-topn input.form-control { padding: 4px 6px; height: 34px; }
  body {
  background-color: #ffffff;
}
  /* Common 34px row height */
  .row-align { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 6px 0 14px; }
  .row-align > * { vertical-align: middle; }

  /* virtualSelect picker: three things need fixing:
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
  
  /* Regions and Filters dropdownButtons: match the 34px row height.
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
}

/* ===== Mobile responsive overrides ===== */
@media (max-width: 768px) {
  /* Map + side info panel: stack instead of sitting side by side */
  .main-view-row { flex-direction: column !important; }
  .main-view-row > div:last-child {
    flex: 1 1 auto !important;
    width: 100% !important;
    max-width: none !important;
    max-height: 320px !important;
  }

  /* Top-10 panel: drop out of the map overlay and become a normal
     block sitting below the map, instead of floating on top of it
     and eating scarce screen space. !important beats Shiny's inline
     position:absolute styling. */
  .top10-panel {
    position: static !important;
    width: 100% !important;
    left: auto !important;
    right: auto !important;
    bottom: auto !important;
    top: auto !important;
    transform: none !important;
    margin-top: 8px;
    box-shadow: none !important;
    border: 1px solid #e0e0e0;
    max-height: 260px;
    overflow-y: auto;
  }

  /* Settings-modal dim rows: switch + radios no longer fit one line.
     Let the radio group drop to its own line under the switch. */
  .dim-row { flex-wrap: wrap; }
  .dim-row > div:first-child { width: 100% !important; }

  /* Header: title + icon buttons no longer fit one row */
  .header-row { flex-wrap: wrap; row-gap: 6px; }
  .header-row h3 { order: -1; width: 100%; text-align: center; }

  /* Row-2 selectors ('Show N suburbs similar to...'): drop the
     side-spacer centering trick and let items wrap onto their own
     lines instead of overflowing off the right edge of the screen. */
  .selector-row { flex-direction: column !important; }
  .selector-row > div:first-child { display: none; }  /* drop left spacer */
  .selector-row > div:last-child {
    flex: 0 0 auto !important;
    justify-content: center !important;
    margin-top: 6px;
  }
  .row-align { justify-content: flex-start !important; }
  .row-align > div[style*='min-width:240px'] {
    min-width: 0 !important;
    max-width: none !important;
    width: 100% !important;
    flex: 1 1 100% !important;
  }

  /* Map: fixed 680px is too tall on a short mobile viewport (leaves the
     rest of the page off-screen) and can also be cramped in landscape.
     Scale to viewport height instead. */
  .map-container, .map-container .leaflet-container {
    height: 60vh !important;
    min-height: 320px;
  }

  /* Icon-only header buttons: bump to a ~44px touch target (Apple/Google
     minimum tap-target guidance) rather than the tighter desktop sizing. */
  .header-row .btn {
    min-width: 44px;
    min-height: 44px;
    padding: 8px 10px;
  }
}

/* ===== near-a-place anchor control ===== */
.anchor-add { font-size: 13px; color: #2c7fb8; text-decoration: none; white-space: nowrap;
              border-bottom: 1px dashed #2c7fb8; margin-left: 6px; }
.anchor-add:hover { color: #1a5a8a; border-bottom-color: #1a5a8a; }
.anchor-block { display: inline-flex; align-items: center; gap: 8px; flex-wrap: wrap;
                margin-left: 4px; padding: 3px 8px; border-radius: 6px;
                background: #fdf2f6; border: 1px solid #f3c7d6; }
.anchor-block .form-group { margin-bottom: 0; }
.anchor-x { font-size: 18px; line-height: 1; color: #888; text-decoration: none; padding: 0 2px; }
.anchor-x:hover { color: #333; }
.anchor-note { font-size: 11px; color: #D6336C; white-space: nowrap; }

/* ===== Suburb profile panel (sentence + charts) ===== */
.st-panel { font-size: 12px; color: #333; }
.st-head { display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap; margin-bottom: 4px; }
.st-name { font-size: 16px; font-weight: 700; color: #222; }
.st-badge { font-size: 9.5px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.4px;
            padding: 2px 7px; border-radius: 8px; background: #333333; color: #fff; }
.st-badge-sel { background: #2C7FB8; }
.st-hint { font-size: 11px; color: #999; }
.st-sentence { font-size: 13px; line-height: 1.4; color: #444; margin: 3px 0 7px; }
.st-legend { display: flex; flex-wrap: wrap; gap: 4px 14px; font-size: 11px; color: #555; margin-bottom: 6px; }
.st-sw { display: inline-block; width: 10px; height: 10px; border-radius: 2px; margin-right: 5px; vertical-align: -1px; }
.st-sw-aus { width: 3px; height: 12px; border-radius: 0; background: #E07B39; vertical-align: -2px; }
.st-tabs { display: flex; gap: 2px; border-bottom: 1px solid #e5e5e5; margin-bottom: 8px; }
.st-tab { border: none; background: none; padding: 5px 10px 6px; font-size: 12px; font-weight: 600;
          color: #888; border-bottom: 3px solid transparent; margin-bottom: -1px; cursor: pointer; }
.st-tab:hover { color: #444; }
.st-tab.active { color: #222; border-bottom-color: var(--tabc); }
.st-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: 8px; }
.st-card { border: 1px solid #ececec; border-radius: 6px; padding: 6px 9px 7px; background: #fcfcfc; min-width: 0; }
.st-card-title { font-size: 10.5px; font-weight: 700; color: #666; text-transform: uppercase;
                 letter-spacing: 0.4px; margin-bottom: 4px; }
.st-divider { border-top: 1px dashed #e3e3e3; margin: 6px 0 5px; }
.st-card-note { font-size: 10.5px; color: #999; font-style: italic; margin: -2px 0 5px; }
.st-empty { font-size: 11px; color: #aaa; padding: 4px 0; }
.st-row { display: grid; grid-template-columns: 42% 1fr 32px; align-items: center; gap: 6px; padding: 1px 0; line-height: 1.25; }
.st-lab { font-size: 11px; color: #555; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.st-v { font-size: 10.5px; color: #333; font-weight: 600; text-align: right; }
.st-track { position: relative; display: flex; flex-direction: column; gap: 2px; }
.st-bar { height: 6px; border-radius: 2px; min-width: 0; }
.st-ref { background: #333333; }
.st-sel { background: #2C7FB8; }
.st-aus { position: absolute; top: -2px; bottom: -2px; width: 2px; margin-left: -1px; background: #E07B39; }
.st-ptrack { position: relative; height: 10px; border-radius: 5px; background: linear-gradient(90deg, #f1f4f7, #d6dee7); }
.st-pmid { position: absolute; left: 50%; top: -3px; bottom: -3px; width: 2px; margin-left: -1px; background: #E07B39; }
.st-dot { position: absolute; top: 50%; width: 10px; height: 10px; margin: -5px 0 0 -5px;
          border-radius: 50%; border: 1.5px solid #fff; box-shadow: 0 0 0 0.5px rgba(0,0,0,0.25); }
.st-axis { display: grid; grid-template-columns: 42% 1fr 32px; gap: 6px; font-size: 9.5px; color: #aaa; margin-top: 2px; }
.st-axis span:nth-child(2) { text-align: center; }
.st-svg { width: 100%; height: auto; display: block; }
.st-svgt { font-size: 9px; fill: #999; }
.st-row-nov { grid-template-columns: 38% 1fr; }
.st-axis-nov { grid-template-columns: 38% 1fr; }
.st-grid-wide { grid-template-columns: repeat(auto-fill, minmax(320px, 1fr)); gap: 10px; }
.st-compare-intro { font-size: 11px; color: #888; margin-bottom: 6px; }
.st-compare-key { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
                  gap: 6px 16px; margin-bottom: 10px; }
.st-compare-name { display: flex; align-items: center; gap: 6px; font-size: 12.5px; font-weight: 600; color: #222; }
.st-compare-name .st-sw { margin-right: 0; }
.st-compare-sentence { font-size: 11.5px; line-height: 1.4; color: #555; margin: 2px 0 0 16px; }
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

    // Suburb profile tabs. The chosen tab is remembered across re-renders
    // and applied to every profile panel on the page (side panel and each
    // list card), so switching suburbs or scrolling the list keeps the same
    // theme in view.

    // Near-a-place: open / close the anchor controls. Opening ticks the
    // hidden anchor_on checkbox (so conditionalPanels and bookmarking see
    // it) and pops the suburb picker open; closing also clears the picks.
    function stAnchorOpen() {
      $('#anchor_on').prop('checked', true).trigger('change');
      setTimeout(function() {
        var el = document.querySelector('#anchor_sub');
        try { if (el && el.open) el.open(); } catch (e) {}
      }, 200);
    }
    function stAnchorClose() {
      $('#anchor_on').prop('checked', false).trigger('change');
      Shiny.setInputValue('anchor_clear', Math.random(), {priority: 'event'});
    }
    window.stTab = window.stTab || 'people';
    function stApplyTabs() {
      $('.st-panel').each(function() {
        var p = $(this);
        if (!p.find('.st-tab[data-tab=\"' + window.stTab + '\"]').length) return;
        p.find('.st-tab').removeClass('active')
         .filter('[data-tab=\"' + window.stTab + '\"]').addClass('active');
        p.find('.st-pane').hide()
         .filter('[data-tab=\"' + window.stTab + '\"]').show();
      });
    }
    $(document).on('click', '.st-tab', function() {
      window.stTab = $(this).data('tab');
      stApplyTabs();
    });
    $(document).on('shiny:value', function() { setTimeout(stApplyTabs, 50); });
  "))
  ),
  # --- Header Layout Block ---
  # Row 1: title centred + buttons right-aligned, on the same row.
  # 3-column flexbox keeps the title in true centre regardless of how wide
  # the buttons cluster ends up being.
  fluidRow(
    column(width = 12,
      tags$div(
        class = "header-row",
        style = "display: flex; align-items: center; margin-top: 10px;
                 margin-bottom: 6px;",
        # Left spacer to balance the right cluster's width
        tags$div(style = "flex: 1;"),
        # Centred title
        tags$div(style = "text-align: center;",
          tags$h3(
            "SUBURB SIMILARITY EXPLORER",
            style = "margin: 0; font-size: 19px; font-weight: 600;
                     color: #333333; letter-spacing: 0.2px;"
          ),
          tags$div(
            "Find suburbs with similar characteristics",
            style = "font-size: 12px; color: #6B7280; margin-top: 1px;"
          )
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
          actionButton("open_compare", label = NULL,
                       icon = icon("code-compare"),
                       class = "btn-outline-secondary",
                       style = "margin-right: 4px;",
                       title = "Compare suburbs"),
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
  # Same 3-column flexbox trick: selectors sit in the true centre.
  # This entire block (through the Map/List conditionalPanels below) only
  # shows on the main search page - the Compare page (added further down)
  # is a fully separate page, not another view_mode.
  conditionalPanel(condition = "output.page_is_main",
  fluidRow(
    column(width = 12,
      tags$div(
        class = "selector-row",
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
                                 keepAlwaysOpen = FALSE,
                                 disableOptionGroupCheckbox = TRUE)),
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
                    style = "font-size:13px; color:#444; margin-left:4px;"),
          # Optional location preference ("near a place"). Off by default:
          # only the link shows until the user opens it. anchor_on is a
          # hidden checkbox so it survives bookmarking like any input.
          conditionalPanel("!output.dream_on", inline = TRUE,
            tags$span(style = "display:none;",
                      checkboxInput("anchor_on", NULL, FALSE)),
            conditionalPanel("!input.anchor_on", inline = TRUE,
              tags$a(href = "#", class = "anchor-add",
                     onclick = "stAnchorOpen(); return false;",
                     title = "Favour suburbs close to a suburb you choose",
                     icon("location-dot"), " near a place")),
            conditionalPanel("input.anchor_on", inline = TRUE,
              tags$span(class = "anchor-block",
                tags$span("near", style = "font-size:14px;"),
                div(style = "min-width:200px; max-width:380px;",
                    virtualSelectInput("anchor_sub", NULL,
                                       choices = anchor_choices,
                                       selected = character(0),
                                       multiple = TRUE, maxValues = anchor_max,
                                       width = "100%", search = TRUE,
                                       placeholder = "Pick up to 3 suburbs\u2026",
                                       optionsCount = 8, showValueAsTags = TRUE,
                                       autoSelectFirstOption = FALSE,
                                       disableOptionGroupCheckbox = TRUE)),
                tags$span("within", style = "font-size:14px;"),
                div(style = "width:96px;",
                    selectInput("anchor_radius", NULL,
                                choices = anchor_radius_choices,
                                selected = "20", width = "100%")),
                dropdownButton(
                  inputId = "anchor_dd", label = NULL, icon = icon("gear"),
                  status = "default", circle = FALSE, right = TRUE,
                  width = "280px",
                  div(style = "padding: 6px;",
                    radioButtons("anchor_strength", "How much location matters",
                                 choices = anchor_strength_choices,
                                 selected = "normal"),
                    radioButtons("anchor_mode",
                                 "With several places, favour suburbs near",
                                 choices = c("Any of them" = "any",
                                             "All of them" = "all"),
                                 selected = "any"),
                    tags$div(style = "font-size:11px; color:#777;",
                      "Distances are straight-line, between suburb centres. ",
                      "The match % stays pure similarity; the ranking also ",
                      "rewards being close."))),
                tags$a(href = "#", class = "anchor-x", title = "Remove location",
                       onclick = "stAnchorClose(); return false;", "\u00d7"))))
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

  # "What's this suburb like" overview: one bullet per active characteristic
  # describing how close the top matches are. Auto-hidden when no results.
  uiOutput("similarity_overview"),

  conditionalPanel(condition = "input.view_mode == 'map'",
    div(class = "main-view-row",
        style = "display: flex; gap: 12px; align-items: stretch;",
      # ----- Map (flexes to fill remaining width) -----
      div(style = "flex: 1 1 auto; min-width: 0; position: relative;",
        div(class = "map-container",
          withSpinner(leafletOutput("map", height = 680),
                      type = 4, color = "#2c7fb8", size = 0.8,
                      caption = "Finding matches")),
        # bottom-left floating panel: top-10 contribution bars
        absolutePanel(
          bottom = 24, left = 12, width = 380, draggable = TRUE,
          class = "top10-panel",
          style = paste("background: rgba(255,255,255,0.92); padding: 8px;",
                        "border-radius: 6px; box-shadow: 0 1px 4px rgba(0,0,0,0.3); z-index: 1000;",
                        "overflow: visible;"),
          uiOutput("top10"))
      ),
      # ----- Side panel: suburb profile (sentence + charts), 580px so the
      # chart grid fits two columns -----
      div(style = "flex: 0 0 580px; max-height: 680px; overflow-y: auto;
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
  ),  # end conditionalPanel(page_is_main)

  # ----- Compare page -------------------------------------------------
  conditionalPanel(condition = "output.page_is_compare",
    tags$div(style = "padding: 10px 4px;",
      tags$div(style = "display:flex; align-items:center; gap:10px;
                        margin: 10px 0 14px 0;",
        actionButton("compare_back", "Back to search",
                    icon = icon("arrow-left"),
                    class = "btn-outline-secondary btn-sm"),
        tags$h4("Compare suburbs", style = "margin:0; font-size:15px;
                                            font-weight:600; color:#444;")),

      tags$div(style = "display:flex; gap:20px; flex-wrap:wrap;
                        align-items:flex-start; margin-bottom:14px;",
        tags$div(style = "min-width:280px;",
          tags$label("Reference suburb(s)", style = "font-size:12px;
                                                      font-weight:600;"),
          virtualSelectInput("compare_refs", NULL,
                             choices = ref_choices, selected = character(0),
                             multiple = TRUE, width = "100%", search = TRUE,
                             placeholder = "Pick one or more references\u2026",
                             optionsCount = 8, showValueAsTags = TRUE,
                             autoSelectFirstOption = FALSE,
                             disableOptionGroupCheckbox = TRUE),
          conditionalPanel(
            condition = "input.compare_refs && input.compare_refs.length > 1",
            radioButtons("compare_mode", NULL,
                        choices  = c("Closest to any individual reference" = "max",
                                     "Closest to all references on average" = "mean"),
                        selected = "max", inline = TRUE))),

        tags$div(style = "min-width:280px;",
          uiOutput("compare_others_label", inline = TRUE),
          virtualSelectInput("compare_others", NULL,
                             choices = ref_choices, selected = character(0),
                             multiple = TRUE, width = "100%", search = TRUE,
                             placeholder = "Pick suburbs to compare\u2026",
                             optionsCount = 8, showValueAsTags = TRUE,
                             maxValues = 9, autoSelectFirstOption = FALSE,
                             disableOptionGroupCheckbox = TRUE))),

      uiOutput("compare_body")
    )
  )
)

# ===== Server =============================================================

server <- function(input, output, session) {
  # add a cache
  raw_cache <- new.env(parent = emptyenv())
  
  # Active state, commits via Apply (modal) or picker/map-click (ref).
  current_ref     <- reactiveVal(default_ref_code)
  settings_active <- reactiveVal(default_settings)
  top_n_active    <- reactiveVal(50)
  region_filter   <- reactiveVal(default_tree_selection)
  # Server-side source of truth for the "Zoom to" mode, kept in sync with
  # the map_zoom dropdown but read directly (no UI round-trip) so the
  # auto-fit check below can never race against a stale/unset input$map_zoom.
  zoom_mode       <- reactiveVal("auto")

  # Which top-level page is showing: "main" (search/map/list) or
  # "compare" (the suburb-comparison page). A separate page, not a
  # view_mode value, since Compare has its own reference/others pickers
  # and doesn't share the main search's results.
  page_mode <- reactiveVal("main")
  output$page_is_main    <- reactive({ page_mode() == "main" })
  output$page_is_compare <- reactive({ page_mode() == "compare" })
  outputOptions(output, "page_is_main",    suspendWhenHidden = FALSE)
  outputOptions(output, "page_is_compare", suspendWhenHidden = FALSE)

  observeEvent(input$open_compare, { page_mode("compare") })
  observeEvent(input$compare_back, { page_mode("main") })

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

  # The suburbs the current results are actually measured against: every
  # suburb picked in the dream modal while dream mode is on, otherwise
  # the single/multi reference selection. Use this (not current_ref())
  # anywhere the UI marks or describes "the reference".
  active_ref_codes <- reactive({
    dream <- dream_refs_active()
    if (!is.null(dream)) unique(unlist(dream$theme_refs)) else current_ref()
  })

  # --- "near a place" anchor -------------------------------------------
  output$dream_on <- reactive({ !is.null(dream_refs_active()) })
  outputOptions(output, "dream_on", suspendWhenHidden = FALSE)

  # NULL unless the control is open, at least one suburb is picked and
  # dream mode is off.
  anchor_state <- reactive({
    if (!isTRUE(input$anchor_on) || !is.null(dream_refs_active())) return(NULL)
    codes <- head(input$anchor_sub %||% character(0), anchor_max)
    codes <- codes[codes %in% names(suburb_centroid_lat)]
    if (!length(codes)) return(NULL)
    list(codes  = codes,
         radius = as.numeric(input$anchor_radius %||% "20"),
         beta   = unname(anchor_strength_beta[input$anchor_strength %||% "normal"]),
         mode   = input$anchor_mode %||% "any")
  })

  observeEvent(input$anchor_clear, {
    updateVirtualSelect(session = session, inputId = "anchor_sub",
                        selected = character(0))
  })

  # Anchor pin(s) and radius ring on the map.
  observeEvent(anchor_state(), {
    a <- anchor_state()
    proxy <- leafletProxy("map") |> clearGroup("anchor")
    if (is.null(a)) return()
    lat <- unname(suburb_centroid_lat[a$codes])
    lng <- unname(suburb_centroid_lng[a$codes])
    nms <- vapply(a$codes, anchor_name, "")
    proxy |>
      addCircles(lng = lng, lat = lat, radius = a$radius * 1000,
                 group = "anchor", color = anchor_colour, weight = 2,
                 opacity = 0.9, dashArray = "6,5", fill = FALSE,
                 options = pathOptions(interactive = FALSE)) |>
      addCircleMarkers(lng = lng, lat = lat, group = "anchor", radius = 7,
                       color = "white", weight = 2, fillColor = anchor_colour,
                       fillOpacity = 1,
                       label = sprintf("%s (location, %g km radius)", nms, a$radius))
  }, ignoreNULL = FALSE)

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
      anchor <- anchor_state()
      top <- head(cd, n_top) |>
        mutate(
          `reference suburb`       = vapply(winning_ref, label, character(1)),
          `most similar suburb`    = vapply(suburb_b,    label, character(1)),
          most_similar_suburb_code = suburb_b,
          match                = round(match, 4),
          distance_km          = round(anchor_km, 1),
          proximity            = round(proximity, 4),
          ranking_score        = round(score, 4)
        ) |>
        mutate(across(all_of(raw_cols), \(x) round(x, 4))) |>
        rename_with(\(nm) paste0("sim_", sub("^raw_", "", nm)),
                    all_of(raw_cols)) |>
        select(`reference suburb`, `most similar suburb`,
               most_similar_suburb_code, rank, match,
               if (!is.null(anchor)) c("distance_km", "proximity", "ranking_score"),
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
        "mortgage_percentile", "rent_percentile",
        "income_archetype", "income_percentile", "education_archetype", "lfs_archetype",
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

      # Settings footer: written as commented lines so the CSV opens cleanly
      # in Excel/R/Python and the provenance survives editing.
      setts <- settings_active()
      mode  <- input$multi_mode %||% "max"

      settings_lines <- c(
        "",
        sprintf("# Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        sprintf("# Reference suburb(s): %s",
                paste(vapply(ref_codes, label, character(1)), collapse = "; ")),
        sprintf("# Top N requested: %d", n_top),
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
          sprintf("# Remoteness filter: %s", paste(cat_remote_filter(), collapse = "; ")) else NULL,
        if (!is.null(anchor)) c(
          sprintf("# Location anchor(s): %s",
                  paste(vapply(anchor$codes, label, character(1)), collapse = "; ")),
          sprintf("# Anchor radius: %g km; strength: beta = %g; multiple anchors: %s",
                  anchor$radius, anchor$beta,
                  if (anchor$mode == "all") "near all" else "near any"),
          "# Ranking: ranking_score = match x proximity^beta, proximity = 0.5^((distance_km / radius)^2)"
        ) else NULL
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
    if (length(state$input$anchor_sub))
      updateVirtualSelect(session, "anchor_sub", selected = state$input$anchor_sub)
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
      # A genuinely new reference area should always auto-pan to its new
      # top-N, even if the user had previously zoomed to a specific state
      # for a different search. Reset server-side state immediately (no
      # round-trip lag) and mirror it in the dropdown for the UI to match.
      zoom_mode("auto")
      updateSelectInput(session, "map_zoom", selected = "auto")
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Inline region picker -> region_filter (updates live, not via modal)
  observeEvent(input$region_pick, {
    region_filter(input$region_pick %||% character(0))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Categorical-filter pickers -> their reactiveVals.
  # When a filter becomes non-empty, also drop the corresponding characteristic
  # from settings_active so the match doesn't double-count it.
  # (One-way: clearing the filter does NOT auto-restore the characteristic.
  # The user can re-add it via the settings modal.)
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
    # When nothing is restricted, hide the qualifier entirely. No need to
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
  # Used as the anchor for "Somewhat different" scoring. Each reference has
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

    # ----- Dream-suburb mode --------------------------------------------
    # The synthetic raw_wide already averages within themes, so we feed it
    # straight through compute_match like a single ref. No MAX/MEAN
    # aggregation needed since there's effectively one "ref": the blend.
    if (!is.null(dream)) {
      raw_wide <- compute_dream_raw(dream$theme_refs)
      if (is.null(raw_wide) || nrow(raw_wide) == 0) return(NULL)

      df <- compute_match(raw_wide, setts, NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$winning_ref <- "dream"
      df <- apply_anchor(df, NULL)   # adds score = match (anchor is off here)

      # Exclude all dream references from results
      dream_refs <- unique(unlist(dream$theme_refs))

      df_national <- df |>
        filter(!suburb_b %in% dream_refs,
               suburb_b %in% allowed_codes_all) |>
        arrange(desc(score), desc(match)) |>
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
        mutate(sim_rank = rank(-match, ties.method = "first")) |>
        arrange(desc(score), desc(match)) |>
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
      df   <- compute_match(rs, setts, meds)
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

    # Location anchor: ranking score = match x proximity^beta (score = match
    # when no anchor is set). The displayed match stays pure similarity.
    combined <- apply_anchor(combined, anchor_state())

    # National pass: exclude all selected references, keep allowed only
    df_national <- combined |>
      filter(!suburb_b %in% ref_codes,
             suburb_b %in% allowed_codes_all) |>
      arrange(desc(score), desc(match)) |>
      mutate(national_rank = row_number(),
             national_pct  = 100 * national_rank / n())

    # Apply region filter + cat filters, then re-rank within the filtered set
    filtered_codes <- codes_from_cat_filters(
      tree_codes = allowed_codes_from_tree(region_filter()),
      coast      = cat_coast_filter(),
      terrain    = cat_terrain_filter(),
      remote     = cat_remote_filter()
    )

    # sim_rank = position on similarity alone, so list cards can say when
    # the anchor moved a suburb.
    df_national |>
      filter(suburb_b %in% filtered_codes) |>
      mutate(sim_rank = rank(-match, ties.method = "first")) |>
      arrange(desc(score), desc(match)) |>
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
    # Which dims are currently being filtered above via cat_* pickers?
    filtered_dims <- c(
      if (length(cat_coast_filter())   > 0) "coast"      else NULL,
      if (length(cat_terrain_filter()) > 0) "terrain"    else NULL,
      if (length(cat_remote_filter())  > 0) "remoteness" else NULL
    )

    # Group into the same 4 themes used in the dream-suburb modal, so the
    # two surfaces stay conceptually consistent. Within each theme, dims
    # are alphabetised. (theme_group_colours is defined globally, shared
    # with the suburb-comparison table.)
    theme_ordered <- lapply(dream_theme_dims, function(dims)
      intersect(sort(dims), all_dim_codes))

    group_header <- function(theme_id, label, colour, all_on, summary_text) {
      tags$div(
        style = "display:flex; align-items:center; gap:8px;
                 margin: 10px 0 4px 0;",
        tags$div(
          style = "display:flex; align-items:center; gap:6px;",
          tags$div(
            style = sprintf(
              "font-size:11px; font-weight:600; letter-spacing:0.5px;
               color:white; background:%s; display:inline-block;
               padding:2px 10px; border-radius:10px;", colour),
            toupper(label)),
          tags$span(style = "font-size:11px; color:#888;", summary_text)),
        materialSwitch(paste0("theme_check_", theme_id), label = NULL,
                       value = all_on, status = "primary", inline = TRUE))
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
        div(style = "width: 215px; flex-shrink: 0;",
          # right = TRUE puts the switch on the left, label on the right
          materialSwitch(paste0("dim_check_", d),
                         label = dim_labels[d],
                         value = is_checked,
                         status = "primary", inline = TRUE, right = TRUE)),
        if (is_filtered) {
          tags$span(
            style = "font-size: 11px; color: #888; font-style: italic;",
            "filtered above, disabled in match")
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

    theme_blocks <- tagList(
      lapply(names(dream_theme_dims), function(theme_id) {
        dims_in_theme <- theme_ordered[[theme_id]]
        # A filtered dim (via the category-filter pickers) is deliberately
        # excluded from the match already, so it shouldn't count against
        # "is this theme fully on" - a single filtered indicator would
        # otherwise flip the whole theme's switch to "off" even though
        # every other indicator in it is still active.
        checkable_dims <- setdiff(dims_in_theme, filtered_dims)
        all_on <- length(checkable_dims) > 0 &&
          all(checkable_dims %in% names(setts))
        n_selected <- sum(dims_in_theme %in% names(setts))
        n_total    <- length(dims_in_theme)
        summary_text <- sprintf("%d of %d selected", n_selected, n_total)
        tagList(
          group_header(theme_id, dream_theme_labels[[theme_id]],
                       theme_group_colours[[theme_id]], all_on, summary_text),
          lapply(dims_in_theme, build_row))
      }))

    modalDialog(
      title = "Match settings", easyClose = FALSE, size = "xl",
      tags$style(HTML(
        ".dim-row .radio-inline { margin: 0 6px 0 0; padding-top: 0;
                                   font-size: 11.5px; white-space: nowrap; }
         .dim-row .form-group   { margin-bottom: 0; }
         .dim-row .shiny-options-group { margin-top: 0; display: flex;
                                          flex-wrap: nowrap; white-space: nowrap; }
         .dim-row .bootstrap-switch { margin-right: 4px; flex-shrink: 0; }
         .dim-row label { font-size: 12.5px; white-space: nowrap; }
        "
      )),
      tags$div(style = "font-size: 11px; color: #666; margin-bottom: 12px;",
        "Your match score is the average of each theme below that has at ",
        "least one characteristic switched on. A theme left entirely off ",
        "is simply left out of the average, ",
        "not treated as 0."),
      tags$hr(style = "margin: 8px 0;"),
      tags$div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
        tags$label(style = "margin:0;", "characteristics and how to score them:"),
        # Bulk-set every characteristic's target in one click (e.g. "most
        # different suburbs"). Only the Similar/Different choice changes;
        # which characteristics are switched on is left as is.
        tags$span(style = "font-size:12px; color:#666;", "Set all to:",
          actionButton("set_all_similar", "Similar",
                       class = "btn-outline-secondary btn-sm",
                       style = "margin-left:4px; padding:1px 8px;"),
          actionButton("set_all_different", "Different",
                       class = "btn-outline-secondary btn-sm",
                       style = "margin-left:2px; padding:1px 8px;"))),
      div(style = "font-size: 11px; color: #666; margin-bottom: 4px;",
          "Similar = match the reference closely. Mostly similar = somewhat closer than average. ",
          "Mostly different = somewhat further. Different = match as little as possible."),
      div(style = "margin-top: 6px;", theme_blocks),
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
  })

  observeEvent(input$apply_settings, {
    new_settings <- list()
    for (d in all_dim_codes) {
      if (isTRUE(input[[paste0("dim_check_", d)]])) {
        new_settings[[d]] <- input[[paste0("dim_target_", d)]] %||% "similar"
      }
    }
    settings_active(new_settings)
    removeModal()
  })

  # "Set all to" buttons in the settings modal. Changes only the radio
  # choices in the open modal; nothing applies until the user clicks Apply.
  lapply(c(similar = "set_all_similar", different = "set_all_different"),
         function(btn) observeEvent(input[[btn]], {
           target <- if (btn == "set_all_similar") "similar" else "different"
           for (d in all_dim_codes)
             updateRadioButtons(session, paste0("dim_target_", d), selected = target)
         }))

  observeEvent(input$reset_settings, {
    for (d in all_dim_codes) {
      updateMaterialSwitch(session, paste0("dim_check_", d), value = TRUE)
      updateRadioButtons(session, paste0("dim_target_", d), selected = "similar")
    }
    for (theme_id in names(dream_theme_dims)) {
      updateMaterialSwitch(session, paste0("theme_check_", theme_id), value = TRUE)
    }
  })

  # Theme-level "select all" switches: flipping one turns every indicator
  # switch within that theme on/off in one click. Registered once here
  # (not inside settings_modal(), which is rebuilt every time the modal
  # opens, to avoid stacking duplicate observers on repeated opens).
  lapply(names(dream_theme_dims), function(theme_id) {
    observeEvent(input[[paste0("theme_check_", theme_id)]], {
      val <- isTRUE(input[[paste0("theme_check_", theme_id)]])
      # A dim currently excluded by a category filter (coast/terrain/
      # remoteness) stays untouched by the bulk toggle - filters are a
      # deliberate, one-way exclusion (see apply_cat_filter above), and a
      # theme-level "turn everything on" shouldn't silently override that.
      filtered_now <- c(
        if (length(cat_coast_filter())   > 0) "coast"      else NULL,
        if (length(cat_terrain_filter()) > 0) "terrain"    else NULL,
        if (length(cat_remote_filter())  > 0) "remoteness" else NULL
      )
      dims <- setdiff(intersect(dream_theme_dims[[theme_id]], all_dim_codes),
                      filtered_now)
      for (d in dims) {
        updateMaterialSwitch(session, paste0("dim_check_", d), value = val)
      }
    }, ignoreInit = TRUE)
  })

  # The Similarity Settings button doubles as a live summary of active
  # settings: a chip per target bucket (only buckets with at least one dim
  # are shown). Clicking opens the settings modal.
  output$settings_btn <- renderUI({
    setts <- settings_active()
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

    all_chips <- chips
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
        "Pick one or more reference suburbs for any theme(s) below for ",
        "your themes of choice. You can leave themes empty too if you ",
        "don't want them included. Each theme can draw on different ",
        "references, e.g. ", tags$em("people like Tecoma"),
        ", ", tags$em("urban feel like Hawthorn"), ", ",
        tags$em("nature like Sorrento"), ". The results are ranked by how ",
        "well each suburb matches the blend."),
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
      style = "background: #F3F0FA; border: 1px solid #B9A6DD;
               border-radius: 4px; padding: 6px 12px; margin-bottom: 8px;
               font-size: 12px; display: flex; align-items: center; gap: 10px;
               flex-wrap: wrap;",
      tags$span(style = "color: #7B5FA8;",
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
          "amenity-access characteristics: public transport, health ",
          "infrastructure, tertiary education, community infrastructure, ",
          "cultural amenities, fresh food, dining, schools, and kinder. ",
          "Each is measured by average distance to and count of nearby ",
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
        tags$li(tags$b("Balanced"), ": simple mean across all selected characteristics."),
        tags$li(tags$b("People-focused"), ": people-group mean × 0.75 + place-group mean × 0.25."),
        tags$li(tags$b("Place-focused"), ": opposite weighting (place × 0.75, people × 0.25).")
      ),
      tags$h5("Multiple reference suburbs"),
      tags$p("When more than one reference is selected: ",
             tags$b("Match any"), " surfaces suburbs strongly similar to any one ",
             "of your references (best individual fit per pair). ",
             tags$b("Balanced across all"), " surfaces suburbs moderately similar ",
             "to every reference (mean across pairs)."),
      tags$h5("Near a place"),
      tags$p("Optionally, pick up to 3 suburbs to stay close to (\u201cnear a place\u201d). ",
             "Each result's similarity is multiplied by a proximity factor that is 1 at ",
             "the place, 0.5 at the chosen radius and close to 0 at twice the radius, ",
             "raised to a strength setting (a little / normal / strong). Results are ",
             "ranked by that combined score, but the match % shown stays pure ",
             "similarity, with the straight-line distance between suburb centres ",
             "shown alongside."),
      tags$h5("Filters"),
      tags$p("Category filters (Coast, Terrain, Remoteness) restrict results to ",
             "the selected categories AND remove that characteristic from the match. ",
             "Region filters restrict by state, GCCSA, or SA4 without affecting scoring."),
      footer = modalButton("Close")
    ))
  })



  # --- Map base ----------------------------------------------------------
  # CARTO basemaps now require an API key (previously free/anonymous via
  # addProviderTiles). Key is read from the CARTO_API_KEY env var. Set it
  # in .Renviron locally, and as a secret/env var in the deployment
  # environment (e.g. Posit Connect Cloud). Never hardcode it here.
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(
        urlTemplate = paste0(
          "https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png",
          "?key=", Sys.getenv("CARTO_API_KEY")),
        attribution = paste0(
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, ',
          '&copy; <a href="https://carto.com/attributions">CARTO</a>'),
        options = tileOptions(subdomains = "abcd", maxZoom = 20)) |>
      setView(lng = 134, lat = -28, zoom = 4)
  })

  output$map_zoom_dropdown <- renderUI({
    states <- states_in_filter(region_filter())
    selectInput("map_zoom", "Zoom to:",
      choices  = c("Auto (fit results)" = "auto",
                   "Australia"          = "australia",
                   setNames(states, states)),
      selected = isolate(zoom_mode()))
  })

  observeEvent(input$map_zoom, {
    #req(input$map_bounds)
    z <- input$map_zoom %||% "auto"
    zoom_mode(z)   # keep the server-side source of truth in sync
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
                    "national_pct", "winning_ref", "anchor_km", raw_cols)
    anchor     <- anchor_state()
    # Popup breakdown charts should only show dims currently switched on
    # in Match Settings, not every dim the underlying data happens to
    # carry (which includes ones the user has turned off).
    active_dims     <- names(settings_active())
    active_raw_cols <- raw_cols[sub("^raw_", "", raw_cols) %in% active_dims]

    shp_sub <- shp_suburb |>
      inner_join(d |> select(all_of(join_cols)),
                 by = c("suburb_code_2021" = "suburb_b")) |>
      mutate(suburb_name_2021 = coalesce(unname(suburb_code_to_name[suburb_code_2021]),
                                          suburb_name_2021))
    
    in_dream <- !is.null(dream_refs_active())
    ref_poly <- shp_suburb |>
      filter(suburb_code_2021 %in% active_ref_codes()) |>
      mutate(suburb_name_2021 = coalesce(unname(suburb_code_to_name[suburb_code_2021]),
                                          suburb_name_2021))

    shown_sa4 <- ref |>
      filter(suburb_code_2021 %in% d$suburb_b) |>
      pull(sa4_name_2021) |> unique()
    sa4_lines <- shp_sa4 |> filter(sa4_name_2021 %in% shown_sa4)

    # Build a "winning ref" label only when more than one reference is
    # selected. Not in dream mode: there winning_ref is always "dream".
    multi_ref       <- !in_dream && length(current_ref()) > 1
    ref_name_lookup <- if (multi_ref) {
      ref |>
        filter(suburb_code_2021 %in% current_ref()) |>
        transmute(code = suburb_code_2021,
                  label = paste0(display_name, ", ",
                                 state_abbr[state_name_2021])) |>
        { \(d) setNames(d$label, d$code) }()
    } else NULL

    shp_sub$popup_html <- vapply(seq_len(nrow(shp_sub)), function(i) {
      raw_vals <- as.list(as.data.frame(st_drop_geometry(shp_sub))[i, active_raw_cols, drop = FALSE])
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
                  winning_ref_label = winning_lbl,
                  anchor_text = anchor_note(code, anchor, straight_line = TRUE))
    }, character(1))

    # Rank-based Blues palette: rank 1 = darkest blue, rank N = lightest.
    # Visually emphasises rank position, which scales cleanly even when
    # match scores cluster in a narrow numeric range.
    # Custom ramp rather than RColorBrewer's stock "Blues": the stock
    # palette fades to near-white at the light end, which is nearly
    # invisible against the light-grey Esri basemap for the lowest-ranked
    # matches. Flooring the light end at a still-clearly-blue shade keeps
    # every rank visible while preserving the dark-to-light rank gradient.
    n_results <- nrow(shp_sub)
    pal_fn <- colorNumeric(
      # Ramp is already ordered dark ("#08306B") -> light ("#6BAED6"),
      # matching domain c(1, n) directly: rank 1 -> dark, rank n -> light.
      # No reverse needed here (unlike RColorBrewer named palettes, which
      # are ordered light-to-dark internally and would need reverse=TRUE).
      palette = colorRampPalette(c("#08306B", "#6BAED6"))(100),
      domain  = c(1, max(n_results, 2))
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
        color = "#4d4d4d", weight = 2,
        label = ~lapply(
          sprintf("%s: rank %d (match %.1f%%%s)<br><i>Click for more info</i>",
                  suburb_name_2021, rank, match * 100,
                  if (is.null(anchor)) "" else paste0(" \u00b7 ", fmt_km(anchor_km))),
          htmltools::HTML),
        popup = ~popup_html,
        popupOptions = popupOptions(minWidth = 260, maxWidth = 300),
        highlightOptions = highlightOptions(weight = 2, color = "#444",
                                            bringToFront = TRUE)) |>
      addPolygons(data = ref_poly, group = "ref",
                  fillColor = "#111", fillOpacity = 0.65,
                  color = "black", weight = 1.4,
                  label = ~paste0(suburb_name_2021,
                                  if (in_dream) " (dream reference)"
                                  else " (reference)")) |>
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
    if (isolate(zoom_mode()) == "auto" && nrow(shp_sub) > 0) {
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
  #   1. Header sentence: names the suburbs that dominate the strongest
  #      bucket across characteristics (the suburbs that show up most often
  #      among the closest matches).
  #   2. "Where things differ": characteristics where the best bucket reached
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
    # remoteness / weather profile), they'd dominate the "almost
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

    # In dream-suburb mode the results come from the theme blend, not from
    # current_ref() (which still holds the last single-reference pick), so
    # the sentence names the dream suburb instead. The banner above already
    # spells out which suburbs make up each theme.
    if (!is.null(dream_refs_active())) {
      ref_label <- "Your dream suburb"
    } else {
      primary_ref <- ref_codes[1]
      ref_label <- sprintf("%s, %s",
        suburb_code_to_name[[primary_ref]] %||% primary_ref,
        suburb_code_to_abbr[[primary_ref]] %||% "")
      if (length(ref_codes) > 1) {
        ref_label <- paste0(ref_label, sprintf(" (and %d other%s)",
          length(ref_codes) - 1,
          if (length(ref_codes) > 2) "s" else ""))
      }
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
    # Coerce defensively. If a prior reactive ever ends up storing this
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
  # Each row is one suburb_b, bar width = match %. Hovering a row
  # reveals an absolutely-positioned tooltip showing the per-dim raw
  # similarity bars + winning-ref label (when multi-ref).
  output$top10 <- renderUI({
    d <- map_data(); if (is.null(d) || nrow(d) == 0) return(NULL)

    top <- d |> arrange(rank) |> head(10)
    if (!nrow(top)) return(NULL)

    # Use the largest match as the bar-scale max (so the top suburb's
    # bar fills the row). Visual spread is more informative than absolute
    # percentage. Keep the % label as the true match value.
    max_v <- max(top$match, na.rm = TRUE)
    if (!is.finite(max_v) || max_v <= 0) max_v <- 1

    has_anchor <- !is.null(anchor_state())
    render_row <- function(i) {
      r <- as.data.frame(top)[i, , drop = FALSE]
      match_pct <- r$match * 100
      bar_width_pct <- pmin(100, (r$match / max_v) * 100)
      nm   <- suburb_code_to_name[[r$suburb_b]] %||% r$suburb_b
      abbr <- suburb_code_to_abbr[[r$suburb_b]] %||% ""
      rea_url <- build_realestate_url(nm, abbr)

      # The row: rank label, clickable suburb name, bar, % label, REA link.
      # Clicking the name opens the full breakdown as a map popup (the
      # same one a direct polygon click shows), rather than a hover
      # tooltip, which often got in the way of the map underneath.
      tags$div(
        class = "top10-row",
        style = paste(
          if (has_anchor)
            "display:grid; grid-template-columns: 26px 118px 1fr 40px 44px 22px;"
          else "display:grid; grid-template-columns: 26px 118px 1fr 44px 22px;",
          "gap:6px; align-items:center; padding:3px 4px; border-radius:3px;"),
        tags$span(style = "font-size:10px; font-weight:bold; color:#666;",
                  sprintf("#%d", r$rank)),
        tags$a(
          href = "#",
          onclick = sprintf(
            "Shiny.setInputValue('top10_click','%s',{priority:'event'});return false;",
            r$suburb_b),
          style = "font-size:11px; white-space:nowrap; overflow:hidden;
                   text-overflow:ellipsis; color:#2c7fb8; text-decoration:none;
                   cursor:pointer; border-bottom:1px dashed #2c7fb8;",
          title = "Click for the full breakdown",
          sprintf("%s, %s", nm, abbr)),
        tags$div(style = "background:#eee; height:14px; border-radius:3px; position:relative;",
          tags$div(style = sprintf(
            "background:#2c7fb8; height:14px; width:%.1f%%; border-radius:3px;",
            bar_width_pct))),
        tags$span(style = "font-size:10px; text-align:right; color:#333;",
                  sprintf("%.1f%%", match_pct)),
        if (has_anchor)
          tags$span(style = sprintf("font-size:10px; text-align:right; color:%s;",
                                    anchor_colour),
                    title = "Straight-line distance to the nearest location anchor",
                    fmt_km(r$anchor_km)),
        tags$a(href = rea_url, target = "_blank", rel = "noopener noreferrer",
               title = "View homes for sale on realestate.com.au",
               style = "color:#888; text-align:center;",
               icon("house-chimney", style = "font-size:12px;")))
    }

    rows <- lapply(seq_len(nrow(top)), render_row)

    tagList(
      tags$style(HTML(".top10-row:hover { background: #f3f3f3; }")),
      tags$div(style = "font-weight: bold; font-size: 11px; margin-bottom: 4px;",
               "Closest matches"),
      tags$div(style = "font-size: 9px; color: #888; margin-bottom: 6px;",
               "Click a suburb name for its full breakdown"),
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
                   "Try widening your region selection. The current region tree has no allowed suburbs."
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
    list_anchor <- anchor_state()

    dream           <- dream_refs_active()
    multi_ref       <- is.null(dream) && length(current_ref()) > 1
    ref_name_lookup <- if (multi_ref) {
      ref |>
        filter(suburb_code_2021 %in% current_ref()) |>
        transmute(code = suburb_code_2021,
                  label = paste0(display_name, ", ",
                                 state_abbr[state_name_2021])) |>
        { \(d) setNames(d$label, d$code) }()
    } else NULL

    # Reference values for the comparison strip. Single/multi reference:
    # the primary reference's archetypes. Dream mode: for each field, the
    # archetypes of every suburb picked for that field's theme.
    ref_info_row <- if (is.null(dream)) {
      info <- get_info(current_ref()[1])
      if (is.null(info)) NULL
      else lapply(setNames(nm = unname(comparison_fields)),
                  function(col) info[[col]])
    } else {
      lapply(setNames(nm = unname(comparison_fields)), function(col) {
        codes <- dream$theme_refs[[comparison_field_theme[[col]]]]
        unlist(lapply(codes, function(c) {
          info <- get_info(c)
          if (is.null(info)) NULL else info[[col]]
        }))
      })
    }

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
          if (!is.null(list_anchor)) tags$span(class = "anchor-note",
            title = "Straight-line distance between suburb centres",
            anchor_note(code, list_anchor),
            if (isTRUE(r$sim_rank != r$rank))
              tags$span(style = "color:#999;",
                        sprintf(" (#%d on similarity alone)", r$sim_rank))),
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
        # "Why this match": top 3 informative characteristics (with structural
        # ~100% matches called out separately so they don't dominate)
        tags$div(style = "font-size: 12px; color: #555; margin: 2px 0 8px 0;
                          font-style: italic;",
          format_why_sentence(raw_vals)
        ),
        # Comparison strip: match's headline archetypes, ticked when they
        # equal the reference suburb's (data: suburb_info summaries)
        build_comparison_strip(ref_info_row, get_info(code)),
        # Body: scoring chart (left) + suburb profile (right). The mini-map has
        # been retired; clicking "Show on map" opens a leaflet popup that
        # shows the suburb in regional context.
        tags$div(style = "display: flex; gap: 12px; align-items: flex-start;",
          tags$div(style = "flex: 2 1 0; min-width: 0;",
                   list_chart_svg(raw_vals, r$match)),
          tags$div(style = "flex: 5 1 0; min-width: 0;",
                   build_profile_panel(code, ref_codes = active_ref_codes(),
                                       show_header = FALSE, show_hint = FALSE))
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

  # Popup tracking: the next click on the basemap (not on a top-N polygon)
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

  # Persistent accent border around whichever suburb the side panel is
  # currently showing - distinct from the reference suburb's own black
  # fill, so it's clear at a glance which suburb's detail you're looking
  # at (as opposed to only relying on the side panel text). Skipped when
  # the selected suburb IS the reference, since that already has its own
  # distinct styling and doesn't need a second, overlapping treatment.
  observeEvent(list(clicked_suburb(), active_ref_codes()), {
    code  <- clicked_suburb() %||% active_ref_codes()[1]
    proxy <- leafletProxy("map") |> clearGroup("selected_highlight")
    if (is.null(code) || code %in% active_ref_codes()) return()
    hit <- shp_suburb |> dplyr::filter(suburb_code_2021 == code)
    if (!nrow(hit)) return()
    proxy |> addPolygons(data = hit, group = "selected_highlight",
                         fill = FALSE, color = "#2C7FB8", weight = 4,
                         opacity = 1)
  }, ignoreInit = FALSE)

  # Switching into or out of dream mode drops any earlier click, so the
  # side panel falls back to a suburb from the new reference set.
  observeEvent(dream_refs_active(), {
    clicked_suburb(NULL)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$map_side_panel <- renderUI({
    code <- clicked_suburb() %||% active_ref_codes()[1]
    if (is.null(code)) {
      return(tags$div(style = "padding: 12px; color: #888; font-size: 13px;",
        "Click any suburb on the map to see its details here."))
    }
    build_profile_panel(code, ref_codes = active_ref_codes(), show_header = TRUE,
                        anchor_text = anchor_note(code, anchor_state()))
  })

  # --- Click on empty map: find suburb, show popup with "set as reference"
  observeEvent(input$map_click, {
    cl <- input$map_click; req(cl$lng, cl$lat)

    # If the user just clicked a top-N polygon, map_click fires alongside
    # map_shape_click. Let the polygon's own popup show, don't double up.
    if (as.numeric(Sys.time()) - last_shape_clk() < 0.2) return()

    # If a popup is already open and this click is on the basemap rather
    # than another mapped polygon, just close it. The next click can then
    # open something new: standard "click outside to dismiss" expectation.
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
      active_dims <- names(settings_active())
      raw_cols <- raw_cols[sub("^raw_", "", raw_cols) %in% active_dims]
      vals <- as.list(as.data.frame(row)[, raw_cols, drop = FALSE])
      names(vals) <- sub("^raw_", "", names(vals))
      win_label <- if (is.null(dream_refs_active()) &&
                       length(current_ref()) > 1) {
        r <- ref |> filter(suburb_code_2021 == row$winning_ref)
        if (nrow(r) == 1) paste0(r$display_name, ", ",
                                 state_abbr[r$state_name_2021]) else NULL
      } else NULL
      build_popup(nm, abbr, row$match, row$rank,
                  row$national_rank, row$national_pct, vals, code,
                  winning_ref_label = win_label,
                  anchor_text = anchor_note(code, anchor_state(), straight_line = TRUE))
    } else build_popup_unranked(nm, abbr, code)

    leafletProxy("map") |>
      clearGroup("clickpop") |>
      addPopups(lng = cl$lng, lat = cl$lat, popup = html, group = "clickpop")
    popup_open(TRUE)
  })

  # Clicking a suburb name in the Top 10 panel opens the same detail
  # popup a direct polygon click would show, anchored at that suburb's
  # centroid. Reuses match_df() for the row and build_popup() for the
  # HTML, so the two entry points always render identically.
  observeEvent(input$top10_click, {
    code <- input$top10_click; req(code)
    hit  <- shp_suburb |> dplyr::filter(suburb_code_2021 == code)
    if (!nrow(hit)) return()
    co   <- sf::st_coordinates(sf::st_centroid(hit))[1, ]
    nm   <- suburb_code_to_name[[code]] %||% hit$suburb_name_2021[1]
    abbr <- suburb_code_to_abbr[[code]] %||% ""

    if (code %in% suburb_info$suburb_code_2021) clicked_suburb(code)

    cdf <- match_df()
    row <- if (!is.null(cdf)) cdf[cdf$suburb_b == code, , drop = FALSE] else NULL

    html <- if (!is.null(row) && nrow(row) == 1) {
      raw_cols <- grep("^raw_", names(row), value = TRUE)
      active_dims <- names(settings_active())
      raw_cols <- raw_cols[sub("^raw_", "", raw_cols) %in% active_dims]
      vals <- as.list(as.data.frame(row)[, raw_cols, drop = FALSE])
      names(vals) <- sub("^raw_", "", names(vals))
      win_label <- if (is.null(dream_refs_active()) &&
                       length(current_ref()) > 1) {
        r <- ref |> filter(suburb_code_2021 == row$winning_ref)
        if (nrow(r) == 1) paste0(r$display_name, ", ",
                                 state_abbr[r$state_name_2021]) else NULL
      } else NULL
      build_popup(nm, abbr, row$match, row$rank,
                  row$national_rank, row$national_pct, vals, code,
                  winning_ref_label = win_label,
                  anchor_text = anchor_note(code, anchor_state(), straight_line = TRUE))
    } else build_popup_unranked(nm, abbr, code)

    leafletProxy("map") |>
      clearGroup("clickpop") |>
      addPopups(lng = co[1], lat = co[2], popup = html, group = "clickpop")
    popup_open(TRUE)
  })

  # ===== Compare page =====================================================
  # Enforce the 10-suburb total cap: as references are added, the max
  # number of "other" suburbs shrinks. Trim any excess selection rather
  # than blocking it outright, since the picker itself has no dynamic
  # max-selection update available.
  observeEvent(list(input$compare_refs, input$compare_others), {
    n_ref <- length(input$compare_refs %||% character(0))
    allowed <- max(0, 10 - n_ref)
    others <- input$compare_others %||% character(0)
    if (length(others) > allowed) {
      updateVirtualSelect(session, "compare_others",
                          selected = head(others, allowed))
    }
  }, ignoreInit = TRUE)

  # Scores every "other" suburb against the Compare page's own reference
  # selection. Deliberately independent of the main search's current_ref()/
  # match_df() - the two pages don't share results, only settings_active()
  # (which characteristics count) stays global. Mirrors match_df()'s
  # single/multi-reference aggregation logic exactly (see compute_match
  # calls there), just scoped to a hand-picked suburb set instead of
  # ranked nationally.
  compare_data <- reactive({
    ref_codes   <- input$compare_refs   %||% character(0)
    other_codes <- input$compare_others %||% character(0)
    if (length(ref_codes) == 0 || length(other_codes) == 0) return(NULL)

    setts <- settings_active()
    mode  <- input$compare_mode %||% "max"

    rs_list <- setNames(lapply(ref_codes, function(code) {
      cached <- raw_cache[[code]]
      if (is.null(cached)) {
        cached <- load_raw(code)
        raw_cache[[code]] <- cached
      }
      cached
    }), ref_codes)

    med_list <- map(rs_list, function(rs) {
      raw_cols <- grep("^raw_", names(rs), value = TRUE)
      setNames(
        sapply(raw_cols, function(col) median(rs[[col]], na.rm = TRUE)),
        sub("^raw_", "", raw_cols))
    })

    per_ref <- imap(rs_list, function(rs, ref_code) {
      meds <- med_list[[ref_code]]
      df   <- compute_match(rs, setts, meds)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df |> mutate(winning_ref = ref_code, .before = 1)
    })
    per_ref <- compact(per_ref)
    if (length(per_ref) == 0) return(NULL)

    if (length(per_ref) == 1 || mode == "max") {
      combined <- bind_rows(per_ref) |>
        group_by(suburb_b) |>
        slice_max(match, n = 1, with_ties = FALSE) |>
        ungroup()
    } else {
      stacked    <- bind_rows(per_ref)
      raw_cols   <- grep("^raw_",   names(stacked), value = TRUE)
      score_cols <- grep("^score_", names(stacked), value = TRUE)
      win_per <- stacked |>
        group_by(suburb_b) |>
        slice_max(match, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(suburb_b, winning_ref)
      combined <- stacked |>
        group_by(suburb_b) |>
        summarise(match = mean(match, na.rm = TRUE),
                  across(all_of(c(raw_cols, score_cols)),
                        \(x) mean(x, na.rm = TRUE)),
                  .groups = "drop") |>
        left_join(win_per, by = "suburb_b") |>
        filter(!is.nan(match))
    }

    combined |> filter(suburb_b %in% other_codes)
  })

  output$compare_others_label <- renderUI({
    n_allowed <- max(0, 10 - length(input$compare_refs %||% character(0)))
    tags$label(sprintf("Suburbs to compare (up to %d)", n_allowed),
               style = "font-size:12px; font-weight:600;")
  })

  output$compare_body <- renderUI({
    ref_codes   <- input$compare_refs   %||% character(0)
    other_codes <- input$compare_others %||% character(0)
    setts       <- settings_active()

    if (length(ref_codes) == 0) {
      return(tags$div(style = "color:#888; font-size:13px; padding:20px 0;",
                      "Pick at least one reference suburb to begin."))
    }

    tagList(
      if (length(other_codes) == 0) {
        tags$div(style = "color:#888; font-size:13px; margin-bottom:16px;",
                "Pick one or more suburbs above to see match scores.")
      } else {
        tags$div(style = "overflow-x:auto; margin-bottom:20px;",
          build_compare_table(ref_codes, other_codes,
                              compare_data(), names(setts),
                              anchor = anchor_state()))
      },

      # All picked suburbs drawn together in the same charts (references
      # in greys, compared suburbs in colours), with each suburb's summary
      # sentence in the key above the charts.
      build_compare_charts(ref_codes, other_codes)
    )
  })
}

shinyApp(ui, server, enableBookmarking = "url")
