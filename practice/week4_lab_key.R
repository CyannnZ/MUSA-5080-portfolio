# =============================================================================
# MUSA 5080 — Week 4 Lab: "Who Does Indego Reach?"
# INSTRUCTOR KEY — all blanks filled in
#
# Run top to bottom before class. Watch for the lines marked  ### CHECK ###
# those are the moments the lab asks students to stop and discuss.
# =============================================================================

library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

# Sanity check on the API key
stopifnot(Sys.getenv("CENSUS_API_KEY") != "")


# -----------------------------------------------------------------------------
# PART 1: Tracts with geometry, plus the Week 3 reliability work
# -----------------------------------------------------------------------------

philly_tracts <- get_acs(
  geography = "tract",
  state     = "PA",
  county    = "Philadelphia",
  variables = c(med_inc   = "B19013_001",
                total_pop = "B01003_001"),
  year         = 2023,
  output       = "wide",
  geometry     = TRUE,
  progress_bar = FALSE
)

class(philly_tracts)      # "sf" "tbl_df" "tbl" "data.frame"
nrow(philly_tracts)       # ~408 tracts on 2020 boundaries

philly_tracts <- philly_tracts %>%
  filter(total_popE > 0) %>%
  mutate(
    med_inc_se  = med_incM / 1.645,
    med_inc_cv  = (med_inc_se / med_incE) * 100,
    reliability = factor(
      case_when(
        med_inc_cv < 12  ~ "High",
        med_inc_cv <= 40 ~ "Moderate",
        TRUE             ~ "Low"
      ),
      levels = c("High", "Moderate", "Low")   # sets legend + color order
    )
  )

# Geometry survived the mutate
names(philly_tracts)

# First map
ggplot(philly_tracts) +
  geom_sf(aes(fill = med_incE), color = NA) +
  scale_fill_viridis_c(labels = scales::dollar, na.value = "grey90") +
  theme_void() +
  labs(title = "Median household income, Philadelphia tracts", fill = NULL)

### CHECK ### Tracts with people but no income estimate
philly_tracts %>% filter(is.na(med_incE)) %>% nrow()

# Reliability breakdown (how much of the city is shaky?)
philly_tracts %>% st_drop_geometry() %>% count(reliability)


# -----------------------------------------------------------------------------
# PART 1b: Join by WHAT — and watch it fail
# -----------------------------------------------------------------------------

income_15 <- get_acs(
  geography = "tract",
  state     = "PA",
  county    = "Philadelphia",
  variables = c(med_inc_15 = "B19013_001"),
  year         = 2015,
  output       = "wide",
  geometry     = FALSE,
  progress_bar = FALSE
) %>%
  select(GEOID, med_inc_15E, med_inc_15M)

### CHECK ### Different row counts: 2010 vs 2020 tract boundaries
nrow(income_15)         # 2010-vintage tracts
nrow(philly_tracts)     # 2020-vintage tracts

philly_tracts <- philly_tracts %>%
  left_join(income_15, by = "GEOID")

nrow(philly_tracts)     # unchanged — left_join never adds or drops left rows

### CHECK ### Count rows, count NAs — every single time
philly_tracts %>%
  st_drop_geometry() %>%
  summarize(n_tracts      = n(),
            no_2015_match = sum(is.na(med_inc_15E)))

# 2023 tracts with no 2015 partner
unmatched_23 <- philly_tracts %>%
  st_drop_geometry() %>%
  filter(is.na(med_inc_15E)) %>%
  select(GEOID, NAME)
head(unmatched_23, 10)

# 2015 tracts with no 2023 partner (anti_join keeps only the non-matches)
unmatched_15 <- income_15 %>%
  anti_join(st_drop_geometry(philly_tracts), by = "GEOID")
head(unmatched_15, 10)

# >>> Grab two or three real GEOIDs from these for the Board 0C drawing. <<<

# Two different kinds of NA: no match at all, vs. matched but suppressed
philly_tracts %>%
  st_drop_geometry() %>%
  mutate(in_2015_table = GEOID %in% income_15$GEOID) %>%
  count(in_2015_table, suppressed = is.na(med_inc_15E))


# -----------------------------------------------------------------------------
# PART 2: Stations, and the CRS rule
# -----------------------------------------------------------------------------

stations <- st_read("https://www.rideindego.com/stations/json/", quiet = TRUE)

nrow(stations)
names(stations)         # kioskId, name, totalDocks, ... (check before class)

st_crs(philly_tracts)$epsg   # 4269 (NAD83)
st_crs(stations)$epsg        # 4326 (WGS84)

### CHECK ### This SHOULD error. Run it so you can show the message.
# st_join(stations, philly_tracts)
#   Error: st_crs(x) == st_crs(y) is not TRUE

# Both layers already have a CRS, so this is a translation, not a label:
philly_tracts <- st_transform(philly_tracts, crs = 2272)
stations      <- st_transform(stations,      crs = 2272)

st_crs(philly_tracts)$units   # "us-ft"  → every distance below is in FEET


# -----------------------------------------------------------------------------
# PART 3: Join by WHERE
# -----------------------------------------------------------------------------

stations_with_tract <- st_join(stations, philly_tracts)

stations_with_tract %>%
  st_drop_geometry() %>%
  select(name, GEOID, med_incE, reliability) %>%
  head()

# Route 1: join, then collapse
station_counts <- stations_with_tract %>%
  st_drop_geometry() %>%
  count(GEOID, name = "n_stations")

# Route 2: count directly
philly_tracts <- philly_tracts %>%
  mutate(n_stations = lengths(st_intersects(., stations)))

### CHECK ### These will NOT match. This is the discussion.
sum(philly_tracts$n_stations)
nrow(stations)

# Why #1: stations that fall in no tract (river, pier, outside the layer)
stations_with_tract %>% st_drop_geometry() %>% filter(is.na(GEOID)) %>% nrow()

# Why #2: stations landing in more than one tract (exactly on a boundary)
sum(lengths(st_intersects(stations, philly_tracts)) > 1)


# -----------------------------------------------------------------------------
# PART 4: Distance to the nearest station
# -----------------------------------------------------------------------------

centroids <- st_centroid(st_geometry(philly_tracts))

### CHECK ### st_distance returns a MATRIX, not a column
dim(st_distance(centroids, stations))    # ~408 x ~300

nearest_idx <- st_nearest_feature(centroids, stations)

philly_tracts <- philly_tracts %>%
  mutate(
    dist_ft = as.numeric(
      st_distance(centroids, stations[nearest_idx, ], by_element = TRUE)
    ),
    dist_mi = dist_ft / 5280
  )

summary(philly_tracts$dist_mi)

philly_tracts %>%
  st_drop_geometry() %>%
  slice_max(dist_mi, n = 5) %>%
  select(NAME, med_incE, med_inc_cv, dist_mi)


# -----------------------------------------------------------------------------
# PART 5: Buffers, filters, and cutting the dough
# -----------------------------------------------------------------------------

# Half a mile = 2640 FEET (units check happened in Part 2)
station_zone <- stations %>%
  st_buffer(dist = 2640) %>%
  st_union()

# Pick whole cookies: tracts that never touch the zone
outside_zone <- philly_tracts %>%
  st_filter(station_zone, .predicate = st_disjoint)
nrow(outside_zone)

# Cut the dough: share of each tract's area inside the zone
# st_make_valid() first — st_intersection() fails on self-intersecting polygons,
# and cartographic boundary files sometimes contain them.
philly_tracts <- philly_tracts %>%
  st_make_valid() %>%
  mutate(tract_area = as.numeric(st_area(.)))

covered <- philly_tracts %>%
  st_intersection(station_zone) %>%
  mutate(covered_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(covered_area = sum(covered_area), .groups = "drop")

philly_tracts <- philly_tracts %>%
  left_join(covered, by = "GEOID") %>%
  mutate(pct_covered = replace_na(covered_area / tract_area, 0) * 100)

summary(philly_tracts$pct_covered)

# NOTE: st_area() and st_distance() return "units" objects, not plain numbers.
# as.numeric() strips the unit so arithmetic and ggplot behave. Backup slide A2.
# NOTE: the  .  inside mutate(lengths(st_intersects(., stations)))  is the
# magrittr pipe's dot. With the native |> pipe it errors. Keep %>% in this lab.


# -----------------------------------------------------------------------------
# PART 6: One worked version of the challenge
# (Their definitions will differ — that's the point of the exercise)
# -----------------------------------------------------------------------------

# Definition 1 of "low income": bottom quartile of tract median income
inc_q25 <- quantile(philly_tracts$med_incE, 0.25, na.rm = TRUE)
inc_q25

philly_tracts <- philly_tracts %>%
  mutate(low_income = med_incE < inc_q25)

# Three definitions of "access", three different answers
answers <- philly_tracts %>%
  st_drop_geometry() %>%
  filter(low_income) %>%
  summarize(
    low_income_tracts = n(),
    no_access_dist    = sum(dist_mi > 0.5),        # centroid > 1/2 mi
    no_access_touch   = sum(pct_covered < 0.01),   # zone effectively never touches
    no_access_cover   = sum(pct_covered < 50)      # <50% of area covered
  )
answers

### CHECK ### How much does the answer move with the definition? (a lot)

# Reliability of those "low income" labels
philly_tracts %>%
  st_drop_geometry() %>%
  filter(low_income, dist_mi > 0.5) %>%
  count(reliability)

# The map
target <- philly_tracts %>% filter(low_income, dist_mi > 0.5)

ggplot() +
  geom_sf(data = philly_tracts, fill = "grey92", color = "white", linewidth = 0.05) +
  geom_sf(data = target, aes(fill = reliability), color = "white", linewidth = 0.05) +
  geom_sf(data = stations, color = "grey30", size = 0.4) +
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  theme_void() +
  labs(
    title    = "Low-income tracts more than a half mile from an Indego station",
    subtitle = "Low income = bottom quartile of tract median income; distance from tract centroid",
    fill     = "Income estimate reliability",
    caption  = "Sources: ACS 2019–2023 5-year estimates; Indego station feed"
  )


# -----------------------------------------------------------------------------
# PRE-CLASS CHECKLIST
# -----------------------------------------------------------------------------
# [ ] Indego feed loaded, and nrow(stations) matches what the slides say
# [ ] Column name for station name is still "name" in the feed
# [ ] Wrote down real unmatched GEOIDs for Board 0C
# [ ] Wrote down sum(n_stations) vs nrow(stations) and the reason for the gap
# [ ] Confirmed whether the income class breaks in the FAQ deck suit this distribution
# [ ] If the feed is down: save stations to data/indego_stations.geojson and
#     swap in  stations <- st_read("data/indego_stations.geojson")
