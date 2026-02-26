# mega-industrial-demography: Codebook
# Geographic Harmonization and Recoding Decisions

## 1. Geographic Concordance

### Province Codes (INDEC standardized, 2-digit)

All census rounds use INDEC province codes. No concordance needed across rounds for
provinces (stable units). Departments/partidos require harmonization (see below).

### Department Harmonization

Department boundaries changed between census rounds due to:
- **Administrative splits:** New departments created from existing ones
- **Boundary adjustments:** Minor shifts in department limits
- **Province reorganizations:** Limited instances of province boundary changes

**Harmonization decision:** All analyses use **2010 department boundaries** as the
reference geography. Pre-2010 census data are aggregated or interpolated to this
concordance using a minimum-overlap spatial crosswalk.

The crosswalk is stored as `data/documentation/geo_concordance.csv` with columns:
- `geo_dept_code`: Round-specific department code
- `census_year`: Census year
- `harmonized_dept`: Corresponding 2010 department code
- `weight`: Population-weighted overlap fraction (for disaggregation)

### CABA Treatment

The Ciudad Autónoma de Buenos Aires (CABA, code `02`) is treated as a single
department-equivalent unit. It is excluded from industrial zone analyses as
it contains no traditional industrial promotion zones.

---

## 2. Variable Recoding Decisions

### Sex (`sex_label`)

| Round | Raw Code | Harmonized |
|---|---|---|
| 1970 | 1 = Varón, 2 = Mujer | "Male", "Female" |
| 1991 | 1 = Varón, 2 = Mujer | "Male", "Female" |
| 2001 | 1 = Varón, 2 = Mujer | "Male", "Female" |
| 2010 | 1 = Varón, 2 = Mujer | "Male", "Female" |
| 2022 | 1 = Varón, 2 = Mujer, 3 = No binario | "Male", "Female", "Other" |

**Note:** 2022 introduces "Non-binary" option. For cross-round comparability, analyses
stratify by Male/Female; "Other" is excluded from longitudinal comparisons but reported
separately for 2022.

### Educational Level (`educ_level`)

Harmonized to 3 categories for cross-round comparability:

| Category | Description | Approx. equivalent |
|---|---|---|
| `primary_or_less` | No schooling through complete primary | < 7 years |
| `secondary` | Incomplete through complete secondary | 7–12 years |
| `tertiary_plus` | Tertiary, university, postgraduate | > 12 years |

Mapping from round-specific codes is documented in `R/01_harmonization.R`,
function `recode_educ()`.

### Employment Sector (`sector_isic`)

Raw census sector codes are mapped to broad ISIC Rev. 2 sections:
- Agriculture (codes 1–5)
- Industry (codes 10–45) ← includes manufacturing, mining, construction
- Commerce (codes 50–55)
- Transport (codes 60–64)
- Finance (codes 65–74)
- Services (codes 75–99)

**Note:** Detailed sectoral analysis uses round-specific codes with round-specific
crosswalks maintained in `data/documentation/sector_crosswalk.csv` (to be created).

---

## 3. Population Weights

All person-level aggregations use the `hh_weight` (expansion factor) variable. For
department-level counts used in the DiD analysis, weighted counts are summed:

```r
pop = sum(hh_weight, na.rm = TRUE)
```

In rounds where weights are not available at the micro level (early rounds with
published tables only), we use published INDEC department-level counts directly.

---

## 4. Migration Variable Construction

### Lifetime Migration
- **Definition:** Person born in a different province than current residence province
- **Variable:** `geo_prov_code != birth_prov`
- **Available:** All rounds

### Period Migration (5-year)
- **Definition:** Person residing in a different province 5 years ago
- **Variable:** `geo_prov_code != res5yr_prov`
- **Available:** 1991, 2001, 2010, 2022

**Limitation:** "5 years ago" refers to the census reference date, not a fixed calendar
year. Across rounds, this window overlaps differently with the inter-census interval.

---

## 5. Industrial Zone Treatment Variable

### Construction

1. Load georeferenced zone polygons from `data/raw/industrial_zones/industrial_zones.gpkg`
2. For each census year, identify zones with `start_year <= census_year` AND
   (`end_year >= census_year` OR `is.na(end_year)`)
3. Spatial join: assign each department a binary indicator `treated = TRUE` if the
   dept polygon intersects any active zone polygon at that census year

### Treatment Variants

| Variable | Description |
|---|---|
| `treated` | Binary: any active zone in department |
| `n_zones` | Count of active zones in department |
| `first_treat_yr` | Year of first zone activation (= `gvar` in DiD) |
| `incentive_types` | Concatenated list of incentive types active |

---

## 6. Known Data Quality Issues

| Issue | Affected Round(s) | Action |
|---|---|---|
| Under-coverage in remote departments | 1970 | Flag with `flag_missing_geo`; exclude from primary analysis |
| Inconsistent sector coding for construction | 1991 | Collapse construction into Industry category |
| Missing occupation data for self-employed | All | Use sector (not occupation) as primary employment indicator |
| CABA reclassification | 1970 | 1970 CABA included in GBA — handle with concordance table |
