# mega-industrial-demography: Data Dictionary
# Module A–E: Variable Definitions and Sources

## Overview

This dictionary covers all five data modules used in the project. For each variable,
we document: name, description, units, source, availability by census year, and
any harmonization notes.

---

## Module A: Census Microdata

**Source:** INDEC (Instituto Nacional de Estadística y Censos), Argentina  
**Rounds:** 1970, 1991, 2001, 2010, 2022  
**Unit of analysis:** Individual (person record)  
**Access:** IPUMS International, INDEC microdata portal (2001+)

| Variable (harmonized) | Description | Type | Units | Available Rounds |
|---|---|---|---|---|
| `census_year` | Census round year | Integer | Year | All |
| `geo_prov_code` | Province code (INDEC standardized) | String | — | All |
| `geo_dept_code` | Department/partido code | String | — | All |
| `age` | Age in completed years | Integer | Years | All |
| `sex` | Sex at census | Factor | M/F | All |
| `sex_label` | Harmonized sex label | Character | "Male"/"Female" | All |
| `age_group5` | 5-year age group | Ordered Factor | — | All |
| `birth_prov` | Province of birth | String | — | All |
| `res5yr_prov` | Province of residence 5 years ago | String | — | 1991+ |
| `educ_max` | Highest educational level attained (raw, round-specific) | Integer | Code | All |
| `educ_level` | Harmonized educational level (3 categories) | Factor | — | All |
| `employed` | Labor force activity status (raw) | Integer | Code | All |
| `sector` | Industrial sector of employment (raw) | Integer | ISIC code | All |
| `sector_isic` | Harmonized ISIC sector (broad) | Factor | — | All |
| `hh_weight` | Person weight (expansion factor) | Numeric | — | All |
| `flag_missing_geo` | Flag: missing geographic identifiers | Logical | — | All |
| `flag_missing_age` | Flag: age imputed or missing | Logical | — | All |

---

## Module B: Industrial Promotion Zones

**Source:** Argentine national legislation (Ley 22.021, Ley 19.640, and derivatives),  
provincial laws, MEOEI registers; manually georeferenced  
**Unit of analysis:** Industrial zone polygon  
**Temporal coverage:** 1972–present

| Variable | Description | Type | Units |
|---|---|---|---|
| `zone_id` | Unique zone identifier | String | — |
| `zone_name` | Zone name | String | — |
| `law_number` | Enabling legislation | String | — |
| `start_year` | Year of policy activation | Integer | Year |
| `end_year` | Year of policy termination (NA if still active) | Integer | Year |
| `incentive_type` | Type of incentive ("tax", "infra", "regulatory", "mixed") | Factor | — |
| `sector_code` | Primary targeted sector | String | ISIC |
| `area_km2` | Zone area | Numeric | km² |
| `geometry` | Zone boundary polygon | Geometry | WGS84 |

---

## Module C: Land Cover and Environmental Data

**Sources:**  
- Hansen et al. (2013) Global Forest Change (30m, annual, 2000–2022)  
- MapBiomas Argentina (land-use classification, 30m, 1985–2022; if available)

**Unit of analysis:** Department (aggregated from raster)  
**Variables:**

| Variable | Description | Type | Units | Source |
|---|---|---|---|---|
| `treecover_pct` | % of department area with ≥30% tree canopy density (baseline 2000) | Numeric | % | Hansen GFW |
| `forest_loss_ha` | Cumulative forest loss area since 2000 | Numeric | ha | Hansen GFW |
| `forest_gain_ha` | Cumulative forest gain since 2000 | Numeric | ha | Hansen GFW |
| `lulc_urban_pct` | % urban land cover | Numeric | % | MapBiomas |
| `lulc_agri_pct` | % agricultural land cover | Numeric | % | MapBiomas |
| `lulc_industrial_pct` | % industrial land cover | Numeric | % | MapBiomas |

---

## Module D: Economic Indicators

**Sources:** INDEC Economic Census, Ministerio de Economía, CAVIAR, OEDE  
**Unit of analysis:** Department-year  

| Variable | Description | Units |
|---|---|---|
| `emp_formal` | Formal employment count | Workers |
| `emp_informal` | Estimated informal employment | Workers |
| `wage_avg_real` | Average real wage (constant ARS or USD) | ARS/USD |
| `industrial_prod_idx` | Industrial production index (base year = 2004) | Index |
| `gva_per_cap` | Gross value added per capita | AR$ |

---

## Module E: Migration Data

**Source:** Derived from Module A (census microdata)  
**Unit of analysis:** Origin-destination province pair × year

| Variable | Description | Type |
|---|---|---|
| `origin` | Province of origin (birth or 5-year residence) | String |
| `destination` | Province of current residence | String |
| `census_year` | Census round | Integer |
| `flow` | Weighted migration flow count | Numeric |
| `migration_type` | "lifetime" (birth vs. current) or "period" (5-year) | Factor |
| `sex_label` | Sex of migrants | Factor |
| `educ_level` | Education of migrants | Factor |

---

## Geographic Units and Concordance

Argentina is divided into **24 provinces** and approximately **530 departments/partidos**.
Geographic boundaries have changed between census rounds. We harmonize to 2010 boundaries
using a crosswalk documented in `data/documentation/codebook.md`.

INDEC province codes are 2-digit strings (zero-padded). Department codes are
3-digit strings. The full code is a 5-digit string: `PPDDDD` where `PP` = province and
`DDD` = department.

---

## Download Instructions

| Module | Dataset | URL |
|---|---|---|
| A (Census) | IPUMS International | https://international.ipums.org |
| A (Census 2001+) | INDEC portal | https://www.indec.gob.ar |
| B (Zones) | Assembled manually — see codebook | — |
| C (Hansen GFW) | Global Forest Change | https://earthenginepartners.appspot.com/science-2013-global-forest |
| C (MapBiomas) | MapBiomas Argentina | https://argentina.mapbiomas.org |
| D (Economic) | OEDE / INDEC | https://www.trabajo.gob.ar/estadisticas/oede |
| E (Migration) | Derived from Module A | — |
