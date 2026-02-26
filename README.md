# mega-industrial-demography

> **Industrial Promotion Policies and Population Change**  
> A reproducible, multi-paper research repository on the demographic effects of industrial development policies in Argentina and Brazil.

[![R](https://img.shields.io/badge/R-%3E%3D4.3-blue)](https://www.r-project.org/)
[![targets](https://img.shields.io/badge/targets-pipeline-green)](https://docs.ropensci.org/targets/)
[![renv](https://img.shields.io/badge/renv-reproducible-orange)](https://rstudio.github.io/renv/)
[![Quarto](https://img.shields.io/badge/Quarto-%3E%3D1.4-blueviolet)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## Overview

This repository hosts a large-scale, multi-paper research program evaluating how industrial promotion and economic development policies reshape population dynamics at the subnational level. The empirical anchor is **Argentina** (scalable to Brazil and the broader Latin America / Global South).

### Research Questions

| # | Question | Paper |
|---|----------|-------|
| 1 | Do industrial promotion policies alter subnational population growth trajectories? | Paper 1 |
| 2 | Are effects mediated by internal and interregional migration flows? | Paper 2 |
| 3 | Do industrial zones trigger measurable land-use and forest transitions? | Paper 3 |
| 4 | Synthesis: unified structural model linking policy → demography → land-use | Book Chapter |

---

## Repository Structure

```
mega-industrial-demography/
│
├── README.md                    # This file
├── LICENSE                      # MIT License
├── CITATION.cff                 # Machine-readable citation
├── _targets.R                   # Pipeline orchestration (targets)
├── renv.lock                    # Reproducible R environment
│
├── data/
│   ├── raw/                     # Immutable raw data (never edited)
│   ├── processed/               # Cleaned, harmonized datasets
│   ├── external/                # External reference datasets
│   └── documentation/           # Data dictionaries, codebooks
│
├── R/
│   ├── 00_setup.R               # Package installation, environment
│   ├── 01_harmonization.R       # Census harmonization pipeline
│   ├── 02_spatial_linkage.R     # Geospatial processing
│   ├── 03_descriptive.R         # Exploratory analysis
│   ├── 04_models/
│   │   ├── did_models.R         # Difference-in-differences
│   │   ├── spatial_models.R     # Spatial econometrics
│   │   ├── bayesian_models.R    # Hierarchical Bayesian
│   │   └── migration_models.R   # Gravity & flow models
│   ├── 05_simulations.R         # Counterfactual scenarios
│   ├── 06_visualization.R       # Figures and maps
│   └── functions/
│       └── utils.R              # Shared utility functions
│
├── papers/
│   ├── paper_1_population_growth/
│   ├── paper_2_migration/
│   ├── paper_3_land_use/
│   └── book_chapter/
│
├── outputs/
│   ├── figures/                 # Publication-ready figures
│   ├── tables/                  # Publication-ready tables
│   ├── maps/                    # Spatial visualizations
│   └── presentations/           # Conference materials
│
├── replication/
│   ├── main_analysis.R          # Master replication script
│   ├── robustness_checks.R      # Alternative specifications
│   └── sensitivity_analyses.R   # Sensitivity tests
│
└── .github/
    └── workflows/
        └── ci.yml               # GitHub Actions CI
```

---

## Data Modules

| Module | Contents | Status |
|--------|----------|--------|
| **A — Census** | Argentina censuses 1970, 1991, 2001, 2010, 2022 (harmonized) | Planned |
| **B — Industrial Policy** | Industrial promotion zones (georeferenced), policy dates, incentive types | Planned |
| **C — Land Cover** | Forest cover time series (Hansen GFW adapted), land-use classification | Planned |
| **D — Economic** | Employment by sector, wages, industrial production (subnational) | Planned |
| **E — Migration** | Place-of-birth/residence matrices, 5-year flows, OD matrices | Planned |

> **Note:** Raw data are stored in `data/raw/` and are never modified. All data exceeding 100 MB should be tracked with [Git LFS](https://git-lfs.com/).

---

## Methodological Toolkit

| Method | Application | R Packages |
|--------|-------------|------------|
| Staggered Difference-in-Differences | Causal effect of industrial zone designation | `did`, `fixest`, `did_multiplegt` |
| Synthetic Control | Counterfactual construction for treated regions | `Synth`, `gsynth`, `scpi` |
| Spatial Econometrics | Spillover detection, spatial dependence | `spdep`, `spatialreg`, `sf` |
| Hierarchical Bayesian | Small-area estimation, partial pooling | `brms`, `rstanarm`, `INLA` |
| Gravity / Flow Models | Origin-destination migration | `migest`, custom implementation |
| Land-Use Change | Transition probability models | `lulcc`, custom R |
| Causal Inference | Matching, weighting | `MatchIt`, `WeightIt`, `cbps` |
| Scenario Projection | Demographic projections | `demography`, `popProj` |

---

## Papers

### Paper 1 — Population Growth and Industrial Policy
- **RQ:** Causal effect of industrial promotion on subnational population growth
- **Methods:** Staggered DiD, event study, synthetic control, spatial econometrics
- **Target journals:** *Regional Studies*, *Journal of Economic Geography*, *Economic Development Quarterly*

### Paper 2 — Migration as Mediator
- **RQ:** Migration as the key mechanism linking industrial policy to population change
- **Methods:** Gravity models, OD flow decomposition, mediation analysis
- **Target journals:** *Demographic Research*, *Population Space and Place*, *Migration Studies*

### Paper 3 — Land-Use and Environmental Transitions
- **RQ:** Industrial zones and land-use/forest cover transitions
- **Methods:** Land change modeling, forest transition analysis, spatial regression
- **Target journals:** *Land Use Policy*, *Environmental Research Letters*, *Applied Geography*

### Book Chapter — Conceptual Synthesis
- **Title:** *Industrial Policy and Demographic Restructuring: A Conceptual Synthesis*
- **Contribution:** Unified structural model; comparative Latin American perspective; policy implications

---

## Setup

### Prerequisites

- R ≥ 4.3
- Quarto ≥ 1.4
- Git + Git LFS

### Installation

```r
# Clone the repository
# git clone https://github.com/<your-org>/mega-industrial-demography.git
# cd mega-industrial-demography

# Restore R environment
install.packages("renv")
renv::restore()

# Verify pipeline (no data required for DAG check)
targets::tar_manifest()
```

### Running the Pipeline

```r
# Full pipeline
targets::tar_make()

# Inspect DAG
targets::tar_visnetwork()
```

### Rendering Papers

```bash
# Paper 1
quarto render papers/paper_1_population_growth/paper_1_population_growth.qmd

# All papers
quarto render
```

---

## Replication

A self-contained replication package is available in `replication/`. Run the master script:

```r
source("replication/main_analysis.R")
```

All outputs are saved to `outputs/`. The full pipeline from raw data to publication-ready figures and tables is deterministic and fully reproducible.

---

## Team & Collaboration

| Role | Responsibility |
|------|---------------|
| Lead researcher | All papers, pipeline design |
| Co-authors | Paper-specific contributions |
| Research assistants | Data processing, replication |

---

## Citation

If you use this repository, please cite:

```bibtex
@software{mega_industrial_demography,
  title        = {mega-industrial-demography: Industrial Promotion Policies and Population Change},
  author       = {{Research Team}},
  year         = {2024},
  url          = {https://github.com/<your-org>/mega-industrial-demography},
  license      = {MIT}
}
```

See also `CITATION.cff` for machine-readable citation metadata.

---

## License

MIT License — see [LICENSE](LICENSE) for details.
