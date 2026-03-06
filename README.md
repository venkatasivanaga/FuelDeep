# FuelDeep3D Capstone — TLS LiDAR Forest Fuel Segmentation

This capstone project builds a deep learning pipeline to segment **Terrestrial Laser Scanning (TLS) LiDAR** point clouds into fuel-relevant classes (initial scope: **3 classes**) using plot-level datasets collected in **Austin Cary Forest (UF forest)**.  
The goal is to reduce manual effort and improve consistency in forest structure/fuel mapping by providing a reproducible preprocessing + modeling + evaluation workflow.

## Project Highlights
- **Data:** TLS LiDAR point clouds (LAS/LAZ), plot-level files (~15–24M points, ~5–8GB per plot; 4+ plots available).
- **Models (Baselines):** PointNet + PointNeXt (advanced PointNet-style architecture) baseline implementations.
- **Scalability Focus:** Memory-safe preprocessing via **tiling + sub-sampling** to prevent crashes on standard workstations.
- **Interoperability:** Includes an **R package interface** that calls Python deep learning libraries via `reticulate`, with environment setup helpers for users.

---

## Repository Structure (How to Navigate)
- `src/` — Core Python source code (preprocessing, models, evaluation utilities).
- `notebooks/` — Exploration and debugging notebooks (data overview, preprocessing checks, baseline training).
- `data/` — Data directories (**raw/processed not committed to Git**). Use `data/sample/` for small demo subsets.
- `environment/` — Reproducible environment setup (conda/pip + R setup scripts).
- `r-package/` — R package wrapper for running the pipeline via `reticulate`.
- `docs/` — Project proposal, weekly reflections, figures, UI mockups.
- `reports/` — Saved metrics, plots, and final report artifacts.
- `scripts/` — Convenience scripts for preprocessing/training/evaluation.

---

## Quick Start (Recommended)
### 1) Environment Setup
**Option A: Conda (recommended)**
- See: `environment/environment.yml`
- Create env:
  - `conda env create -f environment/environment.yml`
  - `conda activate fueldeep3d`

**Option B: pip**
- See: `environment/requirements.txt`
- Install:
  - `pip install -r environment/requirements.txt`

### 2) R Package + Python (reticulate)
If using the R package wrapper, follow:
- `environment/README_env.md`
- Run the helper function in `environment/setup_env.R` (creates/links Python env for reticulate)

---

## Memory Management Note (Important)
TLS point clouds are high-density and memory intensive. Preprocessing uses **tiling and sub-sampling**:
- Tiling splits large plots into manageable blocks
- Sub-sampling controls point density for faster iteration
See: `src/preprocessing/tile_pointcloud.py` and `src/preprocessing/subsample.py`

---

## Baseline Training & Evaluation
- Train PointNeXt baseline: `src/models/pointnext_baseline.py`
- Train PointNet baseline: `src/models/pointnet_baseline.py`
- Metrics + reports: `src/eval/`

Planned evaluation outputs include:
- confusion matrix
- per-class precision/recall/F1
- segmentation metrics (e.g., mIoU) if applicable

---

## Data Privacy / Sharing
Raw TLS datasets are provided by SilvaLab and may be access-controlled. This repository does **not** store raw plot files.  
Only small demo subsets or derived artifacts appropriate for sharing should be placed in `data/sample/`.

---

## Collaborators
Instructor: @EdwinMarteZorrilla  
Advisor: Dr. Carlos Alberto Silva 

---

## Contact
Venkata Naga — venkatasivareddy003@gmail.com