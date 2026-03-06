# Environment Setup (Python + R / reticulate)

This project uses Python deep learning libraries and optionally exposes an R wrapper via `reticulate`.

## Option A — Conda (recommended)
1. Create env:
   conda env create -f environment.yml
2. Activate:
   conda activate fueldeep3d

## Option B — pip
pip install -r requirements.txt

## R / reticulate users
- If using the R package wrapper in `r-package/`, configure reticulate to use the same Python env.
- This repo includes a baseline setup helper you can extend (see `setup_env.R`).
