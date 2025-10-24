Geo-Spatial Analysis
# 📊 Geo-Spatial Poverty Analysis in India

**Author:** [Your Name]  
**Program:** MSc Economics, University of Warwick  
**Supervisor:** [Supervisor Name]  
**Date:** [Month, Year]

---

## 🧠 Overview

This project analyzes the **spatial distribution and determinants of poverty across Indian districts** using spatial econometric methods.  
It combines **exploratory spatial data analysis (ESDA)** — such as Moran’s I and LISA cluster mapping — with **spatial regression modeling** (Spatial Lag Model) to understand how poverty in one region is influenced by that in neighboring areas.

The analysis is conducted in the Jupyter Notebook **`Geo_Spatial_Poverty.ipynb`**.

---

## 🎯 Objectives

1. To examine whether poverty exhibits **spatial autocorrelation** across districts.  
2. To identify **spatial poverty clusters** (high-high, low-low regions) using **LISA maps**.  
3. To estimate a **Spatial Lag Model (SLM)** to measure spillover effects of neighboring poverty.  
4. To investigate the impact of socioeconomic and demographic variables such as:
   - Total working population  
   - Scheduled Caste (SC) population share  
   - Scheduled Tribe (ST) population share  

---

## 🗂️ Data Description

| Dataset | Source | Level | Key Variables |
|----------|---------|--------|----------------|
| Poverty Data | NITI Aayog / NFHS / SECC (whichever used) | District | Poverty index / headcount ratio |
| Census Data | Census of India 2011 | District | Working population, SC%, ST% |
| Spatial Boundaries | GADM / Shapefile from Data.gov.in | District | Geometries for spatial analysis |

All datasets are merged using district identifiers to form a GeoDataFrame for spatial analysis.

---

## 🧩 Methodology

1. **Data Preprocessing**
   - Clean and merge socioeconomic and spatial data.
   - Standardize variables (`X_scaled`).
   - Generate a **spatial weights matrix** (`w`) using queen contiguity.

2. **Exploratory Spatial Data Analysis (ESDA)**
   - Compute **Global Moran’s I** to test for spatial autocorrelation in poverty.
   - Generate **LISA (Local Indicators of Spatial Association)** cluster maps to visualize local clusters.

3. **Spatial Econometric Modeling**
   - Estimate **Ordinary Least Squares (OLS)** as a baseline model.  
   - Estimate a **Spatial Lag Model (SLM)** using:
     ```python
     from spreg import ML_Lag
     slm = ML_Lag(y, X_scaled, w=w, name_y='poverty',
                  name_x=["Total Working Population","SC","ST"])
     print(slm.summary)
     ```
   - Interpret the spatial autoregressive coefficient (ρ) and variable effects.

4. **Diagnostics**
   - Moran’s I of residuals to confirm reduction in spatial dependence.
   - Compare OLS vs SLM using log-likelihood and AIC.

5. **Visualization**
   - Plot LISA cluster maps showing High-High and Low-Low poverty clusters.
   - Choropleth maps for key variables.

---

## 📈 Key Outputs

- **Global Moran’s I value:** indicates degree of spatial clustering.  
- **LISA cluster map:** highlights spatial clusters of poverty.  
- **SLM results:** show spatial dependence coefficient (ρ) and effects of explanatory variables.  
- **Model diagnostics:** AIC, residual spatial autocorrelation.

---

## 📚 Interpretation

- A positive and significant **ρ** indicates that **poverty in one district is positively associated with poverty in neighboring districts** — confirming spatial spillovers.  
- Negative coefficients for **working population** suggest higher employment reduces poverty.  
- Positive coefficients for **SC/ST shares** imply higher social vulnerability in those regions.

---

## 🧾 Dependencies

This notebook uses the following Python libraries:

```bash
numpy
pandas
geopandas
matplotlib
seaborn
esda
libpysal
spreg
