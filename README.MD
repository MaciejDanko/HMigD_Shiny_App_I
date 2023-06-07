# HMigD I App: The Human Migration Database I
### Country- and year-specific harmonized migration flows for European countries in 2002-2018

Shiny application

Version 1.1.3, May 2023

Author and maintainer: Maciej J. Dańko [email1](mailto:danko@demogr.mpg) [email2](mailto:maciej.danko@gmail.com)

Max Planck Institute for Demographic Research, Rostock, Germany

This Shiny app summarizes the assumptions and results of the Bayesian models used to estimate migration flows. The models integrate administrative and Labor Force Survey (LFS) data while accounting for data quality issues and differences in migration definitions. The model is capable of providing reliable and harmonized estimates of bilateral migration flows for 31 countries within the 2002-2018 period.

### Overview of Features and Capabilities

- **Data sources and quality assessment:** The app provides an overview of the input database (administrative data and LFS), including the results of data quality assessment used in the model. The data quality issues include:
    1. Accuracy issues: Random errors in data collection affecting population and migration registers and survey data.
    2. Undercounting: Non-systematic bias in migration estimates due to registration failures and non-response in surveys.
    3. Inconsistencies in coverage: Systematic biases resulting from rules in the data collection process that exclude certain population segments.
    4. Inconsistent migration definitions: Varying international duration of stay criteria.

- **Covariates:** The app provides an overview of the different gravity covariates used in some models.

- **Model schemes:** The app provides an overview of the different models used in the app.

- **Results inspection and user survey:** The app allows for inspecting estimated flows of different models by comparing them to raw input data and their quality features. Additionally, the app provides a feature where users can share their thoughts on specific flows.

- **Model comparison:** The app allows for comparing results obtained from different models to assess the effectiveness of different modeling approaches.

- **Flow aggregation:** The app enables the aggregation of migration flows from different countries using various scenarios related to the freedom of movement covariate.

- **Circular flows plots:** The app provides circular flow plots for selected countries and years.

- **Model mixing:** The app allows for combining the results obtained from different models.
