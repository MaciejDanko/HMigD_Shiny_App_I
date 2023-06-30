
MODELS <<- c('(a) random slope & intercept model without gravity covariates, LFS included',
             '(b) random slope & intercept model with freedom of movement of workers as a single gravity covariate, LFS included',
             '(c) random intercept model with all gravity covariates, LFS included',
             '(d) random intercept model with freedom of movement of workers as a single gravity covariate, LFS included',
             '(e) random slope & intercept model without gravity covariates, LFS not included',
             '(f) random slope & intercept model with freedom of movement of workers as a single gravity covariate, LFS not included')

MODELS2 <<- c('random slope & intercept\nwithout gravity covariates\nLFS included',
              'random slope & intercept\nwith freedom of movement of\n workers, LFS included',
              'random intercept\nall gravity covariates\nLFS included',
              'random intercept\nwith freedom of movement of \n workers, LFS included',
              'random slope & intercept\nwithout gravity covariates\nLFS not included',
              'random slope & intercept\nwith freedom of movement of\nworkers, LFS not included')

FREEDOMCASES <<- c('(1a) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by IE in 2004',
                   '(1b) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by SE in 2004',
                   '(1c) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by UK in 2004',
                   '(1d) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by IE, SE, and UK in 2004',
                   '(2a) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by FI in 2006',
                   '(2b) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by IS in 2006',
                   '(2c) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by NO in 2006',
                   '(2d) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by FI, IS, and NO in 2006',
                   '(3) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by NL in 2007',
                   '(4) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by FR in 2008',
                   '(5a) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by BE in 2009',
                   '(5b) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by DK in 2009',
                   '(5c) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by BE and DK in 2009',
                   '(6a) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by AT in 2011',
                   '(6b) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by CH in 2011(*)',
                   '(6c) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by DE in 2011',
                   '(6d) Freedom of movement of CZ, EE, HU, LT, LV, PL, SI and SK workers granted by AT, CH(*), and DE in 2011',
                   '(7a) Freedom of movement of BG and RO workers granted by AT in 2014',
                   '(7b) Freedom of movement of BG and RO workers granted by BE in 2014',
                   '(7c) Freedom of movement of BG and RO workers granted by DE in 2014',
                   '(7d) Freedom of movement of BG and RO workers granted by FR in 2014',
                   '(7e) Freedom of movement of BG and RO workers granted by NL in 2014',
                   '(7f) Freedom of movement of BG and RO workers granted by UK in 2014',
                   '(7g) Freedom of movement of BG and RO workers granted by AT, BE, DE, FR, NL, and UK in 2014',
                   '(1e) Freedom of movement of CH, CY, CZ, EE, HU, LT, LV, MT, PL, SI and SK workers granted by IE in 2004',
                   '(1f) Freedom of movement of CH, CY, CZ, EE, HU, LT, LV, MT, PL, SI and SK workers granted by SE in 2004',
                   '(1g) Freedom of movement of CH, CY, CZ, EE, HU, LT, LV, MT, PL, SI and SK workers granted by UK in 2004',
                   '(1h) Freedom of movement of CH, CY, CZ, EE, HU, LT, LV, MT, PL, SI and SK workers granted by IE, SE, and UK in 2004',
                   '(5d) Freedom of movement of BG, CZ, EE, HU, LT, LV, PL, RO, SI and SK workers granted by DK in 2009',
                   '(7h) Freedom of movement of BG and RO workers granted by AT, BE, DE, FR, LU, MT, NL, and UK in 2014'
)
aCItxt<<-'Here we use very rough and naive estimates of reliability by aggregating estimated percentiles. Estimation of proper credibility intervals would require sampling from the posterior distribution,
          unfortunately, such pre-calculated credibility intervals for all possible combinations of aggregated flows would take up a lot of space and cannot be uploaded to the server for technical reasons.'
aDATAtxt<<-"Please note that aggregated data points are calculated by summing incompatible data of different quality and definition of a migrant by minimum duration of stay (migration flows measured in different units) and therefore may be misleading."

email <- 'danko@demogr.mpg.de'
email_html<-paste0("<a href='mailto:", email, "'>", email, "</a>")

about_list <- HTML(
  
  "<h3><b>HMigD I App:</b> The Human Migration Database I</h3>",
  "<h4>Country- and year-specific harmonized migration flows for European countries in 2002-2018</h3>",
  "<br>",
  #"<h4>First release (unofficial) March 2023</h4>",
  "<h4>Version 1.2.0, Jun 2023</h4>",
  "<br>",
  paste0("<h4>Author and maintainer: Maciej J. Dańko [",email_html,"] </h4>"),
  '<h4>Max Planck Institute for Demographic Research, Rostock, Germany </h4>',
  
  '<hr style="border-top: 3px solid"',
  
  "<p>Welcome to HMigD I App!</p>", 
  "<br>",
  '<p>To quickly navigate to the results section, click on the <span style="color:green;"><b>Model estimates &amp; comparison</b></span> panel. For direct access to data download, click on the <span style="color:green;"><b>model mixing and download</b></span> panel.</p>',
  "<br>",
  "This Shiny app summarizes the assumptions and results of the Bayesian models used to estimate migration flows. The models integrate administrative and Labor Force Survey (LFS) data while accounting for data quality issues and differences in migration definitions. The model is capable of providing reliable and harmonized estimates of bilateral migration flows for 31 countries within the 2002-2018 period. Here's a short overview of the features and capabilities of the app:",
  "<br>","<br><ul>",
  "<li><b>Data sources and quality assessment:</b> The app provides an overview of the input database (administrative data and LFS), including the results of data quality assessment used in the model. The data quality issues include: (1) Accuracy issues: Random errors in data collection can affect population and migration registers, as well as survey data. (2) Undercounting: Non-systematic bias in migration estimates due to registration failures and non-response in surveys. (3) Inconsistencies in coverage: Systematic biases resulting from rules in the data collection process that exclude certain population segments. (4) Inconsistent migration definitions: Varying international duration of stay criteria.</li>",
  "<li><b>Covariates:</b> The app provides an overview of the different gravity covariates used in some models.</li>",
  "<li><b>Model schemes:</b> The app provides an overview of the different models used in the app.</li>",
  "<li><b>Results inspection and user survey:</b> The app allows for inspecting estimated flows of different models by comparing them to raw input data and their quality features. Additionally, the app provides a feature where users can share their thoughts on specific flows.</li>",
  "<li><b>Model comparison:</b> The app allows for comparing results obtained from different models to assess the effectiveness of different modeling approaches.</li>",
  "<li><b>Flow aggregation:</b> The app enables the aggregation of migration flows from different countries using various scenarios related to the freedom of movement covariate.</li>",
  "<li><b>Circular flows plots:</b> The app provides circular flow plots for selected countries and years.</li>",
  "<li><b>Model mixing:</b> The app allows for combining the results obtained from different models.</li>",
  "</ul>",
  "<br>",
  "<p>The copy of this project can be found on Github: <a href='https://github.com/MaciejDanko/HMigD_Shiny_App_I'> https://github.com/MaciejDanko/HMigD_Shiny_App_I </a></p>",
  "<br>",
  '<p>The app also serves as supplementary material for the paper titled "<i>Modeling international migration flows by integrating multiple data sources</i>" [in preparation].</p>',
  "<br>",
  "<h4><b>Acknowledgements:</b> I would like to thank Emilio Zagheni and Arkadiusz Wiśniowski for their valuable comments on earlier versions of this app.</h4>",
  "<br>",
  "<br>"
)
