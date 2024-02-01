
MODELS <<- c('(a) Freedom of movement of workers excluded, LFS included',
             '(b) Freedom of movement of workers excluded, LFS excluded',
             '(c) Freedom of movement of workers simple, LFS included',
             '(d) Freedom of movement of workers simple, LFS excluded',
             '(e) Freedom of movement of workers new EU -> old EU+, LFS included',
             '(f) Freedom of movement of workers new EU -> old EU+, LFS excluded'
)

MODELS2 <<- c('(a) Freedom of movement of \nworkers excluded, LFS included',
              '(c) Freedom of movement of \nworkers excluded, LFS excluded',
              '(b) Freedom of movement of \nworkers simple, LFS included',
              '(d) Freedom of movement of \nworkers simple, LFS excluded',
              '(e) Freedom of movement of \nworkers new EU -> old EU+, \nLFS included',
              '(f) Freedom of movement of \nworkers new EU -> old EU+, \nLFS excluded'
)
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

Accu_comment_imm<<-'"Low (QuantMig)" indicates that QuantMig estimates were utilized for bilateral flows between Greece (GR), Hungary (HR), Malta (MT), and Portugal (PT). These countries do not provide any bilateral data, necessitating the use of QuantMig estimates to fill completely empty corridors between them (please refer to the immigration/emigration data sources panels). "High (*)" for Germany (DE) implies that while Germany is generally classified as having high accuracy for most reported bilateral flows, for flows to/from Denmark (DK), we assume low accuracy due to the high probability of using mirror statistics by DE or DK for these flows.'
Accu_comment_emi<<-'"High (*)" for Germany (DE) implies that while Germany is generally classified as having high accuracy for most reported bilateral flows, for flows to/from Denmark (DK), we assume low accuracy due to the high probability of using mirror statistics by DE or DK for these flows'

Under_comment_emi<<-'Undercounting was solely determined using pure classification in the UndercountMigScores model. While the app allows for the combination of scores derived from metadata, model, and expert opinions to calculate a new classification, in this specific context, we solely rely on the scores generated by the model.'
Under_comment_imm<<-paste(Under_comment_emi, 'As bilateral flows between Greece (GR), Hungary (HR), Malta (MT), and Portugal (PT) are derived from QuantMig estimates, we assume minimal undercounting for these corridors.')

email <- 'danko@demogr.mpg.de'
email_html<-paste0("<a href='mailto:", email, "'>", email, "</a>")

download_button_txt<-'Download as xlsx'
download_button_leg_txt<-'Download data'
img_format_leg_txt <- 'Plot file format'
save_plot_leg_txt <- 'Save plot'
save_plot_button_txt <- 'Download plot'
about_list <- HTML(
  '<div style="text-align: center;">',
  '<h3><b>HMigD I App:</b> The Human Migration Database I App</h3>',
  '<br>',
  paste0('<h4>Maciej J. Dańko [', email_html, '] </h4>'),
  '<h4>Max Planck Institute for Demographic Research, Rostock, Germany </h4>',
  '<br>',
  '<h4>Version 3.1.1 Beta, February 2024</h4>',
  '<br>',
  '</div>',
  '<hr style="border-top: 3px solid">',
  '<p>Welcome to HMigD I App!</p>',
  '<br>',
  '<div style="text-align: justify;">',
  '<p>This app is a part of the <b>H</b>uman <b>Mig</b>ration <b>D</b>atabase project. Read more about it at <a href="https://www.demogr.mpg.de/en/research_6120/digital_and_computational_demography_zagheni_11666/migration_and_mobility_11669/measuring_and_modeling_migration_and_mobility_11671/the_human_migration_database_hmigd_12092/details">www.demogr.mpg.de</a>.</p>.',
  '<br>',
  '<p>To quickly navigate to the results section, click on the <span style="color:green;"><b>Model estimates &amp; comparison</b></span> panel or <a href="tutorial.html" download="tutorial.html" target="_blank">download the tutorial</a>.</p>',
  '<br>',
  '<p>This Shiny application provides a comprehensive overview of harmonized migration flows for each country and year, using Bayesian models. These models effectively integrate administrative and Labor Force Survey (LFS) data, addressing data quality challenges and variations in migration definitions. The application offers reliable estimates of bilateral migration flows for 31 countries, spanning the years 2002 to 2019. Below, you will find a summary of the application\'s key features and capabilities.</p>',
  '<br>',
  '<ul>',
  '<li><b>Data sources and quality assessment:</b> The app provides an overview of the input database (administrative data and LFS), including the results of data quality assessment used in the model. The data quality issues include: (1) Accuracy issues: Random errors in data collection can affect population and migration registers, as well as survey data. (2) Undercounting: Non-systematic bias in migration estimates due to registration failures and non-response in surveys. (3) Inconsistencies in coverage: Systematic biases resulting from rules in the data collection process that exclude certain population segments. (4) Inconsistent migration definitions: Varying international duration of stay criteria.</li>',
  '<li><b>Covariates:</b> The app provides an overview of the different gravity covariates used in some models.</li>',
  '<li><b>Model schemes:</b> The app provides an overview of the different models used in the app.</li>',
  '<li><b>Results inspection and user survey:</b> The app allows for inspecting estimated flows of different models by comparing them to raw input data and their quality features. Additionally, the app provides a feature where users can share their thoughts on specific flows.</li>',
  '<li><b>Model comparison:</b> The app allows for comparing results obtained from different models to assess the effectiveness of different modeling approaches.</li>',
  '<li><b>Flow aggregation:</b> The app enables the aggregation of migration flows from different countries using various scenarios related to the freedom of movement covariate.</li>',
  '<li><b>Circular flows plots:</b> The app provides circular flow plots for selected countries and years.</li>',
  '<li><b>Model mixing:</b> The app allows for combining the results obtained from different models.</li>',
  '</ul>',
  '<br>',
  '<p>The copy of this project can be found on Github: <a href="https://github.com/MaciejDanko/HMigD_Shiny_App_I">https://github.com/MaciejDanko/HMigD_Shiny_App_I</a></p>',
  '<h4><b>Acknowledgements:</b> I would like to thank Emilio Zagheni, Frans Willekens, Arkadiusz Wiśniowski, Domantas Jasilionis and Dmitri Jdanov for their valuable comments on earlier versions of this app.</h4>',
  '<br>',
  '</div>',
  '<br>'
)




FREEDOM_SRC<-HTML(
  '<ul>
    <li>
    <p>Barker, E. R. (2023), ‘A timeline of freedom of movement in the European economic area’, Open Research Europe 2(133). Last updated: 11 Sep 2023.</p>
    <p>URL: <a href="https://open-research-europe.ec.europa.eu/articles/2-133">https://open-research-europe.ec.europa.eu/articles/2-133</a></p>
    </li>
    
    <li>
    <p>Cassis, I. (2012), ‘Free movement of workers and Schengen, Report ref. 1116899’. European Free Trade Association.</p>
    <p>URL: <a href="https://www.efta.int/sites/default/files/documents/advisory-bodies/parliamentary-committee/pc-reports/14-84773121112_Report_Free_Movement_Workers592473_557211_0.pdf">https://www.efta.int/sites/default/files/documents/advisory-bodies/parliamentary-committee/pc-reports/14-84773121112_Report_Free_Movement_Workers592473_557211_0.pdf</a></p>
    </li>
    
    <li>
    <p>Dølvik, J. E. and Eldring, L. (2006), ‘EU enlargement two years on: what challenges to the Nordic labor market?’</p>
    <p>URL: <a href="http://www.nordiclabourjournal.org/artikler/forskning/research-2006/eu-enlargement-two-years-on-what-challenges-to-the-nordic-labour-market">http://www.nordiclabourjournal.org/artikler/forskning/research-2006/eu-enlargement-two-years-on-what-challenges-to-the-nordic-labour-market</a></p>
    </li>
    
    <li>
    <p>Dølvik, J. E. and Eldring, L. (2006), ‘The Nordic Labor Market two years after the EU enlargement. Mobility, effects and challenges’, TemaNord 558.</p>
    </li>
    
    <li>
    <p>European Commission (2023), ‘Free movement - EU nationals’. Accessed: 2023-09-15.</p>
    <p>URL: <a href="https://ec.europa.eu/social/main.jsp?catId=457">https://ec.europa.eu/social/main.jsp?catId=457</a></p>
    </li>
    
    <li>
    <p>Kennedy, A. and Fiorucci, L. (2023), ‘Free movement of workers’, European Parliament: Fact Sheets on the European Union.</p>
    <p>URL: <a href="https://www.europarl.europa.eu/factsheets/en/sheet/41/free-movement-of-workers">https://www.europarl.europa.eu/factsheets/en/sheet/41/free-movement-of-workers</a></p>
    </li>
    
    <li>
    <p>Koikkalainen, S. (2011), ‘Free Movement in Europe: Past and Present’, Migration Policy Institute: Migration Information Source.</p>
    <p>URL: <a href="https://www.migrationpolicy.org/article/free-movement-europe-past-and-present">https://www.migrationpolicy.org/article/free-movement-europe-past-and-present</a></p>
    </li>
    
    <li>
    <p>Lazarowicz, A. (2015), ‘Governance of the Free Movement of EU Citizens: Weathering the Storm of Politicization’, PISM Policy Papers 105, 1–9</p>
    </li>
    
    <li>
    <p>Traser, J. S. (2007), The Polish Experience of Free Movement of Workers After EU Accession, Biuletyn Informacji Publicznej RPO.</p>
    <p>URL: <a href="https://bip.brpo.gov.pl/pliki/1190979632">https://bip.brpo.gov.pl/pliki/1190979632</a></p>
    </li>
    
    <li>
    <p>Wikipedia: Swobodny przeplyw osob (2023).</p>
    <p>URL: <a href="https://pl.wikipedia.org/wiki/Swobodny_przep%C5%82yw_os%C3%B3b">https://pl.wikipedia.org/wiki/Swobodny_przep%C5%82yw_os%C3%B3b</a></p>
    </li>
    
    <li>
    <p>Wikipedia: Freedom_of_movement_for_workers_in_the_European_Union (2023).</p>
    <p>URL: <a href="https://en.wikipedia.org/wiki/Freedom_of_movement_for_workers_in_the_European_Union">https://en.wikipedia.org/wiki/Freedom_of_movement_for_workers_in_the_European_Union</a></p>
    </li>
    </ul>
   ' 
)
