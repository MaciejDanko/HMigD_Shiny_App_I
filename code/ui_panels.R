jscode_upload_msg <- function(id) paste0(" Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#",id,"_progress').children()[0];
  target.innerHTML = msg;
}); ")

jscode_upload_txt <- function(id) paste0(" Shiny.addCustomMessageHandler('upload_txt', function(txt) {
  var target = $('#",id,"').parent().parent().parent().find('input[type=text]');
  target.val(txt);

}); ")

jsCode_upload_hide <- "
  Shiny.addCustomMessageHandler('hideProgressBar', function() {
    document.getElementById('ModelTableLoad_progress.progress.shiny-file-input-progress').style.visibility = 'hidden';
  });
"

ui_flow_sources_imm <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      br(),
      div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "src_i_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      width='1000px', round=TRUE,
          ),
          br(),
      ),
      
      br(),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "SrcImmPlot",height="auto", width='auto'),
  ),
  SaveBlockSRC('SRC_IMM','I',5,'#F5DFD5')
)


ui_flow_sources_emi<-function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      br(),
      div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "src_e_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      #format = wNumbFormat(decimals = 0), 
                      width='1000px', round=TRUE,
                      #pips=list(mode= 'values',values=seq(min(Years),max(Years),1)
                      #          ),
                      #color="#00B2EEFF"
          ),
          br(),
      ),
      
      br(),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "SrcEmiPlot",height="auto", width='auto'),
  ),
  SaveBlockSRC('SRC_EMI','E',5,'#F5DFD5'),     
)

ui_flow_accuracy_imm <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("acu_i_adjusted", h4('Correct the accuracy classification of specific migration flows, taking into account the undercounting of that flows'), value = TRUE, width = '1100px'),
      ),
  ),    
  
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "AccuImmPlot",height="auto", width='auto'),
  ),
  SaveBlock1('ACCURACY_IMM',5,'#F5DFD5'),     
)

ui_flow_accuracy_emi <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("acu_e_adjusted", h4('Correct the accuracy classification of specific migration flows, taking into account the undercounting of that flows'), value = TRUE, width = '1100px'),
      ),
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "AccuEmiPlot",height="auto", width='auto'),
  ),
  SaveBlock1('ACCURACY_EMI',5,'#F5DFD5'),     
)

ui_flow_undercounting_imm <-function() tagList(
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "UndercountPlotImm",height="auto", width='99%'),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1100px; margin-left:20px; margin-bottom:10px; margin-top:10px; font-size:16px",
          h4('Undercounting was solely determined using pure classification in the UndercountMigScores model. While the app allows for the combination of scores derived from metadata, model, and expert opinions to calculate a new classification, in this specific context, we solely rely on the scores generated by the model.'),
          "The app is accessible through both the ",tags$a(href = "https://github.com/MaciejDanko/UndercountMigScores", "Github repository"),"and the",
          tags$a(href = "https://maciej-jan-danko.shinyapps.io/undercountmigscores/", "Shinyapps.io repository"),
          p(""),
          "The methodology is described in"
      ),         
  ),
  SaveBlock1('UNDERCOUNT_IMM',5,'#F5DFD5'),
)


ui_flow_undercounting_emi <- function() tagList(
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "UndercountPlotEmi",height="auto", width='99%'),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1100px; margin-left:20px; margin-bottom:10px; margin-top:10px; font-size:16px",
          h4('Undercounting was solely determined using pure classification in the UndercountMigScores model. While the app allows for the combination of scores derived from metadata, model, and expert opinions to calculate a new classification, in this specific context, we solely rely on the scores generated by the model.'),
          "The app is accessible through both the ",tags$a(href = "https://github.com/MaciejDanko/UndercountMigScores", "Github repository"),"and the",
          tags$a(href = "https://maciej-jan-danko.shinyapps.io/undercountmigscores/", "Shinyapps.io repository"),
      ),         
  ),
  SaveBlock1('UNDERCOUNT_EMI',5,'#F5DFD5'),
)

ui_flow_coverage <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
      br(),
      # div(style="display:inline-block;vertical-align:top; width:980px; margin-left:20px",
      #     
      #     h3('Estimated migratioon flows for selected models'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:150px; margin-left:20px; margin-top:0px",
          
          downloadButton("CoverageTableDownload", "Download as xlsx"),
          tags$head(tags$style(HTML('#CoverageTableDownload {background-color:#ede594} #CoverageTableDownload:hover{background-color:#FFC0C0}'))),
          br(), 
      ),
      div(style="display:inline-block;vertical-align:top;  margin-left:50px; margin-top:0px; width:980px",
          h4(HTML('The classification of coverage assumes that only Nordic countries, including Denmark (DK), Finland (FI), Iceland (IS), Norway (NO), and Sweden (SE), have the highest quality of registers and, consequently, a high coverage of migration flows.')),
      ),
      div(style="display:inline-block;vertical-align:top;  width:1170px; margin-left:20px;background-color:#FFFFFF",
          br(),br(),
          DTOutput("CoverageTable"),
      ), 
      br(),
      br(),br()
  ),
)

ui_transition_coverage <- function() tagList(
  # div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
  #     h4(HTML('The classification of the coverage is based on the type of households included. <strong>"Low"</strong> coverage includes only private households, while <strong>"high"</strong> coverage includes both private households and collective accommodations.')),
  # ),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
      br(),
      # div(style="display:inline-block;vertical-align:top; width:980px; margin-left:20px",
      #     
      #     h3('Estimated migratioon flows for selected models'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:150px; margin-left:20px; margin-top:0px",
          
          downloadButton("CoverageTransitionsTableDownload", "Download as xlsx"),
          tags$head(tags$style(HTML('#CoverageTransitionsTableDownload {background-color:#ede594} #CoverageTransitionsTableDownload:hover{background-color:#FFC0C0}'))),
          br(), 
      ),
      div(style="display:inline-block;vertical-align:top;  margin-left:50px; margin-top:0px; width:980px",
          h3(HTML('The classification of the coverage is based on the type of households included. <strong>"Low"</strong> coverage includes only private households, while <strong>"high"</strong> coverage includes both private households and collective accommodations.')),
      ),    
      
      div(style="display:inline-block;vertical-align:top;  width:1170px; margin-left:20px;background-color:#FFFFFF",
          br(),br(),
          DTOutput("CoverageTransitionsTable"),
      ), 
      br(),
      
      br(),br()
  ), 
)

ui_transition_undercounting <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
      br(),
      # div(style="display:inline-block;vertical-align:top; width:980px; margin-left:20px",
      #     
      #     h3('Estimated migratioon flows for selected models'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:150px; margin-left:20px; margin-top:0px",
          
          downloadButton("UndercountingTransitionsTableDownload", "Download as xlsx"),
          tags$head(tags$style(HTML('#UndercountingTransitionsTableDownload {background-color:#ede594} #UndercountingTransitionsTableDownload:hover{background-color:#FFC0C0}'))),
          br(), 
      ),
      div(style="display:inline-block;vertical-align:top;  margin-left:50px; margin-top:0px; width:980px",
          h3(HTML(paste0('<i><b>Combined measure</b></i> of undercounting in LFS data is calculated as <i>1 &#9472 (1 &#9472 <b>Fraction of non-responding</b>) x (1 &#9472 <b>Fraction of missing</b>)</i>.',
                         ' Here, <i><b>Fraction of non-responding</i></b> indicates the non-response fraction of the studied population, and <i><b>Fraction of missing</i></b> ',
                         'indicates the fraction of the population group with missing migration data in the total estimated population. The values of ',
                         '<i><b>Fraction of non-responding</i></b> and <i><b>Fraction of missing</i></b> are displayed in the plots below.',
                         'The classification uses the first quartile of <i><b>Combined measure</b></i> as a threshold.'))),
      ),#tertile
      div(style="display:inline-block;vertical-align:top;  width:1170px; margin-left:20px;background-color:#FFFFFF",
          br(),br(),
          DTOutput("UndercountingTransitionsTable"),
      ), 
      br(),
      
      br(),br()
  ),
  
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1180px; margin-left:20px; margin-top:10px",
          h3(HTML("Year-specific (non-averaged) <i><b>Combined measure</b></i> of undercounting in LFS data. The presence of red rectangles indicates missing data or situations where no migration data has been reported.")), 
      ),
      plotOutput(outputId = "TRANSITIONSUNDERCOUNTPlot",height="auto", width='99%'),
  ),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1180px; margin-left:20px; margin-top:10px",

          helper(h3(HTML("Year-specific fraction of the population group with missing migration data in the total estimated population (non-averaged <i><b>Fraction of missing</i></b>). Red rectangles indicate missing data or cases where no migration data has been reported.")),
                 colour='red', content = 'lfs_undercounting',type='markdown',title='', style='font-size:20px;',size='m',
                 buttonLabel = 'Close'),
      ),
      plotOutput(outputId = "TRANSITIONSMISSPlot",height="auto", width='99%'),
  ),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1100px; margin-left:20px; margin-top:10px",

          h3(HTML("Year-specific fraction of the non-responding population (non-averaged <i><b>Fraction of non-responding</i></b>). Red rectangles indicate missing data or cases where no migration data has been reported.")),
      ),    
      plotOutput(outputId = "TRANSITIONSNRSPlot",height="auto", width='99%'),
  ),
)

ui_transition_accuracy <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
      br(),
      # div(style="display:inline-block;vertical-align:top; width:980px; margin-left:20px",
      #     
      #     h3('Estimated migratioon flows for selected models'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:150px; margin-left:20px; margin-top:0px",
          
          downloadButton("AccuracyTransitionsTableDownload", "Download as xlsx"),
          tags$head(tags$style(HTML('#AccuracyTransitionsTableDownload {background-color:#ede594} #AccuracyTransitionsTableDownload:hover{background-color:#FFC0C0}'))),
          br(), 
      ),
      div(style="display:inline-block;vertical-align:top;  margin-left:50px; margin-top:0px; width:980px",
          h3(HTML('The <b><i>Non-adjusted accuracy </i></b> of LFS data is estimated as the year-averaged coefficient of variation (CV) calculated among the number of immigrants at the destination in a particular year. Countries are categorized into high and low classes using the median value as the threshold. The <b><i>Undercounting-adjusted accuracy </i></b> takes into account undercounting, ensuring that accuracy is set to low when undercounting is high.')),
      ),
      div(style="display:inline-block;vertical-align:top;  width:1170px; margin-left:20px;background-color:#FFFFFF",
          br(),br(),
          DTOutput("AccuracyTransitionsTable"),
      ), 
      br(),
      
      br(),br()
  ),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1180px; margin-left:20px; margin-top:10px",
          
          #helper(
          h3(HTML("The <b><i>Non-adjusted accuracy </i></b> of LFS data is estimated as the coefficient of variation (CV) calculated among the number of immigrants at the destination in a particular year. The red rectangles indicate missing data, cases with no migration data reported, or situations where the CV could not be calculated.")), 
          #     colour='red', content = 'lfs_accuracy',type='markdown',title='', style='font-size:20px;',size='m',
          #     buttonLabel = 'Close'),
      ),
      plotOutput(outputId = "TRANSITIONSACCURACYlot",height="auto", width='99%'),
  ),
)

ui_flow_duration_imm <- function() tagList(
  
  #h3('immi, classes 0-2 treated identically in the model,  explain 3,6 - exchange 3,6 -> "3 & 6" '),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "DurImmPlot",height="auto", width='auto'),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1100px; margin-left:20px; margin-top:10px",
          h4('Duration of stay and availability of bilateral flows in the data used. In the Danish(DK) data, “6, 3” means three months for immigration from Switzerland (CH) and six months for immigration from other countries. In the case of Germany (DE), “1.25” is a mean duration of stay among different federal states.')),
  ),
  SaveBlock1('DURATION_IMM',5,'#F5DFD5'),
)

ui_flow_duration_emi <- function() tagList(
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "DurEmiPlot",height="auto", width='auto'),
  ),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      div(style="display:inline-block;vertical-align:top; horizontal-align:center; width:1100px; margin-left:20px; margin-top:10px",
          h4('Duration of stay and availability of bilateral flows in the data used. In the Danish (DK) data “6, 12” means 12 months for emigration to Sweden (SE) or Finland (FI) and six months for emigration to other countries.')
      ),
  ),
  SaveBlock1('DURATION_EMI',5,'#F5DFD5'),
)


ui_covariates_freedom <- function() tagList(
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "FREEDOMPlot",height="auto", width='auto'),
  ),
  
  #SaveBlock1('FREEDOM',5,'#F5DFD5'),   #'#F5DFD5' #FFEABA
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:650px;font-size:16px",
          br(),
          h3('Data sources'),
          
          tags$a(href = "https://en.wikipedia.org/wiki/Freedom_of_movement_for_workers_in_the_European_Union", "Freedom_of_movement_for_workers_in_the_European_Union"),
          " - Wikipedia.",
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('FREEDOM',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),
  #br(),
)

ui_covariates_population<-function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("log_pop", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "POPULATIONPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('POPULATION',5,'#F5DFD5'),     
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:650px;font-size:16px",
          br(),
          h3('Data sources'),
          
          tags$a(href = "https://ec.europa.eu/eurostat/databrowser/view/demo_pjan/default/table", "EUROSTAT - demo_pjan"),
          " - EUROSTAT: Population on 1 January by age and sex.",
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('POPULATION',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),
  #br(),
)

ui_covariates_stocks <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("log_stocks", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "STOCKSPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('STOCKS',5,'#F5DFD5'),     
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:650px;font-size:16px",
          br(),
          h3('Data sources'),
          
          tags$a(href = "https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockByOriginAndDestination_2019.xlsx", "UN_MigrantStockByOriginAndDestination_2019.xlsx"),
          " - United Nations Population Division - Migrant Stock By Origin And Destination 2019 Revision",
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('STOCKS',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),
  #br(),
)

ui_covariates_trade <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:600px",
          br(),
          h3('Display options'),
          #help with table
          br(),
          myPrettyCheckbox("log_trade", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:500px",
          br(),
          # div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
          # ),
          # 
          # div(style="display:inline-block;vertical-align:top; width:500px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "trade_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      
                      width='500px', round=TRUE,
                      
          ),
          br(),
          #),
      ),    
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "TRADEPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('TRADE',5,'#F5DFD5'),     
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:650px;  margin-left:20px;font-size:16px",
          br(),
          h3('Bilateral trade data sources'),
          tags$ul(
            tags$li(
              tags$a(href = "https://ec.europa.eu/eurostat/databrowser/view/DS-018995/", "EUROSTAT DS-018995"),
              " - EU trade since 1999 by SITC"
            ),
            tags$li(
              tags$a(href = "https://ec.europa.eu/eurostat/databrowser/view/DS-043227/ ", "EUROSTAT DS-043227"),
              " - EFTA trade since 1995 by SITC"
            ),
            tags$li(
              tags$a(href = "https://ec.europa.eu/eurostat/databrowser/product/page/ext_tec03", "EUROSTAT ext_tec03"),
              " - Trade by partner country and NACE Rev. 2 activity"
            ),
          ),
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('TRADE',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),br(),
)


ui_covariates_gni_ratio <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:600px",
          br(),
          h3('Display options'),
          #help with table
          br(),
          myPrettyCheckbox("log_gni", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:500px",
          br(),
          # div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
          # ),
          # 
          # div(style="display:inline-block;vertical-align:top; width:500px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "gni_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      
                      width='500px', round=TRUE,
                      
          ),
          br(),
          #),
      ),    
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "GNIPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('GNI',5,'#F5DFD5'),     
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:700px;  margin-left:20px;font-size:16px",
          br(),
          h3('GNI data sources'),
          tags$ul(
            tags$li(
              tags$a(href = "http://api.worldbank.org/v2/en/indicator/NY.GNP.PCAP.CD?downloadformat=csv", "World Bank NY.GNP.PCAP.CD"),
              " - World Bank, GNI per capita,","Atlas method (current US$)",
            ),
            tags$li(
              tags$a(href = "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.GNI.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en", "OECD GNI"),
              " - OECD: stats.oecd.org"
            ),
          ),
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:50px",
          SaveBlock1B('GNI',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
      #br(),
      #br(),
  ),
  #br(),br(),
)

ui_covariates_distance <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("log_dist", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "DISTANCEPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('DISTANCE',5,'#F5DFD5'),    
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px; width:650px;font-size:16px;",
          br(),
          h3('Data sources'),
          
          tags$a(href = "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6", "CEPII - GeoDist"),
          " - CEPII: Research and Expertise on the World Economy",
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('DISTANCE',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),
  #br(),
)  

ui_covariates_language <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px",
          br(),
          h3('Display options'),
          #help with table
          myPrettyCheckbox("log_lang", h4('Show log-centering transformation as it is used in the model'), value = TRUE, width = '1100px'),
      ),
  ),    
  #myPrettyCheckbox("acu_e_adjusted", h4('Correct for undercounting'), value = TRUE, width = '300px'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      
      plotOutput(outputId = "LANGUAGEPlot",height="auto", width='auto'),
  ),
  #SaveBlock1('LANGUAGE',5,'#F5DFD5'),   
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#F5DFD5; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:650px;font-size:16px;",
          br(),
          h3('Data sources'),
          
          tags$a(href = "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=19", "CEPII - Language"),
          " - CEPII: Research and Expertise on the World Economy",
          
      ),
      div(style="display:inline-block;vertical-align:top; width:420px;  margin-left:100px",
          SaveBlock1B('LANGUAGE',5,'#F5DFD5',width_btn=c(200,200,200), width_div=c(200,200,200),mar_left=c(215,0,10),mar_top=c(0,-10,-10)),
      ),
  ),
  #br(),
  #br(),
)  

ui_model_schemes <- function() tagList(
  br(),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#eee0b9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',   #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:800px; margin-left:20px",
          br(),
          h3('Model selection'),
          selectInput("MODEL4", label = NULL,
                      choices = makeList(MODELS),
                      selected = 1, width='800px'),
          
      ),
      div(style="display:inline-block;vertical-align:top; width:100px; margin-left:20px",
          br(), h3(HTML('&#160;')),
          downloadButton('DownloadCode','Download JAGS code')
      ),
  ),
  #h3('Under construction - Model selection and ploting algorithm schemes for selected model'),
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px;  margin-top:5px;  background-color: #FFFFFF',
      #br(),
      h2(HTML('<b><center>The overall scheme of the Bayesian model</center></b>')), 
      br(),
      uiOutput("png_view1"),
      br(),br(),
  ),
  
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px;  margin-top:5px;  background-color: #FFFFFF',
      #br(),
      h2(HTML('<b><center>Diagram of the method of combining administrative and LFS data</b></center>')),
      br(),
      uiOutput("png_view2"),
      br(),br(),
  ),
)

ui_visualize_output <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      # div(style="display:inline-block;vertical-align:top; margin-left:20px;width:400px",
      #     br(),
      #     h3('Model options'),
      #     #help with table
      #     br(),
      #     myPrettyCheckbox("visualize_alt", h4('Use alternative model predicting method'), value = FALSE, width = '1100px'),
      # ),
      div(style="display:inline-block;vertical-align:top; margin-left:20px;width:1200px",
          
          br(),
          div(style="display:inline-block;vertical-align:top; width:100px;  margin-left:20px;",
              h3('Year'),
          ),
          
          div(style="display:inline-block;vertical-align:top; width:1080px;  margin-left:20px; margin-top:5px",
              
              sliderInput(inputId = "visualize_output_year", label = NULL, 
                          min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                          width='1000px', round=TRUE,
              ),
              br(),
          ),
      ),
      br(),
  ),    
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "OutputFlowsPlot",height="auto", width='auto'),
  ),
)

# ui_visualize_output <- function() tagList(
#   div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
#       
#       br(),
#       div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
#           h3('Year'),
#       ),
#       
#       div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
#           
#           sliderInput(inputId = "visualize_output_year", label = NULL, 
#                       min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
#                       width='1000px', round=TRUE,
#           ),
#           br(),
#       ),
#       br(),
#   ),    
#   div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
#       plotOutput(outputId = "OutputFlowsPlot",height="auto", width='auto'),
#   ),
# )


ui_transitions_count <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      
      br(),
      div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "transitions_count_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      width='1000px', round=TRUE,
          ),
          br(),
      ),
      br(),
  ),    
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "TRANSITIONSCOUNTPlot",height="auto", width='auto'),
  ),
  SaveBlockTRANSITIONSCOUNT('TRANSITIONSCOUNT',5,'#F5DFD5'),     
)

ui_flow_imm <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      
      br(),
      div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "flows_imm_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      width='1000px', round=TRUE,
          ),
          br(),
      ),
      br(),
  ),    
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "FLOWSIMMCOUNTPlot",height="auto", width='auto'),
  ),
  SaveBlockFLOWSIMMCOUNT('FLOWSIMMCOUNT',5,'#F5DFD5'),     
)

ui_flow_emi <- function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      
      br(),
      div(style="display:inline-block;vertical-align:top; width:50px;  margin-left:20px; margin-top:20px",
          h3('Year'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:1000px;  margin-left:40px; margin-top:5px",
          
          sliderInput(inputId = "flows_emi_year", label = NULL, 
                      min = min(Years), max = max(Years), value = Years[10], step=1, sep='',
                      width='1000px', round=TRUE,
          ),
          br(),
      ),
      br(),
  ),    
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      plotOutput(outputId = "FLOWSEMICOUNTPlot",height="auto", width='auto'),
  ),
  SaveBlockFLOWSEMICOUNT('FLOWSEMICOUNT',5,'#F5DFD5'),     
)


ui_compare_models_single<-function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#eee0b9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f; height:132px',   #style='margin:0px; padding:0px',
      uiOutput('cm_m12')#
  ),
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#e8e9d9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  #style='margin:0px; padding:0px',
      div(style="id:MM1;display:inline-block;vertical-align:top; width:210px; margin-left:20px",
          br(),    
          h3('Sending country'),
          selectInput("SendingCountry", label = NULL,
                      choices = makeList(CountriesFull),
                      selected = which(Countries=='GR'),width='200px'),
      ),
      div(style="id:MM2;display:inline-block;vertical-align:top; width:180px; margin-left:10px",
          br(),    
          h3(HTML('&#160;')),
          tags$head(
            tags$style(HTML('#Reverse {background-color:#D4DDFF} #Reverse:hover{background-color:#FFC0C0}'))
          ),
          actionButton("Reverse", HTML("<center>&#8592 Swap countries &#8594</center>"), width='160px'),
      ),
      
      div(style="id:MM3;display:inline-block;vertical-align:top; width:210px;  margin-left:0px",
          br(),     
          h3('Receiving country'),
          selectInput("ReceivingCountry", label = NULL, # move to server, uodate sending and input
                      choices = makeList(CountriesFull),
                      selected = which(Countries=='CY'),width='200px'),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:120px",
          br(),
          h3(HTML('Graphical options')),
          myPrettyCheckbox("FixedYMaxCompareModels", h4('Fix maximum value on the Y-axis'), value = FALSE, width = '240px'),
      ),
 
      div(style="display:inline-block; width:155px;  margin-left:20px; margin-top:25px",
        numericInput('YMaxCompareModels', min=0,value=get_ymax_raw('GR','CY',2,2),label='Max Y-axis value')
      ),
      br()
  ),
  
  
  
  div(class="row", style='id:MM4; margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; background-color: #FFFFFF',
      #div(class="col-lg-8",
      plotOutput(outputId = "Model1Plot",height="auto", width='auto'),
      
      #)
  ),
  SaveBlock2b(IDName='Model1', topmargin = 5, 
              bg = '#F5DFD5',
              choice1ID="STYLE1", 
              #choice2ID="PRED1", 
              choices1=c('Linear interpolation','Stair steps plot','Spline interpolation')#, 
              #choices2=c('Basic','Alternative')
  ),
  mySurveyFrame(),
  #br(),br(),br(),br(),br(),
  
)

ui_compare_models_aggregated<-function() tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#eee0b9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f; height:132px',   #style='margin:0px; padding:0px',
      br(),
      div(style = "display: inline-flex; align-items: center; width: 1200px; margin-left: 20px;",
          div(style = "width: 250px;",
              h3("First model #1")
          ),
          tags$head(tags$style(HTML("#MODEL1b.form-control {padding-left: 6px;}"))),
          div(style = "width: 950px;",
              selectInput("MODEL1b", 
                          label = NULL,           
                          choices = makeList(MODELS),
                          selectize = FALSE,
                          selected = 1,
                          width = '920px')
          )
      ),
      div(style = "display: inline-flex; align-items: center; width: 1200px; margin-left: 20px;",
          div(style = "width: 250px;",
              h3("Second model #2")
          ),
          tags$head(tags$style(HTML("#MODEL2b.form-control {padding-left: 6px;}"))),
          div(style = "width: 950px;",
              selectInput(
                "MODEL2b", label = NULL,
                choices = makeList(MODELS),
                selectize = FALSE,
                selected = 2,
                width = '920px'
              )
          )
          
      ),
      
      #div(class="col-lg-4", style='padding-right:0px; max-width:480px; min-width:10px; background-color:#fff0d0',
      # div(style="display:inline-block;vertical-align:top; width:584px; margin-left:20px",
      #     #div(class="col", style='padding-right:0px; width:480px; min-width:10px; background-color:#fff0d0',
      #     br(),
      #     h3('First model'),
      #     selectInput("MODEL1b", label = NULL,
      #                 choices = makeList(MODELS),
      #                 selected = 1, width='565px'),
      #     #br(),
      # ),
      # #div(class="col-lg-4", style='padding-right:0px; max-width:480px; min-width:10px; background-color:#fff0d0',
      # #div(class="col", style='padding-right:0px; width:480px; min-width:10px; background-color:#fff0d0',
      # div(style="display:inline-block;vertical-align:top; width:584px; margin-left:20px",
      #     br(),
      #     h3('Second model'),
      #     selectInput("MODEL2b", label = NULL,
      #                 choices = makeList(MODELS),
      #                 selected = 2, width='565px'),
      #     #br(),
      # ),
      #),
  ),     
  div(class="row", style='margin-left:0px; margin-top:5px; margin-bottom:0px; background-color:#e8e9d9;width=1260px;border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  
      #div(style="display:inline-block;vertical-align:top; width:1200px; margin-left:20px",
      br(),
      div(style = "display: inline-flex; align-items: center; width: 1200px; margin-left: 20px; margin-bottom:-10px;",
          div(style = "width: 250px;",
                    h3('Exemplary cases'),
          ),
          tags$head(tags$style(HTML("#Examples1.form-control {padding-left: 6px;}"))),
          div(style = "width: 950px;",
              selectInput("Examples1", label = NULL, selectize=FALSE,
                      choices = makeList(c('(0) None', FREEDOMCASES)),
                      selected = 4,width='920px'),
        ),
      ),
      hr(style="border-top: 0.5px solid"),
      div(style="display:inline-block;vertical-align:top; width:300px; margin-left:20px",
          #br(),
          h3("Sending countries"),
          myPrettyGroupCheckbox("SendCntrs", label =NULL,
                                choices = CountriesFull, selected = CountriesFull[Countries%in%c('CZ','PL','SK')], inline = TRUE, ncol = 2),
          
          
      ),
      
      div(style="display:inline-block;vertical-align:top; width:300px; margin-left:20px; margin-bottom:20px",
          #br(),
          h3("Receiving countries"),
          myPrettyGroupCheckbox("RecCntrs", label=NULL,
                                choices = CountriesFull, selected = CountriesFull[Countries%in%c('DE')], inline = TRUE, active.bg = "#0861ae", ncol=2),
          
      ),
      div(style="display:inline-block;vertical-align:top; width:525px; margin-left:20px",
          #br(),
          div(style="display:inline-block;vertical-align:top; width:525px; margin-left:0px",
              h3('Options'),
              
              div(myPrettyCheckbox("narm", h4('Ignore missing values when aggregating data'), value = FALSE, width = '525px'),
                  style="margin-bottom:-10px"),
              div(helper(myPrettyCheckbox("sdat", h4('Show aggregated data'), value = FALSE, width = '525px'),
                         colour='#FF0000',type='inline',title='Show aggregated data',buttonLabel = 'Close',
                         content=aDATAtxt), style="margin-bottom:-10px"),
              div(helper(myPrettyCheckbox("aCI", h4('Show naive credibility intervals'), value = FALSE, width = '525px'),
                         colour='#FF0000',type='inline',title='Credibility intervals for aggregated flows',buttonLabel = 'Close',
                         content=aCItxt),
                  style="margin-bottom:-10px"),
              # div(myPrettyCheckbox("UseAltPred2", h4('Use alternative model predicting method'), value = FALSE, width = '525px'),
              #     style="margin-bottom:-10px"),
              
              div(class = 'row', style = 'display: inline-block; align-items: left; margin-bottom:5px; margin-left:0px; width:525px',
                  div(class = 'col', style = 'display: inline-block; text-align: left; width:370px',
                      myPrettyCheckbox("UseThreshold", h4('Mark threshold year on the plot:'), value = FALSE)
                      
                  ),
                  tags$head(
                    tags$style(HTML('#bprev{background-color:#D4DDFF;height:20px} #bprev:hover{background-color:#FFC0C0}'))
                  ),
                  
                  div(class = 'col', style = 'display: inline-block; text-align: center; width:30px',
                      actionButton('bprev',"<<", style="padding-top:0px; padding-left:10px; padding-right:10px; padding-bottom:0px; margin-top:-5px")
                  ),
                  
                  div(class = 'col', style = 'display: inline-block; text-align: center; width:50px; margin-left:10px;',
                      uiOutput("ThrY")
                  ),
                  tags$head(
                    tags$style(HTML('#bnext{background-color:#D4DDFF;height:20px} #bnext:hover{background-color:#FFC0C0}'))
                  ),
                  
                  div(class = 'col', style = 'display: inline-block; text-align: center; width:30px',
                      actionButton('bnext',">>", style="padding-top:0px; padding-left:10px; padding-right:10px; padding-bottom:0px; margin-top:-5px")
                  )
              ),
              # br(), 
              # h3('Exemplary sets'),
              # selectInput("Examples1", label = NULL,
              #             choices = makeList(c('(0) None', FREEDOMCASES)),
              #             selected = 4,width='525px'),
              # 
              
              
          ),
      ),
  ),
  
  # SaveBlock2(IDName='Model2', topmargin = 5, 
  #            bg = '#F5DFD5',
  #            choice1ID="STYLE2", 
  #            choice2ID="PRED2", 
  #            choices1=c('Linear interpolation','Stair steps plot','Spline interpolation'), 
  #            choices2=c('Basic','Alternative')),
  
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px; margin-top:5px; width=1260px; background-color: #FFFFFF',
      #div(class="col-lg-8",
      plotOutput(outputId = "Model2Plot",height="auto", width='99%'),
      
      #)
  ),
  SaveBlock2b(IDName='Model2', topmargin = 5, 
              bg = '#F5DFD5',
              choice1ID="STYLE2", 
              #choice2ID="PRED2", 
              choices1=c('Linear interpolation','Stair steps plot','Spline interpolation')#, 
              #choices2=c('Basic','Alternative')
  ),
  #br(),br(),br(),br(),br(),br(),
)

ui_compare_models_circular <- function() tagList(
  
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#eee0b9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',   #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:825px; margin-left:20px",
          br(),
          h3('Model selection'),
          selectInput("MODEL3", label = NULL,
                      choices = makeList(MODELS),
                      selected = 1, width='825px'),
      ),
      
  ),     
  div(class="row", style='margin-left:0px; margin-top:5px; margin-bottom:0px; background-color:#e8e9d9;border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',  
      div(style="display:inline-block;vertical-align:top; width:475px; margin-left:20px",
          br(),
          h3("Sending countries"),
          
          myPrettyGroupCheckbox("SendCntrs3", label =NULL,
                                choices = CountriesFull, selected = CountriesFull[Countries%in%c('DE','PL','UK','IT','FR')], inline = TRUE),
          tags$head(
            tags$style(HTML('#EqualizeSending{background-color:#D4DDFF} #EqualizeSending:hover{background-color:#FFC0C0}'))
          ),
          br(),
          actionButton("EqualizeSending", "Make sending countries the same as receiving countries", width='475px'),    
          
      ),
      
      div(style="display:inline-block;vertical-align:top; width:475px; margin-left:5px; margin-bottom:20px",
          br(),
          h3("Receiving countries"),
          myPrettyGroupCheckbox("RecCntrs3", label=NULL,
                                choices = CountriesFull, selected = CountriesFull[Countries%in%c('DE','PL','UK','IT','FR')], inline = TRUE,
                                active.bg = "#0861ae"),
          tags$head(
            tags$style(HTML('#EqualizeReceiving{background-color:#D4DDFF} #EqualizeReceiving:hover{background-color:#FFC0C0}'))
          ),
          br(),
          actionButton("EqualizeReceiving", "Make receiving countries the same as sending countries", width='475px'),    
          
          
          # actionButton("SCall", "All"),actionButton("SCnone", "None"),
      ),
      div(style="display:inline-block;vertical-align:top; width:220px; margin-left:5px",
          
          br(),
          h3('Options'),
          div(myPrettyCheckbox("ShowScale", h4('Show scale on the plot'), value = FALSE, width = '240px'),style="margin-bottom:-15px"),
          #myPrettyCheckbox("UseAltPred3", h4('Use alternative model predicting method'), value = FALSE, width = '240px'),
          
      ),
      br(),
  ),
  
  
  div(class="row", style='margin-left:0px;border-style: solid; border-color:#9985A2; border-width:1px;  margin-top:5px;  background-color: #FFFFFF',
      #div(class="col-lg-8",
      div(style="display:inline-block;vertical-align:top; width:800px; margin-left:0px",
          plotOutput(outputId = "Model3Plot",height="auto", width='99%'),
          
      ),
      div(style="display:inline-block;vertical-align:bottom; width:200px; margin-left:0px;",
          br(),
          h3(HTML('<center>Year</center>')),
          noUiSliderInput(inputId = "YearSel", label = NULL, orientation = 'vertical',
                          min = 2002, max = 2018, value = 2012, step=1,height = '700px',
                          format = wNumbFormat(decimals = 0),
                          pips=list(mode= 'values',values=2002:2018),color="#00B2EEFF"
          ),
          #tags$head(tags$style("#YearSel label {font-size:20px}", media="screen", type="text/css")),
      ),
      div(style="display:inline-block;vertical-align:bottom; width:200px; margin-left:0px",
          br(),
          h3(HTML('<center>Limit visible flows (percentile)</center>')),
          noUiSliderInput(inputId = "Percentiles", label = NULL, orientation = 'vertical',
                          min = 0, max = 100, value = 75, step=0.1,height = '700px',
                          format = wNumbFormat(decimals = 1),
                          pips=list(mode= 'values',values=seq(0,100,10)),color="#00B2EEFF")
      ),
  ),
  SaveBlock0(IDName='Model3', topmargin = 5, 
             bg = '#F5DFD5'#,
             #choice1ID="STYLE3", 
             #choice2ID="PRED3", 
             #choices1=c('Linear interpolation','Stair steps plot','Spline interpolation'), 
             #choices2=c('Basic','Alternative')
  ),
  
  #br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
)


ui_output<-function() tagList(
  #h2('Under construction - add credibility intervals'),
  #conditionalPanel(condition = "input.OUTPUT == 1", style='width:1200px',  
  
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#eee0b9; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',   #style='margin:0px; padding:0px',
      br(),
      div(style='margin-left:20px',
          div(style='margin-left:35px',
          h3('Final model selection and mixing'),
          h5('Select models for specific flows by double-clicking on the corresponding cell. Refer to the "Model schemes" panel to see models description.'),
          ),
          #br(),
          # div(style = "display: inline-flex;",#writing-mode: vertical-rl; text-orientation: upright;",
          #     div(style='width:50px; transform: rotate(90deg);',
          #         h3("Sending countries")
          #     ),  
          #     div(style='margin-left:-5px; padding-bottom:5px;',
          #         rHandsontableOutput("ModelTable")   
          #     ),
          # ),
          div(
            style = "display: flex; align-items: center",
            h3(style = "margin: 0 auto;", "Receiving countries")
          ),  
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "width: 20px; transform: rotate(-90deg); position: relative; z-index: 1; white-space: nowrap;",
              h3(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                 
                 "Sending countries")
            ),
            div(
              style = "margin-left: 10px; overflow: hidden; position: relative;",
              rHandsontableOutput("ModelTable")
            )
          ),
          div(style='display:flex;  height: 34px; margin: 0; padding: 0; margin-top:-53px; margin-left:35px; align-self: center; align-items: center;',#margin-top:-15px;
              
              div(style="display:inline-block; margin-top:1px;  margin-left:25px; height:34.5px;",
                  tags$script(HTML(jscode_upload_msg('ModelTableLoad'))),
                  tags$script(HTML(jscode_upload_txt('ModelTableLoad'))),
                  #tags$script(HTML(jsCode_upload_hide)),
                  #tags$head(tags$style('#ModelTableLoad .form-control {display:none; visibility:hidden !important')),
                  #tags$head(tags$style('#ModelTableLoad .form-group.input-group.form-control {display:none; visibility:hidden !important')),
                  #tags$head(tags$style('#ModelTableLoad .input-group {display:none; visibility:hidden !important')),
                  #tags$head(tags$style(HTML("#tab-2594-6 > div:nth-child(3) > div > div > div:nth-child(4) > div:nth-child(1) > div > div.input-group > input { display: none;}"))),
                  
                  fileInput('ModelTableLoad',label=NULL,buttonLabel =HTML(paste0(shiny::icon("upload"),
                                                                                 " Import models table")),
                            accept = '.xlsx',placeholder = 'No file selected'),
              ),
              #tags$head(tags$style(HTML("#ModelTableLoad .custom-file-input::before {width: 200px;}"))),
              downloadButton('ModelTableDownload','Export model table',style='height:34.5px;  margin-left:5px'),
              tags$head(
                tags$style("#ModelTableSolidInput .form-group.shiny-input-container {  margin: 0; }"),
                tags$style("#ModelTableSolidInput .form-group.select-input-container { flex-grow: 1;}"),
                tags$style("#ModelTableSolidInput .select-input-container { flex-grow: 1; display: flex; align-items: center; }"),
                tags$style('#ModelTableSolidInput .selectize-input { height: 34px !important; }'),
              ),
              actionButton('ModelTableRestore','Restore models table',style='height:34.5px;  margin-left:5px', icon=shiny::icon('refresh')),
              actionButton('ModelTableSolidSubmit','Fill table with single model',icon=shiny::icon('adjust'),style='height:34.5px; margin-left:85px'),
              div(style="display:inline-block; margin-top:1px;  margin-left:5px",
                  #tags$head(tags$style(HTML('#ModelTableSolidInput .selectize-input{padding: 5px 12px; border-radius:20px;}'))),
                  #tags$head(tags$style(HTML('#ModelTableSolidInput .selectize-dropdown.form-control{padding: 5px 12px; border-radius:20px;}'))),
                  selectInput('ModelTableSolidInput','',choices= makeList(letters[1:6]), #h4(HTML('&#160;'))
                              selected = 6, width='50px'),
              ),
              #),
              
              
          ),
          br(),br(),#br(),br(),
      ),
      
  ),
  ModelsOutputFrame(),
)