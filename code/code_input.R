SaveBlock0 <- function (IDName, topmargin = -5, bg = '#F5DFD5'){
  print(paste0("Save",IDName,"Data"))
  div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
      div(style="display:inline-block;vertical-align:top; width:760px;  margin-left:0px",
          br()
      ),
      div(style="display:inline-block;vertical-align:top; width:220px;  margin-left:0px",
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='200px')
      ),
      
      div(style="display:inline-block;vertical-align:top; width:220px;  margin-left:0px",
          br(),
          h3('Save plot'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:200px}"), media="screen", type="text/css")),
          #column(3,h5(HTML('&#160;')),downloadButton("Esavedata2", "Save results as xlsx"))
      ),
      
  )
}

SaveBlockTRANSITIONSCOUNT <- function (IDName,  topmargin = -5, bg = '#F5DFD5'){
  #print(paste0("Save",IDName,"Data"))
    tagList(
      div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
          
          div(style="display:inline-block;vertical-align:top; width:550px;  margin-left:20px",
              br(),
              h3('Data source:'),
              tags$ul(
                tags$li(
                  tags$a(href = "https://ec.europa.eu/eurostat/web/microdata/european-union-labour-force-survey", "LFS EUROSTAT"),
                  " - EU labour force survey"
                ),
              ),
          ),
          
          div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:350px",
              br(),

              h3('Save data'),
              
              ui_password_btn(paste0("Save",IDName,"DataRaw")),
              
              
          ),br(),br()),
      #br(),
      #br(),
      #br(),
    ) 
}

SaveBlockFLOWSIMMCOUNT <- function (IDName,  topmargin = -5, bg = '#F5DFD5'){
  print(paste0("Save",IDName,"Data"))
  tagList(
    div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
        
        div(style="display:inline-block;vertical-align:top; width:550px;  margin-left:20px",
            br(),
            # h3('data source:'),
            # tags$ul(
            #   tags$li(
            #     tags$a(href = "https://ec.europa.eu/eurostat/web/microdata/european-union-labour-force-survey", "LFS EUROSTAT"),
            #     " - EU labour force survey"
            #   ),
            # ),
        ),
        
        div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:350px",
            br(),
            
            h3('Save data'),
            
            ui_password_btn(paste0("Save",IDName,"DataRaw")),
            
            
        ),br(),br()),
    #br(),
    #br(),
    #br(),
  ) 
}

SaveBlockFLOWSEMICOUNT <- function (IDName,  topmargin = -5, bg = '#F5DFD5'){
  print(paste0("Save",IDName,"Data"))
  tagList(
    div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
        
        div(style="display:inline-block;vertical-align:top; width:550px;  margin-left:20px",
            br(),
            # h3('data source:'),
            # tags$ul(
            #   tags$li(
            #     tags$a(href = "https://ec.europa.eu/eurostat/web/microdata/european-union-labour-force-survey", "LFS EUROSTAT"),
            #     " - EU labour force survey"
            #   ),
            # ),
        ),
        
        div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:350px",
            br(),
            
            h3('Save data'),
            
            ui_password_btn(paste0("Save",IDName,"DataRaw")),
            
            
        ),br(),br()),
    # br(),
    # br(),
    # br(),
  ) 
}


SaveBlockSRC <- function (IDName, direction, topmargin = -5, bg = '#F5DFD5'){
  print(paste0("Save",IDName,"Data"))
  if (direction == "I"){
    tagList(
    div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:550px;  margin-left:20px",
          br(),
          h3('Immigration data sources'),
          tags$ul(
            tags$li(
              tags$a(href = "https://ec.europa.eu/eurostat/en/web/products-datasets/-/MIGR_IMM5PRV", "EUROSTAT"),
              " - Eurostat migration database."
            ),
            tags$li(
              tags$a(href = "https://www.un.org/development/desa/pd/data/international-migration-flows", "UN"),
              " - United Nation, International Migration Flows to and from Selected Countries: The 2015 Revision."
            ),
            tags$li(
              tags$a(href = "https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=12711&language=en#abreadcrumb", "DE_NSO"),
              " - DESTATIS, Statistisches Bundesamt, GENESIS V4.4.2 - 2022."
            ),
            tags$li(
              tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/ipscountryofbirthbycountryoflastornextresidence", "UK_NSO"),
              " - UK, Office for National Statistics, International Passenger Survey."
            ),
            tags$li(
              #tags$a(href = "https://eprints.soton.ac.uk/51993/", "MIMOSA"),
              tags$a(href = "https://www.imem.cpc.ac.uk/", "IMEM"),
              " - Data used in IMEM and MIMOSA projects (Not avaialbale online)."
            )
          ),
      ),
      
      div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:350px",
          br(),
          h3('Save source data info'),
          downloadButton(paste0("Save",IDName,"DataSrc"), "Save as xlsx", style='width:200px'),
          #tags$head(tags$style(paste0("#Save",IDName,"Data {width:200px}"), media="screen", type="text/css")),
          br(),br(),
          #h3('Save data'),
          #ui_password_btn(paste0("Save",IDName,"DataRaw")),
          
          #tags$head(tags$style(paste0("#Save",IDName,"DataRaw {width:200px}"), media="screen", type="text/css")),
          # h3('Save source data info'),
          # downloadButton(paste0("Save",IDName,"Data"), "Save data as xlsx"),
          # tags$head(tags$style(paste0("#Save",IDName,"Data {width:200px}"), media="screen", type="text/css")),
          
      ),br(),br()),
      # br(),
      # br(),
      # br(),
  ) 
  } else if (direction == "E"){
    tagList(
    div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
        
        div(style="display:inline-block;vertical-align:top; width:550px;  margin-left:20px",
            br(),
            h3('Immigration data sources'),
            tags$ul(
              tags$li(
                tags$a(href = "https://ec.europa.eu/eurostat/en/web/products-datasets/-/MIGR_EMI3NXT", "EUROSTAT"),
                " - Eurostat migration database."
              ),
              tags$li(
                tags$a(href = "https://www.un.org/development/desa/pd/data/international-migration-flows", "UN"),
                " - United Nation, International Migration Flows to and from Selected Countries: The 2015 Revision."
              ),
              tags$li(
                tags$a(href = "https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=12711&language=en#abreadcrumb", "DE_NSO"),
                " - DESTATIS, Statistisches Bundesamt, GENESIS V4.4.2 - 2022."
              ),
              tags$li(
                tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/ipscountryofbirthbycountryoflastornextresidence", "UK_NSO"),
                " - UK, Office for National Statistics, International Passenger Survey."
              ),
              tags$li(
                #tags$a(href = "https://eprints.soton.ac.uk/51993/", "MIMOSA"),
                tags$a(href = "https://www.imem.cpc.ac.uk/", "IMEM"), " - Data used in IMEM and MIMOSA projects (Not avaialbale online)."
              )
            ),
        ),
        
        div(style="display:inline-block;vertical-align:top; width:250px;  margin-left:350px",
            br(),
            h3('Save source data info'),
            downloadButton(paste0("Save",IDName,"DataSrc"), "Save as xlsx", style='width:200px'),
            #tags$head(tags$style(paste0("#Save",IDName,"Data {width:200px}"), media="screen", type="text/css")),
            br(),br(),
           # h3('Save data'),
          #  ui_password_btn(paste0("Save",IDName,"DataRaw")),
            
            # actionButton(paste0("Save",IDName,"DataRaw"), "Save as xlsx"),
            # tags$head(tags$style(paste0("#Save",IDName,"DataRaw {width:200px}"), media="screen", type="text/css")),
            
        ),br(),br()),
        # br(),
        # br(),
        # br(),
    ) 
    
  }
}


SaveBlock1 <- function (IDName, topmargin = -5, bg = '#F5DFD5'){
  print(paste0("Save",IDName,"Data"))
  div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
      
      div(style="display:inline-block;vertical-align:top; width:350px;  margin-left:100px",
          br(),
          h3('Save data'),
          downloadButton(paste0("Save",IDName,"Data"), "Save as xlsx"),
          tags$head(tags$style(paste0("#Save",IDName,"Data {width:200px}"), media="screen", type="text/css")),
      ),
      div(style="display:inline-block;vertical-align:top; width:350px;  margin-left:20px",
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='200px')
      ),
      
      div(style="display:inline-block;vertical-align:top; width:350px;  margin-left:20px",
          br(),
          h3('Save plot'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:200px}"), media="screen", type="text/css")),
          #column(3,h5(HTML('&#160;')),downloadButton("Esavedata2", "Save results as xlsx"))
      ),
      #),
  )
}

SaveBlock1B <- function (IDName, topmargin = -5, bg = '#F5DFD5',width_btn=c(200,200,200), width_div=c(350,350,350),mar_left=c(100,20,20), mar_top=rep(0,3)){
  print(paste0("Save",IDName,"Data"))
  #div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),   #style='margin:0px; padding:0px',
  tagList(
      div(style=paste0("display:inline-block;vertical-align:top; width:",width_div[1],"px;  margin-left:",mar_left[1],"px; margin-top:",mar_top[1],"px"),
          br(),
          h3('Save data'),
          downloadButton(paste0("Save",IDName,"Data"), "Save as xlsx"),
          tags$head(tags$style(paste0("#Save",IDName,"Data {width:",width_btn[1],"px}"), media="screen", type="text/css")),
      ),
      div(style=paste0("display:inline-block;vertical-align:top; width:",width_div[2],"px;  margin-left:",mar_left[2],"px; margin-top:",mar_top[2],"px"),
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width=paste0(width_btn[2],'px'))
      ),
      div(style=paste0("display:inline-block;vertical-align:top; width:",width_div[3],"px;  margin-left:",mar_left[3],"px; margin-top:",mar_top[3],"px" ),
          br(),
          h3('Save plot'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:",width_btn[3],"px}"), media="screen", type="text/css")),
          #column(3,h5(HTML('&#160;')),downloadButton("Esavedata2", "Save results as xlsx"))
      ),
      #),
  )
}


SaveBlock2 <- function (IDName, topmargin = 5, 
                        bg = '#F5DFD5',
                        choice1ID, choice2ID, choices1, choices2){
  div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),  
      div(style="display:inline-block;vertical-align:top; width:300px; margin-left:20px",
          br(),
          h3('Plot style'),
          selectInput(choice1ID, label = NULL,
                      choices = makeList(choices1),
                      selected = 2, width='300px'),
      ),
      div(style="display:inline-block;vertical-align:top; width:300px;  margin-left:60px",
          br(),
          h3('Prediction type'),
          selectInput(choice2ID, label = NULL,
                      choices = makeList(choices2),
                      selected = 1, width='300px'),
      ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')
      ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Save'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:180px}"), media="screen", type="text/css")),
      ),
  )
}

SaveBlock2b <- function (IDName, topmargin = 5, 
                         bg = '#F5DFD5',
                         choice1ID, 
                         #choice2ID, 
                         choices1#, 
                         #choices2
){
  div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),  
      div(style="display:inline-block;vertical-align:top; width:300px;  margin-left:60px",
          br()
      ),
      div(style="display:inline-block;vertical-align:top; width:300px; margin-left:20px",
          br(),
          h3('Plot style'),
          selectInput(choice1ID, label = NULL,
                      choices = makeList(choices1),
                      selected = 2, width='300px'),
      ),
      # div(style="display:inline-block;vertical-align:top; width:300px;  margin-left:60px",
      #     br(),
      #     h3('Prediction type'),
      #     selectInput(choice2ID, label = NULL,
      #                 choices = makeList(choices2),
      #                 selected = 1, width='300px'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')
      ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Save'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:180px}"), media="screen", type="text/css")),
      ),
  )
}


SaveBlock1b <- function (IDName, topmargin = 5, 
                         bg = '#F5DFD5',
                         choice1ID, choice2ID, choices1, choices2){
  div(class="row", style=paste0('margin-left:0px; margin-top:',topmargin,'px; ; background-color:',bg,'; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f'),  
      div(style="display:inline-block;vertical-align:top; width:300px; margin-left:20px",
          br(),
          h3('Plot style'),
          selectInput(choice1ID, label = NULL,
                      choices = makeList(choices1),
                      selected = 2, width='300px'),
      ),
      # div(style="display:inline-block;vertical-align:top; width:300px;  margin-left:60px",
      #     br(),
      #     h3('Prediction type'),
      #     selectInput(choice2ID, label = NULL,
      #                 choices = makeList(choices2),
      #                 selected = 1, width='300px'),
      # ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Image format'),
          selectInput(paste0("Save",IDName,"Format"), NULL,
                      choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')
      ),
      div(style="display:inline-block;vertical-align:top; width:200px;  margin-left:60px",
          br(),
          h3('Save'),
          downloadButton(paste0("Save",IDName,"Plot"), "Save image"),
          tags$head(tags$style(paste0("#Save",IDName,"Plot {width:180px}"), media="screen", type="text/css")),
      ),
  )
}



mysubmenu<-function(ID="Ipanels",
                    choiceNames = makebold(c("Migration flows","Accuracy","Undercounting","Duration of stay","Gravity covariates")),
                    individual=FALSE,
                    space=3, 
                    width=1200,
                    width_panel=width/length(choiceNames) + space,# + space*!individual,
                    passive.bg="#DCA564",
                    passive.bo="#906010",
                    passive.co="#FFFFFF",
                    active.bg="#FFDFAB",
                    active.bo="9985A2",
                    active.co="#6f5b3f",
                    fontsize=16,
                    height=43,
                    left.margin=-space -(space*(!individual)),
                    top.padding=0#,
                    #bottom.margin=0
                    
){
  print(choiceNames)
  print(width_panel)
  style1<-paste0('margin-left: ',left.margin,'px;',' padding-top: ',top.padding,'px; height: ',height+top.padding,'px; width: ',width,'px; padding-bottom:0px;'
                 #'px; margin-bottom: ',bottom.margin,'px; 
  )
  style2 <- paste0('#',ID,' .btn-group{ padding-left: ',space,'px; width:',width_panel,'px; color: ',passive.co,'}')    
  style3 <- paste0("#",ID," .btn-danger {background-color: ",passive.bg,"; border-color: ",passive.bo,"; height:",height,"px; color: ",passive.co,"; font-size: ",fontsize,"px;}")
  style4 <- paste0("#",ID," .btn-danger.active {background-color: ",active.bg,"; border-color: ",active.bo,"; color: ",active.co,"; height: ",height,"px; font-size: ",fontsize,"px;}")
  
  print(style1)
  print(style2)
  print(style3)
  print(style4)
  
  tagList(div(class="row", style=style1,
              radioGroupButtons(
                inputId = ID,
                label = NULL,
                justified= TRUE,
                width='100%',
                individual=individual,
                choiceNames = choiceNames,
                choiceValues = seq_along(choiceNames),
                status = "danger"
              )),
          
          tags$head(tags$style(style2, media="screen", type="text/css")),  
          tags$head(tags$style(style3, media="screen", type="text/css")),
          tags$head(tags$style(style4, media="screen", type="text/css")))
  
}
#mysubmenu()

myPrettyCheckbox<-function(ID, label, value = FALSE,
                           status = "primary",
                           width = NULL, active.bg='#ff8000', passive.bg='#ef9482', mar.top=-5, mar.bottom=0)
  tagList(    
    #tags$head(tags$style(HTML(paste0('#',ID,' :after{background-color:',active.bg,'}, #',ID,' :before{background-color:',passive.bg,';}')))),
    #tags$head(tags$style(HTML(paste0('#',ID,' .awesome-checkbox-primary label::after{background-color:',active.bg,'}, #',ID,' .awesome-checkbox-primary label::before{background-color:',passive.bg,';}')))),
    #tags$head(tags$style(HTML(paste0('#',ID,' .checkbox-primary input[type="checkbox"]:checked + label::before, .checkbox-primary input[type="radio"]:checked + label::before
    #{background-color:',active.bg,'}')))),
    tags$head(tags$style(HTML(paste0('.checkbox-primary input[type="checkbox"]:checked + label::before, .checkbox-primary input[type="radio"]:checked + label::before {background-color:',active.bg,'}')))),
    #tags$head(tags$style(HTML(paste0('.checkbox-primary {margin-top:',mar.top,'px}')))),
    tags$style(HTML(paste0('.awesome-checkbox label::before{top:0px}'))) ,
    tags$style(HTML(paste0('.awesome-checkbox label::after{top:0px}'))) ,
    #tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::before{margin-top:',mar.top,'px; margin-right:0px}'))) ,
    #tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::after{margin-top:',mar.top,'px; margin-right:0px}'))) ,
    
    awesomeCheckbox(ID, label=label, value=value, status=status, width=width))


myPrettyGroupCheckbox<-function(ID, label, choices, selected, inline, active.bg='#F04040', passive.bg='#ef9482', ncol=3, mar.top=-2)
  tagList(    
    tags$style(HTML(paste0('#',ID,' .awesome-checkbox.checkbox-inline{margin-left:-10px; margin-top:0px; margin-right:0px}'))) ,
    # tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::before{margin-top:',mar.top,'px; margin-right:0px;border-radius:10px;width:17px;height:17px;}'))) ,
    # tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::after{margin-top:',mar.top,'px; margin-right:0px;border-radius:10px;width:17px;height:17px;}'))) ,
    tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::before{top:0px; margin-right:0px;border-radius:10px;width:17px;height:17px;}'))) ,
    tags$style(HTML(paste0('#',ID,' .awesome-checkbox label::after{top:0px; margin-right:0px;border-radius:10px;width:17px;height:17px;}'))) ,
    tags$head(tags$style(HTML(paste0(".",ID,"multicol {-webkit-column-count:",ncol,";-moz-column-count:",ncol,";column-count:",ncol,";-moz-column-fill:auto;-column-fill:auto;column-gap:0px;}")))),
    tags$head(tags$style(HTML(paste0('#',ID,' :after{background-color:',active.bg,';border-color:#000000}, #',ID,' :before{background-color:',passive.bg,';}')))),
    tags$div(align = 'left',class = paste0(ID,'multicol'), 
             awesomeCheckboxGroup(ID, label =label, choices = choices, selected = selected, inline = inline),
    ),
  )

ModelsOutputFrame <- function(DTid="MixedModelTable",BTNid='SelectedModelTableDownload',headtxt='Estimated migratioon flows for selected models'){
  tagList(
  div(class="row", style='margin-left:0px; margin-top:5px; background-color:#FFFFFF; width:1215px; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
      #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
      br(),
      div(style="display:inline-block;vertical-align:top; width:980px; margin-left:20px",
          
          h3(headtxt),
      ),
      div(style="display:inline-block;vertical-align:top; width:150px; margin-left:20px; margin-top:0px",
  
          downloadButton(BTNid, "Download estimates"),
          tags$head(tags$style(HTML(paste0('#',BTNid,' {background-color:#ede594} #',BTNid,':hover{background-color:#FFC0C0}')))),
          br(), 
      ),
      
      div(style="display:inline-block;vertical-align:top;  width:1170px; margin-left:20px;background-color:#FFFFFF",
          br(),br(),
          DTOutput(DTid),
      ), 
      br(),
      
      br(),br()
  )
  )
}

mySurveyFrame <-function(ID='DB'){
  tagList(
    div(class="row", style='margin-left:0px; margin-top:5px; background-color:#c7d3f1; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
        br(),
        div(style="display:inline-block;vertical-align:top; width:1200px; margin-left:20px",
            h3('Feedback and comments')
        ),
        
        div(style="display:inline-block;vertical-align:top; width:584px; margin-left:20px",
            uiOutput(paste0("h",ID,'name')),#,h3(id='hDBname','Please provide your name'),   
            tags$head(tags$style(HTML(paste0('#',ID,'name {margin-top:-20px}')))),
            textInput(paste0(ID,'name'),label='', width='565px',placeholder = 'Your name'),
        ),
        div(style="display:inline-block;vertical-align:top; width:584px; margin-left:20px",
            uiOutput(paste0("h",ID,'email')),#h3(id='hDBemail','Please provide your emial or contact information'),   
            tags$head(tags$style(HTML(paste0('#',ID,'email {margin-top:-20px}')))),
            textInput(paste0(ID,'email'),label='', width='565px',placeholder = 'Your contact info'),
            
        ),
        
        div(style="display:inline-block;vertical-align:top; width:1170px; margin-left:20px", #1168
            br(),   
            #h3(id='hDBcomment','Please provide yor comment and press submit button below'),
            uiOutput(paste0("h",ID,'comment')),
            tags$head(tags$style(HTML(paste0('#',ID,'comment {margin-top:-20px}')))),
            textAreaInput(paste0(ID,'comment'),label='', width='1170px', height='100px',placeholder = 'Your comment', resize="vertical"),
            
            tags$head(tags$style(HTML('#submit {background-color:#ede594} #submit:hover{background-color:#FFC0C0}'))),
            actionButton("submit", HTML(paste(shiny::icon("cloud-upload"),"Submit your response"))),
            tags$head(tags$style(HTML('#showsurvey {background-color:#ede594} #showsurvey:hover{background-color:#FFC0C0}'))),
            actionButton("showsurvey", HTML(paste(shiny::icon('lock'),"Show all responses"))),
            
            #br(),
            div(style='display:none',textInput("showArchiveDiv", "", value = "false")), 
            #div(textInput("showArchiveDiv", "", value = "false")), 
        ),
        br(),br(),
    ),
    
    conditionalPanel(condition = 'input.showArchiveDiv == "true"',
                     #div(id='SurveyArchive',
                     div(class="row", style='margin-left:0px; margin-top:5px; background-color:#c7d3f1; border-style: solid; border-color:#9985A2; border-width:1px; color:#2f4b2f',
                         #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
                         br(),
                         div(style="display:inline-block;vertical-align:top; width:1170px; margin-left:20px",
                             
                             h3('Collected comments and remarks'),
                         ),
                         div(style="display:inline-block;vertical-align:top; width:1170px; margin-left:20px;background-color:#FFFFFF",
                             
                             DTOutput("surveyTable"),
                         ), 
                         br(),
                         div(style="display:inline-block;vertical-align:top; width:1170px; margin-left:20px; margin-top:20px",
                             actionButton("remove", HTML(paste(shiny::icon('eraser'),"Remove selected row(s)"))),
                             tags$head(tags$style(HTML('#remove {background-color:#ede594} #remove:hover{background-color:#FFC0C0}'))),
                             downloadButton("downloadDataBase", "Download survey database"),
                             tags$head(tags$style(HTML('#downloadDataBase {background-color:#ede594} #downloadDataBase:hover{background-color:#FFC0C0}'))),
                             br(),
                         ),
                         br(),br(),
                         # #tags$hr(style = "border-top: 4px solid #9985A2; font-weight: bold;"),
                         # div(style="display:inline-block;vertical-align:top; width:1170px; margin-left:20px; margin-top:20px",
                         #     br(),
                         #     h3('List of files on server'),
                         #     DTOutput("FileTable1"),
                         #     br(),br(),
                         # )
                     )     
                     
    )
  )
}

passwordWrapper<-function(id, idbutton){
  tagList(
    tags$script(HTML(paste0("$(document).keyup(function(event) {if ($('#",id,"').is(':focus') && (event.keyCode == 13)) {$('#",idbutton,"').click();}});"))),
    tags$div(
      tags$style(
        ".password-wrapper {position: relative;} .password-wrapper input[type='password'] {padding-right: 2.5rem !important;}"
      ),
      tags$div(
        class = "password-wrapper",
        tags$input(
          type = "password",
          class = "form-control",
          id = id,
          placeholder = "Enter Password"
        ),
        tags$span(
          class = "glyphicon glyphicon-eye-open",
          style = "position: absolute; top: 0.8rem; right: 0.5rem; cursor: pointer;",
          onclick = "togglePassword(this.previousElementSibling)"
        )
      )
    ),
    tags$script("function togglePassword(input) { var x = input;  if (x.type === 'password') { x.type = 'text';} else { x.type = 'password';}}")
  )
}

eraser_password_input <<- function(rows,session) {
  modalDialog(
    h3(paste('The following row(s) will be removed:',make_str_list(rows))),
    h4(HTML('Are you sure you want to proceed? Confirm by entering the correct password. You may also consider submitting a new note where you describe your changes to the comment referring to the "<b>id</b>" number(s) of the note(s) you now want to delete.')),
    passwordWrapper("eraser_password","submit2"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit2", "Submit")
    )
  )
  #session$sendCustomMessage("jsCode", "$(document).ready(function() { $('#eraser_password').focus(); });")
  #session$sendCustomMessage("jsCode", "$.ajaxStop(function() { $('#eraser_password').focus(); });")
  
}

showsurvey_password_input <<- function(session) {
  modalDialog(
    
    h4(HTML('Please provide password to see the entries of other users.')),
    passwordWrapper("showsurvey_password","submit_showsurvey_password"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit_showsurvey_password", "Submit")
    )
  )
}

CleanModelTable_1 <<- function(session) {
  modalDialog(
    h4(HTML('Are you sure to restore default values?')),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("CleanModelTableYes1", "Yes")
    )
  )
}

CleanModelTable_2 <<- function(session, val) {
  modalDialog(
    h4(HTML(paste('Are you sure to fill whole table with',val,'?'))),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("CleanModelTableYes2", "Yes")
    )
  )
}

WrongModelTableFile <<- function() {
  modalDialog(
    h4(HTML(paste('Wrong format or inconsistent column-/row- names.'))),
    footer = tagList(
      modalButton("Cancel"),
    )
  )
}


setModalsStyle<-function(style="background-color: #F2E2C2;", 
                         button.hover.style="background-color: #ff7b00; border-color: #7b3b00;", 
                         button.primary.style="background-color: #feb957; border-color: #ae9937;")
  tagList(
    tags$head(tags$style(HTML(paste(".modal-content {",style,"}")))),
    tags$head(tags$style(HTML(paste(".modal-footer .btn{",button.primary.style,"}")))),
    tags$head(tags$style(HTML(paste(".modal-footer .btn:hover {",button.hover.style,"}"))))
  )
