runexample1<-function(session, input){
  if(input$Examples1==2) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK', 'HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('IE')])
    TrYear(2004)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==3) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('SE')])
    TrYear(2004)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==4) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('UK')])
    TrYear(2004)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==5) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('IE','SE','UK')])
    TrYear(2004)      
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==6) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('FI')])
    TrYear(2006)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==7) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('IS')])
    TrYear(2006)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==8) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('NO')])
    TrYear(2006)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==9) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('FI','IS','NO')])
    TrYear(2006)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==10) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('NL')])
    TrYear(2007)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==11) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('FR')])
    TrYear(2008)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==12) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('BE')])
    TrYear(2009)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==13) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL','SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('DK')])
    TrYear(2009)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==14) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('BE','DK')])
    TrYear(2009)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==15) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('AT')])
    TrYear(2011)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==16) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('CH')])
    TrYear(2011)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==17) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('DE')])
    TrYear(2011)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==18) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('AT','CH','DE')])
    TrYear(2011)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==19) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('AT')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==20) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('BE')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==21) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('DE')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==22) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('FR')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==23) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('NL')])
    TrYear(2014)      
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==24) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('UK')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==25) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('AT', 'BE', 'DE', 'FR', 'NL', 'UK')])
    TrYear(2014)  
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==26) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CY','CZ', 'CH','EE', 'LT', 'LV', 'PL', 'SI', 'SK','MT','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('IE')])
    TrYear(2004)      
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==27) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CY','CZ', 'CH','EE', 'LT', 'LV', 'PL', 'SI', 'SK','MT','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('SE')])
    TrYear(2004)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==28) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CY','CZ','CH', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK', 'MT','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('UK')])
    TrYear(2004)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==29) {
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('CY','CZ', 'CH','EE', 'LT', 'LV', 'PL', 'SI', 'SK','MT','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('IE','SE','UK')])
    TrYear(2004)      
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==30) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','CZ', 'EE', 'LT', 'LV', 'PL','RO', 'SI', 'SK','HU')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('DK')])
    TrYear(2009)
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  } else if(input$Examples1==31) {                          
    updateAwesomeCheckboxGroup(session = session, inputId = "SendCntrs", selected = CountriesFull[Countries%in% c('BG','RO')])
    updateAwesomeCheckboxGroup(session = session, inputId = "RecCntrs", selected = CountriesFull[Countries%in% c('AT', 'BE', 'DE', 'FR', 'NL', 'UK','MT','LU')])
    TrYear(2014)    
    updateAwesomeCheckbox(session=session,"UseThreshold", value=TRUE)
  }
}