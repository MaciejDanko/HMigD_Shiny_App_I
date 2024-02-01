
DBNAME <<- "HMigD_Flows_Survey"
suppressMessages(capture.output(ExistsDB <<- drive_get(DBNAME)$id))
if(!length(ExistsDB)) suppressMessages(capture.output(googlesheets4::gs4_create(name = DBNAME, sheets = "Comments"))) else cat('It exists!')
sheet_id <<- googledrive::drive_get(DBNAME)$id
#save(file = './.secrets/test.rda',list = 'DBNAME')

write_survey_to_google<-function(session, input){
  isolate({
    newrow<-data.frame(
      hash=randcode(25),
      ip=content(GET("https://api.ipify.org?format=text"), "text"),
      time=paste(Sys.time(),Sys.timezone()),
      name=input$DBname, 
      email=input$DBemail, 
      sending=Countries[as.numeric(input$SendingCountry)], 
      receiving=Countries[as.numeric(input$ReceivingCountry)], 
      m1=input$MODEL1,
      m2=input$MODEL2, 
      option=paste(input$UseAltPred1,collapse = ','), 
      comment=input$DBcomment)
    values <- googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
    if (nrow(values) == 0) {
      googlesheets4::sheet_write(data = newrow, ss = sheet_id, sheet = "Comments")
    } else {
      googlesheets4::sheet_append(data = newrow, ss = sheet_id, sheet = "Comments")
    }
  })
}
