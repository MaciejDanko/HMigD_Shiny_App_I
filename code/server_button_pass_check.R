  ui_password_btn <- function(btn_id="downloadBtn"){
    uiOutput(paste(btn_id,"buttonSection",sep='_'))
  }
  
  # user function must be a downloadHandler
  server_password_btn<-function(input, output, session, 
                                btn_id="downloadBtn", 
                                width = "200px",
                                filenameFunc=function() NULL, 
                                contentFunc=function() NULL,
                                pass='123',
                                download_btn = TRUE,
                                custom_ui_output = NULL){
    rv <- reactiveValues()
    print('INIT')
    #nam_correctPassword<-paste(btn_id,'correctPassword',sep='_')
    nam_PasswordPassed<-paste(btn_id,'PasswordPassed',sep='_')
    nam_modalShown<-paste(btn_id,'ModalShown',sep='_')
    nam_passwordModal_input<-paste(btn_id,'Password_Modal_InputLine',sep='_')
    nam_passwordModal_cancel<-paste(btn_id,'Password_Modal_CancelBtn',sep='_')
    nam_passwordModal_submit<-paste(btn_id,'Password_Modal_SubmitBtn',sep='_')
    nam_buttonSection <- paste(btn_id,"buttonSection",sep='_')
    
    #nam_activeButtonClicked<-paste(btn_id, "ActiveButtonClicked",sep="_")
    nam_activeButton<-paste(btn_id, "ActiveButton",sep="_")
    nam_downloadButton<-paste(btn_id, "DownloadButton",sep="_")
    
    #rv[[nam_correctPassword]] <- pass
    #rv[[nam_activeButtonClicked]] <- FALSE
    rv[[nam_PasswordPassed]] <- FALSE
    rv[[nam_modalShown]] <- FALSE
    
    #observeEvent(input$activeBtn, {
    #     activeButtonClicked(TRUE)
    #   })
    
    
    observeEvent(input[[nam_activeButton]], {
      print('BTN PRESSED')
      if (!rv[[nam_modalShown]] && !rv[[nam_PasswordPassed]]) {
        showModal(
          modalDialog(
            id = nam_passwordModal_input,
            passwordWrapper(nam_passwordModal_input, "Enter Password:"),
            footer = tagList(
              actionButton(nam_passwordModal_cancel,"Cancel"),
              actionButton(nam_passwordModal_submit, "Submit")
            )
          )
        )
        rv[[nam_modalShown]] <- TRUE
      } else {
        print('MODAL CLOSED')
        if (rv[[nam_modalShown]]){
          removeModal()
          rv[[nam_modalShown]] <- FALSE
        }
        #rv[[nam_activeButtonClicked]] <- TRUE
      }
    })
    
    observeEvent(input[[nam_passwordModal_cancel]], {
      removeModal()
      rv[[nam_modalShown]] <- FALSE
    })
    
    # Check the password when the submit button is pressed
    observeEvent(input[[nam_passwordModal_submit]], {
      password <- isolate(input[[nam_passwordModal_input]])
      rv[[nam_modalShown]]<-FALSE
      removeModal()
      if (password == pass){#rv[[nam_correctPassword]]) {
        rv[[nam_PasswordPassed]]<-TRUE
        #rv[[nam_activeButtonClicked]] <- TRUE
      } else {
        # Password is incorrect, show an error message
        showModal(
          modalDialog(
            title = "Incorrect Password",
            "Please enter the correct password.",
            footer = modalButton("OK")
          )
        )
      }
    })
    
    if (download_btn) {
    output[[nam_buttonSection]] <- renderUI({
      if (rv[[nam_PasswordPassed]]) {
        tagList(
          downloadButton(nam_downloadButton, "Download data",style=paste0("width:",width),icon = shiny::icon("download")),
          # br(),
          # br(),
          actionButton(nam_activeButton, "Unlock download", style = paste0("display:none;width:",width),icon = shiny::icon("download"))
        )
      } else {
        actionButton(nam_activeButton, "Unlock download",icon = shiny::icon("download"),style = paste0("width:",width))
      }
    })
    
    output[[nam_downloadButton]] <- downloadHandler(
          filename = filenameFunc,
          content = contentFunc
        )
    } else {
      output[[nam_buttonSection]] <- renderUI({
        if (rv[[nam_PasswordPassed]]) {
          tagList(
            custom_ui_output,
            actionButton(nam_activeButton, "Unlock advanced options", style = paste0("display:none;width:",width),icon = shiny::icon("lock"))
          )
        } else {
          colorActionButton(nam_activeButton, "Unlock advanced options",icon = shiny::icon("lock"),style = paste0("width:",width))
        }
      })
    }
  }
  
  
  
  # server_password_btn_2<-function(input, output, session, btn_id="downloadBtn", pass='123',  userfunc=function(input, output, session) {print('test')} ){
  #   rv <- reactiveValues()
  #   print('INIT')
  #   #nam_correctPassword<-paste(btn_id,'correctPassword',sep='_')
  #   nam_PasswordPassed<-paste(btn_id,'PasswordPassed',sep='_')
  #   nam_modalShown<-paste(btn_id,'ModalShown',sep='_')
  #   nam_passwordModal_input<-paste(btn_id,'Password_Modal_InputLine',sep='_')
  #   nam_passwordModal_cancel<-paste(btn_id,'Password_Modal_CancelBtn',sep='_')
  #   nam_passwordModal_submit<-paste(btn_id,'Password_Modal_SubmitBtn',sep='_')
  #   
  #   #rv[[nam_correctPassword]] <- pass
  #   rv[[nam_PasswordPassed]] <- FALSE
  #   rv[[nam_modalShown]] <- FALSE
  #   
  #   observeEvent(input[[btn_id]], {
  #     print('BTN PRESSED')
  #     if (!rv[[nam_modalShown]] && !rv[[nam_PasswordPassed]]) {
  #       showModal(
  #         modalDialog(
  #           id = nam_passwordModal_input,
  #           passwordWrapper(nam_passwordModal_input, "Enter Password:"),
  #           footer = tagList(
  #             actionButton(nam_passwordModal_cancel,"Cancel"),
  #             actionButton(nam_passwordModal_submit, "Submit")
  #           )
  #         )
  #       )
  #       rv[[nam_modalShown]] <- TRUE
  #     } else {
  #       print('MODAL CLOSED')
  #       if (rv[[nam_modalShown]]){
  #         removeModal()
  #         rv[[nam_modalShown]] <- FALSE
  #       }
  #       userfunc(input,output,session)
  #     }
  #   })
  #   
  #   observeEvent(input[[nam_passwordModal_cancel]], {
  #     removeModal()
  #     rv[[nam_modalShown]] <- FALSE
  #   })
  #   
  #   # Check the password when the submit button is pressed
  #   observeEvent(input[[nam_passwordModal_submit]], {
  #     password <- isolate(input[[nam_passwordModal_input]])
  #     rv[[nam_modalShown]]<-FALSE
  #     removeModal()
  #     if (password == pass){#rv[[nam_correctPassword]]) {
  #       rv[[nam_PasswordPassed]]<-TRUE
  #       userfunc(input,output,session)
  #     } else {
  #       # Password is incorrect, show an error message
  #       showModal(
  #         modalDialog(
  #           title = "Incorrect Password",
  #           "Please enter the correct password.",
  #           footer = modalButton("OK")
  #         )
  #       )
  #     }
  #   })
  # }
  # 