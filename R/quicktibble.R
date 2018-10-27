#' RStudio addin to quickly build small tibbles from an existing data frame or array.
#'
#' RStudio addin to quickly build small tibbles from an existing data frame or array.
#' Useful with dplyr::left_join().
#'
#' @param .data Optional data to initialize the new table. If a data frame, then the selection will default to this data frame. If an array, then the input will be the array.
#' @param .name Optional name for tribble output
#' @return Text sting that can be read as a tribble in R
#' @export

quicktibble <- function(.data = NULL){
  ################
  # Non-Reactive functions ----
  ################
  if(is.null(.data)){
    input.data <- NULL
    data.check <- TRUE
  } else if(is.data.frame(.data)){
    input.data <- deparse(substitute(.data))
    data.check <- TRUE
  } else if(is.vector(.data) || is.factor(.data)){
    input.data <- deparse(substitute(.data))
    data.check <- FALSE
  } else {
    stop("Input data should be a data frame or a vector")
  }

  appCSS <-
    "#selInputData   ~ .selectize-control.single .selectize-dropdown [data-value='Provided Array'] { color: blue }
     #selInputColumn ~ .selectize-control.single .selectize-dropdown [data-value=`Provided Array'] { color: blue }"

  ################
  # UI ----
  ################
  ui <- miniUI::miniPage(
    tags$head(tags$style(HTML(appCSS))),
    miniUI::gadgetTitleBar("quicktibble"),
    conditionalPanel(
      condition = "input.selInputBuild == 0",
      miniUI::miniButtonBlock(
        fluidRow(
          column(width = 3,
                 selectInput("selInputData", label = "From data frame",
                             choices = c("None",
                                         names(eapply(.GlobalEnv,is.data.frame))[unlist(eapply(.GlobalEnv,is.data.frame))]),
                             selected = input.data),
                 conditionalPanel(
                   condition = "input.selInputData != 'Provided Array'",
                   selectInput("selInputColumn", label = "From column",
                               choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.selInputData == 'Provided Array'",
                   textInput("selInputArray", label = "Array name",
                              value = gsub("$", "_", deparse(substitute(.data)), fixed=TRUE))
                 )
          ),
          column(width = 3,
                 strong("Column names"),
                 helpText("Leave blank to not include"),
                 textInput("setCol2", label = NULL, value ="Col2", placeholder = "Column 2 name"),
                 textInput("setCol3", label = NULL, value ="",     placeholder = "Column 3 name")

          ),
          column(width = 3,
                 strong("Column names"),
                 helpText("---"),
                 textInput("setCol4", label = NULL, value ="",     placeholder = "Column 4 name"),
                 textInput("setCol5", label = NULL, value ="",     placeholder = "Column 5 name")
          ),
          column(width = 3,
                 checkboxInput("setWeight", "Weight column?", value = FALSE),
                 checkboxInput("setUnique", "Unique values", value = TRUE),
                 hr(),
                 actionButton("selInputBuild", label = "Create", icon = shiny::icon("table"),
                              style = "background-color:#4040FF; color:#ffffff;")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.selInputBuild != 0",
      miniUI::miniButtonBlock(
        textInput("setOPname", label = "Output table name", value = "QuickTibble", placeholder = "Output Name"),
        conditionalPanel(
          condition = "input.setWeight == true",
          p("When complete, scale weights?"),
          radioButtons("setWeightScale", label = NULL,
                       choices = "none",
                       selected = "none",
                       inline = TRUE),
          helpText("Choose a column name to scale each group to 100%.")
        )
      )
    ),
    miniUI::miniContentPanel(
      rhandsontable::rHandsontableOutput("hot"),
      strong("Output preview"),
      helpText("When finished, click 'Done' to paste in editor."),
      verbatimTextOutput("opPreview")
    )

  ) #End UI

  ################
  # Server ----
  ################

  server <- function(input, output, session) {

    ###
    # Update selects
    ###
    # DF selection
    observe({
      if(!data.check){
        updateSelectInput(session, "selInputData",
                          choices = c("Provided Array", "None",
                                      names(eapply(.GlobalEnv,is.data.frame))[unlist(eapply(.GlobalEnv,is.data.frame))]),
                          selected = "Provided Array")
      }
    })
    # Column selection
    observe({
      if(input$selInputData == "None"){
        updateSelectInput(session, "selInputColumn",
                          choices = "Select a data frame")
      } else if(input$selInputData == "Provided Array"){
        updateSelectInput(session, "selInputColumn",
                          choices = "Provided Array")
      } else{
        updateSelectInput(session, "selInputColumn",
                          choices = names(get(input$selInputData)))
      }
    })

    # Weight scaling
    observe({
      wghtChoices <- c(input$setCol2, input$setCol3, input$setCol4, input$setCol5)
      wghtChoices <- wghtChoices[wghtChoices != ""]

      updateRadioButtons(session, "setWeightScale",
                         choices = c("No"="no", "All to 100%"="total", input$selInputColumn, wghtChoices),
                         selected = "no",
                         inline = TRUE)
    })

    ###
    # Build starting data frame
    ###
    df.start <- eventReactive(input$selInputBuild, {

      #If starting is the provided array...
      if(!data.check && input$selInputColumn == "Provided Array"){
        starting.column <- as.character(.data)
      } else {
        starting.column <- tibble::as.tibble(get(input$selInputData)[, input$selInputColumn]) %>% dplyr::pull()
      }

      if(input$setUnique){
        starting.column <- unique(starting.column)
      }

      dat <- tibble::tibble(starting.column, V2 = "")

      # #If starting is the provided array...
      # if(!data.check && input$selInputColumn == "Provided Array"){
      #   # names(dat)[1] <- gsub("[^[:alnum:] ]", "", input$selInputArray)
      #   names(dat)[1] <- "Colllll"
      # } else {
        names(dat)[1] <- input$selInputColumn
      # }

      names(dat)[2] <- input$setCol2

      #Additional columns
      if(input$setCol3 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol3
      }
      if(input$setCol4 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol4
      }
      if(input$setCol5 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol5
      }

      #Weight Column
      if(input$setWeight){
        dat <- dat %>%
          dplyr::mutate(.Weight = 1)
      }

      return(dat)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$hot = rhandsontable::renderRHandsontable({
      if (!is.null(input$hot)) {
        DF = rhandsontable::hot_to_r(input$hot)
      } else {
        DF = df.start()
      }

      rhandsontable::rhandsontable(DF, height = (nrow(DF)+2)*25) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>%
        rhandsontable::hot_col(col = input$selInputColumn,  strict = FALSE, allowInvalid = TRUE)
    })

    # Finalize the output

    #Scale Weights
    tbScaled <- reactive({
      hot = input$hot
      if (!is.null(hot)) {
        dat <- rhandsontable::hot_to_r(hot) %>%
          dplyr::mutate_if(is.factor, as.character) %>%
          #Replace blanks each NA
          dplyr::mutate_all(dplyr::funs(ifelse(. == "", NA, .)))

        #Check to see if weight column
        if(input$setWeight){
          #Scale to total...
          if(input$setWeightScale == "total"){
            dat <- dat %>%
              dplyr::mutate(.Weight = round(.Weight / sum(.Weight, na.rm = TRUE), 5))
            #Scale to a column....
          } else if(input$setWeightScale != "no"){
            dat <- dat %>%
              dplyr::group_by(!!!rlang::syms(c(input$setWeightScale))) %>%
              dplyr::mutate(.Weight = round(.Weight / sum(.Weight, na.rm = TRUE), 5)) %>%
              dplyr::ungroup() %>%
              as.data.frame()
          } else {dat <- dat} #Other don't scale
        }

      } else {dat <- df.start()}

      return(dat)
    })

    # Preview the output
    codePreview <- reactive({
      hot = input$hot

      if (!is.null(hot)) {

        opName <- if(input$setOPname == ""){"QuickTibble"}else{input$setOPname}
        dat <- tbScaled()

        if(names(dat)[1] == "Provided Array"){
          names(dat)[1] <- gsub("[^[:alnum:] ]", "", input$selInputArray)
        }

        op <- df_to_tribble(dat, opName)

      } else {op <- "Print Preview"}

      return(op)

    })
    output$opPreview <- renderText({
      return(codePreview())
    })
    # output$opPreview <- renderTable({
    #   return(tbScaled())
    # })

    # Listen for 'done' events.
    observeEvent(input$done, {
      hot = isolate(input$hot)
      if (!is.null(hot)) {
        rstudioapi::insertText(Inf, codePreview())
      }
      stopApp()
    })

    session$onSessionEnded(function() {
      stopApp()
    })

  } #End Server

  ####
  # Addin settings ----
  ####

  viewer <- dialogViewer(paste("quicktibble"), width = 1200, height= 1200)


  runGadget(ui, server, viewer = viewer)
}

