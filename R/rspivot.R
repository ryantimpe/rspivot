library(shiny)
library(miniUI)
library(DT)
library(tidyverse)
library(lazyeval)

rspivot <- function(df=.Last.value) {

  ################ ----
  # UI
  ################
  ui <- miniPage(
    gadgetTitleBar("RS Pivot"),
    miniTabstripPanel(
      miniTabPanel(
        title = "Pivot",
        icon = icon("table"),
        miniContentPanel(
          scrollable = TRUE,
          fillRow(
            flex = c(1, 4),
            list(
              selectInput("PivCols", label = "Columns",
                          choices = NULL , selected = NULL),
              selectInput("PivRows", label = "Rows",
                          choices = NULL , selected = NULL),
              selectInput("PivRowNest", label = "Nested Rows",
                          choices = NULL , selected = NULL),
              hr(),
              uiOutput("selects")
            ),
            list(
              DT::dataTableOutput("df_table")
            )
          ) #End Row

        )
      ), #End Pivot Tab
      miniTabPanel(
        title = "Data Options",
        icon = icon("edit"),
        fluidRow(
          column(width = 3,
                 selectInput("dataMetric", label = "Data Metric",
                             choices = c("Values", "Growth", "Shares"), selected = "Values"),
                 conditionalPanel(
                   condition = "input.dataMetric != 'Values'",
                   selectInput("dataMetricSeries", label = "Metric over",
                               choices = NULL , selected = NULL)
                 )
                 ),
          column(width = 3,
                 strong("Decimals"),
                 numericInput("decValues", label = "Value metric", value = 0,
                              min = 0, max = 5, step = 1),
                 numericInput("decMetric", label = "Growth/Share metric", value = 1,
                              min = 0, max = 5, step = 1),
                 numericInput("decNested", label = "Nested metric", value = 3,
                              min = 0, max = 8, step = 1),
                 hr(),
                 selectInput("oomValues", label = "Value Order of Magnitude",
                             choices = c("Units" = 1, "Thousands" = 10^-3, "Millions" = 10^-6, "Billions" = 10^-9),
                             selected = 1)
                 )
        )
      )
      )
  )

  ################ ----
  # Non-Reactive functions
  ################

  dim_names <- names(df)[!(names(df) %in% c("value"))]
  all.elements <- "Show All"

  ################ ----
  # Server
  ################

  server <- function(input, output, session) {

    dat0 <- reactive({df})

    ###
    #Create UI menus ----
    ###

    #Col/Row selection
    updateSelectInput(session, "PivCols",
                      choices = dim_names, selected = tail(dim_names, 1)[1])

    updateSelectInput(session, "PivRows",
                      choices = dim_names, selected = tail(dim_names, 2)[1])

    updateSelectInput(session, "PivRowNest",
                      choices = c("None", dim_names), selected = "None")

    #Series filtering
    output$selects <- renderUI({
      #Loop through each dimension to build a filter
      lapply(seq_along(dim_names), function(i){
        dat <- dat0()

        #Treat all series the same... time, nonagg, etc...
        choice.list <- c(all.elements, unique(dat0()[, dim_names[i]]))
        #Choose that total
        choice.selected <- choice.list[1]
        #Multiple allowed
        choice.mult <- TRUE
        #Is a numeric input?
        series.num <- names(dat)[sapply(dat[, dim_names], is.numeric)]

        #Number input
        if(dim_names[i] %in% series.num){
          slide.min <- min(dat[, dim_names[i]])
          slide.max <- max(dat[, dim_names[i]])
        }

        # Build the Menu for each dimension
        list(
           #Filter for numeric dimension
           if(dim_names[i] %in% series.num){
             sliderInput(
               inputId = paste0("Sel", i),
               label = paste0(dim_names[i]),
               min = slide.min,
               max = slide.max,
               value = c(slide.min, slide.max),
               sep=""
             )
           } else {
             selectInput(
               inputId = paste0("Sel", i),
               label = paste0(dim_names[i]),
               choices = choice.list,
               selected = choice.selected,
               multiple = choice.mult
             )
           }
        ) #End List

      })
    })


    #Data edits
    observe({
      req(input$PivCols)
      updateSelectInput(session, "dataMetricSeries",
                        choices = dim_names, selected = input$PivCols)
    })

    #Allow nested metric if metric turned on
    observe({
      if(input$dataMetric != "Values"){
        updateSelectInput(session, "PivRowNest",
                          choices = c("None", "*Metric*" = "Metric_calc", dim_names), selected = "None")
      }
    })


    ###
    # Edit table
    ###


    #1 - Filter
    dat1 <- reactive({
      req(input$PivCols, input$PivRows, input$PivRowNest,
          dat0())

      dat <- dat0()
      datF <- dat

      for(i in seq_along(dim_names)){
        # print(dim_names[i])
        get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check
        # print(get_input)

        #Is a numeric input?
        series.num <- names(dat)[sapply(dat[, dim_names], is.numeric)]

        #If no items are selected or the Select All is selected, show ALL items
        if(length(get_input) == 0 || all.elements %in% get_input){
          get_series <- unique(dat[, dim_names[i]]) %>% pull()
        }
        #For Numeric series
        else if(dim_names[i] %in% series.num){
          get_series <- as.numeric(get_input[1]:get_input[2])
        } else {
          get_series <- as.character(get_input)
        }
        # print(get_series)

        filter_criteria_T <- interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....

        #.... Do this
        datF <- datF %>%
          filter_(filter_criteria_T)

      } #End for

      return(as.data.frame(datF))
    })
    #2 - ???
    dat2 <- reactive({
      return(dat1())
    })
    #3 - Reduce ----
    dat3 <- reactive({
      req(input$PivCols, input$PivRows, input$PivRowNest,
          dat2())

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- if(input$PivRowNest %in% c("None", "Metric_calc")){NULL}else{input$PivRowNest}
      sel_metric <- if(input$dataMetric == "Values"){NULL}else{input$dataMetricSeries}

      dat <- dat2() %>%
        group_by_(.dots = as.list(c(sel_col, sel_row, sel_nest, sel_metric))) %>%
        summarize(value = sum(value, na.rm=TRUE)) %>%
        ungroup()

      return(dat)
    })
    #4 - Modes ----
    dat4 <- reactive({
      req(input$PivCols, input$PivRows, input$PivRowNest,
          input$dataMetricSeries,
          dat3())

      dat <- dat3() %>%
        mutate(value = value * as.numeric(input$oomValues))

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- input$PivRowNest
      sel_metric <- input$dataMetricSeries

      if(input$dataMetric == "Values"){
        dat <- dat %>%
          mutate(Metric_calc = "Values")
      }

      if(input$dataMetric == "Growth"){

        dat <- dat %>%
          group_by_(.dots = names(.)[!(names(.) %in% c(sel_metric, "value"))]) %>%
          mutate(Growth = (value / lag(value, 1) - 1)) %>%
          ungroup() %>%
          rename(Values = value) %>%
          gather(Metric_calc, value, Values, Growth)

      }

      if(input$dataMetric == "Shares"){

        dat <- dat %>%
          group_by_(.dots = names(.)[!(names(.) %in% c(sel_metric, "value"))]) %>%
          mutate(Shares = (value / sum(value))) %>%
          ungroup() %>%
          rename(Values = value) %>%
          gather(Metric_calc, value, Values, Shares)

      }

      datZ <- dat %>%
        do(
          if(sel_nest == "Metric_calc"){.}else{
            filter(., Metric_calc == input$dataMetric) %>%
              select(-Metric_calc)
          }
        ) %>%
        #This time, sum over the metric'd dimension
        group_by_(.dots = as.list(names(.)[names(.) %in% c(sel_col, sel_row, sel_nest)])) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        spread(sel_col, value) %>%
        rowwise() %>%
        mutate_if(is.character, funs(ifelse(nchar(.) > 12, substr(., 1, 12), .))) %>%
        ungroup() %>%
        as.data.frame()

      return(datZ)
    })

    # dat4 <- reactive({dat1()})

    ###
    # Prepare Data Table ----
    ###

    cols_numeric <- reactive({
      req(dat4(), input$PivCols)
      dat <- as.data.frame(dat0())
      return(as.character(names(dat4())[names(dat4()) %in% unique(dat[, input$PivCols])]))
    })
    cols_text <- reactive({
      req(dat4(), input$PivRows, input$PivRowNest)
      nms <- c(input$PivRows, input$PivRowNest)
      nms <- nms[nms != "None"]
      return(nms)
    })

    ###
    # Publish pivot ----
    ###
    # output$test <- renderText(cols_numeric())
    output$df_table <- DT::renderDataTable({
      dt <- datatable(as.data.frame(dat4()),
                      extensions = c('FixedColumns', 'Scroller'),
                      options = list(
                        #dom = 't',
                        scrollX = TRUE,
                        scrollY = 500,
                        scroller = TRUE, deferRender = TRUE,
                        rownames = FALSE,
                        fixedHeader = TRUE,
                        fixedColumns = list(leftColumns = ifelse(input$PivRowNest == "None", 2, 3))
                      )
                      ) %>%
        formatRound(columns = if(input$dataMetric == "Values" & input$PivRowNest != "Metric_calc"){cols_numeric()}else{1},
                    digits = input$decValues) %>%
        formatPercentage(columns = if(input$dataMetric != "Values" & input$PivRowNest != "Metric_calc"){cols_numeric()}else{1},
                    digits = input$decMetric) %>%
        formatRound(columns = if(input$PivRowNest == "Metric_calc"){cols_numeric()}else{1},
                    digits = input$decNested) %>%
        formatStyle(
          columns = if(input$PivRowNest == "Metric_calc"){cols_numeric()}else{1},
          valueColumns = if(input$PivRowNest == "Metric_calc"){"Metric_calc"}else{1},
          target = 'row',
          backgroundColor = styleEqual(c(input$dataMetric), c('#ffffcc')),
          color = styleEqual(c(input$dataMetric), c('#992020'))
        )

      return(dt)
      }
    )

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      #Insert script here to create new data frame of changed data
      stopApp()
    })

  }

  viewer <- dialogViewer(paste("RSPivot -", deparse(substitute(df))), width = 1400, height= 2000)
  runGadget(ui, server, viewer = viewer)

}

df<- GVAIndustry
# Run it
rspivot()

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!
