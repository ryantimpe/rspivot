#' View data frames as Shiny pivot tables
#'
#' View data frames as Shiny pivot tables. This is an alternative to \code{View()} to view data frames as summarized data.
#'
#' @param df A data frame flat file to be converted to pivot format. Data should be in "long" format, with a single column of values named \code{value}.
#' The function defaults to showing the most recent object made in R.
#' @param valueName Name of series in \code{df} containing data values. Defaults to "value".
#' Can also accept an array of strings containing the names of multiple value columns in the data frame.
#' @param initCols Specify the series to be displayed as columns. If blank, defaults to the right-most series in the data frame.
#' @param initRows Specify the series to be displayed as rows. If blank, defaults to the 2nd right-most series in the data frame.
#' @param initNest Specify the series to be displayed as nested rows. If blank, no nested rows are displayed.
#' @param initFilters Optional list of initial filter selections. Leave a series blank or use "Show All" to select all. Pass series names to \code{make.names()} to ensure correct use.
#' Alternatively, leave this blank on the intiial run, and use the Save Function feature after manually selecting filters.
#' @param initMetric Optional list of the initial data metrics to display.
#' Specify \code{list(metric = "c("Values", "Growth", "Shares"), series = "")} where \code{series} is a single dimension name.
#' Alternatively, leave this blank on the intiial run, and use the Save Function feature after manually setting the data metrics.
#' @return An RStudio dialog box will pop-up with a Shiny pivot table of the data.
#' @examples
#' \dontrun{
#' rspivot(GVAIndustry)
#'
#' GVAIndustry2 <- GVAIndustry %>%
#'      spread(Econ, value)
#' rspivot(GVAIndustry2, valueName = c("Employment", "GDP"))
#'
#' rspivot(GVAIndustry, initRows = "Country", initNest = "Industry")
#'}
#' @export

rspivot <- function(df=.Last.value, valueName = "value",
                    initCols = "", initRows = "", initNest = "",
                    initFilters = list(), initMetric = list(metric = "Values", series = "")) {

  library(shiny)
  library(miniUI)
  library(DT)
  library(tidyverse)
  library(lazyeval)

  ################ ----
  # Non-Reactive functions
  ################

  if(length(valueName) > 1){
    df0 <- df %>%
      gather(ValueNames, value, valueName) %>%
      mutate_if(is.factor, as.character)
  } else{
    df0 <- df %>%
      mutate_if(is.factor, as.character)

    names(df0)[names(df0) == valueName] <- "value"
  }

  names(df0) <- make.names(names(df0))
  dim_names <- names(df0)[!(names(df0) %in% c("value"))]
  all.elements <- "Show All"
  df.name <- deparse(substitute(df))

  #Move value to end
  df0a <- df0[, dim_names] %>%
    bind_cols(data.frame(value = df0$value, stringsAsFactors = FALSE))

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
            fluidRow(
              column(width = 3,
                     uiOutput("ui_update_data"),
                     p("Click here to update the data in the pivot table after filter changes.")
                     ),
              column(width = 3,
                selectInput("PivRows", label = "Rows",
                          choices = NULL , selected = NULL)#,
                #checkboxInput("PivRows_tot", label = "Show Row Totals", value=TRUE)
                ),
              column(width = 3,
                selectInput("PivRowNest", label = "Nested Rows",
                          choices = NULL , selected = NULL)#,
                #checkboxInput("PivRowNest_tot", label = "Show Nest Totals", value=FALSE)
                ),
              column(width = 3,
                selectInput("PivCols", label = "Columns",
                          choices = NULL , selected = NULL),
                checkboxInput("PivCols_tot", label = "Show Column Totals", value=TRUE)
                )
          ),
          fluidRow(
            column(
              width = 3,
              uiOutput("selects")
            ),
            column(
              width = 9,
              list(
                span(
                  textOutput("need.data.frame"),
                  style = "color:red; font-size:18pt"),
                DT::dataTableOutput("df_table"),
                hr(),
                strong("Save function call"),
                helpText("Running this function next time will resume the pivot in its current state."),
                verbatimTextOutput("stateSave"),
                fluidRow(
                  column(width = 3,
                         actionButton("stateClipboard", label = "Copy to Clipboard", icon = icon("clipboard"))
                         ),
                  column(width = 9,
                         radioButtons("stateWrite", label = strong("When 'Done', write function"),
                                      choices = c("Nowhere" = 0,
                                                  "At curser position" = 1,
                                                  "At end of document" = 2),
                                      selected = 0))
                ),
                br()
              )
            )

          ) #End Row

        )
      ), #End Pivot Tab
      miniTabPanel(
        title = "Plot",
        icon = icon("bar-chart"),
        fluidRow(
          column(width = 3,
                 radioButtons("PlotToggle", label = "Chart Type",
                              choices = c("Line" = "line", "Stacked Column" = "stacked", "Grouped Column" = "grouped"),
                              selected = "line", inline = TRUE)
                 )
        ),
        fluidRow(
          column(width = 12,
                 plotOutput("df_plot")
                 )
        )
      ), #End plot
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
                 hr(),
                 selectInput("oomValues", label = "Value Order of Magnitude",
                             choices = c("Units" = 1, "Thousands" = 10^-3, "Millions" = 10^-6, "Billions" = 10^-9),
                             selected = 1)
                 ),
          column(width = 3,
                 strong("Text"),
                 numericInput("textTruncate", label = "Truncate long labels", value = 15,
                               min = 5, max = 50, step = 5))
        )
      )
      )
  )


  ################ ----
  # Server
  ################

  server <- function(input, output, session) {

    ####
    # Initialize ----
    ####
    if(!is.data.frame(df0a)){
      message("Supplied object is not a data frame.")
      output$need.data.frame <- renderText({
        return("Supplied object is not a data frame.")
      })
    } else{
      dat0 <- reactive({
        dat <- df0a
        return(dat)
      })
    }


    ###
    #Create UI menus ----
    ###

    #Col/Row selection
    updateSelectInput(session, "PivCols",
                      choices = dim_names,
                      selected = (if(initCols == ""){tail(dim_names, 1)[1]}else{make.names(initCols)})
                      )

    updateSelectInput(session, "PivRows",
                      choices = dim_names,
                      selected = (if(initRows == ""){tail(dim_names, 2)[1]}else{make.names(initRows)})
                      )

    updateSelectInput(session, "PivRowNest",
                      choices = c("None", "*Metric*" = "Metric_calc", dim_names),
                      selected = (if(initNest == ""){"None"}else{make.names(initNest)})
                      )

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

        #Is an initial input supplied
        if(!is.null(initFilters[[dim_names[i]]])){
          #Drop inputs that are wrong
          choice.input <- initFilters[[dim_names[i]]]
          # choice.input <- choice.input[choice.input == all.elements | (choice.input %in% choice.list[2])]
          choice.selected <- choice.input
        }

        #Number input
        if(dim_names[i] %in% series.num){
          slide.min <- floor(min(dat[, dim_names[i]]))
          slide.max <- ceiling(max(dat[, dim_names[i]]))

          #Numeric with input
          if(!is.null(initFilters[[dim_names[i]]])){
            choice.min <- min(as.numeric(initFilters[[dim_names[i]]]))
            choice.max <- max(as.numeric(initFilters[[dim_names[i]]]))
          }
          #Numeric without input
          else {
            choice.min <- slide.min
            choice.max <- slide.max
          }
        }

        # Build the Menu for each dimension
        list(
           #Filter for numeric dimension
           if(dim_names[i] %in% series.num){
             sliderInput(
               inputId = paste0("Sel", i),
               label = paste0(dim_names[i]),
               min = slide.min, max = slide.max,
               step = 1,
               value = c(choice.min, choice.max),
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

    ##
    #Data edits
    ##
    observe({
      req(input$PivCols)
      updateSelectInput(session, "dataMetricSeries",
                        choices = dim_names, selected = input$PivCols)

      if(initMetric$metric != "Values"){
        updateSelectInput(session, "dataMetric",
                          selected = initMetric$metric)
        updateSelectInput(session, "dataMetricSeries",
                          selected = initMetric$series)
      }
    })

    # ##
    # #Allow nested metric if metric turned on
    # ##
    #
    # current_nest <- reactiveVal(NULL)
    # # observeEvent(input$PivRowNest, {
    # #   current_nest(input$PivRowNest)
    # # })
    #
    # observe({
    #   current_nest(input$PivRowNest)
    #   if(input$dataMetric != "Values"){
    #     updateSelectInput(session, "PivRowNest",
    #                       choices = c("None", "*Metric*" = "Metric_calc", dim_names),
    #                       selected = current_nest())
    #   }
    # })

    ###
    # Action Button
    ###
    #Want to build the action button AFTER the menus are initialized
    output$ui_update_data <- renderUI({
      actionButton("update_data", label = "Refresh Data", icon = shiny::icon("refresh"),
                   style = "background-color:#4040FF; color:#ffffff;")
    })

    ###
    # Edit table
    ###

    #1 - Filter
    # Only update filters when clicked
    dat1 <- eventReactive(input$update_data,
                          ignoreNULL = FALSE, {
      #req(dat0())

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- if(input$PivRowNest %in% c("None", "Metric_calc")){NULL}else{input$PivRowNest}

      dat <- dat0()
      datF <- dat

      for(i in seq_along(dim_names)){
        get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check

        #Is a numeric input?
        series.num <- names(dat)[sapply(dat[, dim_names], is.numeric)]

        #If no items are selected or the Select All is selected, show ALL items
        if(length(get_input) == 0 || all.elements %in% get_input){
          get_series <- as.tibble(dat[, dim_names[i]]) %>% distinct() %>% pull()

          filter_criteria_T <- interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
        }
        #For Numeric series
        else if(dim_names[i] %in% series.num){
          get_series <- as.numeric(get_input)
          filter_criteria_T <- interp( ~ (which_column >= get_series[1]) & (which_column <= get_series[2]),
                                       which_column = as.name(dim_names[i])) #If a Filter is selected....
        } else {
          get_series <- as.character(get_input)
          filter_criteria_T <- interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
        }

        #.... Do this
        datF <- datF %>%
          filter_(filter_criteria_T)

      } #End for

      #After filtering, add leading space to each element...
      # This helps to push all calculated fields to the bottom
      datF2 <- datF %>%
        mutate_at(vars(c(sel_row, sel_col, sel_nest)), funs(paste0(" ", .)))

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

      #Col / Row sums
      if(input$PivCols_tot){
        sel_row2 <- if(sel_row == sel_col){NULL}else{sel_row}
        sel_nest2 <- if(!is.null(sel_nest)){if(sel_nest == sel_col){NULL}else{sel_nest}}

        dat <- dat %>%
          bind_rows(
            group_by_(., .dots = as.list(names(.)[names(.) %in% c(sel_row2, sel_nest2)])) %>%
              summarize(value = sum(value)) %>%
              ungroup()
          ) %>%
          mutate_at(vars(sel_col), funs(ifelse(is.na(.), "*Total*", .)))
      }
      # if(input$PivRows_tot){
      #   dat <- dat %>%
      #     bind_rows(
      #       group_by_(., .dots = as.list(names(.)[names(.) %in% c(sel_col, sel_nest)])) %>%
      #         summarize(value = sum(value)) %>%
      #         ungroup()
      #     ) %>%
      #     mutate_at(vars(sel_row), funs(ifelse(is.na(.), "*Total*", .)))
      # }

      # if(input$PivRowNest_tot & !(sel_nest %in% c("None", "Metric_calc"))){
      #   dat <- dat %>%
      #     bind_rows(
      #       group_by_(., .dots = as.list(names(.)[names(.) %in% c(sel_col, sel_row)])) %>%
      #         summarize(value = sum(value)) %>%
      #         ungroup()
      #     ) %>%
      #     mutate_at(vars(sel_nest), funs(ifelse(is.na(.), "*Total*", .)))
      # }

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

      sel_truncate <- input$textTruncate

      if(input$dataMetric == "Values"){
        dat <- dat %>%
          mutate(Metric_calc = "Values")
      }

      if(input$dataMetric == "Growth"){

        dat <- dat %>%
          group_by_(.dots = names(.)[!(names(.) %in% c(sel_metric, "value"))]) %>%
          mutate(Growth = (value / lag(value, 1) - 1) *
                   (if(sel_nest =="Metric_calc"){100}else{1})
                    ) %>%
          ungroup() %>%
          rename(Values = value) %>%
          gather(Metric_calc, value, Values, Growth)

      }

      if(input$dataMetric == "Shares"){

        dat <- dat %>%
          group_by_(.dots = names(.)[!(names(.) %in% c(sel_metric, "value"))]) %>%
          mutate(Shares = (value / sum(value)) *
                   (if(sel_nest =="Metric_calc"){100}else{1})
                 ) %>%
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
        mutate_at(vars(sel_row), as.character()) %>%
        do(
          if(sel_nest != "None"){
            mutate_at(., vars(sel_nest), as.character())
          } else {.}
        ) %>%
        mutate_at(vars(sel_col), as.character()) %>%  #If its numeric, needs to be char before spreading
        spread(sel_col, value) %>%
        #Truncate Text
        rowwise() %>%
        mutate_if(is.character, funs(ifelse(nchar(.) > sel_truncate, substr(., 1, sel_truncate), .))) %>%
        ungroup() %>%
        as.data.frame()

      #Move Total to end
      # if(input$PivRows_tot == TRUE){
      #   filter_criteria_F <- interp( ~ which_column != "*Total*", which_column = as.name(sel_row))
      #   filter_criteria_T <- interp( ~ which_column == "*Total*", which_column = as.name(sel_row))
      #
      #   datZ <- datZ %>%
      #     filter_(filter_criteria_F) %>%
      #     bind_rows(datZ %>% filter_(filter_criteria_T))
      # }
      if(input$PivCols_tot == TRUE){
        datZ <- datZ[, names(datZ)[names(datZ) != "*Total*"]] %>%
          bind_cols(tibble(`*Total*` = datZ$`*Total*`))
      }

      return(datZ)
    })

    # dat4 <- reactive({dat1()})

    ###
    # Prepare Data Table ----
    ###

    cols_numeric <- reactive({
      req(dat4(), input$PivCols)
      dat <- as.data.frame(dat0())
      return(as.character(names(dat4())[names(dat4()) %in% c(unique(dat[, input$PivCols]), "*Total*")]))
    })

    ###
    # Publish pivot ----
    ###
    # output$test <- renderText(cols_numeric())
    output$df_table <- DT::renderDataTable({
      dat <- dat4()

      dt <- datatable(as.data.frame(dat),
                      extensions = c('FixedColumns', 'Scroller'),
                      # extensions = c('FixedColumns', 'Scroller', 'ColReorder', 'RowReorder'),
                      options = list(
                        #dom = 't',
                        scrollX = TRUE,
                        scrollY = 500,
                        scroller = TRUE, deferRender = TRUE,
                        rownames = FALSE,
                        fixedHeader = TRUE,
                        fixedColumns = list(leftColumns = ifelse(input$PivRowNest == "None", 2, 3))#,
                        #colReorder = list(realtime = FALSE),
                        #rowReorder = TRUE
                      )
                      ) %>%
        formatCurrency(columns = if(input$dataMetric == "Values" & input$PivRowNest != "Metric_calc"){cols_numeric()}else{99},
                    currency = "", digits = input$decValues) %>%
        formatPercentage(columns = if(input$dataMetric != "Values" & input$PivRowNest != "Metric_calc"){cols_numeric()}else{99},
                    digits = input$decMetric) %>%
        formatCurrency(columns = if(input$PivRowNest == "Metric_calc"){cols_numeric()}else{99},
                       currency = "", digits = input$decMetric)%>%
        formatStyle(
          columns = if(input$PivRowNest == "Metric_calc"){cols_numeric()}else{-1},
          valueColumns = if(input$PivRowNest == "Metric_calc"){"Metric_calc"}else{-1},
          target = 'row',
          backgroundColor = styleEqual(c("Growth", "Shares"), c('#ffffcc', '#eeeebb')),
          color = styleEqual(c("Growth", "Shares"), c('#992020', '#992020'))
        ) %>%
        formatStyle(
          columns = if(input$PivCols_tot == TRUE){"*Total*"}else{999},
          target = 'cell',
          backgroundColor = "#ffcccc"
        )
        # formatStyle(
        #   columns = cols_numeric(),
        #   valueColumns = input$PivRows,
        #   target = 'row',
        #   backgroundColor = styleEqual(c("*Total*"), c('#ccffff')),
        #   color = styleEqual(c(input$dataMetric), c('black'))
        # )

      return(dt)
    })

    ###
    # Graph pivot
    ###

    output$df_plot <- renderPlot({

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- input$PivRowNest

      sel_type <- input$PlotToggle

      dat0 <- dat4()

      if(sel_nest == "None"){
        dat <- as.data.frame(dat0) %>%
          gather(dim_x, value, 2:ncol(.)) %>%
          filter(dim_x != "*Total*")
      } else {
        dat <- as.data.frame(dat0) %>%
          gather(dim_x, value, 3:ncol(.)) %>%
          filter(dim_x != "*Total*")

        names(dat)[names(dat) == sel_nest] <- "dim_z"
      }

      names(dat)[names(dat) == sel_row] <- "dim_y"

      gg <- ggplot(data = dat, aes(x = dim_x, y = value, group = dim_y))

      #How to display data
      if(sel_type == "line"){
        gg <- gg + geom_line(aes(color = dim_y), size = 1.1)
      } else if(sel_type == "stacked") {
        gg <- gg + geom_col(aes(fill = dim_y), color = "black")
      } else {
        gg <- gg + geom_col(aes(fill = dim_y), color = "black", position = "dodge")
      }

      #Nested?
      if(sel_nest != "None"){
        gg <- gg + facet_wrap(~dim_z, scales = "free")
      }

      gg <- gg +
        ggtitle(sel_row) +
        xlab(sel_col) +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
          strip.background = element_rect(fill = "#00436b"),
          strip.text = element_text(color = "white", face = "bold", size = 12),
          plot.title = element_text(color = "#00436b", face = "bold", size = 16),
          plot.subtitle = element_text(color = "#00436b", size = 14),
          plot.caption = element_text(size = 11)
        )
      return(gg)
    })



    #Also save each filter state
    stateSave_Text <- reactive({
      # req(dat0())

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- if(input$PivRowNest %in% c("None")){"None"}else{input$PivRowNest}

      dat <- dat0()

      ##
      # Row/Cols
      ##
      state_rowcol <- paste(paste0('initCols = "', sel_col, '"'),
                            paste0('initRows = "', sel_row, '"'),
                            paste0('initNest = "', sel_nest, '"'),
                            sep = ', ')

      ##
      # Filters
      ##
      filterList <- c()

      for(i in seq_along(dim_names)){
        get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check

        #Is a numeric input?
        series.num <- names(dat)[sapply(dat[, dim_names], is.numeric)]

        #If no items are selected or the Select All is selected, show ALL items
        if(length(get_input) == 0 || all.elements %in% get_input){
          #get_series <- all.elements
          next()
        }
        #For Numeric series
        else if(dim_names[i] %in% series.num){
          get_series <- as.numeric(get_input)
        } else {
          get_series <- as.character(get_input)
        }

        if(dim_names[i] %in% series.num){ #Don't include single quotes for numeric
          filterList <- c(filterList,
                          paste0(dim_names[i], ' = c(', paste(get_series, collapse = ', '), ')'))
        } else {
          filterList <- c(filterList,
                          paste0(dim_names[i], ' = c("', paste(get_series, collapse = '", "'), '")'))
        }

      } #End for

      state_filter <- paste0("initFilters = list(", paste(filterList, collapse = ", "), ")")

      ##
      # Metric
      ##
      if(input$dataMetric == "Values"){
        state_metric <- NULL
      } else {
        state_metric <- paste0(', initMetric = list(metric = "', input$dataMetric, '", ',
                               'series = "', input$dataMetricSeries, '")')
      }

      ##
      #Combine
      ##

      state_all <- paste0("rspivot(", df.name, ", ",
                          state_rowcol, ", ",
                          state_filter,
                          state_metric,
                          ")")

      # writeClipboard(state_all)
      return(state_all)

    })
    output$stateSave <- renderText(stateSave_Text())
    observeEvent(input$stateClipboard, {
      writeClipboard(stateSave_Text())
    })

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      #Save function
      if(input$stateWrite == 1){ #At curser
        rstudioapi::insertText(stateSave_Text())
      } else if(input$stateWrite == 2){ #At end
        rstudioapi::insertText(Inf, stateSave_Text())
      }

      stopApp()
    })

  } #End Server

  viewer <- dialogViewer(paste("RSPivot -", deparse(substitute(df))), width = 1400, height= 2000)
  runGadget(ui, server, viewer = viewer)

}

# GVAIndustry2 <- GVAIndustry %>%
#   spread(Econ, value)
#
# df<- GVAIndustry
# # Run it
# GV2 <- GVAIndustry %>%
#   rename(`Country (15)` = Country)
# rspivot(GV2, initRows = "Country (15)")
# rspivot(GVAIndustry, initRows = "Country")
# rspivot(GVAIndustry2, valueName = c("Employment", "GDP"))

# load("Z:/Shared/P-Drive/Huawei/2016 H2 (Phase 1)/03 WORK (ANALYSIS)/Centralized Integration/_Model Output/3_IntegrateFile_Start")
# rspivot(IntegrateFile.Start, valueName="value", initCols = "Year", initRows = "Region")


# econ <- read_delim("F:/Intel/Intel MA/2017-10 October/DATA/Econ 17Q3 - Full Comp File - PASS 4 - IFW_Recast of E17Q3P1.txt",
#                                                                     "\t", escape_double = FALSE, trim_ws = TRUE)
# econ2 <- econ %>%
#   gather(Year, value, 6:ncol(.)) %>%
#   mutate(Year = as.numeric(Year)) %>%
#   filter(Year %in% 2010:2020)
#
# rspivot(econ2, initRows = "SCENARIO")
#
#
# rspivot(econ2, initCols = "Year", initRows = "SCENARIO", initNest = "None",
#         initFilters = list(MEASURE = c("Real"), ECON = c("GDP"), QUARTER = c("Q1", "Q2", "Q3", "Q4"), Year = c(2014, 2020)))

