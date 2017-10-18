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
#' @return An RStudio dialog box will pop-up with a Shiny pivot table of the data.
#' @examples
#' rspivot(GVAIndustry)
#'
#' GVAIndustry2 <- GVAIndustry %>%
#'      spread(Econ, value)
#' rspivot(GVAIndustry2, valueName = c("Employment", "GDP"))
#'
#' rspivot(GVAIndustry, initRows = "Country", initNest = "Industry")
#'
#' @export

rspivot <- function(df=.Last.value, valueName = "value",
                    initCols = "", initRows = "", initNest = "") {

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

  dim_names <- names(df0)[!(names(df0) %in% c("value"))]
  all.elements <- "Show All"

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
              column(width = 3),
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
                  style = "color:red; font-size:20pt"),
                DT::dataTableOutput("df_table")
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
                 )
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
                      choices = dim_names, selected = (if(initCols == ""){tail(dim_names, 1)[1]}else{initCols})
                      )

    updateSelectInput(session, "PivRows",
                      choices = dim_names, selected = (if(initRows == ""){tail(dim_names, 2)[1]}else{initRows})
                      )

    updateSelectInput(session, "PivRowNest",
                      choices = c("None", dim_names), selected = (if(initNest == ""){"None"}else{initNest}))

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
               min = floor(slide.min), max = ceiling(slide.max),
               step = 1,
               value = c(floor(slide.min), ceiling(slide.max)),
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
      req(dat0())

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
        dat <- dat %>%
          bind_rows(
            group_by_(., .dots = as.list(names(.)[names(.) %in% c(sel_row, sel_nest)])) %>%
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
        rowwise() %>%
        mutate_if(is.character, funs(ifelse(nchar(.) > 12, substr(., 1, 12), .))) %>%
        ungroup() %>%
        as.data.frame()

      #Move Total to end
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
          backgroundColor = styleEqual(c(input$dataMetric), c('#ffffcc')),
          color = styleEqual(c(input$dataMetric), c('#992020'))
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

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      #Insert script here to create new data frame of changed data
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
# rspivot(GVAIndustry, initRows = "Country")
# rspivot(GVAIndustry2, valueName = c("Employment", "GDP"))

# load("Z:/Shared/P-Drive/Huawei/2016 H2 (Phase 1)/03 WORK (ANALYSIS)/Centralized Integration/_Model Output/3_IntegrateFile_Start")
# rspivot(IntegrateFile.Start, valueName="value", initCols = "Year", initRows = "Region")

