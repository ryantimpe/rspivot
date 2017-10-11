library(shiny)
library(miniUI)
library(DT)
library(tidyverse)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'rspivot()'.
rspivot <- function(df=.Last.value) {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("RS Pivot"),
    miniTabstripPanel(
      miniTabPanel(
        title = "Pivot",
        icon = icon("table"),
        fillRow(
          padding = 5,
          flex = c(1, 4),
          list(
            selectInput("PivCols", label = "Columns",
                        choices = NULL , selected = NULL),
            selectInput("PivRows", label = "Rows",
                        choices = NULL , selected = NULL),
            selectInput("PivRowNest", label = "Nested Rows",
                        choices = NULL , selected = NULL),
            hr(),
            selectInput("Dim1", label = "First Dim",
                        choices = letters[1:11], selected = 1)
          ),
          list(
            textOutput("test"),
            DT::dataTableOutput("df_table")
          )
        ) #End Row
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
                 )
        )
      )
      )
  )

  dim_names <- names(df)[!(names(df) %in% c("value"))]

  server <- function(input, output, session) {

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

    #Data edits
    observe({
      req(input$PivCols)
      updateSelectInput(session, "dataMetricSeries",
                        choices = dim_names, selected = input$PivCols)
    })


    ###
    # Edit table
    ###
    dat0 <- reactive({df})

    #1 - Filter
    dat1 <- reactive({
      return(dat0())
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
      sel_nest <- if(input$PivRowNest == "None"){NULL}else{input$PivRowNest}
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

      dat <- dat3()

      sel_col <- input$PivCols
      sel_row <- input$PivRows
      sel_nest <- if(input$PivRowNest == "None"){NULL}else{input$PivRowNest}
      sel_metric <- input$dataMetricSeries

      if(input$dataMetric == "Growth"){

        dat <- dat %>%
          group_by_(.dots = names(.)[!(names(.) %in% c(sel_metric, "value"))]) %>%
          mutate(value = (value / lag(value, 1) - 1)) %>%
          ungroup()

      }

      datZ <- dat %>%
        #This time, sum over the metric'd dimension
        group_by_(.dots = as.list(c(sel_col, sel_row, sel_nest))) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        spread(sel_col, value) %>%
        rowwise() %>%
        mutate_if(is.character, funs(ifelse(nchar(.) > 12, substr(., 1, 12), .))) %>%
        ungroup() %>%
        as.data.frame()

      return(datZ)
    })

    ###
    # Prepare Data Table
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
        # formatRound(columns = cols_numeric(), 0) %>%
        formatRound(columns = if(input$dataMetric == "Values"){cols_numeric()}else{1},
                    digits = 0) %>%
        formatPercentage(columns = if(input$dataMetric != "Values"){cols_numeric()}else{1},
                    digits = 1)



      return(dt)
      }
    )

    # Listen for 'done' events. When we're finished, we'll
    observeEvent(input$done, {
      #Insert script here to create new data frame of changed data
      stopApp()
    })

  }

  viewer <- dialogViewer(paste("RSPivot -", deparse(substitute(df))), width = 1400, height= 1400)
  runGadget(ui, server, viewer = viewer)

}

df<- GVAIndustry
# Run it
rspivot()

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!
