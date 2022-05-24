#' Shiny App to Geocode a single address
#'
#' @rdname census_geocode_components
#'
#' @export
app_census_geocode_components <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel(
      "Geocode a single address"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("street", "Street"),
        shiny::textInput("city", "City"),
        shiny::textInput("state", "State"),
        shiny::textInput("postal_code", "Postal Code"),
        shiny::selectInput("vintage", "Vintage", seq(2010, 2000, -10)),
        shiny::tags$br(),
        shiny::actionButton("submit", "Submit"),
        shiny::actionButton("clear", "Clear"),
        width = 2
      ),
      shiny::mainPanel(
        shiny::tableOutput("results")
      )
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    r <- shiny::eventReactive(input$submit, {
      driver::census_geocode_components(
        input$street,
        input$city,
        input$state,
        input$postal_code,
        input$vintage
      ) %>%
        dplyr::arrange(dplyr::desc(.data[["accuracy"]])) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        tidyr::pivot_longer(
          tidyselect::everything(),
          "Field",
          values_to = "Value"
        )
    })

    output$results <- shiny::renderTable({
      r()
    })

    shiny::observeEvent(input$clear, {
      for (field in c("street", "city", "state", "postal_code")) {
        session$sendInputMessage(field, list(value = ""))
      }
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}



#' Shiny App to Lookup Urban and Rural Data for a Given Census Block Group
#'
#' @rdname census_block_group_population
#'
#' @export
app_census_block_group_population <- function() { # nolint
  ui <- shiny::fluidPage(
    shiny::titlePanel(
      "Lookup Urban and Rural Data for a Given Census Block Group"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("fips_code_12", "12-Digit FIPS Code"),
        shiny::selectInput("vintage", "Vintage", seq(2010, 2000, -10)),
        width = 2
      ),
      shiny::mainPanel(
        shiny::tableOutput("results")
      )
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    output$results <- shiny::renderTable({
      driver::census_block_group_population(
        input$fips_code_12,
        vintage = input$vintage
      ) %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        tidyr::pivot_longer(
          tidyselect::everything(),
          "Field",
          values_to = "Value"
        )
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}



#' Shiny App to Lookup Urban and Rural Data for a Given 5-Digit Zip Code
#'
#' @rdname census_zipcode_population
#'
#' @export
app_census_zipcode_population <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel(
      "Lookup Urban and Rural Data for a Given 5-Digit Zip Code"
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("zip_code_5", "5-Digit Zip Code"),
        shiny::selectInput("vintage", "Vintage", seq(2010, 2000, -10)),
        width = 2
      ),
      shiny::mainPanel(
        shiny::tableOutput("results")
      )
    )
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    output$results <- shiny::renderTable({
      driver::census_zipcode_population(
        input$zip_code_5,
        vintage = input$vintage
      ) %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        tidyr::pivot_longer(
          tidyselect::everything(),
          "Field",
          values_to = "Value"
        )
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
