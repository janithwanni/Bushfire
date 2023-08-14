# User interface ----
ui <- fluidPage(
  navbarPage("VICfire",
    id = "main",
    tabPanel(
      "Historical fire map",
      fluidRow(
        column(8,
          align = "left",
          sidebarLayout(
            sidebarPanel(
              helpText("Historical locations on fires, and ignition causes, in Victoria over 2000-2019."),
              sliderInput("year",
                label = "Choose year:",
                value = c(2010, 2017),
                min = min(mydata$year),
                max = max(mydata$year),
                step = 1,
                sep = ""
              ),
              checkboxGroupButtons("month",
                label = "Choose month:",
                choices = c(
                  "Jan", "Feb", "Mar",
                  "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep",
                  "Oct", "Nov", "Dec"
                ),
                individual = TRUE,
                justified = FALSE,
                selected = c("Nov", "Dec", "Jan"),
                width = "100%"
              ),
              checkboxGroupInput("reason",
                label = "Choose reason:",
                choices = levels(factor(mydata$new_cause)),
                selected = c("lightning", "arson", "accident")
              ),
              actionButton("showd", "Show density plot"),
              actionButton("cleard", "Clear density plot")
            ),
            mainPanel(
              leafletOutput(outputId = "map", height = 587)
            )
          )
        ),
        column(
          4,
          conditionalPanel(
            # The conditionals are set based on
            # whether coordinates are being clicked on.
            # TODO: rename condition1 to a more descriptive name
            condition = "output.condition1 == 0",
            br(),
            tags$h4("About", style = "color:blue"),
            p(
              "This Shiny App helps visualise fires in Victoria for last two decades.
              YAfter choose the year, month, and ignition reason. The fires match these condition will automatically shown on the map.
              Due to the limitation of the package, the density plot cannot be refreshed automatically. Each time you change the conditions, you have to clear and re-plot the densitiy plot.
              By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below."
            )
          ),
          conditionalPanel(
            condition = "output.condition1 == 1",
            # TODO: rename condition1 to a more descriptive name
            tabsetPanel(
              tabPanel(
                tags$em("Percentage", style = "font-size:100%"),
                # TODO: rename ids to more descriptive name
                plotlyOutput("p1", height = 300),
                # TODO: rename ids to more descriptive name
                plotlyOutput("p2", height = 200)
              ),
              tabPanel(
                tags$em("Rainfall", style = "font-size:100%"),
                tags$hr(style = "border-color:  #d27979;"),
                plotlyOutput("rain")
              ),
              tabPanel(
                tags$em("Temperature", style = "font-size:100%"),
                tags$hr(style = "border-color:  #ffc266;"),
                plotlyOutput("temp")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "2019-2020 Predicted causes",
      fluidRow(
        column(8,
          align = "left",
          sidebarLayout(
            sidebarPanel(
              helpText("Predicted causes for fire ignition points over Oct 2019 through Mar 2020."),
              checkboxGroupButtons("month1",
                label = "Choose month:",
                choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                individual = TRUE, justified = FALSE, selected = c("Dec"),
                width = "100%"
              ),
              checkboxGroupInput("reason1",
                label = "Choose reason:",
                choices = levels(factor(prediction$new_cause)),
                selected = c("arson", "lightning", "accident")
              )
            ),
            mainPanel(leafletOutput(outputId = "map2", height = 587))
          )
        ),
        column(
          4,
          conditionalPanel(
            # TODO: rename condition2 to a more descriptive name
            condition = "output.condition2 == 0",
            br(),
            tags$h4("About this page", style = "color:blue"),
            p(
              "The fires shown on this page happened 2019.10-2020.4 located by Himawari 8. The causes of such fires are yet to be investigated.
              All ignition reasons are predicted based on our model.
              By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below."
            )
          ),
          conditionalPanel(
            # TODO: rename condition2 to a more descriptive name
            condition = "output.condition2 == 1",
            tabsetPanel(
              tabPanel(
                tags$em("Rainfall", style = "font-size:100%"),
                tags$hr(style = "border-color:  #d27979;"),
                plotlyOutput("rain1")
              ),
              tabPanel(
                tags$em("Temperature", style = "font-size:100%"),
                tags$hr(style = "border-color:  #ffc266;"),
                plotlyOutput("temp1")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Fire risk map",
      fluidRow(
        column(8,
          align = "left",
          sidebarLayout(
            sidebarPanel(
              helpText("Predicted fire probability maps"),
              radioButtons("month2",
                label = "Choose month:",
                choices = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                selected = c("Oct")
              ),
              radioButtons("reason2",
                label = "Choose reason:",
                choices = levels(factor(prediction$new_cause)),
                selected = c("arson")
              )
            ),
            mainPanel(leafletOutput(outputId = "map3", height = 587))
          )
        )
      )
    ),
    tabPanel("Data", DT::dataTableOutput("data")),
    tabPanel("Information", includeMarkdown("readme.md"))
  )
)
