header <- dashboardHeader(title = "World University")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Home",
      tabName = "home",
      icon = icon("h-square")
    ),
    menuItem(
      text = "Student",
      tabName = "student",
      icon = icon("graduation-cap")
    ),
    menuItem(
      text = "Map",
      tabName = "map",
      icon = icon("globe")
    ),
    menuItem(
      text = "Dataset",
      tabName = "data",
      icon = icon("book")
    )
  )
)

body <- dashboardBody(
  customTheme,
  tabItems(
    tabItem(
      tabName = "home",
      fluidPage(
        box(width = 12,
            includeHTML("penjelasan.html")
        )
      ),
      fluidPage(
        box(width = 9,
            solidHeader = TRUE,
            title = "Top University",
            plotlyOutput(outputId = "topskor")
        ),
        box(width = 3,
            height = 140,
            title = "Input",
            status = "primary",
            solidHeader = TRUE,
            sliderInput(inputId = "numbertopskor", 
                        label = "Jumlah Universitas", 
                        min = 3,
                        max = 30, 
                        value = 20)
        ),
        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
        valueBox(width = 3,
                 value = tags$p("31.6", style = "font-size: 100%;"), 
                 subtitle = "Rerata Skor Akhir", 
                 icon = icon("wave-square"),
                 color = "maroon"),
        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
        valueBox(width = 3,
                 value = tags$p("1396", style = "font-size: 100%;"), 
                 subtitle = "Jumlah Universitas", 
                 icon = icon("city"),
                 color = "red")
      ),
      fluidPage(
        box(width = 12,
            solidHeader = TRUE,
            title = "Top Country",
            plotlyOutput(outputId = "topcountry")
        )
      )
    ),
    tabItem(
      tabName = "student",
      fluidPage(
        box(width = 8,
            solidHeader = TRUE,
            title = "Correlation",
            plotlyOutput(outputId = "corrskor")
        ),
        box(width = 4,
            title = "Input",
            status = "warning",
            height = 300,
            solidHeader = TRUE,
            radioButtons(inputId = "skor", 
                         label = "Pilih X-axis", 
                         choices = univ_rank %>% 
                           select(Teaching, Research, Citations, Industry_Income, International_Outlook) %>% 
                           colnames() %>% 
                           str_replace_all(pattern = "_", replacement = " ")),
            sliderInput(inputId = "numberuniversity", 
                        label = "Jumlah Universitas", 
                        min = 5,
                        max = 30, 
                        value = 20)
        ),
        valueBoxOutput(width = 4,
                       "CorrBox")
    ),
    fluidPage(
      box(width = 12,
          solidHeader = TRUE,
          title = "Jumlah Mahasiswa",
          plotlyOutput(outputId = "numberstudent", height = "500px")
      )
    ),
    fluidPage(
      tabBox(width = 12,
             title = tags$b("Comparison Plot"),
             id = "tabset1",
             side = "left",
             tabPanel(tags$b("Gender Mahasiswa"), 
                      plotlyOutput("gender", height = "500px")
             ),
             tabPanel(tags$b("Mahasiswa per Staf"), 
                      plotlyOutput("studentstaf", height = "500px")
             )    
      )
    )
    ),
    tabItem(
      tabName = "map",
      fluidPage(
        box(
          width = 9,
          title = tags$b("Peta Lokasi Universitas"),
          solidHeader = TRUE,
          leafletOutput(outputId = "univmap",
                        height = 550)
        ),
        box(width = 3,
            title = tags$b("Input"),
            height = 610,
            status = "primary",
            solidHeader = TRUE,
            tags$style("input[type=checkbox] {
                    transform: scale(1.8);
           }"),
            tags$style("#mapinput {
                       font-size:16px;
                       height:16px;
                       }"),
            checkboxGroupInput(inputId = "mapinput",
                               label = "Pilih Kategori Peringkat",
                               choices = selectcategory,
                               selected = selectcategory[1:5])
      )
    )),
    tabItem(
      tabName = "data",
      h2(tags$b("World University Data 2020")),
      dataTableOutput("data_univ")
    )
  )
)

dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar
)
