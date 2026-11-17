# =========================================================
# ПРОЕКТ: "Учебный портал" 🎓 (Мега-версия 7.0)
# =========================================================

library(shiny)
library(shinydashboard)
library(bslib)
library(plotly)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(readxl)
library(base64enc)
library(digest)
library(dplyr)

# ---------------- UI ----------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$a(href = "#", "🎓 Учебный портал"),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📐 Математика", tabName = "math", icon = icon("calculator"),
               menuSubItem("Квадратное уравнение", tabName = "quad"),
               menuSubItem("Калькулятор", tabName = "calc")
      ),
      menuItem("⚛️ Физика", tabName = "physics", icon = icon("atom"),
               menuSubItem("Бросок под углом", tabName = "projectile")
      ),
      menuItem("🧪 Химия", tabName = "chem", icon = icon("flask"),
               menuSubItem("Молярная масса", tabName = "molar")
      ),
      menuItem("🔐 Криптография", tabName = "crypto", icon = icon("lock"),
               menuSubItem("Шифры и кодирование", tabName = "crypto_tools")
      ),
      menuItem("🎮 Игры", tabName = "games", icon = icon("gamepad"),
               menuSubItem("Угадай число", tabName = "guess")
      ),
      menuItem("📊 Анализ данных", tabName = "stats", icon = icon("chart-pie"),
               menuSubItem("Загрузка данных", tabName = "upload"),
               menuSubItem("Статистики", tabName = "desc"),
               menuSubItem("Графики", tabName = "hist"),
               menuSubItem("Генератор данных", tabName = "gen")
      ),
      menuItem("💻 Программирование", tabName = "prog", icon = icon("laptop-code"),
               menuSubItem("Визуализация сортировки", tabName = "sorting")
      ),
      menuItem("💼 Финансы", tabName = "fin", icon = icon("wallet"),
               menuSubItem("Сложный процент", tabName = "interest"),
               menuSubItem("Инфляция", tabName = "inflation"),
               menuSubItem("Кредит", tabName = "credit")
      )
    )
  ),
  
  dashboardBody(
    theme = bs_theme(
      version = 5, bg = "#F4F9FB", fg = "#0B3C5D", primary = "#2EC4B6", secondary = "#0B3C5D",
      base_font = font_google("Montserrat"), heading_font = font_google("Montserrat")
    ),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #F4F9FB !important; }
        .box { border-radius: 12px !important; box-shadow: 0 4px 15px rgba(11, 60, 93, 0.05) !important; border-top: 4px solid #2EC4B6 !important; background: #FFFFFF; padding: 15px; margin-bottom: 20px; }
        .box-title { font-size: 18px; font-weight: 700; color: #0B3C5D; }
        .skin-blue .main-header .logo { background: #0B3C5D !important; font-weight: bold; }
        .skin-blue .main-header .navbar { background: #0B3C5D !important; }
        .skin-blue .main-sidebar { background: #FFFFFF !important; box-shadow: 2px 0 10px rgba(0,0,0,0.05); }
        .sidebar-menu > li > a { color: #4A6572 !important; font-weight: 600; border-radius: 8px; margin: 5px 10px; }
        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a { background: #EAF6FA !important; color: #2EC4B6 !important; }
        .btn { background-color: #2EC4B6 !important; color: white !important; border-radius: 8px !important; font-weight: 600; border: none; padding: 10px 20px; transition: 0.3s; }
        .btn:hover { background-color: #0B3C5D !important; transform: translateY(-2px); }
        .form-control { border-radius: 8px !important; border: 1px solid #D1E2E8 !important; box-shadow: none !important; }
        .form-control:focus { border-color: #2EC4B6 !important; box-shadow: 0 0 0 0.2rem rgba(46, 196, 182, 0.25) !important; }
        .main-footer { background: #0B3C5D !important; color: #2EC4B6 !important; text-align: center; font-weight: bold; padding: 15px; }
      "))
    ),
    
    tabItems(
      # --- МАТЕМАТИКА ---
      tabItem(tabName = "quad",
              h2("📐 Квадратное уравнение"),
              fluidRow(
                box(title = "Параметры", width = 4, solidHeader = TRUE,
                    numericInput("qa", "a", 1), numericInput("qb", "b", -3), numericInput("qc", "c", 2),
                    actionButton("go_quad", "Решить", icon = icon("play"))
                ),
                box(title = "Решение", width = 8, solidHeader = TRUE, verbatimTextOutput("quad_out"))
              ),
              box(title = "График", width = 12, solidHeader = TRUE, withSpinner(plotlyOutput("quad_plot")))
      ),
      tabItem(tabName = "calc",
              h2("📐 Калькулятор"),
              box(width = 6, solidHeader = TRUE,
                  numericInput("a", "Число 1", 0), numericInput("b", "Число 2", 0),
                  selectInput("op", "Операция", c("+"="+", "-"="-", "*"="*", "/"="/")),
                  actionButton("go_calc", "Вычислить")
              ),
              box(width = 6, solidHeader = TRUE, verbatimTextOutput("calc_out"))
      ),
      
      # --- ФИЗИКА ---
      tabItem(tabName = "projectile",
              h2("⚛️ Бросок тела под углом"),
              fluidRow(
                box(width = 4, solidHeader = TRUE,
                    numericInput("v0", "Скорость (м/с)", 20), numericInput("angle", "Угол", 45), numericInput("g", "g", 9.81),
                    actionButton("go_phys", "Построить")
                ),
                box(width = 8, solidHeader = TRUE, verbatimTextOutput("phys_out"))
              ),
              box(width = 12, solidHeader = TRUE, withSpinner(plotlyOutput("phys_plot")))
      ),
      
      # --- ХИМИЯ ---
      tabItem(tabName = "molar",
              h2("🧪 Молярная масса и раствор"),
              fluidRow(
                box(width = 4, solidHeader = TRUE,
                    selectInput("elem", "Элемент:", choices = c("H (1)", "O (16)", "C (12)", "Na (23)", "Fe (56)")),
                    numericInput("moles", "Количество (моль):", 1),
                    actionButton("go_chem", "Рассчитать")
                ),
                box(width = 8, solidHeader = TRUE, verbatimTextOutput("chem_out"))
              ),
              box(width = 12, solidHeader = TRUE, withSpinner(plotlyOutput("chem_plot")))
      ),
      
      # --- КРИПТОГРАФИЯ ---
      tabItem(tabName = "crypto_tools",
              h2("🔐 Шифры и кодирование"),
              tabsetPanel(
                tabPanel("Шифр Цезаря",
                         br(),
                         box(width = 12, fluidRow(
                           column(6, textAreaInput("caesar_text", "Текст:", "Привет Мир!", rows = 4)),
                           column(6, numericInput("caesar_shift", "Сдвиг:", 3, min = 1, max = 32),
                                  actionButton("go_caesar", "Зашифровать", style = "margin-top: 25px;"))
                         ), verbatimTextOutput("caesar_out"))
                ),
                tabPanel("Шифр Атбаш",
                         br(),
                         box(width = 12, textAreaInput("atbash_text", "Текст:", "Азбука", rows = 4),
                             actionButton("go_atbash", "Преобразовать"), verbatimTextOutput("atbash_out"))
                ),
                tabPanel("Base64",
                         br(),
                         box(width = 12,
                             textAreaInput("b64_text", "Текст:", "Hello", rows = 2),
                             actionButton("go_b64_encode", "Закодировать"),
                             hr(),
                             textAreaInput("b64_decode_text", "Base64:", "", rows = 2),
                             actionButton("go_b64_decode", "Декодировать"),
                             verbatimTextOutput("b64_out"))
                ),
                tabPanel("Хеширование (MD5)",
                         br(),
                         box(width = 12, textInput("hash_text", "Пароль:", "my_password"),
                             actionButton("go_hash", "Хеш"), verbatimTextOutput("hash_out"))
                )
              )
      ),
      
      # --- ИГРЫ ---
      tabItem(tabName = "guess",
              h2("🎮 Угадай число (от 1 до 100)"),
              fluidRow(
                box(width = 4, solidHeader = TRUE,
                    numericInput("guess_input", "Ваша догадка:", 50, min = 1, max = 100),
                    actionButton("guess_btn", "Проверить"),
                    actionButton("reset_btn", "Новая игра")
                ),
                box(width = 8, solidHeader = TRUE, verbatimTextOutput("guess_out"))
              ),
              box(width = 12, solidHeader = TRUE, withSpinner(plotlyOutput("guess_plot")))
      ),
      
      # --- СТАТИСТИКА ---
      tabItem(tabName = "upload",
              h2("📊 Загрузка"),
              box(width = 12, fileInput("file", "CSV/Excel"), withSpinner(DTOutput("csv_table")))
      ),
      tabItem(tabName = "desc",
              h2("📊 Статистики"),
              box(width = 12, fileInput("file2", "Файл"), withSpinner(DTOutput("desc_table")))
      ),
      tabItem(tabName = "hist",
              h2("📊 Графики"),
              box(width = 12, fileInput("file3", "Файл"), selectInput("hist_col", "Столбец:", choices = NULL), withSpinner(plotlyOutput("hist_plot")))
      ),
      tabItem(tabName = "gen",
              h2("📊 Генератор случайных данных"),
              box(width = 12,
                  actionButton("gen_btn", "Сгенерировать 100 студентов"),
                  withSpinner(DTOutput("gen_table")),
                  withSpinner(plotlyOutput("gen_plot"))
              )
      ),
      
      # --- ПРОГРАММИРОВАНИЕ ---
      tabItem(tabName = "sorting",
              h2("💻 Визуализация сортировки"),
              box(width = 12,
                  actionButton("shuffle_btn", "Перемешать"),
                  actionButton("sort_btn", "Сортировать"),
                  withSpinner(plotlyOutput("sort_plot"))
              )
      ),
      
      # --- ФИНАНСЫ ---
      tabItem(tabName = "interest",
              h2("💼 Сложный процент"),
              fluidRow(
                box(width = 4, numericInput("sum_start", "Сумма", 100000), numericInput("rate", "Ставка %", 10), numericInput("years", "Лет", 10), actionButton("go_interest", "Рассчитать")),
                box(width = 8, verbatimTextOutput("interest_out"), withSpinner(plotlyOutput("interest_plot")))
              )
      ),
      tabItem(tabName = "inflation",
              h2("💼 Влияние инфляции"),
              fluidRow(
                box(width = 4, numericInput("money", "Сумма", 100000), numericInput("infl_rate", "Инфляция %", 8), numericInput("infl_years", "Лет", 10), actionButton("go_inflation", "Рассчитать")),
                box(width = 8, verbatimTextOutput("inflation_out"), withSpinner(plotlyOutput("inflation_plot")))
              )
      ),
      tabItem(tabName = "credit",
              h2("💼 Кредит"),
              fluidRow(
                box(width = 4, numericInput("credit_sum", "Сумма", 1000000), numericInput("credit_rate", "Ставка %", 15), numericInput("credit_years", "Лет", 5), actionButton("go_credit", "Рассчитать")),
                box(width = 8, verbatimTextOutput("credit_out"), plotlyOutput("credit_donut"))
              )
      )
    ),
    
    tags$footer("🎓 от Финансового Университета | 2026")
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  read_data <- function(path, name) {
    if (grepl("\\.csv$", name, ignore.case = TRUE)) read.csv(path, stringsAsFactors = FALSE)
    else read_excel(path)
  }
  
  # --- МАТЕМАТИКА ---
  observeEvent(input$go_quad, {
    a <- input$qa; b <- input$qb; c <- input$qc
    if (a == 0) { output$quad_out <- renderPrint("a не может быть 0"); return() }
    D <- b^2 - 4 * a * c
    output$quad_out <- renderPrint({
      cat("D =", D, "\n")
      if (D > 0) cat("x1 =", round((-b + sqrt(D)) / (2*a), 2), "| x2 =", round((-b - sqrt(D)) / (2*a), 2))
      else if (D == 0) cat("x =", round(-b/(2*a), 2)) else cat("Корней нет")
    })
    x_vals <- seq(-10, 10, length.out = 200)
    df <- data.frame(x = x_vals, y = a * x_vals^2 + b * x_vals + c)
    p <- ggplot(df, aes(x, y)) + geom_line(color = "#0B3C5D", size = 1.5) + theme_minimal()
    output$quad_plot <- renderPlotly(ggplotly(p))
  })
  
  observeEvent(input$go_calc, {
    res <- switch(input$op, "+"=input$a+input$b, "-"=input$a-input$b, "*"=input$a*input$b, "/"=if(input$b==0) "Ошибка" else input$a/input$b)
    output$calc_out <- renderPrint(res)
  })
  
  # --- ФИЗИКА ---
  observeEvent(input$go_phys, {
    v0 <- input$v0; ang <- input$angle * pi / 180; g <- input$g
    vx <- v0 * cos(ang); vy <- v0 * sin(ang)
    t_flight <- 2 * vy / g
    t_seq <- seq(0, t_flight, length.out = 100)
    x <- vx * t_seq; y <- vy * t_seq - 0.5 * g * t_seq^2
    output$phys_out <- renderPrint({ cat("Дальность:", round(max(x), 2), "м\nВысота:", round(max(y), 2), "м") })
    df <- data.frame(x = x, y = y)
    p <- ggplot(df, aes(x, y)) + geom_line(color = "#E76F51", size = 1.5) + theme_minimal()
    output$phys_plot <- renderPlotly(ggplotly(p))
  })
  
  # --- ХИМИЯ ---
  observeEvent(input$go_chem, {
    val <- as.numeric(gsub("[^0-9]", "", input$elem))
    m <- input$moles
    total_mass <- val * m
    output$chem_out <- renderPrint({ cat("Молярная масса элемента:", val, "г/моль\nОбщая масса:", total_mass, "г") })
    df <- data.frame(Тип = c("Масса элемента", "Остаток до 100г"), Сумма = c(total_mass, max(0, 100 - total_mass)))
    output$chem_plot <- renderPlotly({
      plot_ly(df, labels = ~Тип, values = ~Сумма, type = 'pie', hole = 0.5, marker = list(colors = c('#2EC4B6', '#EAF6FA')))
    })
  })
  
  # --- КРИПТОГРАФИЯ ---
  observeEvent(input$go_caesar, {
    txt <- input$caesar_text; k <- input$caesar_shift
    shift_char <- function(ch, shift) {
      if (grepl("[a-z]", ch)) intToUtf8((utf8ToInt(ch) - 97 + shift) %% 26 + 97)
      else if (grepl("[A-Z]", ch)) intToUtf8((utf8ToInt(ch) - 65 + shift) %% 26 + 65)
      else if (grepl("[а-я]", ch)) intToUtf8((utf8ToInt(ch) - 1072 + shift) %% 32 + 1072)
      else if (grepl("[А-Я]", ch)) intToUtf8((utf8ToInt(ch) - 1040 + shift) %% 32 + 1040)
      else ch
    }
    chars <- strsplit(txt, "")[[1]]
    output$caesar_out <- renderPrint({ paste(sapply(chars, shift_char, shift = k), collapse = "") })
  })
  
  observeEvent(input$go_atbash, {
    txt <- input$atbash_text
    ru_up <- paste0(intToUtf8(1040:1071), collapse=""); ru_low <- paste0(intToUtf8(1072:1103), collapse="")
    en_up <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; en_low <- "abcdefghijklmnopqrstuvwxyz"
    chars <- strsplit(txt, "")[[1]]
    res <- sapply(chars, function(ch) {
      if (grepl("[А-Я]", ch)) strsplit(paste0(intToUtf8(1071:1040), collapse=""), "")[[1]][match(ch, strsplit(ru_up, "")[[1]])]
      else if (grepl("[а-я]", ch)) strsplit(paste0(intToUtf8(1103:1072), collapse=""), "")[[1]][match(ch, strsplit(ru_low, "")[[1]])]
      else if (grepl("[A-Z]", ch)) strsplit(paste0(rev(strsplit(en_up, "")[[1]]), collapse=""), "")[[1]][match(ch, strsplit(en_up, "")[[1]])]
      else if (grepl("[a-z]", ch)) strsplit(paste0(rev(strsplit(en_low, "")[[1]]), collapse=""), "")[[1]][match(ch, strsplit(en_low, "")[[1]])]
      else ch
    })
    output$atbash_out <- renderPrint({ paste(res, collapse = "") })
  })
  
  observeEvent(input$go_b64_encode, { output$b64_out <- renderPrint({ base64enc::base64encode(charToRaw(input$b64_text)) }) })
  observeEvent(input$go_b64_decode, { output$b64_out <- renderPrint({ rawToChar(base64enc::base64decode(input$b64_decode_text)) }) })
  observeEvent(input$go_hash, { output$hash_out <- renderPrint({ digest::digest(input$hash_text, algo = "md5", serialize = FALSE) }) })
  
  # --- ИГРЫ ---
  target_num <- reactiveVal(sample(1:100, 1))
  guesses <- reactiveVal(numeric(0))
  
  observeEvent(input$reset_btn, {
    target_num(sample(1:100, 1))
    guesses(numeric(0))
    output$guess_out <- renderPrint("Новая игра! Введите число.")
  })
  
  observeEvent(input$guess_btn, {
    g <- input$guess_input
    t <- target_num()
    guesses(c(guesses(), g))
    
    if (g == t) {
      output$guess_out <- renderPrint(paste("Угадали! Это было число", t))
    } else if (g < t) {
      output$guess_out <- renderPrint("Больше!")
    } else {
      output$guess_out <- renderPrint("Меньше!")
    }
    
    df <- data.frame(Попытка = 1:length(guesses()), Значение = guesses())
    p <- ggplot(df, aes(x = Попытка, y = Значение)) +
      geom_line(color = "#0B3C5D") + geom_point(color = "#2EC4B6", size = 3) +
      geom_hline(yintercept = t, linetype = "dashed", color = "red") + theme_minimal()
    output$guess_plot <- renderPlotly(ggplotly(p))
  })
  
  # --- СТАТИСТИКА ---
  data_in <- reactive({ req(input$file); read_data(input$file$datapath, input$file$name) })
  output$csv_table <- renderDT({ req(data_in()); datatable(data_in(), options = list(pageLength = 5)) })
  
  data_in2 <- reactive({ req(input$file2); read_data(input$file2$datapath, input$file2$name) })
  output$desc_table <- renderDT({
    req(data_in2()); df <- data_in2()
    stat <- function(x, f) if(is.numeric(x)) round(f(x, na.rm=TRUE), 2) else NA
    res <- data.frame(Переменная=names(df), Среднее=sapply(df, stat, f=mean), Медиана=sapply(df, stat, f=median))
    datatable(res, options=list(dom='t'))
  })
  
  observe({
    req(input$file3)
    df <- read_data(input$file3$datapath, input$file3$name)
    updateSelectInput(session, "hist_col", choices = names(df)[sapply(df, is.numeric)])
  })
  
  output$hist_plot <- renderPlotly({
    req(input$file3, input$hist_col)
    df <- read_data(input$file3$datapath, input$file3$name)
    p <- ggplot(df, aes_string(x = input$hist_col)) + geom_histogram(fill = "#2EC4B6", color = "white") + theme_minimal()
    ggplotly(p)
  })
  
  # --- ГЕНЕРАТОР ДАННЫХ ---
  gen_data <- reactiveVal(NULL)
  observeEvent(input$gen_btn, {
    gen_data(data.frame(
      Студент = paste("Студент", 1:100),
      Рост = round(rnorm(100, 175, 10)),
      Вес = round(rnorm(100, 70, 15))
    ))
  })
  
  output$gen_table <- renderDT({ req(gen_data()); datatable(gen_data(), options = list(pageLength = 5)) })
  output$gen_plot <- renderPlotly({
    req(gen_data())
    p <- ggplot(gen_data(), aes(x = Рост, y = Вес)) + geom_point(color = "#2EC4B6", size = 3) + theme_minimal()
    ggplotly(p)
  })
  
  # --- ПРОГРАММИРОВАНИЕ (Сортировка) ---
  sort_vals <- reactiveVal(sample(1:50, 20))
  
  observeEvent(input$shuffle_btn, { sort_vals(sample(1:50, 20)) })
  observeEvent(input$sort_btn, { sort_vals(sort(sort_vals())) })
  
  output$sort_plot <- renderPlotly({
    df <- data.frame(idx = 1:length(sort_vals()), val = sort_vals())
    p <- ggplot(df, aes(x = factor(idx), y = val, fill = val)) +
      geom_bar(stat = "identity") + scale_fill_gradient(low = "#2EC4B6", high = "#0B3C5D") + theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # --- ФИНАНСЫ ---
  observeEvent(input$go_interest, {
    P <- input$sum_start; r <- input$rate/100; t <- input$years
    S <- P * (1+r)^t
    output$interest_out <- renderPrint({ cat("Итого:", round(S, 2), "| Прибыль:", round(S-P, 2)) })
    df <- data.frame(Год = 0:t, Вложения = P, Прибыль = P*(1+r)^(0:t) - P)
    p <- ggplot(df, aes(x = Год)) + geom_area(aes(y = Вложения, fill = "Тело"), alpha = 0.6) +
      geom_area(aes(y = Прибыль, fill = "Прибыль"), alpha = 0.6) +
      scale_fill_manual(values = c("Тело" = "#0B3C5D", "Прибыль" = "#2EC4B6")) + theme_minimal()
    output$interest_plot <- renderPlotly(ggplotly(p))
  })
  
  observeEvent(input$go_inflation, {
    M <- input$money; i <- input$infl_rate/100; t <- input$infl_years
    real_val <- M / (1+i)^t
    output$inflation_out <- renderPrint({ cat("Реальная стоимость через", t, "лет:", round(real_val, 2), "| Потери:", round(M - real_val, 2)) })
    df <- data.frame(Год = 0:t, Реальная_стоимость = M / (1+i)^(0:t))
    p <- ggplot(df, aes(x = Год, y = Реальная_стоимость)) + geom_line(color = "#E76F51", size = 1.5) + theme_minimal()
    output$inflation_plot <- renderPlotly(ggplotly(p))
  })
  
  observeEvent(input$go_credit, {
    S <- input$credit_sum; r <- input$credit_rate/100/12; n <- input$credit_years*12
    pay <- S * r / (1 - (1+r)^-n); total <- pay * n; overpay <- total - S
    output$credit_out <- renderPrint({ cat("Платёж:", round(pay, 2), "| Переплата:", round(overpay, 2)) })
    df_donut <- data.frame(Тип = c("Долг", "Проценты"), Сумма = c(S, overpay))
    output$credit_donut <- renderPlotly({
      plot_ly(df_donut, labels = ~Тип, values = ~Сумма, type = 'pie', hole = 0.6, marker = list(colors = c('#0B3C5D', '#2EC4B6')))
    })
  })
}

shinyApp(ui, server)