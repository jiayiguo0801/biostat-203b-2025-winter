library(shiny)
library(tidyverse)
library(gtsummary)
library(bigrquery)
library(DBI)
library(ggnewscale)

# Install the dataset
icu_cohort <- read_rds("mimic_icu_cohort.rds")
# 列出现在所在文件夹名字
list.files()
# 列出当前工作文件路径
getwd()
# Connect with the SQL database
satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)
dbListTables(con_bq)

# 从sql里读取数据
patients_tble <- tbl(con_bq, "patients")
admissions_tble <- tbl(con_bq, "admissions")
transfers_tble <- tbl(con_bq, "transfers")
diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd")
d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses")

ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort summaries"),
  tabsetPanel(
    tabPanel("Patients infomation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_var", "Select variable:",
                             choices = c(
                               "Age at ICU admission" = "age_intime",
                               "Gender" = "gender",
                               "Race" = "race",
                               "Insurance" = "insurance",
                               "Marital Status" = "marital_status",
                               "Lab Measurements (last before ICU)",
                               "Vitals (first in ICU)",
                               "First Care Unit" = "first_careunit"
                             ),
                             selected = "age_intime"
                 )
               ),
               mainPanel(
                 plotOutput("variable_plot"),
                 tableOutput("summary_by_group")
               )
             )
    ),
    tabPanel("Patient's ICU Information",
             sidebarLayout(
               sidebarPanel(
                 # 使用文本框输入subject_id，并添加确认按钮表示输入完成
                 textInput("subject_id", "Enter the subject_id:"),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 plotOutput("patient_ADT_history")
               )
             )
            
      
    )
  )
    
)

server <- function(input, output, session) {
  # Define categorical variables
  categorical_vars <- c("race", "insurance", "gender", "first_careunit", "marital_status")
  # Plot the variable
  output$variable_plot <- renderPlot({
    # If variable is lab measurements
    if(input$selected_var == "Lab Measurements (last before ICU)") {
      # 选择测量结果绘图
      # 定义你关心的lab指标
      lab_vars <- c("Bicarbonate", "Chloride", "Creatinine", "Glucose", 
                    "Potassium", "Sodium", "Hematocrit", "White Blood Cells")
      
      # 批量绘制散点图和回归拟合曲线
      plots <- map(lab_vars, function(var) {
        ggplot(icu_cohort, aes(x = .data[[var]], y = los)) +
          geom_point(alpha = 0.3, color = "blue") +
          geom_smooth(method = "lm", color = "black", se = TRUE) +
          labs(title = paste("ICU LOS vs", var),
               x = var,
               y = "Length of ICU Stay (days)") +
          theme_minimal()
      })
      
      # 使用patchwork合并所有的图
      wrap_plots(plots, ncol = 2)
    } 
    # If variable is vitals
    else if(input$selected_var == "Vitals (first in ICU)") {
      
      vital_vars <- c("Heart Rate", 
                      "Respiratory Rate", 
                      "Non Invasive Blood Pressure systolic", 
                      "Non Invasive Blood Pressure diastolic", 
                      "Temperature Fahrenheit")
      
      plots <- map(vital_vars, function(var) {
        ggplot(icu_cohort, aes(x = .data[[var]], y = los)) +
          geom_point(alpha = 0.3, color = "darkgreen") +
          geom_smooth(method = "lm", color = "black", se = TRUE) +
          labs(title = paste("ICU LOS vs", var),
               x = var,
               y = "Length of ICU Stay (days)") +
          theme_minimal()
      })
      
      wrap_plots(plots)
    } 
    # If variable is categorical
    else if(input$selected_var %in% categorical_vars) {
      icu_cohort %>%
        ggplot(aes(x = fct_infreq(.data[[input$selected_var]]))) +
        # Show it in a bar chart
        geom_bar(fill = "lightblue", width = 0.3) +
        labs(title = paste("LOS by", input$selected_var),
             x = input$selected_var, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1))
    }
    # If variable is numeric(age)
    else {
      ggplot(icu_cohort, aes(x = .data[[input$selected_var]])) +
        geom_histogram( binwidth = 5, fill = "lightblue", color = "black", alpha = 0.5) +
       labs(title = paste(input$selected_var, "with Distribution"),
             x = input$selected_var) +
        theme_minimal()
    }
    
  })
  
  # Table summary
  output$summary_by_group <- renderTable({
    if(input$selected_var == "Lab Measurements (last before ICU)"){
      # 选择lab指标
      lab_vars <- c("Bicarbonate", "Chloride", "Creatinine", "Glucose", 
                    "Potassium", "Sodium", "Hematocrit", "White Blood Cells")
      # 计算lab指标的平均值、中位数、四分之一和四分之三分位数
      map_dfr(lab_vars, function(var) {
        icu_cohort %>%
          summarise(
            Variable = var,
            mean = mean(.data[[var]], na.rm = TRUE),
            median = median(.data[[var]], na.rm = TRUE),
            sd = sd(.data[[var]], na.rm = TRUE),
            q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),  # 计算第一四分位数
            q3 = quantile(.data[[var]], 0.75, na.rm = TRUE),  # 计算第三四分位数
            cor_value = cor(icu_cohort[[var]], icu_cohort$los, use="complete.obs")
          )
      })
      
      
    }
    else if(input$selected_var == "Vitals (first in ICU)") {
      # 选择vital指标
      vital_vars <- c("Heart Rate", 
                      "Respiratory Rate", 
                      "Non Invasive Blood Pressure systolic", 
                      "Non Invasive Blood Pressure diastolic", 
                      "Temperature Fahrenheit")
      
      # 计算vital指标和los相关性系数
      map_dfr(vital_vars, function(var) {
        
      })
      # 计算vital指标的平均值、中位数、四分之一和四分之三分位数
      map_dfr(vital_vars, function(var) {
        icu_cohort %>%
          summarise(
            Variable = var,
            mean = mean(.data[[var]], na.rm = TRUE),
            median = median(.data[[var]], na.rm = TRUE),
            sd = sd(.data[[var]], na.rm = TRUE),
            q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),  # 计算第一四分位数
            q3 = quantile(.data[[var]], 0.75, na.rm = TRUE),   # 计算第三四分位数
            # 计算vital指标和los相关性系数
            cor_value = cor(icu_cohort[[var]], icu_cohort$los, use="complete.obs")
          )
      })
    }
    # Statistical summary for categorical variables
    else if(input$selected_var %in% categorical_vars) {
      icu_cohort %>%
        group_by(.data[[input$selected_var]]) %>%
        summarise(
          count = n(),
          median_los = median(los, na.rm=TRUE),
          mean_los = mean(los, na.rm=TRUE),
          sd_los = sd(los, na.rm=TRUE)
        ) %>%
        arrange(desc(mean_los))
    } 
    else {
      # 数值型变量：计算相关系数
      cor_value <- cor(icu_cohort[[input$selected_var]], icu_cohort$los, use="complete.obs")
      data.frame(
        Variable = input$selected_var,
        Correlation_with_LOS = round(cor_value, 3)
      )
      # 数值型变量：计算平均值、中位数、四分之一和四分之三分位数
      icu_cohort %>%
        summarise(
          Variable = input$selected_var,
          count = sum(!is.na(.data[[input$selected_var]])),  # 计算非缺失值的数量
          mean = mean(.data[[input$selected_var]], na.rm = TRUE),
          median = median(.data[[input$selected_var]], na.rm = TRUE),
          sd = sd(.data[[input$selected_var]], na.rm = TRUE),
          q1 = quantile(.data[[input$selected_var]], 0.25, na.rm = TRUE),  # 计算第一四分位数
          q3 = quantile(.data[[input$selected_var]], 0.75, na.rm = TRUE)   # 计算第三四分位数
        )
    }
  })
  
  # Use reactive to receive the subject_id
  subject_id_input <- eventReactive(input$submit, {
    input$subject_id
  })
  output$patient_ADT_history <- renderPlot({
    # 从输入框中读取subject_id
    subject_id_selected <- as.numeric(subject_id_input())
    # 从数据库中读取数据并执行筛选
    patients_tble <- tbl(con_bq, "patients")
    admissions_tble <- tbl(con_bq, "admissions")
    transfers_tble <- tbl(con_bq, "transfers")
    diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd")
    d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")
    d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses")
    
    labevents_tble <- tbl(con_bq, "labevents") |>
      select(subject_id, hadm_id, charttime) |>
      filter(subject_id == subject_id_selected) |>
      mutate(
        chartdate = as.Date(charttime)
      ) |>
      rename(chartdate_lab = chartdate) |>
      collect()
    
    procedures_icd_tble <- tbl(con_bq, "procedures_icd") |>
      select(subject_id, hadm_id, icd_code, chartdate) |>
      filter(subject_id == subject_id_selected) |>
      left_join(d_icd_procedures_tble, by = "icd_code") |>
      select(subject_id, chartdate, long_title) |>
      collect() 
    # 获取当前患者的所有手术类型
    procedure_type <- unique(procedures_icd_tble$long_title)
    
    # 为了保证每次都随机，可以准备一组可用的 shape 编号
    shapes_available <- c(16, 17, 15, 19, 25, 8, 9, 10, 12, 13, 14, 1, 2, 3, 4)
    
    # 根据手术类型个数，随机抽取
    set.seed(Sys.time())  # 可根据需要设置随机种子
    my_shape_mapping <- sample(shapes_available, size = length(procedure_type))
    
    # 将抽到的形状编号命名为 procedure_type
    names(my_shape_mapping) <- procedure_type 
    # 增加随机颜色映射：定义一组颜色（确保颜色数量不小于手术类型数）
    colors_available <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "magenta", "yellow")
    # 随机抽取颜色（不重复）
    my_color_mapping <- sample(colors_available, size = length(procedure_type))
    names(my_color_mapping) <- procedure_type
    
    
    diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd") |>
      select(subject_id, hadm_id, icd_code, seq_num) |>
      filter(subject_id == subject_id_selected) |>
      left_join(d_icd_diagnoses_tble, by = "icd_code") |>
      distinct(seq_num, .keep_all = TRUE) |>
      # 只取这位病人的前三个诊断
      slice_min(order_by = seq_num, n = 3) |>
      arrange(seq_num) |>
      collect()
    diagnoses_text <- paste(diagnoses_icd_tble$long_title, collapse = "\n") 
    
    # 从数据库中读取病人数据
    patient_aimed <- patients_tble %>%
      filter(subject_id == subject_id_selected) %>%
      left_join(admissions_tble, by = "subject_id") %>%
      left_join(transfers_tble, by = c("subject_id", "hadm_id")) %>%
      mutate(
        intime_date = as.Date(intime),
        outtime_date = as.Date(outtime)
      ) |>
      select(subject_id, hadm_id, 
             eventtype, careunit, 
             intime_date, outtime_date) |>
      mutate(is_icu = if_else
             (grepl("ICU", careunit), 3.5, 0.8)) |>
      collect() 
    
    # 将patients里的基础信息作为标题文本
    patient_info <- patients_tble %>%
      filter(subject_id == subject_id_selected) %>%
      left_join(admissions_tble, by = "subject_id") %>%
      collect()
    patient_info_text <- paste(
      "Patient,",
      subject_id_selected,
      ",",
      patient_info$gender,
      ",",
      patient_info$anchor_age,
      "years old,",
      patient_info$race
    )
    # 定义careunit颜色
    careunit_colors <- c(
      "Emergency Department" = "red",
      "Emergency Department Observation" = "red",
      "Observation" = "red",
      
      "Medicine" = "blue",
      "Med/Surg" = "blue",
      "Med/Surg/GYN" = "blue",
      "Med/Surg/Trauma" = "blue",
      "Medical/Surgical (Gynecology)" = "blue",
      "Surgery" = "blue",
      "Surgery/Trauma" = "blue",
      "Surgery/Pancreatic/Biliary/Bariatric" = "blue",
      
      "Medical Intensive Care Unit (MICU)" = "orange",
      "Surgical Intensive Care Unit (SICU)" = "orange",
      "Trauma SICU (TSICU)" = "orange",
      "Medical/Surgical Intensive Care Unit (MICU/SICU)" = "orange",
      "Cardiac Vascular Intensive Care Unit (CVICU)" = "orange",
      "Neuro Surgical Intensive Care Unit (Neuro SICU)" = "orange",
      "Intensive Care Unit (ICU)" = "orange",
      
      "Medicine/Cardiology" = "darkred",
      "Cardiology" = "darkred",
      "Cardiac Surgery" = "darkred",
      "Medicine/Cardiology Intermediate" = "darkred",
      "Cardiology Surgery Intermediate" = "darkred",
      "Coronary Care Unit (CCU)" = "darkred",
      
      "Neurology" = "lightgreen",
      "Neuro Surgical Intensive Care Unit (Neuro SICU)" = "lightgreen",
      "Neuro Intermediate" = "lightgreen",
      "Neuro Stepdown" = "lightgreen",
      
      "Obstetrics (Postpartum & Antepartum)" = "pink",
      "Obstetrics Antepartum" = "pink",
      "Obstetrics Postpartum" = "pink",
      "Labor & Delivery" = "pink",
      
      "Nursery" = "lightblue",
      "Special Care Nursery (SCN)" = "lightblue",
      
      "Hematology/Oncology" = "green",
      "Hematology/Oncology Intermediate" = "green",
      "Oncology" = "green",
      "Transplant" = "green",
      
      "Psychiatry" = "purple",
      
      "PACU" = "darkgreen",
      "Surgical Intermediate" = "darkgreen",
      "Surgery/Vascular/Intermediate" = "darkgreen",
      
      "Discharge Lounge" = "lightyellow",
      "UNKNOWN" = "lightyellow",
      "Unknown" = "lightyellow")
    ggplot() +
      geom_segment(
        data = patient_aimed |>
          filter(careunit != "UNKNOWN"),
        aes(
          x = intime_date,
          xend = outtime_date,
          y = "ADT",
          yend = "ADT",
          color = careunit,
          linewidth = is_icu
        )
      ) +
      scale_color_manual(values = careunit_colors) +
      guides(linewidth = "none") +
      new_scale_color() +
      geom_point(
        data = labevents_tble,
        aes(x = chartdate_lab, y = "Lab"),
        shape = 3, size = 3
      ) +
      geom_point(
        data = procedures_icd_tble,
        aes(x = chartdate, y = "Procedure", 
            shape = long_title, color = long_title),
        size = 3
      ) +
      scale_shape_manual(values = my_shape_mapping) +
      scale_color_manual(values = my_color_mapping) +
      labs(title = patient_info_text,
           subtitle = diagnoses_text,
           x = "Date",
           y = "",
           shape = "Procedure Type",
           color = "Procedure Type") +
      theme_minimal()
  })
  

  
  
}




# Run the application 
shinyApp(ui, server)
