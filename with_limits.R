library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(psych)
library(ggplot2)
library(car)
library(corrplot)
library(RColorBrewer)
library(fixest)
library(modelsummary)
library(tibble)
library(lmtest)
library(sandwich)
library(ivreg)
library(AER)
library(systemfit)
library(knitr)


# 1. PANEL DATA
file <- "Data.xlsx"

read_var_long <- function(sheet_name, var_name) {
  read_excel(file, sheet = sheet_name) %>%
    select(
      `Country Name`,
      `Country Code`,
      `2009 [YR2009]`:`2019 [YR2019]`
    ) %>%
    rename(
      country = `Country Name`,
      code    = `Country Code`
    ) %>%
    pivot_longer(
      cols = starts_with("20"),
      names_to  = "year_raw",
      values_to = var_name
    ) %>%
    mutate(
      year = as.integer(substr(year_raw, 1, 4)),
      !!var_name := ifelse(!!sym(var_name) == "..",
                           NA,
                           !!sym(var_name)),
      !!var_name := as.numeric(!!sym(var_name))
    ) %>%
    select(country, code, year, all_of(var_name)) %>%
    filter(year >= 2009, year <= 2019)
}

gdp       <- read_var_long("GDP per person employed ", "gdp_emp")
internet  <- read_var_long("Individuals using the Internet ", "internet_users")
services  <- read_var_long("Services(% of GDP)", "services_gdp")
lifeexp   <- read_var_long("Life expectancy at birth", "lifeexp")
industry  <- read_var_long("Industry(% of GDP)", "industry_gdp")
urban     <- read_var_long("Urban population (%)", "urban_share")
tertiary  <- read_var_long("School enrollment, tertiary ", "tertiary_enrol")

panel_data <- list(
  gdp, internet, services, lifeexp, industry, urban, tertiary
) %>%
  reduce(left_join, by = c("country", "code", "year"))

write.csv(panel_data, "panel_data.csv", row.names = FALSE)
panel_data <- read.csv("panel_data.csv")

# 2. DESCRIPTIVE STATISTICS
numeric_panel_data <- panel_data %>%
  select(gdp_emp, internet_users, services_gdp, lifeexp, industry_gdp, urban_share, tertiary_enrol)

descriptive_summary <- describe(numeric_panel_data)
write.csv(descriptive_summary, "descriptive_statistics.csv", row.names = TRUE)

# 3. VISUALISATIONS
panel_data_long <- panel_data %>%
  select(year, gdp_emp, internet_users, services_gdp, lifeexp, industry_gdp, urban_share, tertiary_enrol) %>%
  pivot_longer(cols = c(-year), names_to = "variable", values_to = "value")

density_plot <- ggplot(panel_data_long, aes(x = value)) +
  geom_density(fill = "grey70", alpha = 0.7, colour = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Density Plots for Panel Data (Pooled)", x = "Value", y = "Density") +
  theme_minimal()

ggsave("density_plots.jpg", density_plot, width = 10, height = 8, dpi = 300)

boxplot_time <- ggplot(panel_data_long, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Boxplots of Variables Over Time", x = "Year", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("boxplots_over_time.jpg", boxplot_time, width = 12, height = 8, dpi = 300)

scatter_plot <- ggplot(panel_data, aes(x = internet_users, y = gdp_emp)) +
  geom_point(alpha = 0.4, colour = "black") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(title = "Scatterplot: GDP per Person Employed vs Internet Users (%)",
       x = "Internet Users (% of population)", y = "GDP per Person Employed") +
  theme_minimal()

ggsave("scatter_gdp_vs_internet.jpg", scatter_plot, width = 8, height = 6, dpi = 300)

cor_matrix <- cor(numeric_panel_data, use = "pairwise.complete.obs")
png("correlation_matrix.jpg", width = 2000, height = 2000, res = 300)
corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  addCoef.col = "black",
  number.cex = 0.7,
  col = brewer.pal(n = 8, name = "RdYlBu")
)
dev.off()

# 4. REGRESSION ANALYSIS
panel_reg <- panel_data %>%
  mutate(
    ln_gdp        = log(gdp_emp),
    internet_users = internet_users / 100,
    tertiary_enrol = tertiary_enrol / 100,
    services_gdp   = services_gdp / 100,
    industry_gdp   = industry_gdp / 100,
    urban_share    = urban_share / 100
  )

# FE Models
m1 <- feols(ln_gdp ~ internet_users | code + year, data = panel_reg, cluster = ~ code)
m2 <- feols(ln_gdp ~ internet_users + tertiary_enrol | code + year, data = panel_reg, cluster = ~ code)
m3 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp | code + year, data = panel_reg, cluster = ~ code)
m4 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp | code + year, data = panel_reg, cluster = ~ code)
m5 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp | code + year, data = panel_reg, cluster = ~ code)
m6 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp + urban_share | code + year, data = panel_reg, cluster = ~ code)

models <- list("Model 1"=m1,"Model 2"=m2,"Model 3"=m3,"Model 4"=m4,"Model 5"=m5,"Model 6"=m6)

countries_vec <- map_dbl(models, ~ length(fixef(.x)[["code"]]))

countries_row <- tibble::tribble(
  ~term, ~`Model 1`, ~`Model 2`, ~`Model 3`, ~`Model 4`, ~`Model 5`, ~`Model 6`,
  "Countries", as.character(countries_vec[1]), 
  as.character(countries_vec[2]), 
  as.character(countries_vec[3]),
  as.character(countries_vec[4]), 
  as.character(countries_vec[5]), 
  as.character(countries_vec[6])
)

gof_map <- tibble::tribble(
  ~raw,        ~clean,        ~fmt,
  "nobs",      "Observations", 0,
  "within.r2", "Within R²",    3
)

nice_names <- c(
  "internet_users" = "Internet users (share)",
  "tertiary_enrol" = "Tertiary enrollment (share)",
  "lifeexp"        = "Life expectancy (years)",
  "services_gdp"   = "Services, value added (share of GDP)",
  "industry_gdp"   = "Industry, value added (share of GDP)",
  "urban_share"    = "Urban population (share)"
)

msummary(
  models,
  coef_map  = nice_names,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c('*'=0.10,'**'=0.05,'***'=0.01),
  gof_omit  = "AIC|BIC|Log.Lik|RMSE|Std.Errors|Adj|Pseudo|R2 \\(between\\)|R2 \\(overall\\)|^FE",
  gof_map   = gof_map,
  add_rows  = countries_row,
  output    = "fe_table.html"
)

# OLS MODELS
ols1 <- feols(ln_gdp ~ internet_users, data = panel_reg, cluster = ~ code)
ols2 <- feols(ln_gdp ~ internet_users + tertiary_enrol, data = panel_reg, cluster = ~ code)
ols3 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp, data = panel_reg, cluster = ~ code)
ols4 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp, data = panel_reg, cluster = ~ code)
ols5 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp, data = panel_reg, cluster = ~ code)
ols6 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp + urban_share, data = panel_reg, cluster = ~ code)

ols_models <- list("Model 1"=ols1,"Model 2"=ols2,"Model 3"=ols3,"Model 4"=ols4,"Model 5"=ols5,"Model 6"=ols6)

count_countries <- function(vars, data) {
  df <- data[, c("code", vars)]
  df <- df[complete.cases(df), ]
  length(unique(df$code))
}

ols_countries <- c(
  count_countries(c("ln_gdp","internet_users"), panel_reg),
  count_countries(c("ln_gdp","internet_users","tertiary_enrol"), panel_reg),
  count_countries(c("ln_gdp","internet_users","tertiary_enrol","lifeexp"), panel_reg),
  count_countries(c("ln_gdp","internet_users","tertiary_enrol","lifeexp","services_gdp"), panel_reg),
  count_countries(c("ln_gdp","internet_users","tertiary_enrol","lifeexp","services_gdp","industry_gdp"), panel_reg),
  count_countries(c("ln_gdp","internet_users","tertiary_enrol","lifeexp","services_gdp","industry_gdp","urban_share"), panel_reg)
)

ols_countries_row <- tibble::tribble(
  ~term, ~`Model 1`, ~`Model 2`, ~`Model 3`, ~`Model 4`, ~`Model 5`, ~`Model 6`,
  "Countries", as.character(ols_countries[1]), 
               as.character(ols_countries[2]), 
               as.character(ols_countries[3]),
               as.character(ols_countries[4]), 
               as.character(ols_countries[5]), 
               as.character(ols_countries[6])
)


ols_gof_map <- tibble::tribble(
  ~raw,            ~clean,              ~fmt,
  "nobs",          "Observations",        0

)

msummary(
  ols_models,
  coef_map  = nice_names,
  estimate  = "{estimate}{stars}",     
  statistic = "({std.error})",      
  stars     = c('*'=0.10,'**'=0.05,'***'=0.01),
  gof_omit  = "AIC|BIC|Log.Lik|RMSE|Std.Errors|Adj|Pseudo|R2 \\(within\\)|R2 \\(between\\)|R2 \\(overall\\)|^FE",
  gof_map   = ols_gof_map,
  add_rows  = ols_countries_row,
  output    = "ols_table.html"
)

# 6. LIMITATIONS
#VIF test for multicollinearity
vif_summary <- lapply(names(model_list), function(m_name){
  mod <- model_list[[m_name]]
  df <- as.data.frame(t(vif(mod)))
  df$Model <- m_name
  df
}) %>% bind_rows() %>%
  select(Model, everything())

kable(vif_summary, caption = "VIF Values for Models 2–6")

# Cross-section Fisher test
country_obs <- panel_reg %>%
  group_by(code) %>%
  summarise(n_obs = n(), complete_gdp = sum(!is.na(ln_gdp)), .groups = "drop")

median_obs <- median(country_obs$n_obs)
high_count <- sum(country_obs$n_obs > median_obs & country_obs$complete_gdp > 0)
low_count  <- sum(country_obs$n_obs <= median_obs & country_obs$complete_gdp > 0)

high_missing <- sum(country_obs$n_obs > median_obs & country_obs$complete_gdp == 0)
low_missing  <- sum(country_obs$n_obs <= median_obs & country_obs$complete_gdp == 0)

cont_table <- matrix(c(high_count, low_count, high_missing, low_missing), nrow = 2, byrow = TRUE)

cs_p <- fisher.test(cont_table)$p.value
cs_res <- data.frame(Test = "cross_section", p_value = cs_p)

kable(cs_res, caption = "Cross-Section Test Result")




# 7. VALIDITY
#Creating a table with data on country classifications

file_path <- "Data of countries classification.xlsx"
df <- read_excel(file_path, sheet = "Лист1")

df_income <- df %>%
  mutate(across(`2009`:`2019`, ~na_if(., ".."))) %>%
  pivot_longer(cols = `2009`:`2019`, 
               names_to = "Year", 
               values_to = "Income_Group") %>%
  mutate(
    Year = as.numeric(Year),
    Income_Group = factor(Income_Group, 
                          levels = c("L", "LM", "UM", "H"),
                          labels = c("Low", "Lower Middle", "Upper Middle", "High"))
  ) %>%
  rename(year = Year)  
panel_data_with_income <- panel_data %>%
  left_join(df_income, by = c("code", "year"))



# Scatter Plot: GDP per Employed vs Internet Users by Income Group

plot_data <- panel_data_with_income %>%
  filter(!is.na(Income_Group))

scatter_main <- ggplot(plot_data,
                       aes(x = internet_users, 
                           y = gdp_emp,
                           color = Income_Group)) +
  

  geom_point(alpha = 0.6, size = 2) +
  
  # General trend line (black, dotted)
  geom_smooth(aes(group = 1), 
              method = "lm", 
              se = FALSE, 
              color = "black", 
              linetype = "dashed",
              size = 1) +
  
  # Trend lines within groups (colored, solid)
  geom_smooth(method = "lm", se = FALSE, size = 0.8) +
  
  scale_color_brewer(
    name = "Income Group (World Bank)",
    palette = "Set1"
  ) +
  
  labs(
    title = "GDP per Employed vs Internet Users by Income Group",
    subtitle = "Panel: 2009-2019, N = {nrow(plot_data)} country-year observations",
    x = "Internet Users (% of population)",
    y = "GDP per Person Employed (constant 2017 US$)",
    caption = "Solid lines: within-group trends | Dashed line: pooled trend"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10)
  )

scatter_main$labels$subtitle <- paste(
  "Panel: 2009-2019, N =", nrow(plot_data), "country-year observations"
)

print(scatter_main)

ggsave("scatter_income_groups.jpg", scatter_main,
       width = 10, height = 7, dpi = 300)





# Scatter Plot: Separate Relationships by Income Group

scatter_facets_simple <- ggplot(plot_data,
                                aes(x = internet_users, y = gdp_emp)) +
  
  geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  
  facet_wrap(~ Income_Group, ncol = 2, scales = "fixed") +
  

  scale_y_continuous(
    labels = scales::comma_format(),  
    breaks = scales::breaks_extended(n = 8)  
  ) +
  

  scale_x_continuous(
    labels = scales::comma_format(),
    breaks = scales::breaks_extended(n = 6)
  ) +
  
  labs(
    title = "Separate Relationships by Income Group",
    x = "Internet Users (%)",
    y = "GDP per Employed ($)"
  ) +
  
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines"),

    axis.text = element_text(size = 9, color = "black"),
    axis.text.y = element_text(margin = margin(r = 5))
  )

print(scatter_facets_simple)
ggsave("scatter_facets_simple.jpg", scatter_facets_simple, 
       width = 10, height = 8, dpi = 300)




#Descriptive Statistics by Income Group

desc_table <- panel_data_with_income %>%
  filter(!is.na(Income_Group)) %>%
  
  mutate(Income_Group = factor(Income_Group,
                               levels = c("Low", "Lower Middle", 
                                          "Upper Middle", "High"))) %>%
  
  group_by(Income_Group) %>%
  summarise(
    Countries = n_distinct(code),
    Observations = n(),
    
    `GDP per employed` = paste0(
      format(round(mean(gdp_emp, na.rm = TRUE), 0), big.mark = ","),
      " ± ", format(round(sd(gdp_emp, na.rm = TRUE), 0), big.mark = ",")
    ),
    
    `Internet users (%)` = paste0(
      round(mean(internet_users, na.rm = TRUE), 1),
      " ± ", round(sd(internet_users, na.rm = TRUE), 1)
    ),
    
    `Tertiary enrollment (%)` = paste0(
      round(mean(tertiary_enrol, na.rm = TRUE), 1),
      " ± ", round(sd(tertiary_enrol, na.rm = TRUE), 1)
    ),
    
    `Life expectancy (years)` = paste0(
      round(mean(lifeexp, na.rm = TRUE), 1),
      " ± ", round(sd(lifeexp, na.rm = TRUE), 1)
    ),
    
    `Urban population (%)` = paste0(
      round(mean(urban_share, na.rm = TRUE), 1),
      " ± ", round(sd(urban_share, na.rm = TRUE), 1)
    ),
    
    `Services (% of GDP)` = paste0(
      round(mean(services_gdp, na.rm = TRUE), 1),
      " ± ", round(sd(services_gdp, na.rm = TRUE), 1)
    ),
    
    `Industry (% of GDP)` = paste0(
      round(mean(industry_gdp, na.rm = TRUE), 1),
      " ± ", round(sd(industry_gdp, na.rm = TRUE), 1)
    )
  ) %>%
  
  rename(`Income Group` = Income_Group)

print(desc_table)


#APPENDIX:Income-Based Heterogeneity Analysis (Fixed Effects Models)

income_2009 <- df_income %>%
  filter(year == 2009) %>%
  select(code, Income_Group_2009 = Income_Group)

panel_reg_groups <- panel_reg %>%
  left_join(income_2009, by = "code") %>%
  filter(!is.na(Income_Group_2009))

create_group_fe_table <- function(group_name) {
  
  group_data <- panel_reg_groups %>%
    filter(Income_Group_2009 == group_name)

  m1 <- feols(ln_gdp ~ internet_users | code + year, 
              data = group_data, cluster = ~ code)
  m2 <- feols(ln_gdp ~ internet_users + tertiary_enrol | code + year, 
              data = group_data, cluster = ~ code)
  m3 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp | code + year, 
              data = group_data, cluster = ~ code)
  m4 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp | code + year, 
              data = group_data, cluster = ~ code)
  m5 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp | code + year, 
              data = group_data, cluster = ~ code)
  m6 <- feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp + urban_share | code + year, 
              data = group_data, cluster = ~ code)

  models <- list(
    "Model 1" = m1,
    "Model 2" = m2,
    "Model 3" = m3,
    "Model 4" = m4,
    "Model 5" = m5,
    "Model 6" = m6
  )
  
  countries_vec <- map_dbl(models, ~ length(fixef(.x)[["code"]]))
  
  countries_row <- tibble::tribble(
    ~term, ~`Model 1`, ~`Model 2`, ~`Model 3`, 
    ~`Model 4`, ~`Model 5`, ~`Model 6`,
    "Countries", 
    as.character(countries_vec[1]), 
    as.character(countries_vec[2]), 
    as.character(countries_vec[3]),
    as.character(countries_vec[4]), 
    as.character(countries_vec[5]), 
    as.character(countries_vec[6])
  )
  
  gof_map <- tibble::tribble(
    ~raw,        ~clean,        ~fmt,
    "nobs",      "Observations", 0,
    "within.r2", "Within R²",    3
  )
  
  nice_names <- c(
    "internet_users" = "Internet users (share)",
    "tertiary_enrol" = "Tertiary enrollment (share)",
    "lifeexp"        = "Life expectancy (years)",
    "services_gdp"   = "Services, value added (share of GDP)",
    "industry_gdp"   = "Industry, value added (share of GDP)",
    "urban_share"    = "Urban population (share)"
  )
  
  msummary(
    models,
    coef_map  = nice_names,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    stars     = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
    gof_omit  = "AIC|BIC|Log.Lik|RMSE|Std.Errors|Adj|Pseudo|R2 \\(between\\)|R2 \\(overall\\)|^FE",
    gof_map   = gof_map,
    add_rows  = countries_row,
    title     = paste("Table: Fixed Effects Estimates -", group_name, "Income Countries"),
    notes     = c("Standard errors clustered at the country level in parentheses.",
                  "*** p<0.01, ** p<0.05, * p<0.1.",
                  paste("Sample:", group_name, "income countries, 2009-2019")),
    output    = paste0("fe_table_", gsub(" ", "_", group_name), ".html")
  )

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("ГРУППА:", group_name, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Стран:", countries_vec[1], "\n")
  cat("Наблюдений:", nobs(m1), "\n")

  cat("\nКоэффициенты Internet users:\n")
  for(i in 1:6) {
    model <- models[[i]]
    coef_df <- broom::tidy(model)
    internet_coef <- coef_df %>% filter(term == "internet_users")
    
    if(nrow(internet_coef) > 0) {
      sig <- case_when(
        internet_coef$p.value < 0.01 ~ "***",
        internet_coef$p.value < 0.05 ~ "**",
        internet_coef$p.value < 0.10 ~ "*",
        TRUE ~ ""
      )
      cat(sprintf("Model %d: %.3f%s (%.3f) [p=%.3f]\n", 
                  i, internet_coef$estimate, sig, 
                  internet_coef$std.error, internet_coef$p.value))
    }
  }
  
  cat("\nФайл сохранён:", paste0("fe_table_", gsub(" ", "_", group_name), ".html"), "\n")
}

groups <- c("Low", "Lower Middle", "Upper Middle", "High")

for(group in groups) {
  create_group_fe_table(group)
  cat("\n\n")
}

cat("\n", paste(rep("#", 100), collapse = ""), "\n")
cat("СВОДНАЯ ТАБЛИЦА: КОЭФФИЦИЕНТЫ INTERNET USERS ПО ГРУППАМ\n")
cat(paste(rep("#", 100), collapse = ""), "\n\n")

summary_coefs <- data.frame(
  Model = paste("Model", 1:6),
  Low = NA, `Lower.Middle` = NA, `Upper.Middle` = NA, High = NA
)

for(group in groups) {
  group_data <- panel_reg_groups %>%
    filter(Income_Group_2009 == group)

  models <- list(
    m1 = feols(ln_gdp ~ internet_users | code + year, data = group_data, cluster = ~ code),
    m2 = feols(ln_gdp ~ internet_users + tertiary_enrol | code + year, data = group_data, cluster = ~ code),
    m3 = feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp | code + year, data = group_data, cluster = ~ code),
    m4 = feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp | code + year, data = group_data, cluster = ~ code),
    m5 = feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp | code + year, data = group_data, cluster = ~ code),
    m6 = feols(ln_gdp ~ internet_users + tertiary_enrol + lifeexp + services_gdp + industry_gdp + urban_share | code + year, data = group_data, cluster = ~ code)
  )

  for(i in 1:6) {
    model <- models[[i]]
    coef_df <- broom::tidy(model)
    internet_coef <- coef_df %>% filter(term == "internet_users")
    
    if(nrow(internet_coef) > 0) {
      sig <- case_when(
        internet_coef$p.value < 0.01 ~ "***",
        internet_coef$p.value < 0.05 ~ "**",
        internet_coef$p.value < 0.10 ~ "*",
        TRUE ~ ""
      )

      col_name <- gsub(" ", ".", group)
      summary_coefs[i, col_name] <- sprintf("%.3f%s (%.3f)", 
                                            internet_coef$estimate, 
                                            sig, 
                                            internet_coef$std.error)
    }
  }
}






