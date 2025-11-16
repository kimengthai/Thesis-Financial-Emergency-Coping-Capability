## libraries
library(sampleSelection)
library(dplyr)
library(modelsummary)
library(car)
library(ggplot2)
library(broom)
library(janitor)
library(readxl)
library(performance)
library(ggeffects)
library(AER)
library(margins)
library(here)

################################################################################
# PART I: Data
################################################################################

#Dataset
df <- read_excel(
  here("data", "20250509_Financial Inclusion_3 countries.xlsx"),
  sheet = "3 countries"
) %>%
  clean_names()

# Variables

df <- df %>%
  mutate(
    #country fixed effect
    economycode = factor(economycode),
    # Outcome variables
    emergency = case_when(
      fin24 == 7 ~ 0,
      fin24 %in% 1:6 ~ 1,
      TRUE ~ NA_real_
    ),
    emergency_source_secure = case_when(
      fin24 %in% c(1, 4) ~ 1,
      fin24 %in% c(2, 3, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    #Predictors and controls
    account = ifelse(account == 1, 1, ifelse(account == 0, 0, NA)),
    saved = ifelse(saved == 1, 1, ifelse(saved == 0, 0, NA)),
    mobile = ifelse(mobileowner == 1, 1, ifelse(mobileowner == 2, 0, NA)),
    internetaccess = ifelse(internetaccess == 1, 1, ifelse(internetaccess == 2, 0, NA)),
    female = ifelse(female == 1, 1, ifelse(female == 2, 0, NA)),
    urban = ifelse(urbanicity_f2f == 2, 1, ifelse(urbanicity_f2f == 1, 0, NA)),
    emp_in = ifelse(emp_in == 1, 1, ifelse(emp_in == 2, 0, NA)),
    educ = ifelse(educ %in% 1:3, educ, NA),
    inc_q = ifelse(inc_q %in% 1:5, inc_q, NA),
    
    saver_type = case_when(
      saved == 1 & account == 1 ~ "formal",
      saved == 1 & account == 0 ~ "informal",
      saved == 0 ~ "none"
    ),
    saver_type = factor(saver_type, levels = c("none", "informal", "formal")),
    
    #IV Instrument
    Sent_domestic_remittances = case_when(
      fin26 == 1 ~ 1,     
      fin26 == 2 ~ 0,     
      fin26 %in% c(3, 4) ~ NA_real_,
      TRUE ~ NA_real_  
    ))

################################################################################
# PART II: Emergency Fund Access Models (Model 1–3)
################################################################################

# Model 1: Financial Account and Emergency Fund Access
model1_probit <- glm(emergency ~ account + mobile + internetaccess +
                       female + urban + educ + inc_q + emp_in + economycode,
                     data = df, family = binomial(link = "probit"))

model1_logit <- glm(emergency ~ account + mobile + internetaccess +
                      female + urban + educ + inc_q + emp_in + economycode,
                    data = df, family = binomial(link = "logit"))

# Model 2: Saving and Emergency Fund Access
model2_probit <- glm(emergency ~ saved + mobile + internetaccess +
                       female + urban + educ + inc_q + emp_in + economycode,
                     data = df, family = binomial(link = "probit"))

model2_logit <- glm(emergency ~ saved + mobile + internetaccess +
                      female + urban + educ + inc_q + emp_in + economycode,
                    data = df, family = binomial(link = "logit"))

# Model 3: Account + Saving and Emergency Fund Access (with controls)
model3_probit <- glm(emergency ~ account + saved + mobile + internetaccess +
                       female + urban + educ + inc_q + emp_in + economycode,
                     data = df, family = binomial(link = "probit"))

model3_logit <- glm(emergency ~ account + saved + mobile + internetaccess +
                      female + urban + educ + inc_q + emp_in + economycode,
                    data = df, family = binomial(link = "logit"))


### Summary Result Table 1 : COMBINING Model 1 to  3:
library(modelsummary)
modelsummary(
  list(
    "Model 1: Account (Probit)" = model1_probit,
    "Model 1: Account (Logit)"  = model1_logit,
    "Model 2: Saved (Probit)"   = model2_probit,
    "Model 2: Saved (Logit)"    = model2_logit,
    "Model 3: Full (Probit)"    = model3_probit,
    "Model 3: Full (Logit)"     = model3_logit
  ),
  title = "Table 1: Financial Inclusive and Emergency Fund Access (Probit and Logit)",
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = "p.value",
  coef_map = c(
    account        = "Has Account",
    saved          = "Saved Money",
    mobile         = "Owns Mobile",
    internetaccess = "Internet Access",
    female         = "Female",
    urban          = "Urban",
    educ           = "Education Level",
    inc_q          = "Income Quintile",
    emp_in         = "In Workforce"
  ),
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  notes = "Note: Each cell shows the coefficient estimate followed by significance stars, with 95% confidence intervals in square brackets. P-values are reported in parentheses. *, **, 
  and *** indicate statistical significance at the 10%, 5%, and 1% levels, respectively.",
  output = "20251023_Table1_Financial_Inclusive_and_Emergency_Fund_Model1_3.xlsx"
)

#html
#Excel: "20251023_Table1_Financial_Inclusive_and_Emergency_Fund_Model1_3.xlsx"

### Summary Result Table 2: Marginal Effects for Model 1–3

modelsummary(
  list(
    "Model 1: Account" = margins(model1_probit),
    "Model 2: Saved"   = margins(model2_probit),
    "Model 3: Combine of Account and Saved"= margins(model3_probit)
  ),
  title = "Emergency Secure Models (Marginal Effects)",
  estimate = "{estimate}{stars}",   
  statistic = "p.value",            
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  notes = "Note: Each cell shows the coefficient estimate followed by significance stars, with 95% confidence intervals in square brackets. P-values are reported in parentheses. *, **, 
  and *** indicate statistical significance at the 10%, 5%, and 1% levels, respectively.",
  output = "20251023_Table2_marginal_effects_Model1_3.xlsx"
)

#html
#Excel: 20251023_Table2_marginal_effects_Model1_3.xlsx


################################################################################
# PART III: Emergency Source Security Models (Model 4–5)
################################################################################

# Filter for valid data
df_emergency <- df %>%
  filter(!is.na(emergency_source_secure), !is.na(saved), !is.na(account),
         !is.na(female), !is.na(urban), !is.na(emp_in), !is.na(educ),
         !is.na(inc_q), !is.na(mobile), !is.na(internetaccess))

# Model 4: Saved + Account and Secure Emergency Sources
model4_probit <- glm(emergency_source_secure ~ saved + account + mobile + internetaccess +
                       female + urban + emp_in + educ + inc_q + economycode,
                     data = df_emergency, family = binomial(link = "probit"))

model4_logit <- glm(emergency_source_secure ~ saved + account + mobile + internetaccess +
                      female + urban + emp_in + educ + inc_q + economycode,
                    data = df_emergency, family = binomial(link = "logit"))

# Model 5: Saver Type and Secure Emergency Sources
model5_probit <- glm(emergency_source_secure ~ saver_type + mobile + internetaccess +
                       female + urban + emp_in + educ + inc_q + economycode,
                     data = df_emergency, family = binomial(link = "probit"))

model5_logit <- glm(emergency_source_secure ~ saver_type + mobile + internetaccess +
                      female + urban + emp_in + educ + inc_q + economycode,
                    data = df_emergency, family = binomial(link = "logit"))

### Summary Result Table 3: Combing model 4 and 5
modelsummary(
  list(
    "Probit: Saved + Account"   = model4_probit,
    "Logit: Saved + Account"    = model4_logit,
    "Probit: Saver Type"        = model5_probit,
    "Logit: Saver Type"         = model5_logit
  ),
  title = "Emergency Source Security (Saved + Account vs. Saver Type)",
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = "p.value",
  coef_map = c(
    saved = "Saved",
    account = "Has Account",
    saver_typeinformal = "Informal Saver",
    saver_typeformal = "Formal Saver",
    mobile = "Owns Mobile",
    internetaccess = "Internet Access",
    female = "Female",
    urban = "Urban",
    emp_in = "In Workforce",
    educ = "Education Level",
    inc_q = "Income Quintile"
  ),
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  notes = "Note: Each cell shows the coefficient estimate followed by significance stars, with 95% confidence intervals in square brackets. P-values are reported in parentheses. *, **, 
  and *** indicate statistical significance at the 10%, 5%, and 1% levels, respectively.",
  output = "20251023_Table3_combined_emergency_secure.xlsx"
)

#html
#Excel: 20251023_Table3_combined_emergency_secure.xlsx

#####

### Summary Result Table 4:Marginal Effects for Emergency Secure Models

modelsummary(
  list(
    "Model 4: Saved and Account" = margins(model4_probit),
    "Model 5: Saver Type"      = margins(model5_probit)
  ),
  title = "Emergency Secure Models (Marginal Effects)",
  estimate = "{estimate}{stars}",   
  statistic = "p.value",            
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  notes = "Note: Each cell shows the coefficient estimate followed by significance stars, with 95% confidence intervals in square brackets. P-values are reported in parentheses. *, **, 
  and *** indicate statistical significance at the 10%, 5%, and 1% levels, respectively.",
  output = "20251023_Table4_Marginal_Effect_Emergency_Secure_Sources.xlsx"
)

#html
#Excel: 20251023_Table4_Marginal_Effect_Emergency_Secure_Sources.xlsx

################################################################################
# PART IV: Robustness Check – Instrumental Variable (IV) Probit Model
# Goal: Test whether saving behavior (endogenous) affects emergency fund access,
#       using remittances as an instrument in a two-stage IV probit model
################################################################################

library(sampleSelection)
library(dplyr)
library(broom)
library(modelsummary)
library(writexl)
library(gt)
library(stringr)

# Instrument: Sent_domestic_remittances
# Stage 1: saved ~ instrument (remittances) + controls
# Stage 2: emergency ~ saved + controls

iv_model_Sent_domestic_remittances <- selection(
  selection = saved ~ Sent_domestic_remittances + account + mobile + internetaccess +
    female + urban + educ + inc_q + emp_in + factor(economycode),
  
  outcome = emergency ~ saved + account + mobile + internetaccess +
    female + urban + educ + inc_q + emp_in + factor(economycode),
  
  data = df,
  method = "ml"  # Maximum likelihood
)

iv_model_Sent_domestic_remittances

stage1 <- tidy(iv_model_Sent_domestic_remittances, component = "selection") %>%
  mutate(Stage = "Stage 1: Selection (Saved ~ Remittances)")

stage2 <- tidy(iv_model_Sent_domestic_remittances, component = "outcome") %>%
  mutate(Stage = "Stage 2: Outcome (Emergency ~ Saved)")


library(dplyr)
library(modelsummary)

# Combine and format results
combined <- bind_rows(stage1, stage2) %>%
  mutate(
    Estimate = paste0(
      round(estimate, 3),
      case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      )
    ),
    `Std. Error` = round(std.error, 3)
  ) %>%
  select(Stage, term, Estimate, `Std. Error`) %>%
  rename(Variable = term)

# Export custom summary table
datasummary_df(
  combined,
  title = "20251023_table5_iv_probit_model.xlsx",
  output = "html"
)

#html
#excel: 20251023_table5_iv_probit_model.xlsx


################################################################################
# PART V: Summary Statistics Table 
################################################################################

library(psych)
library(dplyr)


# Compute summary statistics
summary_stats <- describe(df)

# Clean and select relevant columns
summary_stats_clean <- summary_stats %>%
  select(n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se) %>%
  mutate(variable = rownames(summary_stats)) %>%
  relocate(variable)

# Export to Excel
library(writexl)
write_xlsx(summary_stats_clean, "20251023_summary_statistics.xlsx")

################################################################################

# Test IV

library(AER)

# First-stage regression: regress 'saved' on 'remit' + controls
first_stage <- lm(saved ~ Sent_domestic_remittances + account + mobile + internetaccess +
                    female + urban + educ + inc_q + emp_in + factor(economycode), data = df)
summary(first_stage)

# Extract F-statistic for the instrument
fs_test <- summary(first_stage)$fstatistic
fs_f_value <- fs_test[1]
fs_p_value <- pf(fs_test[1], df1 = fs_test[2], df2 = fs_test[3], lower.tail = FALSE)

cat("First-stage F-statistic:", fs_f_value, "\n")
cat("p-value:", fs_p_value, "\n")

# IV estimation using 2SLS
library(AER)
iv_model <- ivreg(
  emergency ~ saved + account + mobile + internetaccess + female + urban + educ + inc_q + emp_in + factor(economycode) |
    Sent_domestic_remittances + account + mobile + internetaccess + female + urban + educ + inc_q + emp_in + factor(economycode),
  data = df
)
summary(iv_model)

# Result Table
library(stargazer)
stargazer(first_stage, iv_model, type = "text",
          title = "First-Stage and Instrumental Variable (IV) Regression Results",
          column.labels = c("First Stage", "Second Stage IV"),
          dep.var.labels = c("Savings", "Coping Capability"),out = "20251028_IV_Regression_Results.txt")
