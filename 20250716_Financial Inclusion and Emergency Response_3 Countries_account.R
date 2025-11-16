################################################################################
# Financial Inclusion and Emergency Response
# Updated: October 23, 2025
################################################################################

## Load libraries
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

################################################################################
# PART I: Data
################################################################################

#Dataset

setwd("C:/Users/Lenovo PC/OneDrive/1 - Thesis/20250509_Data Analyzing_MAIN/MAIN 01 - Dataset and R")
df <- read_excel("20250509_Financial Inclusion_3 countries.xlsx", sheet = "3 countries") %>%
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
# PART VI: Descriptive Visualizations (Plots/Figures/Graphs)
################################################################################

library(ggeffects)
library(ggplot2)
library(dplyr)
library(scales)

source_props <- df_sources %>%
  filter(!is.na(emergency_source_label)) %>%
  count(emergency_source_label) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(round(prop * 100), "%")
  )


#1 - Distribution of Main Emergency Funding Sources Among Respondents

df_sources <- df %>%
  mutate(
    emergency_source_label = case_when(
      fin24 == 1 ~ "Saving",
      fin24 == 2 ~ "Family/Friends",
      fin24 == 3 ~ "Working",
      fin24 == 4 ~ "Loan*",
      fin24 == 5 ~ "Selling Assets",
      fin24 == 6 ~ "Other",
      fin24 == 7 ~ "No Solution",
      TRUE ~ NA_character_
    )
  )
library(writexl)
write_xlsx(source_props, "20251023_Plot_proportion_emergency_sources.xlsx")

#2 - Proportion of Individuals Using Secure Emergency Sources, by financial access group

df_emergency <- df %>%
  filter(!is.na(emergency_source_secure), !is.na(saved), !is.na(account),
         !is.na(female), !is.na(urban), !is.na(emp_in), !is.na(educ),
         !is.na(inc_q), !is.na(mobile), !is.na(internetaccess))

df_emergency %>%
  group_by(saver_type) %>%
  summarise(
    prop_secure = mean(emergency_source_secure, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = saver_type, y = prop_secure, fill = saver_type)) +
  geom_col() +
  geom_text(aes(label = percent(prop_secure, 1)), vjust = -0.5) +
  labs(
    title = "Share of Individuals Using Secure Emergency Sources",
    subtitle = "By Saver Type (Descriptive)",
    x = "Saver Type", y = "Proportion Using Secure Sources"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

secure_share <- df_emergency %>%
  group_by(saver_type) %>%
  summarise(
    prop_secure = mean(emergency_source_secure, na.rm = TRUE),
    n = n()
  )

library(writexl)
write_xlsx(secure_share, "20251023_Plot_secure_source_share_by_financial_access_group.xlsx")




################################################################################
################################################################################
################################################################################
#############################  Extra  ##########################################
################################################################################
################################################################################
################################################################################

library(sampleSelection)

# Bi-Probit model (recursive bivariate probit)
bi_probit_model <- selection(
  selection = saved ~ Sent_domestic_remittances + account + mobile + internetaccess + female + urban + educ + inc_q + emp_in + factor(economycode),
  outcome   = emergency ~ saved + account + mobile + internetaccess + female + urban + educ + inc_q + emp_in + factor(economycode),
  method = "ml",  # Maximum Likelihood
  data = df
)

summary(bi_probit_model)

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

################################################################################
# PART X: Correlation Matrix with p-values
################################################################################

library(Hmisc)
library(reshape2)

# Select numeric variables only
numeric_df <- df %>% select(where(is.numeric))

# Compute correlation matrix with p-values
rcorr_result <- rcorr(as.matrix(numeric_df))

# Convert to long format
corr_matrix <- melt(round(rcorr_result$r, 2))
p_matrix <- melt(rcorr_result$P)

# Combine correlation and p-values
corr_p_table <- cbind(corr_matrix, p_value = p_matrix$value)
colnames(corr_p_table) <- c("Variable_1", "Variable_2", "Correlation", "p_value")

# View
print(head(corr_p_table))

# Export full correlation table
write_xlsx(corr_p_table, "20250816_correlations_with_pvalues.xlsx")

# Filtered correlations for selected key variables
filtered_corr <- corr_p_table %>%
  filter(Variable_1 %in% c("account", "saved"))

write_xlsx(filtered_corr, "20250816_filtered_correlations_with_p_value.xlsx")

####################################################

# Recap

library(stargazer)

#Access to Emergency Funds: Account holders & Savers

model1_probit
model1_logit
stargazer(model1_probit, model1_logit, type = "text")

model2_probit
model2_logit

model3_probit
model3_logit

#Quality of Emergency Response: 
#Type of sources (Secure Vs. Insecure)
#Formal, Informal, None (groups)

model4_probit
model4_logit

model5_probit
model5_logit

#Robustness Check
iv_model_Sent_domestic_remittances

################################################################################

# Summarize by Saver Type # NOT USE !!! #
source_by_saver <- df_sources_2 %>%
  filter(!is.na(emergency_source_label), !is.na(saver_type)) %>%
  group_by(saver_type, emergency_source_label, source_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(saver_type) %>%
  mutate(prop = n / sum(n))

# Plot
ggplot(source_by_saver, aes(x = emergency_source_label, y = prop, fill = source_type)) +
  geom_col() +
  facet_wrap(~saver_type) +
  scale_fill_manual(values = c("Secure" = "steelblue", "Insecure" = "tomato")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Emergency Source Breakdown by Saver Type",
    subtitle = "Colored by Source Type (Secure vs. Insecure)",
    x = "Emergency Source", y = "Proportion",
    fill = "Source Type",
    caption = "Note: *Loan = bank/employer/private lender."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

library(writexl)
write_xlsx(source_by_saver, "20251023_Plot_emergency_source_by_saver_type.xlsx")

### 3 - Emergency Source Breakdown by Saver Type


ggplot(source_props, aes(x = reorder(emergency_source_label, -prop), y = prop, fill = emergency_source_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Emergency Sources Used",
    subtitle = "Based on fin24 responses",
    x = "Emergency Source", y = "Proportion of Respondents",
    caption = "Note: *Loan = bank/employer/private lender."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

df_sources_2 <- df_sources %>%
  mutate(
    source_type = case_when(
      fin24 %in% c(1, 4) ~ "Secure",
      fin24 %in% c(2, 3, 5, 6, 7) ~ "Insecure",
      TRUE ~ NA_character_
    )
  )
