library(dplyr)
library(ggplot2)
library(gplots)
library(purrr)
library(tidyr)
library(multcomp)
library(stats)
library(haven)
library(naniar)
library(broom)
library("mice")
library(fastDummies)
library(lavaan)
library(patchwork)
library(car)
library(gt)
library(cowplot)
library(DT)
library(lme4)
library(lmerTest)
# Import data and select relative columns
data <- read_sav("dataverse_files/STAR_Students.sav")
which(colnames(data) == "g1schid")
which(colnames(data) == "g1selfconcraw")
grade_1 <- data[, c(1:6,8,55:81)]
grade_1$g1classtype <- data$g1classtype
# Data set exploration
# First of all, we divide the student into two part: involved in STAR or not, based on 'FLAGSG1'
star_not_involved <- grade_1 %>% filter(FLAGSG1 == 0)
star_involved <- grade_1 %>% filter(FLAGSG1 == 1)

# Remove student id and FLAGSG1
star_involved <- star_involved %>% dplyr::select(-FLAGSG1)
star_involved <- star_involved %>% dplyr::select(-stdntid)

# Categorical variable preparation
star_involved$race <- as.factor(star_involved$race)
star_involved$gender <- as.factor(star_involved$gender)
star_involved$g1freelunch <- as.factor(star_involved$g1freelunch)
star_involved$g1surban <- as.factor(star_involved$g1surban)
star_involved$g1tgen <- as.factor(star_involved$g1tgen)
star_involved$g1thighdegree <- as.factor(star_involved$g1thighdegree)
star_involved$g1trace <- as.factor(star_involved$g1trace)
star_involved$g1tcareer <- as.factor(star_involved$g1tcareer)
star_involved$g1classtype <- as.factor(star_involved$g1classtype)
star_involved$g1schid <- as.factor(star_involved$g1schid)

# Categories rename
star_involved$race <- factor(star_involved$race, levels = c("1", "2", "3", "4", "5", "6"), labels = c("White", "Black","Asian","Hispanic","Native American", "Other"))
star_involved$gender <- factor(star_involved$gender, levels = c("1", "2"), labels = c("Male", "Female"))
star_involved$g1surban <- factor(star_involved$g1surban, levels = c("1", "2", "3", "4"), labels = c("Inner City", "Suburban", "Rural", "Urban"))
star_involved$g1freelunch <- factor(star_involved$g1freelunch, levels = c("1", "2"), labels = c("Free-lunch", "Non-free lunch"))
star_involved$g1classtype <- factor(star_involved$g1classtype, levels = c("1", "2", "3"), labels = c("Small", "Regular","Regular_Aide"))
# explore missing value overall
sapply(star_involved, function(y) sum(length(which(is.na(y)))))
sapply(star_not_involved, function(y) sum(length(which(is.na(y)))))
na_counts <- sapply(star_involved, function(y) sum(is.na(y)))
total_rows <- nrow(star_involved)

na_percentage <- sapply(star_involved, function(y) mean(is.na(y))) * 100

na_data <- data.frame(Variables = names(na_counts), 
                      NA_Count = na_counts, 
                      Percentage = na_percentage)

# Create the bar plot with percentages labeled on each bar
ggplot(na_data, aes(x = Variables, y = NA_Count, label = sprintf("%.1f%%", Percentage))) +
  geom_bar(stat = "identity", fill = "lightpink") +
  geom_text(position = position_stack(vjust = 0.5), size = 2) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Column", y = "Number of Missing Values", 
       title = "Missing values in variables of 1st-year students involved in STAR")
# Logistic regression to check bias from missing value
star_involved_log <- star_involved %>%
  mutate(g1tmathss_missing = as.numeric(is.na(g1tmathss))) %>%
  dplyr::select(race, gender, g1freelunch, g1surban, g1tmathss_missing, g1tgen, g1thighdegree, g1trace, g1tcareer, g1classtype)

star_involved_log$race <- as.factor(star_involved_log$race)
star_involved_log$gender <- as.factor(star_involved_log$gender)
star_involved_log$g1freelunch <- as.factor(star_involved_log$g1freelunch)
star_involved_log$g1surban <- as.factor(star_involved_log$g1surban)
star_involved_log$g1tgen <- as.factor(star_involved_log$g1tgen)
star_involved_log$g1thighdegree <- as.factor(star_involved_log$g1thighdegree)
star_involved_log$g1trace <- as.factor(star_involved_log$g1trace)
star_involved_log$g1tcareer <- as.factor(star_involved_log$g1tcareer)
star_involved_log$g1classtype <- as.factor(star_involved_log$g1classtype)

missing_model <- glm(g1tmathss_missing ~ race + gender + g1freelunch + g1surban + g1tgen + g1thighdegree + g1trace + g1tcareer+ g1classtype, 
                     data = star_involved_log, family = "binomial")
summary(missing_model)
# Little test for MCAR
little_test_subset <- star_involved %>%
  dplyr::select(g1tmathss, g1freelunch, g1mathbsobjpct,g1mathbsobjraw,g1mathbsraw,g1tmathss, g1promote,g1absent,g1present)
naniar::mcar_test(little_test_subset)
# Median impute
columns_num <- c("g1tmathss")
median_imputed <- star_involved %>%
  dplyr::mutate(across(all_of(columns_num), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
sum(is.na(median_imputed$g1tmathss))
# apply regression impute to numerical variables
math_score_involved <- data %>% filter(FLAGSG1 == 1)
math_score <- math_score_involved[c("gktmathss","g1tmathss","g2tmathss")]
other_score <- math_score_involved[c("g1treadss","g1tlistss","g1wordskillss")]
regression_imputed_data <- cbind(math_score,other_score)
regression_imputed <- star_involved
# predict g1tmathss
observed_data <- regression_imputed_data[!is.na(regression_imputed_data$g1tmathss), ]
missing_data <- regression_imputed_data[is.na(regression_imputed_data$g1tmathss), ]
model <- lm(g1tmathss ~ ., data = observed_data)
predicted_values <- predict(model, newdata = missing_data)
regression_imputed$g1tmathss[is.na(regression_imputed$g1tmathss)] <- predicted_values
sum(is.na(regression_imputed$g1tmathss))
sum(is.na(math_score_involved$g1tmathss))
# Missing value pattern
gg_miss_upset(regression_imputed_data, nsets = 10, nintersects = 10)
# Hetrogenity analysis
grade_1$FLAGSG1 <- factor(grade_1$FLAGSG1, labels = c("Students not-involved in STAR", "Students involved in STAR"))
grade_1$race <- factor(grade_1$race, labels = c("White", "Black","Asian","Hispanic","Native American", "Other"))

race_distribution <- grade_1 %>%
  group_by(FLAGSG1, race) %>%
  summarise(Count = n(), .groups = 'drop')

race_distribution <- race_distribution %>% 
  filter(!is.na(race))

race_distribution$race <- as.character(race_distribution$race)
race_distribution$race <- ifelse(race_distribution$race %in% c("Asian", "Hispanic", "Native American", "Other"), "Others", race_distribution$race)
race_distribution$race <- factor(race_distribution$race)

# calculate proportions after combining races
race_distribution <- race_distribution %>%
  group_by(FLAGSG1, race) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  group_by(FLAGSG1) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

# STAR data pie chart (involved and non-involved)
star_race_plot <- ggplot(race_distribution, aes(x = "", y = Proportion, fill = race)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~ FLAGSG1, ncol = 1) +
  geom_label(aes(label = sprintf("%.1f%%", Proportion * 100)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Race", y = "Proportion", title = "STAR Project Race Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("White" = "#F8766D", "Black" = "#00BA38", "Others" = "#619CFF"))

# National reference pie chart
racial_composition <- data.frame(
  race = c("White", "Black", "Others"),
  percentage = c(77, 12, 8 + 2 + 0.6 + 0.2)  # Combine percentages for non-White and non-Black
)

national_race_plot <- ggplot(racial_composition, aes(x = "", y = percentage, fill = race)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_label(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
  labs(fill = "Race", title = "National Racial Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("White" = "#F8766D", "Black" = "#00BA38", "Others" = "#619CFF"))

combined_plot <- star_race_plot + national_race_plot + plot_layout(widths = c(2, 1))
combined_plot

# urban analysis
star_involved_cleaned <- star_involved %>%
  filter(!is.na(race),!is.na(g1freelunch))
star_involved_cleaned$race <- as.character(star_involved_cleaned$race)
star_involved_cleaned$race <- ifelse(star_involved_cleaned$race %in% c("White", "White"), "White",
                                     ifelse(star_involved_cleaned$race %in% c("Black", "Black"), "Black", "Other"))

star_involved_cleaned$race <- factor(star_involved_cleaned$race, levels = c("White", "Black", "Other"))


stacked_bar_plot <- ggplot(star_involved_cleaned, aes(x = g1surban, fill = race)) +
  geom_bar(position = "stack") +
  labs(x = "School Urbanicity", y = "Count", fill = "Race",
       title = "School Urbanicity Distribution by Race") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10)) 


# Stacked bar plot
free_lunch_distribution_plot <- ggplot(star_involved_cleaned, aes(x = g1freelunch, fill = race)) +
  geom_bar(position = "stack", width = 0.5) +
  labs(x ="Lunch Status" , y = "Count", fill = "Race",
       title = "Free Lunch Distribution by Race") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10)) 

plot_grid(stacked_bar_plot, free_lunch_distribution_plot, ncol=2)
# Univariate descriptive statistics
# g1tmathss
descriptive_stats <- star_involved %>%
  summarise(
    Statistic = c("Mean", "Median", "Standard Deviation", "1st Quartile", "3rd Quartile", "Missing Values"),
    Value = c(round(mean(g1tmathss, na.rm = TRUE), 2),
              round(median(g1tmathss, na.rm = TRUE), 2),
              round(sd(g1tmathss, na.rm = TRUE), 2),
              round(quantile(g1tmathss, 0.25, na.rm = TRUE), 2),
              round(quantile(g1tmathss, 0.75, na.rm = TRUE), 2),
              round(sum(is.na(g1tmathss)), 2))
  )

# statistics table
descriptive_stats_table <- gt(descriptive_stats) %>%
  tab_header(
    title = md("**Descriptive Statistics for First Grade Math Scores (SATs)**")
  ) %>%
  tab_options(
    table.font.size = px(12), # Adjust general font size
    data_row.padding = px(5), # Adjusts padding inside cells
    column_labels.padding = px(5) # Adjusts padding in column labels
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = vars(Statistic, Value)
    )
  ) 

descriptive_stats_table
# explore the 4 math scores distribution by hist plot with density line
p1 <- ggplot(data = star_involved, aes(x = g1mathbsraw)) +
  geom_histogram(aes(y = ..density..), binwidth = 3, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Math Raw Score BSF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 9))
p2 <- ggplot(data = star_involved, aes(x = g1tmathss)) +
  geom_histogram(aes(y = ..density..), binwidth = 15, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Total Math Scale Score SAT") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 9))
p3 <- ggplot(data = star_involved, aes(x = g1mathbsobjraw)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightgrey") + 
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Math number Objectives Mastered BSF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 9))
p4 <- ggplot(data = star_involved, aes(x = g1mathbsobjpct)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Math Percent Objectives Mastered BSF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 9))

combined_plots <- p1 + p2 + p3 + p4 + 
  plot_layout(ncol = 2)

print(combined_plots)
# Check math score normality from q-q plot 
ggplot(data = star_involved, aes(sample = g1tmathss)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of math scores g1tmathss vs. Normal Distribution") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")
# Math scores mean and median compare
summary_star <- star_involved %>%
  group_by(g1tchid,g1classtype,g1classsize,g1schid,g1surban) %>%
  dplyr::summarize(mean = mean(g1tmathss, na.rm = TRUE), 
                   median = median(g1tmathss, na.rm = TRUE), 
                   sd = sd(g1tmathss, na.rm = TRUE), 
                   quantile = IQR(g1tmathss, na.rm = TRUE), 
                   count = n(), 
                   black_ratio = sum(race == "Black") / count,  # Calculate black ratio
                   free_lunch_ratio = sum(g1freelunch == "Free-lunch") / count,  # Calculate free lunch ratio
                   .groups = 'drop' )

summary_star_long <- summary_star %>%
  gather(key = "Statistic", value = "Score", mean, median)

# Hist plot with density lines - mean and median
mean_median_compare <- ggplot(summary_star_long, aes(x = Score, color = Statistic, group = Statistic)) +
  geom_density() +
  facet_wrap(~ g1classtype) +
  labs(x = "Math Score", y = "Density", title = "Density of Mean and Median Math Scores by Class Type") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  scale_color_brewer(palette = "Set1", name = "Measure")

print(mean_median_compare)
# Univariate descriptive analysis cont
# Categorical variables
long_data <- star_involved %>%
  dplyr::select(gender, g1surban, g1freelunch,g1classtype) %>%
  pivot_longer(cols = everything(), names_to = "Attribute", values_to = "Value") %>%
  count(Attribute, Value) %>%
  group_by(Attribute) %>%
  mutate(Proportion = n / sum(n))

gender_data <- long_data %>% filter(Attribute == "gender")
urbanity_data <- long_data %>% filter(Attribute == "g1surban")
freelunch_data <- long_data %>% filter(Attribute == "g1freelunch")
classtype_data <- long_data %>% filter(Attribute == "g1classtype")

# Pie chart for Gender distribution
gender_pie_chart <- ggplot(gender_data, aes(x = 1, y = Proportion, fill = Value)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste0(round(Proportion * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(y = "", fill = "Gender", title = "Gender Distribution") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


# Pie chart for Urbanicity distribution
urbanity_pie_chart <- ggplot(urbanity_data, aes(x = 1, y = Proportion, fill = Value)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste0(round(Proportion * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(y = "", fill = "urbanicity", title = "Urbanicity Distribution") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# Pie chart for Free Lunch distribution
freelunch_pie_chart <- ggplot(freelunch_data, aes(x = 1, y = Proportion, fill = Value)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste0(round(Proportion * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(y = "", fill = "Free Lunch", title = "Free Lunch Distribution") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# Pie chart for Class Type distribution
classtype_pie_chart <- ggplot(classtype_data, aes(x = 1, y = Proportion, fill = Value)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste0(round(Proportion * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(y = "", fill = "Class Type", title = "CLass Type Distribution") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) 

combined_pie_chart_1 <- plot_grid(gender_pie_chart, urbanity_pie_chart, ncol = 2)
combined_pie_chart_2 <- plot_grid(freelunch_pie_chart, classtype_pie_chart, ncol = 2)

print(combined_pie_chart_1)
print(combined_pie_chart_2)
# Main effect plot for class type
plotmeans(median~g1classtype,data=summary_star,xlab="Class type",ylab="Math score", main="Main effect plot of class type",cex.lab=1) 
# Class size vs math scores
scatter_plot <- ggplot(summary_star, aes(x = g1classsize, y = median)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Math socres among class size",
       x = "Class Size",
       y = "Median Math Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(scatter_plot)
# School ID vs math scores
summary_school_star <- star_involved %>%
  group_by(g1schid,g1surban) %>%
  dplyr::summarize(median = median(g1tmathss, na.rm = TRUE), .groups = 'drop' )

line_plot <- ggplot(summary_school_star, aes(x = factor(g1schid), y = median, group = 1)) +
  geom_line() +
  geom_point() +  # Adding points for visibility of each school's median score
  labs(title = "Median Math Scores Distribution Among Schools",
       x = "School ID",
       y = "Median Math Score") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))

print(line_plot)
## Interaction plot distinguishing math scores from school and class types
class_colors <- rainbow(length(unique(star_involved$g1classtype)))
school_colors <- rainbow(length(unique(star_involved$g1schid)))

interaction.plot(summary_star$g1classtype, summary_star$g1schid, summary_star$median, 
                 col = class_colors[star_involved$g1classtype],
                 cex.lab = 1.5, ylab = "Math score", xlab = 'Class Type', main = "Class type-school interaction plot")
# Multivariate descriptive analysis cont
# Urbanicity
urban_box <- ggplot(data = summary_school_star) +
  geom_boxplot(mapping = aes(x = g1surban, y = median, fill = g1surban)) +
  labs(title = "Median math scores among urbanicity", 
       x = "Class Type", 
       y = "Median g1tmathss") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

# Race
star_involved_filtered <- star_involved %>% 
  filter(!is.na(race),!is.na(g1freelunch)) %>%
  mutate(race = case_when(
    race == "White" ~ "White",
    race == "Black" ~ "Black",
    race %in% c("Asian","Hispanic","Native American", "Other") ~ "Other",
    TRUE ~ race  # Keep for unforeseen values
  ))

star_involved_filtered$race <- factor(star_involved_filtered$race)

race_box <- ggplot(data = star_involved_filtered) +
  geom_boxplot(mapping = aes(x = race, y = g1tmathss, fill = race)) +
  labs(title = "Median math scores among race", 
       x = "Race", 
       y = "Median math score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

# Free-lunch
star_involved_filtered$g1freelunch <- factor(star_involved_filtered$g1freelunch)
lunch_box <- ggplot(data = star_involved_filtered) +
  geom_boxplot(mapping = aes(x = g1freelunch , y = g1tmathss, fill = g1freelunch)) +
  labs(title = "Median math scores among free-lunch", 
       x = "Free-lunch Type", 
       y = "Median math score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

plot_grid(urban_box, race_box, lunch_box, ncol=3)
# Main effect plot of urban, race and free-lunch
par(mfrow = c(1, 3), mar = c(2, 2, 2, 1))

plotmeans(g1tmathss ~ g1surban, data = star_involved_filtered, xlab = "Urbanicity", ylab = "Math score",
          main = "Main effect plot of urbanicity")

plotmeans(g1tmathss ~ race, data = star_involved_filtered, xlab = "Race", ylab = "Math score",
          main = "Main effect plot of race")

plotmeans(g1tmathss ~ g1freelunch, data = star_involved_filtered, xlab = "Free-lunch", ylab = "Math score",
          main = "Main effect plot of free-lunch")
# Test for interactions 
full_model=lm(median ~ g1classtype + g1schid + g1classtype*g1schid, data = summary_star);
reduced_model=lm(median ~ g1classtype + g1schid,data= summary_star); 
anova_result <- anova(reduced_model,full_model)

anova_table <- gt(anova_result) 

anova_table_customized <- gt(anova_result) %>%
  tab_header(
    title = "ANOVA Comparison Between Reduced and Full Models",
    subtitle = "Assessing the Interaction Effect"
  ) %>%
  fmt_number(
    columns = c("Res.Df", "RSS", "Df", "Sum of Sq", "F", "Pr(>F)"),
    decimals = 2
  ) %>%
  cols_label(
    Res.Df = "Residual Degrees of Freedom",
    RSS = "Residual Sum of Squares",
    Df = "Degrees of Freedom",
    `Sum of Sq` = "Sum of Squares",
    F = "F-value",
    `Pr(>F)` = "p-value"
  )

anova_table_customized <- anova_table_customized %>%
  tab_options(
    table.font.size = 12,  # Adjusts overall font size
    heading.title.font.size = 14,  # Adjusts title font size
    heading.subtitle.font.size = 12  # Adjusts subtitle font size
  ) %>%
  tab_style(
    style = cell_text(size = 10),  # Adjuststext size for table body
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(size = 12, weight = "bold"),  # Adjusts text size for column labels
    locations = cells_column_labels(columns = everything())
  )

anova_table_customized
# Fitted mixed-effect model
model_mix_anova <- lmer(median ~ g1classtype + (1 | g1schid), data = summary_star)

# Calculate the proportion of variability due to schools
# Extract variance components
var_comp <- as.data.frame(VarCorr(model_mix_anova))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model_mix_anova), "sc")^2  # Residual variance

# Calculate the proportion of variability due to variability between schools
school_var_prop <- school_var / (school_var + residual_var)

summary_model <- summary(model_mix_anova)

# Extract fixed effects coefficients
fixed_effects <- summary_model$coefficients

fixed_effects_df <- as.data.frame(fixed_effects)

fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <- cbind(rownames(fixed_effects_df),fixed_effects_df)

fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Mixed-effect Model"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  # Print the table
  print(fixed_effects_table)
# Turkey HSD test for class type
anova.fit_class <- aov(median ~ g1classtype, data = summary_star)
tukey_test <- TukeyHSD(anova.fit_class, conf.level = 1 - 0.05)
plot(tukey_test, cex.axis = 0.8, cex.lab = 0.8)
# Causal effects plot for model 1
coefficients <- c(-12.35, -12.63)  # Coefficient estimates
std_errors <- c(2.32, 2.41)     # Standard errors
predictor_names <- c("Regular", "Regular_Aide")

effect_data_1 <- data.frame(predictor = predictor_names, 
                            coefficient = coefficients,
                            std_error = std_errors)

causal_plot_1 <- ggplot(effect_data_1, aes(x = predictor, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black",width = 0.3) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                width = 0.3, color = "black") +
  labs(x = "Predictor Variable", y = "Coefficient Estimate",
       title = "Causal Effects of Predictor Variables on Math Scores (Model 1)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(causal_plot_1)
# Model diagnostics
par(mfrow = c(2, 2))

# Check for homoscedasticity by Residuals vs Fitted plot
plot(predict(model_mix_anova), residuals(model_mix_anova), 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Check for normality of residuals by normal Q-Q Plot
qqnorm(resid(model_mix_anova),main="Normal Q-Q Plot of residuals")
qqline(resid(model_mix_anova), col = "red")


# Check for normality of random effect by normal Q-Q Plot
random_effects <- ranef(model_mix_anova)$g1schid[, "(Intercept)"]
random_effects_named <- as.numeric(random_effects)
qqnorm(random_effects_named, main = "Normal Q-Q Plot of random effects")
qqline(random_effects_named, col = "red")

# Calculate correlation of residuals and random effects
random_effects <- ranef(model_mix_anova)$g1schid
random_effects_new <- cbind(id = rownames(random_effects), x = random_effects$`(Intercept)`)


residuals <- residuals(model_mix_anova)
residuals_aggregated <- aggregate(residuals, by = list(summary_star$g1schid), FUN = mean)

# Extract aggregated residuals and random effects
residuals_school <- residuals_aggregated$x
random_effects_school <- random_effects_new[, "x"]
random_effects_school <- as.numeric(random_effects_school)

# Calculate correlation
correlation <- cor(random_effects_school, residuals_school)

print(correlation)
# Model 2
model.2 <- lmer(median ~ g1classtype + (1 | g1schid) + black_ratio + g1surban + free_lunch_ratio, data = summary_star)

var_comp <- as.data.frame(VarCorr(model.2))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model.2), "sc")^2  # Residual variance

# Calculate the proportion of variability due to variability between schools
school_var_prop <- school_var / (school_var + residual_var)

summary_model <- summary(model.2)

# Extract fixed effects coefficients
fixed_effects <- summary_model$coefficients

fixed_effects_df <- as.data.frame(fixed_effects)

fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <-cbind(rownames(fixed_effects_df),fixed_effects_df)

fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Model 2"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  print(fixed_effects_table)
# Multicollinearity test
vif_result <- vif(model.2)
vif_result_table <- data.frame(
  Variable = c("g1classtype", "black_ratio", "g1surban", "free_lunch_ratio"),
  GVIF = c(1.032102, 3.871431, 4.295135, 1.965195),
  Df = c(2, 1, 3, 1),
  `GVIF^(1/(2*Df))` = c(1.007931, 1.967595, 1.274959, 1.401854)
)

vif_table <- vif_result_table %>%
  gt() %>%
  tab_header(
    title = "Results of Multicollinearity Assessment (VIF)",
    subtitle = "Variables with GVIF > 4 are considered to have multicollinearity issues"
  ) %>%
  tab_options(
    table.font.size = px(12),
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  fmt_number(columns = vars(-Variable), decimals = 3)

vif_table
# Causal effects plot for model 2
coefficients <- c(-9.43, -9.6, -8.665, 3.23, 6.19, 7.95, -25.245)  # Coefficient estimates
std_errors <- c(3.007, 3.073, 10.336, 9.33, 10.38, 11.48, 9.554)     # Standard errors
predictor_names <- c("Regular", "Regular_Aide", "Black_Ratio", "Suburban","Rural","Urban", "Free_Lunch_Ratio")

effect_data_2 <- data.frame(predictor = predictor_names, 
                            coefficient = coefficients,
                            std_error = std_errors)

causal_plot_2 <- ggplot(effect_data_2, aes(x = predictor, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black",width = 0.7) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                width = 0.3, color = "black") +
  labs(x = "Predictor Variable", y = "Coefficient Estimate",
       title = "Causal Effects of Predictor Variables on Math Scores (Model 2)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(causal_plot_2)
# Plot origin, imputation and aggregation math score
plot1 <- ggplot(data = star_involved, aes(x = g1tmathss)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Original Math Scores") +
  xlab("Original Math Scores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 10))

plot2 <- ggplot(data = median_imputed, aes(x = g1tmathss)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Median Imputation Math Scores") +
  xlab("Median Imputated Math Scores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 10))

plot3 <- ggplot(data = summary_star, aes(x = median)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Aggregated Median Math Scores by Class") +
  xlab("Median Aggregated Math Scores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 10))

plot4 <- ggplot(data = summary_star, aes(x = mean)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightgrey") +
  geom_density(color = "skyblue", size = 0.5) +
  ggtitle("Aggregated Mean Math Scores by Class") +
  xlab("Mean Aggregated Math Scores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title.x = element_text(size = 9))

combined_plots <- plot1 + plot2 + plot3 + plot4 + 
  plot_layout(ncol = 2)

print(combined_plots)
# Effect of math score imputation
# aggregate imputed data
# model 3 (imputed data) vs model 1 (origin data)
summary_star_imputed <- median_imputed %>%
  group_by(g1tchid,g1classtype,g1classsize,g1schid,g1surban) %>%
  dplyr::summarize(mean = mean(g1tmathss, na.rm = TRUE), 
                   median = median(g1tmathss, na.rm = TRUE), 
                   sd = sd(g1tmathss, na.rm = TRUE), 
                   quantile = IQR(g1tmathss, na.rm = TRUE), 
                   count = n(), 
                   black_ratio = sum(race == "Black") / count,  # Calculate black ratio
                   free_lunch_ratio = sum(g1freelunch == "Free-lunch") / count,  # Calculate free lunch ratio
                   .groups = 'drop' )

# Fitted mixed-effect model
model.3 <- lmer(median ~ g1classtype + (1 | g1schid), data = summary_star_imputed)

# Calculate the proportion of variability due to schools
var_comp <- as.data.frame(VarCorr(model.3))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model.3), "sc")^2  # Residual variance
school_var_prop <- school_var / (school_var + residual_var)

summary_model <- summary(model.3)

fixed_effects <- summary_model$coefficients
fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <- cbind(rownames(fixed_effects_df),fixed_effects_df)


fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Imputed-Data Model 3"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  print(fixed_effects_table)

# model 4 (imputed data) vs model 2 (origin data)

# Fitted mixed-effect model
model.4 <- lmer(median ~ g1classtype + (1 | g1schid) + black_ratio + g1surban + free_lunch_ratio, data = summary_star_imputed)

# Calculate the proportion of variability due to schools
var_comp <- as.data.frame(VarCorr(model.4))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model.4), "sc")^2  # Residual variance
school_var_prop <- school_var / (school_var + residual_var)

summary_model <- summary(model.4)


fixed_effects <- summary_model$coefficients
fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <- cbind(rownames(fixed_effects_df),fixed_effects_df)

fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Imputed-Data Model 4"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  print(fixed_effects_table)
# Causal effects plot for model 1
coefficients_1 <- c(-12.35, -12.63)  # Coefficient estimates
std_errors_1 <- c(2.32, 2.41)     # Standard errors
predictor_names_1 <- c("Regular", "Regular_Aide")

effect_data_1 <- data.frame(predictor = predictor_names_1, 
                            coefficient = coefficients_1,
                            std_error = std_errors_1)

# Causal effects plot for model 3
coefficients_3 <- c(-11.68, -11.75)  # Coefficient estimates
std_errors_3 <- c(2.25, 2.34)     # Standard errors
predictor_names_3 <- c("Regular", "Regular_Aide")

effect_data_3 <- data.frame(predictor = predictor_names_3, 
                            coefficient = coefficients_3,
                            std_error = std_errors_3)

combined_data_1_3 <- rbind(cbind(effect_data_1, model = "Model 1 (Origin Data)"), cbind(effect_data_3, model = "Model 3 (Imputed Data)"))

# Combined causal effect plot for models 1 and 3
combined_plot_1_3 <- ggplot(combined_data_1_3, aes(x = predictor, y = coefficient, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.3) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                position = position_dodge(width = 0.7), width = 0.1, color = "black") +
  labs(x = "Predictor Variable", y = "Coefficient Estimate",
       title = "Causal Effects of Imputation on Math Scores (Models 1 and 3)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Model 1 (Origin Data)" = "skyblue", "Model 3 (Imputed Data)" = "pink"))
print(combined_plot_1_3)
# Combined causal effects plot for model 2 and model 4
# Causal effects plot for model 2
coefficients_2 <- c(-9.43, -9.6, -8.665, 3.23, 6.19, 7.95, -25.245)  # Coefficient estimates
std_errors_2 <- c(3.007, 3.073, 10.336, 9.33, 10.38, 11.48, 9.554)     # Standard errors
predictor_names_2 <- c("Regular", "Regular_Aide", "Black_Ratio", "Suburban","Rural","Urban", "Free_Lunch_Ratio")

effect_data_2 <- data.frame(predictor = predictor_names_2, 
                            coefficient = coefficients_2,
                            std_error = std_errors_2)

# Causal effects plot for model 4
coefficients_4 <- c(-9, -9.42, -9.02, 3.9, 6.28, 8.03, -23.51)  # Coefficient estimates
std_errors_4 <- c(2.92, 2.98, 9.89, 8.91, 9.9, 10.95, 9.21)     # Standard errors
predictor_names_4 <- c("Regular", "Regular_Aide", "Black_Ratio", "Suburban","Rural","Urban", "Free_Lunch_Ratio")

effect_data_4 <- data.frame(predictor = predictor_names_4, 
                            coefficient = coefficients_4,
                            std_error = std_errors_4)

combined_data_2_4 <- rbind(cbind(effect_data_2, model = "Model 2 (Origin Data)"), cbind(effect_data_4, model = "Model 4 (Imputed Data)"))

# Combine causal effect plot for models 2 and 4
combined_plot_2_4 <- ggplot(combined_data_2_4, aes(x = predictor, y = coefficient, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                position = position_dodge(width = 0.6), width = 0.1, color = "black") +
  labs(x = "Predictor Variable", y = "Coefficient Estimate",
       title = "Causal Effects of Imputation on Math Scores (Models 2 and 4)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Model 2 (Origin Data)" = "skyblue", "Model 4 (Imputed Data)" = "pink"))

print(combined_plot_2_4)
# Effect of data aggregation
# model 5 (unaggregated data) vs model 1 (aggregated data)

# Fitted mixed-effect model
model.5 <- lmer(g1tmathss ~ g1classtype + (1 | g1schid), data = star_involved)

# Calculate the proportion of variability due to schools
var_comp <- as.data.frame(VarCorr(model.5))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model.5), "sc")^2  # Residual variance
school_var_prop <- school_var / (school_var + residual_var)
summary_model <- summary(model.5)
fixed_effects <- summary_model$coefficients

fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <- cbind(rownames(fixed_effects_df),fixed_effects_df)

fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Imputed-Data Model 5"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  print(fixed_effects_table)

# model 5 (unaggregated data) vs model 2 (aggregated data)

# Fitted mixed-effect model
model.6 <- lmer(g1tmathss ~ g1classtype + (1 | g1schid) + race + g1surban + g1freelunch, data = star_involved)

# Calculate the proportion of variability due to schools
var_comp <- as.data.frame(VarCorr(model.6))
school_var <- var_comp[1, "vcov"]  # Variance for schools
residual_var <- attr(VarCorr(model.6), "sc")^2  # Residual variance

school_var_prop <- school_var / (school_var + residual_var)

summary_model <- summary(model.6)

fixed_effects <- summary_model$coefficients
fixed_effects_df <- as.data.frame(fixed_effects)

fixed_effects_df <- round(fixed_effects_df, 2)
fixed_effects_df <- cbind(rownames(fixed_effects_df),fixed_effects_df)

fixed_effects_table <- gt(fixed_effects_df) %>%
  tab_header(
    title = "Summary of Fixed Effects Coefficients of Imputed-Data Model 6"
  ) %>%
  tab_options(
    table.font.size = px(12), 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = TRUE)
  ) %>%
  
  
  print(fixed_effects_table)
# Causal effects plot for model 5
coefficients_5 <- c(-13.32, -10.87)  # Coefficient estimates
std_errors_5 <- c(1.18, 1.21)     # Standard errors
predictor_names_5 <- c("Regular", "Regular_Aide") 

effect_data_5 <- data.frame(predictor = predictor_names_5, 
                            coefficient = coefficients_5,
                            std_error = std_errors_5)

combined_data_1_5 <- rbind(cbind(effect_data_1, model = "Model 1 (Aggregated Data)"), cbind(effect_data_5, model = "Model 5 (Unaggregated Data)"))

# Combined causal effect plot for models 1 and 5
combined_plot_1_5 <- ggplot(combined_data_1_5, aes(x = predictor, y = coefficient, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.3) +
  geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                position = position_dodge(width = 0.7), width = 0.1, color = "black") +
  labs(x = "Predictor Variable", y = "Coefficient Estimate",
       title = "Causal Effects of Data Aggregation on Math Scores (Models 1 and 5)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Model 1 (Aggregated Data)" = "skyblue", "Model 5 (Unaggregated Data)" = "pink"))

print(combined_plot_1_5)
# BIC (Bayesian Information Criterion) to compare model
BIC(model_mix_anova, model.2)
