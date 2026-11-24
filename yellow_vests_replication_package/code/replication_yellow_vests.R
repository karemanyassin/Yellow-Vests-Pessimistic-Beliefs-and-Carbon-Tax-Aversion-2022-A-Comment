# Ottawa Replication Games
# May, 2023

# Replication of 
# Douenne, Thomas, and Adrien Fabre. "Yellow vests, pessimistic beliefs, and carbon tax aversion." American Economic Journal: Economic Policy 14.1 (2022): 81-110.

# Relevant package
library(tidyverse)
library(rdrobust)
library(rddensity)
library(rddtools)
library(fastDummies)


# Load the original csv
# This was created using "write_csv(s, "s_original.csv")" after running preparation.R
s <- read_csv("../original_data/s_original.csv")

# Main outcome variable is "believes does not lose"
s$believes_does_not_lose <- s$gagnant_feedback_categorie != "Perdant"
# Some robustness checks also use "believes wins"
s$believes_wins <- s$gagnant_feedback_categorie == "Gagnant"


# Plot of mis-classified observations
#   Running variable is "simule_gain". This is predicted benefit in euros per year
#   Cutoff is "simule_gagnant". This should happen when simule_gain>0
#   The text (p.23) says "The binary win/lose feedback is a variable
#   \hat{Gamma} that jumps from 0 to 1 when our continuous estimation of respondents'
#   net gains \hat{gamma} exceeds the zero threshold.
ggplot(s |> filter(simule_gain < 50 & simule_gain > -50) , aes(x=simule_gain , y=simule_gagnant, colour=simule_gain>0, shape=simule_gain>0)) + 
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = expression(paste("simule_gain (", hat(gamma), ")")),
       y = expression(paste("simule_gagnant (", hat(Gamma), ")"))) +
  annotate(geom="text", x=-60, y=0.75, label="Misclassified observations", hjust=0) +
  annotate(geom="segment", x=-30, y=0.8, xend=-15, yend=1,
           arrow = arrow()) +
  geom_vline(xintercept = 0) +
  theme(legend.position="bottom")
ggsave("../figures_tables/misclassified.png", width=6, height=6)


# Table of mis-classified observations
table(s$simule_gagnant,s$simule_gain>0)
# Restricted to -50 to 50
table(s$simule_gagnant[s$simule_gain>-50 & s$simule_gain<50], s$simule_gain[s$simule_gain>-50 & s$simule_gain<50]>0)

# Write the table  
tab_misclass <- s |> 
  group_by(simule_gain>0, simule_gagnant==1) |> 
  summarise(n = n(), .groups = "drop") |> 
  rename_with(~c("gain", "gagnant", "n")) |> 
  arrange(gain, gagnant)
sink("../figures_tables/classification_table.tex")
cat("\\begin{tabular}[t]{llc} \n")
cat("\\toprule \n")
cat("$\\hat{\\gamma} > 0$ & $\\hat{\\Gamma} = 1$ & n \\\\ \n")
cat("\\midrule \n")
cat("FALSE & FALSE &", tab_misclass$n[1], "\\\\ \n")
cat("FALSE & TRUE &", tab_misclass$n[2], "\\\\ \n")
cat("TRUE & FALSE &", tab_misclass$n[3], "\\\\ \n")
cat("TRUE & TRUE &", tab_misclass$n[4], "\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()


# This plot visualizes the estimation in the paper:
#   *Global* quadratic in the running variable
#   Manual bandwidth choice
#   Uniform Kernel
#   (note there are no covariates included in this estimation)
ggplot(s |> filter(abs(simule_gain) < 50), aes(x=simule_gain, y=as.numeric(believes_does_not_lose))) + 
  geom_point(aes(colour=simule_gain>0)) +
  geom_smooth(method="lm", formula = y~x+I(x^2) + I(x>0), se=F, colour="black") +
  theme_bw() +
  labs(x=expression(paste("simule_gain (", hat(gamma), ")")),
       y="Probability believes does not lose") +
  theme(legend.position="bottom")
ggsave("../figures_tables/RD_plot_run_in_paper.png", width=6, height=6)


# Here is the rdplot corresponding to the estimation
#   Full support of running variable
#   4th degree *local* polynomial
#   Triangular kernel
#   (note that there are no covariates included in this estimation)
rdplot(y=s$believes_does_not_lose, x=s$simule_gain, c=0)
# With weights
rdplot(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose")
ggsave("../figures_tables/rd_plot.png", width = 6, height = 6)

#Plot dropping low-level outliers
s_within <- s |> filter(simule_gain > -300)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose")
ggsave("../figures_tables/rd_plot_within.png", width = 6, height = 6)

# Density of observations
ggplot(s, aes(x=simule_gain)) +
  geom_density(colour="firebrick") +
  theme_bw() +
  labs(x = expression(paste("simule_gain (", hat(gamma), ")")))
ggsave("../figures_tables/simule_gain_density.png", width=6, height=6)


# Table with and without covariates//with and without corrected cutoff
# Their column (1)
tab42_v1_c1 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                    Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
                    sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                    nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                    uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                    percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                    I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                    I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                           70, 0)) ,
                  data = s, 
                  subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                  weights = weight)

# Their column (1) with no covariates
tab42_v1_c2 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant +
                    Simule_gain + Simule_gain2  ,
                  data = s, 
                  subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                  weights = weight)

# Their column (1) with correctly coded win/lose
tab42_v1_c3 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ (simule_gain>0) + tax_acceptance +
                    Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
                    sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                    nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                    uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                    percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                    I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                    I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                           70, 0)) ,
                  data = s, 
                  subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                  weights = weight)

# Their column (1) with correctly coded win/lose and no covariates
tab42_v1_c4 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ (simule_gain>0) +
                    Simule_gain + Simule_gain2  ,
                  data = s, 
                  subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                  weights = weight)

# Create data frame for results table
tab42_v1_list <- list(tab42_v1_c1, tab42_v1_c2, tab42_v1_c3, tab42_v1_c4)
tab42_v1_out <- tibble(coefs = map_dbl(tab42_v1_list, ~summary(.x)$coefficients[2,1]),
                       ses = map_dbl(tab42_v1_list, ~summary(.x)$coefficients[2,2]),
                       pvals = map_dbl(tab42_v1_list, ~summary(.x)$coefficients[2,4]),
                       nobs = map_dbl(tab42_v1_list, ~nobs(.x)),
                       rsq = map_dbl(tab42_v1_list, ~summary(.x)$r.squared)) |> 
  mutate(across(c(coefs, ses, rsq), ~sprintf("%.3f", .x)),
         nobs = sprintf("%.0f", nobs),
         pvals = if_else(pvals < 0.001, "<0.001", sprintf("%.3f", pvals))) |> 
  mutate(across(everything(), ~paste0("\\num{", .x, "}")),
         ses = paste0("(", ses, ")"),
         pvals = paste0("[", pvals, "]"))

# Write to a table
sink("../figures_tables/table42_v1.tex")
cat("\\begin{tabular}[t]{lccccc} \n")
cat("\\toprule \n")
cat("& Original & \\multicolumn{4}{c}{Reproduction} \\\\ \n")
cat("\\cmidrule(r){2-2} \\cmidrule(l){3-6} \n")
cat("& (1) & (2) & (3) & (4) & (5) \\\\ \n")
cat("\\midrule \n")
cat("Coded as predicted winner ($\\hat{\\Gamma} = 1$) & \\num{0.269} &", tab42_v1_out$coefs[1], "&", tab42_v1_out$coefs[2], "& & \\\\ \n")
cat("& (\\num{0.058}) &", tab42_v1_out$ses[1], "&", tab42_v1_out$ses[2], "& & \\\\ \n")
cat("& &", tab42_v1_out$pvals[1], "&", tab42_v1_out$pvals[2], "& & \\\\ \n")
cat("Predicted winner ($\\hat{\\gamma} > 0$) & & & &", tab42_v1_out$coefs[3], "&", tab42_v1_out$coefs[4], "\\\\ \n")
cat("& & & &", tab42_v1_out$ses[3], "&", tab42_v1_out$ses[4], "\\\\ \n")
cat("& & & &", tab42_v1_out$pvals[3], "&", tab42_v1_out$pvals[4], "\\\\ \n")
cat("\\midrule \n")
cat("Covariates & X & X & & X & \\\\ \n")
cat("Observations & \\num{757} &", paste(tab42_v1_out$nobs, collapse = " & "), "\\\\ \n")
cat("$R^2$ & \\num{0.301} &", paste(tab42_v1_out$rsq, collapse = " & "), "\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()


# Now implement as a RDD
rdd1 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight)
# c4 <- RD with manual bandwidth and uniform kernel
rdd2 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, h=50, kernel = "uniform")
# uniform kernel 
rdd3 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, kernel="uniform")
# local quadratic
rdd4 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, p=2)

# code with covariates
s_cov <-  s |>
  mutate(pr1 = (pmin(percentile_revenu - 20, 0)),
         pr2 = (pmin(percentile_revenu - 70, 0)),
         pr3 = (pmin(percentile_revenu_conjoint - 20, 0)),
         pr4 = (pmin(percentile_revenu_conjoint - 70, 0))) |> 
  dplyr::select( tax_acceptance,
                 single,  hausse_depenses_par_uc, 
                 sexe,  statut_emploi,  csp,  region,  diplome4,  taille_menage, 
                 nb_14_et_plus, nb_adultes, fume, actualite, taille_agglo,
                 uc , niveau_vie , age_18_24 , age_25_34 , age_35_49 , age_50_64 ,
                 percentile_revenu , percentile_revenu_conjoint, pr1, pr2, pr3, pr4, weight)
# code as dummies
s_cov <- s_cov |>
  dummy_cols()  |>
  dplyr::select(-where(is.character))

rdd5 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, covs = s_cov)

# Use only observations that are correctly assigned to treatment
s_correct <- s |>
  filter(as.numeric(simule_gain > 0) == simule_gagnant)
rdplot(y=s_correct$believes_does_not_lose, x=s_correct$simule_gain, c=0)
rdd6 <- rdrobust(y=s_correct$believes_does_not_lose, x=s_correct$simule_gain, c=0, weights = s_correct$weight)

# Create data frame for results table
tab42_v2_list <- list(rdd1, rdd2, rdd3, rdd4, rdd5, rdd6)
tab42_v2_out <- tibble(conv_coefs = map_dbl(tab42_v2_list, ~.x$coef[1]),
                       conv_ses = map_dbl(tab42_v2_list, ~.x$se[1]),
                       conv_pvals = map_dbl(tab42_v2_list, ~.x$pv[1]),
                       bc_coefs = map_dbl(tab42_v2_list, ~.x$coef[2]),
                       bc_ses = map_dbl(tab42_v2_list, ~.x$se[2]),
                       bc_pvals = map_dbl(tab42_v2_list, ~.x$pv[2]),
                       rob_coefs = map_dbl(tab42_v2_list, ~.x$coef[3]),
                       rob_ses = map_dbl(tab42_v2_list, ~.x$se[3]),
                       rob_pvals = map_dbl(tab42_v2_list, ~.x$pv[3]),
                       nobs = map_dbl(tab42_v2_list, ~sum(.x$N_h))) |> 
  mutate(across(ends_with(c("coefs", "ses")), ~sprintf("%.3f", .x)),
         nobs = sprintf("%.0f", nobs),
         across(ends_with("pvals"), ~if_else(.x < 0.001, "<0.001", sprintf("%.3f", .x)))) |> 
  mutate(across(everything(), ~paste0("\\num{", .x, "}")),
         across(ends_with("ses"), ~paste0("(", .x, ")")),
         across(ends_with("pvals"), ~paste0("[", .x, "]")))

# Write to a table
sink("../figures_tables/table42_v2.tex")
cat("\\begin{tabular}[t]{lcccccc} \n")
cat("\\toprule \n")
cat("& (1) & (2) & (3) & (4) & (5) & (6) \\\\ \n")
cat("\\midrule \n")
cat("Conventional &", paste(tab42_v2_out$conv_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$conv_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$conv_pvals, collapse = " & "), "\\\\ \n")
cat("Bias-Corrected &", paste(tab42_v2_out$bc_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$bc_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$bc_pvals, collapse = " & "), "\\\\ \n")
cat("Robust &", paste(tab42_v2_out$rob_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$rob_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v2_out$rob_pvals, collapse = " & "), "\\\\ \n")
cat("\\midrule \n")
cat("Kernel & Triangular & Uniform & Uniform & Triangular & Triangular & Triangular \\\\ \n")
cat("Bandwidth & mserd & Manual & mserd & mserd & mserd & mserd \\\\ \n")
cat("Polynomial & Linear & Linear & Linear & Quadratic & Linear & Linear \\\\ \n")
cat("Covariates & & & & & X & \\\\ \n")
cat("Drop misclassified & & & & & & X \\\\ \n")
cat("Observations &", paste(tab42_v2_out$nobs, collapse = " & "), "\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()


# What about placebo outcomes?
s$male <- s$sexe == "Masculin"
rdd_placebo1 <- rdrobust(y=s$male, x=s$simule_gain, c=0)
s$young <- s$age < 30
rdd_placebo2 <- rdrobust(y=s$young, x=s$simule_gain, c=0)
s$old <- s$age >= 65 
rdd_placebo3 <- rdrobust(y=s$old, x=s$simule_gain, c=0)
rdd_placebo4 <- rdrobust(y=s$taille_menage, x=s$simule_gain, c=0)

# Create data frame for results table
tab42_v3_list <- list(rdd_placebo1, rdd_placebo2, rdd_placebo3, rdd_placebo4)
tab42_v3_out <- tibble(conv_coefs = map_dbl(tab42_v3_list, ~.x$coef[1]),
                       conv_ses = map_dbl(tab42_v3_list, ~.x$se[1]),
                       conv_pvals = map_dbl(tab42_v3_list, ~.x$pv[1]),
                       bc_coefs = map_dbl(tab42_v3_list, ~.x$coef[2]),
                       bc_ses = map_dbl(tab42_v3_list, ~.x$se[2]),
                       bc_pvals = map_dbl(tab42_v3_list, ~.x$pv[2]),
                       rob_coefs = map_dbl(tab42_v3_list, ~.x$coef[3]),
                       rob_ses = map_dbl(tab42_v3_list, ~.x$se[3]),
                       rob_pvals = map_dbl(tab42_v3_list, ~.x$pv[3]),
                       nobs = map_dbl(tab42_v3_list, ~sum(.x$N_h))) |> 
  mutate(across(ends_with(c("coefs", "ses")), ~sprintf("%.3f", .x)),
         nobs = sprintf("%.0f", nobs),
         across(ends_with("pvals"), ~if_else(.x < 0.001, "<0.001", sprintf("%.3f", .x)))) |> 
  mutate(across(everything(), ~paste0("\\num{", .x, "}")),
         across(ends_with("ses"), ~paste0("(", .x, ")")),
         across(ends_with("pvals"), ~paste0("[", .x, "]")))

# Write to a table
sink("../figures_tables/table42_v3.tex")
cat("\\begin{tabular}[t]{lcccc} \n")
cat("\\toprule \n")
cat("& Male & Young & Old & HH size \\\\ \n")
cat("& (1) & (2) & (3) & (4) \\\\ \n")
cat("\\midrule \n")
cat("Conventional &", paste(tab42_v3_out$conv_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$conv_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$conv_pvals, collapse = " & "), "\\\\ \n")
cat("Bias-Corrected &", paste(tab42_v3_out$bc_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$bc_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$bc_pvals, collapse = " & "), "\\\\ \n")
cat("Robust &", paste(tab42_v3_out$rob_coefs, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$rob_ses, collapse = " & "), "\\\\ \n")
cat("&", paste(tab42_v3_out$rob_pvals, collapse = " & "), "\\\\ \n")
cat("\\midrule \n")
cat("Kernel & Triangular & Triangular & Triangular & Triangular \\\\ \n")
cat("Bandwidth & mserd & mserd & mserd & mserd \\\\ \n")
cat("Polynomial & Linear & Linear & Linear & Linear \\\\ \n")
cat("Observations &", paste(tab42_v3_out$nobs, collapse = " & "), "\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()


# What about density? (a la mcrary)
rddensity(s$simule_gain, c=0) |> summary()


# Placebo cutoffs 
pp <- rdd_data(y=s$believes_does_not_lose, x=s$simule_gain, cutpoint=0)
ppp <- rdd_reg_np(rdd_object=pp)
p1 <- plotPlacebo(ppp, plot = FALSE)
p1_placeb <- subset(p1, position!="True")
p1_true <- subset(p1, position=="True")
p1_last_left <- nrow(subset(p1_placeb, position=="left"))
p1_W <- diff(p1_placeb[c(p1_last_left, p1_last_left+1), "cutpoint"])/5
qplot(x=cutpoint, y=LATE, data=p1_placeb, geom="line", colour=position) +
  geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=p1_placeb, stat="identity") +
  geom_hline(yintercept=0) +
  geom_point(aes(x=cutpoint, y=LATE), data=p1_true) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), data=p1_true, width=p1_W) +
  theme_bw() +
  theme(legend.position="none")
ggsave("../figures_tables/placebo_cutoffs.png", width = 6, height = 6)


# Replicate their column 3 of table 4 -- note that simule_gagnant is incorrectly coded here.
# This is pretty close to what they report
tab42_v4_c3 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                    Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
                    sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                    nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                    uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                    percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                    I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                    I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                           70, 0)) +
                    # These are the additional controls for column 3
                    simule_gagnant * (gilets_jaunes > 0) +
                    simule_gagnant * tax_acceptance +
                    simule_gagnant * (gagnant_categorie!='Perdant') ,
                  data = s, 
                  subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                  weights = weight)

# Add interactions between the interaction variables and the running variable
tab42_v4_c3a <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                     Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
                     sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                     nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                     uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                     percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                     I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                     I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                            70, 0)) +
                     # These are the additional controls for column 3
                     simule_gagnant * (gilets_jaunes > 0) +
                     simule_gagnant * tax_acceptance +
                     simule_gagnant * (gagnant_categorie!='Perdant') +
                     # These are interactions between the sub group variables and running variables
                     Simule_gain * (gilets_jaunes > 0) +
                     Simule_gain2 * (gilets_jaunes > 0) +
                     Simule_gain * tax_acceptance +
                     Simule_gain2 * tax_acceptance +
                     Simule_gain * (gagnant_categorie!='Perdant') +
                     Simule_gain2 * (gagnant_categorie!='Perdant') 
                   
                   ,
                   data = s, 
                   subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                   weights = weight)

# Replace quadratic with local linear
tab42_v4_c3b <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                     simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
                     sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                     nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                     uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                     percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                     I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                     I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                            70, 0)) +
                     # These are the additional controls for column 3
                     simule_gagnant * (gilets_jaunes > 0) +
                     simule_gagnant * tax_acceptance +
                     simule_gagnant * (gagnant_categorie!='Perdant') +
                     # These are interactions between the sub group variables and running variables
                     (simule_gain * simule_gagnant) * (gilets_jaunes > 0) +
                     (simule_gain * simule_gagnant) * tax_acceptance +
                     (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
                   
                   ,
                   data = s, 
                   subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                   weights = weight)


# Use triangular kernel weighting
tab42_v4_c3c <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                     simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
                     sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                     nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                     uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                     percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                     I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                     I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                            70, 0)) +
                     # These are the additional controls for column 3
                     simule_gagnant * (gilets_jaunes > 0) +
                     simule_gagnant * tax_acceptance +
                     simule_gagnant * (gagnant_categorie!='Perdant') +
                     # These are interactions between the sub group variables and running variables
                     (simule_gain * simule_gagnant) * (gilets_jaunes > 0) +
                     (simule_gain * simule_gagnant) * tax_acceptance +
                     (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
                   
                   ,
                   data = s, 
                   subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                   weights = weight * (1 - (abs(simule_gain) / 50)))

# Create data frame for results table
tab42_v4_list <- list(tab42_v4_c3, tab42_v4_c3a, tab42_v4_c3b, tab42_v4_c3c)
tab42_v4_out <- tibble(coefs1 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[2,1]),
                       ses1 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[2,2]),
                       pvals1 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[2,4]),
                       coefs2 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[3,1]),
                       ses2 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[3,2]),
                       pvals2 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[3,4]),
                       coefs3 = map2_dbl(tab42_v4_list, c(54, 54, 53, 53), ~summary(.x)$coefficients[.y,1]),
                       ses3 = map2_dbl(tab42_v4_list, c(54, 54, 53, 53), ~summary(.x)$coefficients[.y,2]),
                       pvals3 = map2_dbl(tab42_v4_list, c(54, 54, 53, 53), ~summary(.x)$coefficients[.y,4]),
                       coefs4 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[57,1]),
                       ses4 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[57,2]),
                       pvals4 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[57,4]),
                       coefs5 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[56,1]),
                       ses5 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[56,2]),
                       pvals5 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[56,4]),
                       coefs6 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[58,1]),
                       ses6 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[58,2]),
                       pvals6 = map_dbl(tab42_v4_list, ~summary(.x)$coefficients[58,4]),
                       nobs = map_dbl(tab42_v4_list, ~nobs(.x)),
                       rsq = map_dbl(tab42_v4_list, ~summary(.x)$r.squared)) |> 
  mutate(across(starts_with(c("coefs", "ses", "rsq")), ~sprintf("%.3f", .x)),
         nobs = sprintf("%.0f", nobs),
         across(starts_with("pvals"), ~if_else(.x < 0.001, "<0.001", sprintf("%.3f", .x)))) |> 
  mutate(across(everything(), ~paste0("\\num{", .x, "}")),
         across(starts_with("ses"), ~paste0("(", .x, ")")),
         across(starts_with("pvals"), ~paste0("[", .x, "]")))

# Write to a table
sink("../figures_tables/table42_v4.tex")
cat("\\begin{tabular}[t]{lccccc} \n")
cat("\\toprule \n")
cat("& Original & \\multicolumn{4}{c}{Reproduction} \\\\ \n")
cat("\\cmidrule(r){2-2} \\cmidrule(l){3-6} \n")
cat("& (1) & (2) & (3) & (4) & (5) \\\\ \n")
cat("\\midrule \n")
cat("Predicted winner ($\\hat{\\Gamma} == 1$) & \\num{0.246} &", paste(tab42_v4_out$coefs1, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.064}) &", paste(tab42_v4_out$ses1, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals1, collapse = " & "), "\\\\ \n")
cat("Initial tax acceptance & \\num{0.015} &", paste(tab42_v4_out$coefs2, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.087}) &", paste(tab42_v4_out$ses2, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals2, collapse = " & "), "\\\\ \n")
cat("Yellow Vests supporter & \\num{0.019} &", paste(tab42_v4_out$coefs3, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.058}) &", paste(tab42_v4_out$ses3, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals3, collapse = " & "), "\\\\ \n")
cat("$\\hat{\\Gamma} \\times $ Initial tax acceptance & \\num{0.166} &", paste(tab42_v4_out$coefs4, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.079}) &", paste(tab42_v4_out$ses4, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals4, collapse = " & "), "\\\\ \n")
cat("$\\hat{\\Gamma} \\times $ Yellow Vests supporter & \\num{-0.151} &", paste(tab42_v4_out$coefs5, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.069}) &", paste(tab42_v4_out$ses5, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals5, collapse = " & "), "\\\\ \n")
cat("$\\hat{\\Gamma} \\times $ Initial belief to not lose & \\num{0.080} &", paste(tab42_v4_out$coefs6, collapse = " & "), "\\\\ \n")
cat("& (\\num{0.079}) &", paste(tab42_v4_out$ses6, collapse = " & "), "\\\\ \n")
cat("& &", paste(tab42_v4_out$pvals6, collapse = " & "), "\\\\ \n")
cat("\\midrule \n")
cat("Kernel & Uniform & Uniform & Uniform & Uniform & Triangular \\\\ \n")
cat("Bandwidth & Manual & Manual & Manual & Manual & Manual \\\\ \n")
cat("Polynomial & Global quad. & Global quad. & Global quad. & Local lin. & Local lin. \\\\ \n")
cat("Interactions with polynomial & & & X & X & X \\\\ \n")
cat("Observations & \\num{757} &", paste(tab42_v4_out$nobs, collapse = " & "), "\\\\ \n")
cat("$R^2$ & \\num{0.419} &", paste(tab42_v4_out$rsq, collapse = " & "), "\\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()


# Create rdplots for heterogeneity
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$tax_acceptance==TRUE,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially accept tax")
ggsave("../figures_tables/rd_plot_het_tax.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$tax_acceptance!=TRUE,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially do not accept tax")
ggsave("../figures_tables/rd_plot_het_notax.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$gilets_jaunes > 0,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who support Yellow Vests")
ggsave("../figures_tables/rd_plot_het_gj.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$gilets_jaunes <= 0,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who do not support Yellow Vests")
ggsave("../figures_tables/rd_plot_het_nogj.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$gagnant_categorie!='Perdant',
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially believe they do not lose")
ggsave("../figures_tables/rd_plot_het_nolose.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s_within$gagnant_categorie=='Perdant',
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially believe they lose")
ggsave("../figures_tables/rd_plot_het_lose.png", width = 6, height = 6)
