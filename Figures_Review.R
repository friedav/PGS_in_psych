### Figure 1 heritability and PGS prediction ###

library(tidyverse)

condition <- c("Anorexia", "Anxiety", "ADHD", "ASD", "AUD", "Bipolar", "MDD", "SCZ")

h2_twin <-      c(0.404, 0.40, 0.675, 0.80, 0.55, 0.678, 0.452, 0.774)
# refs h2_twin "AN", "ANX", "ADHD", "ASD", "AUD", "BD", "MDD", "SCZ"
# Poldermann et al. 2015 Nature Genetics: ANX, ADHD, BD, MDD (recurrent), SCZ, Eating disorders 0.404

h2_SNP <-       c(0.13, 0.059, 0.14, 0.08, 0.08, 0.21, 0.084, 0.25)
# refs h2_SNP "AN", "ANX", "ADHD", "ASD", "AUD", "BD", "MDD", "SCZ"
# BD: O'Connel et al. 2025 Nature
# MDD: Adams et al. 2025 Cell
# SCZ: Owen et al. 2023 Mol Psych
# AN: Termorshuizen et al. 2025 medRxiv
# ANX: Skelton, Mitchell et al. 2025 medRxiv (Generalised anxiety symptoms)


PGS_pred <-     c(0.023, 0.034, 0.03, 0.01, 0.02, 0.09, 0.058, 0.10)
# refs PGS_pred "AN", "ANX", "ADHD", "ASD", "AUD", "BD", "MDD", "SCZ"
# (same as GWAS for h2_SNP?)

df_fig1 <- data.frame(condition, h2_twin, h2_SNP, PGS_pred)
df_fig1_long <- pivot_longer(df_fig1, cols = c(h2_twin, h2_SNP, PGS_pred), names_to = "Estimate")
df_fig1_long$Estimate <- factor(df_fig1_long$Estimate, levels = c("h2_twin", "h2_SNP", "PGS_pred"))

fig1 <- ggplot(df_fig1_long, aes(x = Estimate, y = value, fill = Estimate)) +
    facet_wrap(~ condition, nrow = 1) +
    geom_bar(stat = "identity", width = 0.6) +
    labs(
        x = "Estimate",
        y = "% variance explained",
        fill = "Estimate") +
    scale_fill_viridis_d(option = "D", end = 0.85) +
    scale_y_continuous(breaks = c(0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1), limits = c(0, 1)) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13)) 


pdf("Figure_1.pdf", width = 15)
fig1
dev.off()

### Figure 2 relative and absolute risk ###

prevalence <- c(0.01, 0.05, 0.20)
h2 <- c(0.10, 0.20, 0.40)
fam_risk <- c("1st_deg_rel", "none")

df_fig2 <- tidyr::crossing(prevalence, h2, fam_risk)
df_fig2$gen_cor <- ifelse(df_fig2$fam_risk == "1st_deg_rel", 0.5, 0)

risk <- liability_risk_fdr_vec(K = df_fig2$prevalence, h2 = df_fig2$h2, r = df_fig2$gen_cor)


fig2a <- ggplot(risk, aes(x = K, y = absolute_risk_FDR, fill = K)) +
    facet_wrap(~ h2, labeller = label_both) +
    geom_point(aes(x = K, y = absolute_risk_FDR, color = as.factor(r), shape = as.factor(r)), size = 4) + 
    geom_line(aes(x = K, y = absolute_risk_FDR, color = as.factor(r)), linewidth = 1) +
    labs(
        x = "Prevalence",
        y = "Absolute risk",
        fill = "Prevalence") +
    scale_colour_viridis_d(option = "D", end = 0.85) +
    scale_x_continuous(breaks = c(0.01, 0.05, 0.10, 0.15, 0.20)) +
    scale_y_continuous(breaks = c(0.01, 0.05, 0.10, 0.20, 0.30, 0.40)) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)
    ) 


rel_risk <- risk[risk$r == 0.5, ]

fig2b <- ggplot(rel_risk, aes(x = as.factor(K), y = relative_risk, fill = as.factor(K))) +
    facet_wrap(~ h2, labeller = label_both) +
    geom_bar(stat = "identity", position = position_dodge(width = 1)) +
    geom_hline(yintercept = 1) +
    labs(
        x = "Prevalence",
        y = "Relative risk",
        fill = "Prevalence") +
    scale_fill_viridis_d(option = "D", end = 0.85) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)
    ) 


pdf("Figure_2.pdf", width = 12, height = 12)
cowplot::plot_grid(fig2a, fig2b, labels = c("a", "b"), ncol = 1, nrow = 2)
dev.off()
