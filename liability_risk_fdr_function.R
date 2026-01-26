liability_risk_fdr <- function(K, h2, r = 0.5) {
    # K  = population prevalence (0 < K < 1)
    # h2 = heritability on liability scale (0–1)
    # r  = genetic relatedness (0.5 for first-degree relatives)
    
    # 1. Threshold on liability scale
    T <- qnorm(1 - K)
    
    # 2. Standard normal density at threshold
    phi_T <- dnorm(T)
    
    # 3. Mean liability of an affected person
    mean_affected <- phi_T / K
    
    # 4. Mean liability shift given affected relative
    mu_fdr <- r * h2 * mean_affected
    
    # 5. Absolute risk given affected first-degree relative
    K_fdr <- 1 - pnorm(T - mu_fdr)
    
    # 6. Relative risk
    RR <- K_fdr / K
    
    list(
        population_prevalence = K,
        heritability = h2,
        threshold = T,
        absolute_risk_FDR = K_fdr,
        absolute_risk_noFDR = K,
        relative_risk = RR
    )
}
