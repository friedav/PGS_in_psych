liability_risk_fdr_vec <- function(K, h2, r = 0.5) {
    # All inputs can be vectors of the same length
    
    T <- qnorm(1 - K)
    phi_T <- dnorm(T)
    mean_affected <- phi_T / K
    mu_fdr <- r * h2 * mean_affected
    K_fdr <- 1 - pnorm(T - mu_fdr)
    RR <- K_fdr / K
    
    data.frame(
        K = K,
        h2 = h2,
        r = r,
        threshold = T,
        absolute_risk_FDR = K_fdr,
        absolute_risk_noFDR = K,
        relative_risk = RR
    )
}
