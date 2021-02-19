plot_chains <- function(imps){
  chain_means <- plyr::adply(imps$chainMean, 1:3) %>%
    as_tibble() %>%
    rename(var = 1, iteration = 2, .imp = 3, Mean = 4)

  chain_sd <- plyr::adply(imps$chainVar, 1:3) %>%
    as_tibble() %>%
    rename(var = 1, iteration = 2, .imp = 3, variance = 4)

  full_join(chain_means, chain_sd, by = c("var", "iteration", ".imp")) %>%
    rename(imp = .imp) %>%
    filter(str_detect(var, "\\.", TRUE)) %>%
    mutate_at(c(2, 4, 5), as.numeric) %>%
    mutate(imp = str_sub(imp, 7),
           SD = sqrt(variance)) %>%
    filter(!is.nan(Mean),
           !is.nan(SD)) %>%
    pivot_longer(cols = c("Mean", "SD"), names_to = "stat") %>%
    ggplot(aes(x = iteration, y = value, color = imp)) +
    geom_line() +
    facet_wrap(var ~ stat, scales = "free_y",
               ncol = 6) +
    theme_minimal() +
    guides(color = FALSE) + labs(x = "Iteration", y = "Value")
}
