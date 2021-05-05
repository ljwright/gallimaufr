get_desc <- function(data, id_var = "id", imp_var = NULL,
                     weight_var = NULL, group_var = NULL){
  if (is.null(weight_var)){
    data$wt <- 1
  } else{
    data <- rename(data, wt = all_of(!!weight_var))
  }
  data <- data %>%
    mutate(wt = wt*n()/sum(wt, na.rm = TRUE))

  if (is.null(group_var)){
    data$group_var <- "1"
  } else{
    data <- rename(data, group_var = all_of(!!group_var))
  }

  if (is.null(imp_var)){
    data$imp <- 1
  } else{
    data <- rename(data, imp = all_of(!!imp_var))
  }
  data <- rename(data, id = all_of(!!id_var))

  n_imps <- unique(data$imp) %>% length()

  desc_n <- data %>%
    count(group_var, wt = wt) %>%
    mutate(n = n/n_imps, var = "n", cat = "n",
           string = round(n, 2) %>%
             format(big.mark = ",") %>%
             trimws()) %>%
    select(group_var, var, cat, string)

  desc_cat <- data %>%
    select(id, wt, group_var, where(is.factor)) %>%
    pivot_longer(-c(id, wt, group_var), names_to = "var", values_to = "cat") %>%
    drop_na() %>%
    count(group_var, var, cat, wt = wt) %>%
    mutate(n = n/n_imps) %>%
    filter(!is.na(cat)) %>%
    group_by(group_var, var) %>%
    mutate(prop = n*100/sum(n),
           across(c(n, prop), round, 2),
           n = format(n, big.mark = ",") %>%
             trimws(),
           string = glue("{n} ({prop}%)")) %>%
    select(group_var, var, cat, string)

  desc_missing <- data %>%
    mutate(across(-group_var,  ~ ifelse(is.na(.x), 1, 0))) %>%
    group_by(group_var) %>%
    summarise(across(everything(), mean), .groups = "drop") %>%
    pivot_longer(-group_var, names_to = "var", values_to = "miss") %>%
    mutate(miss = glue("{round(miss*100, 2)}%"))

  pool_scalar <- function(mean, var, n){
    pooled <- pool.scalar(mean, var, n)

    tibble(mean = pooled$qbar,
           var = pooled$t)
  }

  desc_cont <- data %>%
    select(id, imp, wt, group_var, where(is.numeric)) %>%
    mutate(across(where(is.numeric), as.numeric)) %>%
    pivot_longer(-c(id, wt, imp, group_var), names_to = "cat", values_to = "value") %>%
    group_by(group_var, imp, cat) %>%
    summarise(mean = wtd.mean(value, wt),
              var = wtd.var(value, wt),
              n = n(), .groups = "drop") %>%
    mutate(i = ifelse(n_imps > 1, 1, 2)) %>%
    uncount(i) %>%
    select(-imp) %>%
    chop(c(mean, var)) %>%
    mutate(pool = pmap(list(mean, var, n), pool_scalar)) %>%
    select(-mean, -var, -n) %>%
    unnest(pool) %>%
    mutate(sd = sqrt(var),
           across(c(mean, sd), round, 2),
           string = glue("{mean} ({sd})")) %>%
    mutate(var = cat) %>%
    select(group_var, var, cat, string)

  desc_df <- bind_rows(desc_n, desc_cont, desc_cat) %>%
    left_join(desc_missing, by = c("group_var", "var"))

  return(desc_df)
}
