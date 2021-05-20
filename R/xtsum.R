xtsum <- function(df, id){
  df %>%
    rename(id = {{ id }}) %>%
    select(id, where(is.numeric)) %>%
    pivot_longer(-id, names_to = "var") %>%
    group_by(var, id) %>%
    mutate(u_id = mean(value, na.rm = TRUE)) %>%
    group_by(var) %>%
    mutate(u_pop = mean(value, na.rm = TRUE),
           w = value - u_id + u_pop,
           t = value) %>%
    group_by(var, id) %>%
    mutate(b = ifelse(row_number()==1, u_id, NA)) %>%
    group_by(var) %>%
    summarise(mean = mean(t, na.rm = TRUE),
              across(c(t, w, b),
                     list(sd = ~ sd(.x, na.rm = TRUE)),
                     .names = "{.fn}_{.col}"))
}
