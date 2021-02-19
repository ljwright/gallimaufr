make_dummies <- function(df, cols){
  make_dum <- function(data, the_var){
    data %>%
      mutate(!!the_var := as.numeric(.data[[!!the_var]]),
             default = 1) %>%
      pivot_wider(names_from = all_of(the_var), values_from = default,
                  values_fill = list(default = 0),
                  names_prefix = glue("{the_var}_")) %>%
      select(-matches(glue("{the_var}_1")))
  }
  reduce(cols, make_dum, .init = df)
}
