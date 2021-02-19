make_lbls <- function(pretty_names, data){
  enframe(pretty_names, name = "var", value = "var_clean") %>%
    mutate(cat = map(var, ~ paste0(.x,levels(data[[.x]])))) %>%
    unnest(cols = cat) %>%
    mutate(cat_clean = ifelse(var==cat,
                              var_clean,
                              str_replace(cat, var, "")),
           index = row_number()) %>%
    group_by(var) %>%
    mutate(level = row_number(), levels = n(),
           ref_cat = first(cat_clean)) %>%
    ungroup() %>%
    mutate(var_ref = glue("{var_clean} (ref. {ref_cat})"),
           var_ref = ifelse(var == cat, var_clean, var_ref)) %>%
    select(-ref_cat)
}
