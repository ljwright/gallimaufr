make_lbls <- function(pretty_names, data){
  enframe(pretty_names, name = "var", value = "var_clean") %>%
    mutate(cat = map(var, ~ paste0(.x,levels(data[[.x]]))),
           desc_cat = map(var, ~ levels(data[[.x]]))) %>%
    unnest(cols = c(cat, desc_cat)) %>%
    mutate(desc_cat = ifelse(is.na(desc_cat), var, desc_cat),
           cat_clean = ifelse(var == desc_cat, var_clean, desc_cat),
           index = row_number()) %>%
    group_by(var) %>%
    mutate(level = row_number(), levels = n(),
           ref_cat = first(cat_clean)) %>%
    ungroup() %>%
    mutate(var_ref = glue("{var_clean} (ref. {ref_cat})"),
           var_ref = ifelse(var == cat, var_clean, var_ref)) %>%
    select(-ref_cat)
}
