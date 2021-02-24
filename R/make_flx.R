make_flx <- function(data, header_lbls){
  flextable(data) %>%
    set_header_labels(values = header_lbls) %>%
    border_remove() %>%
    merge_v(1) %>%
    merge_h(part = "header") %>%
    border_inner_h(border = fp_border(color="grey50", width = 1, style = "dashed"),
                   part = "body") %>%
    hline_top(border = fp_border(color="black", width = 2), part = "all") %>%
    hline_bottom(border = fp_border(color="black", width = 2), part = "all") %>%
    fix_border_issues(part = "all") %>%
    align(j = 1:2, align = "right", part = "all") %>%
    align(j = 3:ncol(data), align="center", part = "all") %>%
    valign(j = 1, valign = "center") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    autofit()
}
