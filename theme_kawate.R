# theme_kawate
# このテーマは theme_classic() を基準としています。
# 二つの凡例の間の空白を削除したテーマ
theme_kawate = 
  function(base_size = 11, 
           base_family = "",
           base_line_size = base_size/22,
           base_rect_size = base_size/22) {
  theme_classic(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size,
                base_rect_size = base_rect_size) %+replace%
    theme(axis.title.x = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.8), angle = 90),
          axis.line = element_line(),
          legend.title = element_blank(),
          legend.position = c(0, 1),
          legend.justification = c(0,1),
          legend.background = element_blank(),
          legend.spacing.y = unit(0, "lines"),
          legend.text = element_text(margin = unit(c(0,0,0,0.5), "char")),
          legend.margin = margin(l = 0.5, t = 0, b = 0, unit = "lines"),
          strip.background = element_blank(),
          strip.text = element_blank())  
}