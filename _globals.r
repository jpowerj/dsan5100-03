set.seed(5100)
# knitr::opts_template$set(
#   tikz_settings = list(
#     fig.ext = "svg",
#     fig.align = "center",
#     engine.opts = list(
#       dvisvgm.opts = "--font-format=woff"
#     )
#   ),
#   #tikz_settings_fancy = list(
#   #  fig.ext = "svg",
#   #  fig.align = "center",
#   #  engine.opts = list(
#   #    dvisvgm.opts = "--font-format=woff",
#   #    template = "../assets/code/tikz2pdf.tex"
#   #  )
#   #)
# )
# disp <- function(df, obs_per_page = 6, custom_callback = NULL, ...) {
#   # If length of df is less than obs_per_page, disable pagination
#   dom_str <- "tp"
#   if (nrow(df) <= obs_per_page) {
#     dom_str <- "t"
#   }
#   return(
#     DT::datatable(
#       df,
#       extensions = c("FixedColumns", "FixedHeader"),
#       options = list(
#         pageLength = obs_per_page,
#         scrollX = TRUE,
#         paging = TRUE,
#         dom = dom_str,
#         fixedHeader = TRUE,
#         filter = FALSE,
#         ordering = FALSE,
#         language = list(
#           "paginate" = list(
#             "previous" = "<i class='bi bi-chevron-left'></i>",
#             "next" = "<i class='bi bi-chevron-right'></i>"
#           )
#         )
#       ),
#       callback = custom_callback
#     )
#   )
# }

#library(knitr)
## define a method for objects of the class data.frame
#knit_print.data.frame <- function(x, ...) {
#    #res <- paste(c("", "", disp(x)), collapse = "\n")
#    asis_output(disp(x))
#}
## register the method
#registerS3method("knit_print", "data.frame", knit_print.data.frame)

# For slides
library(ggplot2)
cbPalette <- c(
    "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7"
)
options(ggplot2.discrete.colour = cbPalette)
# Theme generator, for given sizes
dsan_theme <- function(plot_type = "full", base_size = 16) {
    if (plot_type == "full") {
        custom_base_size <- 16
    } else if (plot_type == "half") {
        custom_base_size <- 22
    } else if (plot_type == "quarter_small") {
      custom_base_size <- 25
    } else if (plot_type == "quarter") {
        custom_base_size <- 28
    } else {
        # plot_type == "custom"
        custom_base_size <- base_size
    }
    theme <- theme_classic(base_size = custom_base_size) +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.title = element_text(hjust = 0.5),
            legend.box.background = element_rect(colour = "black")
        )
    return(theme)
}

knitr::opts_chunk$set(fig.align = "center")
g_pointsize <- 5
g_linesize <- 1
# Technically it should always be linewidth
g_linewidth <- 1
g_textsize <- 14

remove_legend_title <- function() {
  return(
    theme(
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm")
    )
  )
}

remove_legend <- function() {
  return(
    theme(
      legend.position = "none"
    )
  )
}
