
plotSaveFun <- function(obj, fn) {
    ggsave(filename = paste0(fn, ".png"),
       plot = obj, 
       path = "Img/", width = 6, height = 6, units = "in",
       dpi = 125)
}
