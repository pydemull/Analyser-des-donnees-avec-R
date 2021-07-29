plot_distri <- function(x = NULL, binwidth = NULL) {
  require(ggplot2)
  require(magrittr)
  require(patchwork)
  
  data <- as.data.frame(x)
  names(data) <- "x"
  
  g1 <- 
    data %>%
    ggplot(aes(x = x)) +
    geom_histogram(binwidth = binwidth, fill = "white", color = "black") +
    coord_cartesian(xlim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))) +
    theme(axis.title.x = element_blank())
  
  g2 <- 
    data %>% 
    ggplot(aes(x = x)) +
    geom_boxplot() +
    coord_cartesian(xlim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))) +
    theme(axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank())
  
  g3 <- 
    data %>% 
    ggplot(aes(x = x)) +
    geom_point(aes(x = x, y = ""), shape = 21, fill = "white", alpha = 0.5, size = 3,
               position = position_jitter(seed = 123, width = 0.04)) +
    ylab("") +
    coord_cartesian(xlim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  g1 + g2 + g3 + plot_layout(nrow = 3, heights = c(2, 0.2, 0.2))
}