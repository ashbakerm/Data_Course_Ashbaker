
library(ggplot2)
library(ggthemes)

iris_versicolor <- subset(iris, Species == "versicolor")

ggplot(iris_versicolor, aes(x = factor(Sepal.Length), y = Petal.Length, color = Species, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.3, linewidth = 5) +
  geom_point(size = 12, shape = 24, stroke = 6, position = position_jitter(width = 0.2, height = 0)) +
  scale_y_reverse() +
  scale_color_manual(values = c("cyan")) +
  scale_fill_manual(values = c("limegreen")) +
  labs(
    title = "vErSiCoLoR",
    x = "SoMeTHinG in cM?",
    y = "iDeK bUt iTs uPsIdE dOwN"
  ) +
  theme(
    plot.title = element_text(size = 30, family = "Comic Sans MS", face = "italic", color = "purple"),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1, color = "darkred"),
    axis.text.y = element_text(size = 18, color = "darkgreen"),
    axis.title.x = element_text(size = 25, face = "bold", color = "orange"),
    axis.title.y = element_text(size = 25, angle = 180, color = "red"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "pink", color = "black", linewidth = 3),
    panel.background = element_rect(fill = "yellow"),
    panel.grid.major = element_line(color = "black", linetype = "dotted", linewidth = 2),
    panel.grid.minor = element_line(color = "gray", linetype = "twodash", linewidth = 1)
  )

ggsave("UglyGraph1_fixed.png", width = 16, height = 6, dpi = 300, bg = "white")
