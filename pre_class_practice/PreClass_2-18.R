
view (gapminder)

dat <- gapminder

p1 <- dat %>% 
  ggplot(aes(x = year,
             y = lifeExp, 
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent)

p1
ggsave('my_graph.png', plot = p1)

p2 <- p1 + theme_bw()
p3 <- p2 + theme_dark()

p1 + p2
p1 / p3
(p1 + p2) / p3 + plot_annotation('test title')


c1 <- ggplot(mtcars, aes())