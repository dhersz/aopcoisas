library(data.table)
library(ggplot2)
library(cowplot)
library(ggtext)

set.seed(0)

dist <- data.table(tt = 1:120)
dist[, access := abs(rnorm(120))]
dist[, cum_access := cumsum(access)]

tema <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(arrow = arrow(type = "closed", length = unit(5, "points"))),
    axis.title.x = element_text(hjust = 1, margin = margin(t = -8)),
    axis.text.x = element_markdown(margin = margin())
  )

p1 <- ggplot(dist) +
  geom_line(aes(tt, cum_access), color = "gray40") +
  scale_x_continuous(name = "T") +
  scale_y_continuous(name = "A")

parrow <- ggplot() +
  geom_segment(
    aes(x = 0, xend = 1, y = 0, yend = 0),
    arrow = arrow(length = unit(10, "points"))
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(r = -15)
  )

janela1 <- c("t<sub>w1,min</sub>" = 15, "t<sub>w1,max</sub>" = 45)
janela2 <- c("t<sub>w2,min</sub>" = 75, "t<sub>w1,max</sub>" = 105)

access1 <- c(
  "a<sub>w1,min</sub>" = dist[tt == janela1[1]]$cum_access,
  "a<sub>w1,max</sub>" = dist[tt == janela1[2]]$cum_access
)
access2 <- c(
  "a<sub>w2,min</sub>" = dist[tt == janela2[1]]$cum_access,
  "a<sub>w2,max</sub>" = dist[tt == janela2[2]]$cum_access
)

p1_com_janelas <- ggplot(dist) +
  geom_line(aes(tt, cum_access), color = "gray40") +
  annotate(
    geom = "segment",
    x = janela1[1],
    xend = janela1[1],
    y = 0,
    yend = 40,
    linetype = "longdash",
    color = "firebrick3"
  ) +
  annotate(
    geom = "text",
    x = janela1[1] + 2,
    y = 1,
    color = "firebrick3",
    label = "w1",
    size = 3,
    hjust = 0,
    vjust = 0
  ) +
  annotate(
    geom = "segment",
    x = janela1[2],
    xend = janela1[2],
    y = 0,
    yend = 40,
    linetype = "longdash",
    color = "firebrick3"
  ) +
  annotate(
    geom = "segment",
    x = janela2[1],
    xend = janela2[1],
    y = 0,
    yend = 80,
    linetype = "longdash",
    color = "dodgerblue3"
  ) +
  annotate(
    geom = "text",
    x = janela2[1] + 2,
    y = 1,
    color = "dodgerblue3",
    label = "w2",
    size = 3,
    hjust = 0,
    vjust = 0
  ) +
  annotate(
    geom = "segment",
    x = janela2[2],
    xend = janela2[2],
    y = 0,
    yend = 80,
    linetype = "longdash",
    color = "dodgerblue3"
  )

p2 <- p1_com_janelas +
  geom_line(
    data = dist[tt >= janela1[1] & tt <= janela1[2]],
    mapping = aes(tt, cum_access),
    color = "firebrick3"
  ) +
  geom_line(
    data = dist[tt >= janela2[1] & tt <= janela2[2]],
    mapping = aes(tt, cum_access),
    color = "dodgerblue3"
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela1[1],
    y = dist[tt == janela1[1]]$cum_access,
    yend = dist[tt == janela1[1]]$cum_access,
    linetype = "dotted",
    color = "firebrick3",
    alpha = 0.5
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela1[2],
    y = dist[tt == janela1[2]]$cum_access,
    yend = dist[tt == janela1[2]]$cum_access,
    linetype = "dotted",
    color = "firebrick3",
    alpha = 0.5
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela2[1],
    y = dist[tt == janela2[1]]$cum_access,
    yend = dist[tt == janela2[1]]$cum_access,
    linetype = "dotted",
    color = "dodgerblue3",
    alpha = 0.5
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela2[2],
    y = dist[tt == janela2[2]]$cum_access,
    yend = dist[tt == janela2[2]]$cum_access,
    linetype = "dotted",
    color = "dodgerblue3",
    alpha = 0.5
  ) +
  scale_x_continuous(
    name = "T",
    breaks = c(janela1, janela2),
    expand = expansion(0, 0.05)
  ) +
  scale_y_continuous(
    name = "A",
    breaks = c(access1, access2),
    expand = expansion(0, 0.05)
  ) +
  tema +
  theme(
    axis.text.y = element_markdown(),
    axis.title.y = element_text(hjust = 1, angle = 0, margin = margin(r = -28)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 28)
  )

avg_access1 <- mean(dist[tt >= janela1[1] & tt <= janela1[2]]$cum_access)
avg_access2 <- mean(dist[tt >= janela2[1] & tt <= janela2[2]]$cum_access)

p3 <- p1_com_janelas +
  geom_segment(
    aes(x = janela1[1], xend = janela1[2], y = avg_access1, yend = avg_access1),
    color = "firebrick3"
  ) +
  geom_segment(
    aes(x = janela2[1], xend = janela2[2], y = avg_access2, yend = avg_access2),
    color = "dodgerblue3"
  ) +
  scale_x_continuous(
    name = "T", breaks = c(janela1, janela2),
    expand = expansion(0, 0.05)
  ) +
  scale_y_continuous(
    name = "A",
    breaks = c(avg_access1, avg_access2),
    labels = c(TeX("\\bar{$a_{w1}$}"), TeX("\\bar{$a_{w2}$}")),
    expand = expansion(0, 0.05)
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela1[1],
    y = avg_access1,
    yend = avg_access1,
    linetype = "dotted",
    color = "firebrick3",
    alpha = 0.5
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = janela2[1],
    y = avg_access2,
    yend = avg_access2,
    linetype = "dotted",
    color = "dodgerblue3",
    alpha = 0.5
  ) +
  tema +
  theme(
    axis.title.y = element_text(hjust = 1, angle = 0, margin = margin(r = -12)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 28)
  ) +
  expand_limits(x = 0, y = 0)

pf <- plot_grid(
  p2, parrow, p3,
  nrow = 1,
  rel_widths = c(1, 0.15, 1)
)

ggsave(
  "resultados/figura_esquematica_artigo_for.jpg",
  plot = pf,
  width = 16,
  height = 5,
  units = "cm"
)

