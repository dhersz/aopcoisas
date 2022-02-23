library(sf)
library(data.table)
library(ggplot2)
library(cowplot)

setores <- st_sf(
  renda_total = c(50, 30, 20),
  id_setor = 1:3,
  geometry = st_sfc(
    st_polygon(list(rbind(c(0, 0), c(7, 0), c(6, 3), c(4, 5), c(3, 9), c(0, 9), c(0, 0)))),
    st_polygon(list(rbind(c(7, 0), c(6, 3), c(9, 9), c(9, 0), c(7, 0)))),
    st_polygon(list(rbind(c(6, 3), c(4, 5), c(3, 9), c(9, 9), c(6, 3))))
  )
)

grade <- st_sf(
  populacao = c(10, 0, 5, 35, 20, 0, 10, 20, 0),
  id_grade = 1:9,
  geometry = st_sfc(
    st_polygon(list(rbind(c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)))),
    st_polygon(list(rbind(c(3, 0), c(6, 0), c(6, 3), c(3, 3), c(3, 0)))),
    st_polygon(list(rbind(c(6, 0), c(9, 0), c(9, 3), c(6, 3), c(6, 0)))),
    st_polygon(list(rbind(c(0, 3), c(3, 3), c(3, 6), c(0, 6), c(0, 3)))),
    st_polygon(list(rbind(c(3, 3), c(6, 3), c(6, 6), c(3, 6), c(3, 3)))),
    st_polygon(list(rbind(c(6, 3), c(9, 3), c(9, 6), c(6, 6), c(6, 3)))),
    st_polygon(list(rbind(c(0, 6), c(3, 6), c(3, 9), c(0, 9), c(0, 6)))),
    st_polygon(list(rbind(c(3, 6), c(6, 6), c(6, 9), c(3, 9), c(3, 6)))),
    st_polygon(list(rbind(c(6, 6), c(9, 6), c(9, 9), c(6, 9), c(6, 6))))
  )
)

# interpolacao convencional

inter_conv <- st_interpolate_aw(setores, grade, extensive = TRUE)
inter_conv$id_setor <- NULL

# interpolacao dasimetrica

grade$area_grade <- st_area(grade)
setores$area_setor <- st_area(setores)

inter_dasim <- st_intersection(grade, setores)
setDT(inter_dasim)
inter_dasim[, area_pedaco := st_area(st_sf(inter_dasim))]
inter_dasim[, prop_area_grade := area_pedaco / area_grade]
inter_dasim[, pop_pedaco := prop_area_grade * populacao]
inter_dasim[, pop_setor := sum(pop_pedaco), by = id_setor]
inter_dasim[, prop_pop_setor := pop_pedaco / pop_setor]
inter_dasim[is.nan(prop_pop_setor), prop_pop_setor := 0]
inter_dasim[, renda_pedaco := renda_total * prop_pop_setor]
inter_dasim <- inter_dasim[, .(renda_total = sum(renda_pedaco)), by = id_grade]
inter_dasim[as.data.table(grade), on = "id_grade", geometry := i.geometry]
inter_dasim <- st_sf(inter_dasim)

# figura

borda <- st_union(grade)

tema_padrao <- theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      vjust = 0.5,
      size = 11,
      margin = margin(b = 0)
    )
  )

plot_setores <- ggplot(setores) +
  geom_sf(aes(fill = renda_total), color = "ivory3") +
  geom_sf(data = borda, color = "ivory4", fill = NA) +
  scale_fill_gradient(low = "ivory", high = "dodgerblue4", limits = c(0, 70)) +
  ggtitle("Distribuição de renda total\npor setor censitário") +
  tema_padrao

plot_grade <- ggplot(grade) +
  geom_sf(aes(fill = populacao), color = "ivory3") +
  geom_sf(data = borda, color = "ivory4", fill = NA) +
  scale_fill_gradient(low = "ivory", high = "firebrick4") +
  ggtitle("Distribuição de população\nna grade") +
  tema_padrao

plot_inter_conv <- ggplot(inter_conv) +
  geom_sf(aes(fill = renda_total), color = "ivory3") +
  geom_sf(data = borda, color = "ivory4", fill = NA) +
  scale_fill_gradient(low = "ivory", high = "dodgerblue4", limits = c(0, 70)) +
  ggtitle("Interpolação espacial\nconvencional") +
  tema_padrao

plot_inter_dasim <- ggplot(inter_dasim) +
  geom_sf(aes(fill = renda_total), color = "ivory3") +
  geom_sf(data = borda, color = "ivory4", fill = NA) +
  scale_fill_gradient(low = "ivory", high = "dodgerblue4", limits = c(0, 70)) +
  ggtitle("Interpolação espacial\ndasimétrica") +
  tema_padrao

titulo_cima <- ggdraw() + draw_label("Dados de entrada", size = 15, angle = 90)
plot_cima <- plot_grid(
  titulo_cima,
  plot_setores,
  plot_grade,
  nrow = 1,
  rel_widths = c(0.3, 3, 3)
)

titulo_baixo <- ggdraw() + draw_label("Resultados", size = 15, angle = 90)
plot_baixo <- plot_grid(
  titulo_baixo,
  plot_inter_conv,
  plot_inter_dasim,
  nrow = 1,
  rel_widths = c(0.3, 3, 3)
)

plot_final <- plot_grid(plot_cima, plot_baixo, ncol = 1)

ggsave(
  "resultados/proposta_figura_esquematica_interp_dasim.png",
  plot = plot_final,
  width = 4.4,
  height = 4,
  units = "in"
)
