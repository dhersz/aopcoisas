corrigir_gtfs_rio <- function(ano = c(2018, 2019, 10000)) {
  ano <- ano[1]

  caminho_gtfs <- data.table::fcase(
    ano == 2018, "../../data-raw/gtfs/rio/2018/gtfs_rio_fetranspor_2018-11_fixed_subway.zip",
    ano == 2019, "../../data-raw/gtfs/rio/2019/gtfs_rio_fetranspor_2019-10_fixed_subway.zip",
    ano == 10000, "../msc-thesis/otp/graphs/rio_no_inter/gtfs_fetranspor_fsub_ninter_nfresc_nout.zip"
  )

  gtfs <- gtfstools::read_gtfs(caminho_gtfs, encoding = "UTF-8")

  # os GTFS dos dois anos (e o usado na minha dissertação, que ta no ano 10000
  # mas na verdade é o mesmo de 2019 mas sem algumas linhas) têm o mesmo
  # problema: todas as viagens de vlt estão alocadas nos sábados, mesmo que
  # naquela época os vlts ja rodassem normalmente durante a semana.
  # ao total, 6 diferents viagens de vlt são encontradas no feed. elas não
  # parecem fazer distinção entre dia de semana e final de semana, conforme
  # descritas nas tabelas - parecem ser 3 rotas distintas, com 2 direções
  # diferentes, totalizando 6 viagens. a solução escolhida, nesse caso, é de
  # alocar essas viagens para todos os dias de semana, inclusive mantendo os
  # serviços operantes no sábado também.

  rotas_vlt <- gtfs$routes[grepl("VLT", route_short_name)]$route_id
  servicos_vlt <- gtfs$trips[route_id %chin% rotas_vlt]$service_id

  gtfs$calendar[
    service_id %chin% servicos_vlt,
    `:=`(
      monday = 1,
      tuesday = 1,
      wednesday = 1,
      thursday = 1,
      friday = 1
    )
  ]

  caminho_final <- ifelse(
    ano != 10000,
    sub(".zip", "_fixed_vlt.zip", caminho_gtfs),
    "../../data/gtfs/rio/2019/gtfs_fetranspor_fsub_ninter_nfresc_nout_fvlt.zip"
  )
  gtfstools::write_gtfs(gtfs, caminho_final)
}
