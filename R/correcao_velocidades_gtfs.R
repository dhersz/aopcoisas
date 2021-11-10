corrigir_velocidades_gtfs <- function() {

  anos <- 2017:2020
  cidades <- munis_list$munis_df$abrev_muni

  # pra cada ano e cidade, checa quais possuem arquivos gtfs
  for (ano in anos) {
    for (cidade in cidades) {
      message(cidade, " - ", ano)

      dir_arquivos <- file.path("../../otp/graphs", ano, cidade)
      arquivos <- list.files(dir_arquivos)

      arquivos_gtfs <- arquivos[grepl("\\.zip$", arquivos)]

      if (!identical(arquivos_gtfs, character(0))) {

        dir_resultado <- file.path("../../data/gtfs", cidade, ano)
        if (!dir.exists(dir_resultado)) {
          dir.create(dir_resultado, recursive = TRUE)
        }

        # pra cada arquivo gtfs encontrado, corrige a velocidade e salva na nova
        # pasta com nome atualizado
        for (antigo_gtfs in arquivos_gtfs) {
          caminho_antigo_gtfs <- file.path(dir_arquivos, antigo_gtfs)

          novo_gtfs <- corrigir_velocidades(caminho_antigo_gtfs)
          caminho_novo_gtfs <- file.path(
            dir_resultado,
            sub("\\.zip", "_updated_speed.zip", antigo_gtfs)
          )

          gtfstools::write_gtfs(novo_gtfs, caminho_novo_gtfs)
        }
      }
    }
  }

}



corrigir_velocidades <- function(caminho) {
  gtfs <- gtfstools::read_gtfs(caminho)

  if (gtfsio::check_files_exist(gtfs, "shapes")) {
    viagens <- gtfstools::get_trip_speed(gtfs)
    viagens_problematicas <- viagens[speed > 80 | speed < 2]$trip_id

    # talvez adicionar um na.rm = TRUE aqui depois
    velocidade_media <- mean(
      viagens[speed <= 80 & speed >= 2,]$speed
    )

    novo_gtfs <- gtfstools::set_trip_speed(
      gtfs,
      viagens_problematicas,
      speed = velocidade_media
    )
  }

  return(gtfs)
}
