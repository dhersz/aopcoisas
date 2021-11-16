corrigir_velocidades_gtfs <- function(anos = NULL, cidades = NULL, log = TRUE) {

  if (is.null(anos)) anos <- 2017:2020
  if (is.null(cidades)) cidades <- munis_list$munis_df$abrev_muni

  if (log) {
    caminho_log <- "../../data/gtfs/log.txt"
    con <- file(caminho_log)
    on.exit(close(con), add = TRUE)
    sink(caminho_log)
    on.exit(sink(), add = TRUE)
  }

  # pra cada ano e cidade, checa quais possuem arquivos gtfs
  for (ano in anos) {
    for (cidade in cidades) {
      cat(cidade, "-", ano, "\n")

      arquivos_gtfs <- lista_arquivos_gtfs(ano, cidade)

      if (!identical(arquivos_gtfs, character(0))) {

        withCallingHandlers(
          {
            dir_resultado <- file.path("../../data/gtfs", cidade, ano)
            if (!dir.exists(dir_resultado)) {
              dir.create(dir_resultado, recursive = TRUE)
            }

            # pra cada arquivo gtfs encontrado, corrige a velocidade e salva na
            # nova pasta com nome atualizado
            for (antigo_gtfs in arquivos_gtfs) {
              nome_gtfs <- basename(antigo_gtfs)

              cat("  *", nome_gtfs, "\n")
              cat("    - caminho antigo:", antigo_gtfs, "\n")

              novo_gtfs <- corrigir_velocidades(antigo_gtfs, log, ano, cidade)
              caminho_novo_gtfs <- file.path(
                dir_resultado,
                sub("\\.zip", "_updated_speed.zip", nome_gtfs)
              )

              gtfstools::write_gtfs(novo_gtfs, caminho_novo_gtfs)
              cat("    - caminho final:", caminho_novo_gtfs, "\n")
            }
          },
          warning = function(cnd) if (log) print(cnd$message)
        )

        if (log) cat("\n")
      }
    }
  }

}



lista_arquivos_gtfs <- function(ano, cidade) {

  if (ano == 2019 && (cidade == "goi" || cidade == "rec")) {
    dir_arquivos <- file.path("../../data-raw/gtfs", cidade, ano)
  } else if (ano == 2020 && (cidade == "goi" || cidade == "rec")) {
    dir_arquivos <- file.path("../../data-raw/gtfs", cidade, ano - 1)
  } else {
    dir_arquivos <- file.path("../../otp/graphs", ano, cidade)
  }

  arquivos <- list.files(dir_arquivos)
  arquivos <- arquivos[grepl("\\.zip$", arquivos)]

  if ((ano == 2019 || ano == 2020) && (cidade == "rec")) {
    arquivos <- arquivos[grepl("2019-09_mod", arquivos)]
  }

  arquivos_gtfs <- file.path(dir_arquivos, arquivos)

  return(arquivos_gtfs)

}



corrigir_velocidades <- function(caminho_gtfs, log, ano, cidade) {
  gtfs <- gtfstools::read_gtfs(caminho_gtfs)

  caminho_shape_municipio <- file.path(
    "../../data-raw/municipios",
    ano,
    paste0("municipio_", cidade, "_", ano, ".rds")
  )
  shape_municipio <- readRDS(caminho_shape_municipio)

  if (gtfsio::check_file_exists(gtfs, "shapes")) {
    gtfs <- gtfstools::filter_by_sf(gtfs, shape_municipio)

    viagens <- gtfstools::get_trip_speed(gtfs)

    if (log) cat("    - total de viagens:", nrow(viagens), "\n")

    # talvez adicionar um na.rm = TRUE aqui depois
    velocidade_media <- mean(
      viagens[speed <= 80 & speed >= 2,]$speed
    )

    if (log) cat("    - velocidade media:", velocidade_media, "\n")

    viagens_problematicas <- viagens[speed > 80 | speed < 2]$trip_id

    if (log) {
      cat("    - viagens problematicas:", length(viagens_problematicas), "\n")
      cat("      > muito lentas:", nrow(viagens[speed < 2]), "\n")
      cat("      > muito rapidas:", nrow(viagens[speed > 80]), "\n")
    }

    gtfs <- gtfstools::set_trip_speed(
      gtfs,
      viagens_problematicas,
      speed = velocidade_media
    )
  }

  return(gtfs)
}
