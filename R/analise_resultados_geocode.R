#' Análise dos resultados do geocode da nova base
#'
#' @description
#' Calcula o percentual de erro do resultado do geocode feito com a nova base,
#' segundo as categorias apresentadas pelo Marcus: `VALID`, `GOOD`, `AVERAGE` e
#' `BAD`. Apresenta esses resultados também segundo o `score` de corte.
#'
#' A função `analisar_resultados_galileo()` roda a mesma análise pros resultados
#' do Galileo (fora a análise por `score`, já que não existe esse campo), e a
#' `gerar_analises_geocode()` é uma wrapper pra rodar as análises pra todas as
#' bases.
#'
#' @param base Qual das bases de análise utilizar. Utiliza o primeiro valor do
#'   vetor.
#'
#' @return Chamada pelos side-effects, cria um gráfico com os resultados.
#'
#' @export
analisar_resultados_geocode <- function(base = c("address", "address_zip", "completo")) {

  base <- base[1]

  # le os dados e transforma a classificacao em factor

  amostra <- data.table::fread(
    paste0("../../data/geocode/streetmap_eval/validated_sample_", base, ".csv")
  )

  classificacao <- c("VALID", "GOOD", "AVERAGE", "BAD")
  labels <- c("VAL.", "GD.", "AVE.", "BAD")
  amostra[
    ,
    geocode_result := factor(
      geocode_result,
      levels = classificacao,
      labels = labels
    )
  ]

  # cria varias "bases", cada uma guardando dados de diferentes intervalos de
  # avaliacao do geocode

  valores_limites <- c(70, 75, 80, 85, 90, 95, 100)

  base_qualidade <- lapply(
    valores_limites,
    function(i) {
      base <- data.table::copy(amostra)
      base <- base[geocode_score >= i]
      base[
        ,
        intervalo := factor(
          paste0(">= ", i),
          levels = paste0(">= ", valores_limites)
        )
      ]
    }
  )
  base_qualidade <- data.table::rbindlist(base_qualidade)

  # transforma o tipo de geocode em factor e ordena ele segundo a qtd de vezes
  # que cada categoria aparece

  qtd_tipo <- base_qualidade[, .N, by = geocode_type]
  qtd_tipo <- qtd_tipo[order(-N)]
  base_qualidade[
    ,
    geocode_type := factor(geocode_type, levels = qtd_tipo$geocode_type)
  ]

  # prepara uma base pra gerar labels pro grafico

  pcts <- base_qualidade[, .N, by = .(geocode_type, geocode_result, intervalo)]
  pcts[, labels := N / sum(N) * 100, by = .(geocode_type, intervalo)]
  pcts[, labels := paste0(formatC(labels, format = "f", digits = 1), "%")]

  # prepara uma base pra gerar o total de observacoes por facet

  totais <- pcts[, sum(N), by = .(geocode_type, intervalo)]

  # prepara o grafico

  grafico <- ggplot2::ggplot(base_qualidade) +
    ggplot2::facet_grid(geocode_type ~ intervalo) +
    ggplot2::geom_bar(aes(geocode_result)) +
    ggplot2::geom_text(
      data = pcts,
      aes(geocode_result, N, label = labels),
      size = 2.4,
      vjust = -0.3
    ) +
    ggplot2::geom_text(
      data = totais,
      aes(4, 8100, label = V1),
      size = 2.7,
      vjust = 0.7
    ) +
    ggplot2::ylim(0, 8100) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )

  # cria uma pasta e salva o grafico

  if (!dir.exists("./resultados")) dir.create("./resultados")

  if (!dir.exists("./resultados/analise_resultados_geocode"))
    dir.create("./resultados/analise_resultados_geocode")

  nome_arquivo <- paste0(
    "./resultados/analise_resultados_geocode/",
    "analise_", base, ".png"
  )

  ggplot2::ggsave(
    nome_arquivo,
    width = 23,
    height = nrow(qtd_tipo) * 2.5,
    units = "cm",
    dpi = 150
  )

}


#' @rdname analisar_resultados_geocode
#' @export
analisar_resultados_galileo <- function() {

  # le os dados e transforma a classificacao em factor

  amostra <- data.table::fread(
    "../../data/geocode/streetmap_eval/validated_sample_galileo.csv"
  )

  classificacao <- c("VALID", "GOOD", "AVERAGE", "BAD")
  labels <- c("VAL.", "GD.", "AVE.", "BAD")
  amostra[
    ,
    geocode_result := factor(
      geocode_result,
      levels = classificacao,
      labels = labels
    )
  ]

  # transforma a precisao de geocode em factor

  prcs <- c("4 Estrelas", "3 Estrelas", "2 Estrelas", "1 Estrela")
  amostra[, geocode_precision := factor(geocode_precision, levels = prcs)]

  # prepara uma base pra gerar labels pro grafico

  pcts <- amostra[, .N, by = .(geocode_precision, geocode_result)]
  pcts[, labels := N / sum(N) * 100, by = .(geocode_precision)]
  pcts[, labels := paste0(formatC(labels, format = "f", digits = 1), "%")]

  # prepara uma base pra gerar o total de observacoes por facet

  totais <- pcts[, sum(N), by = .(geocode_precision)]

  # prepara o grafico

  grafico <- ggplot2::ggplot(amostra) +
    ggplot2::facet_wrap(~ geocode_precision, ncol = 4) +
    ggplot2::geom_bar(aes(geocode_result)) +
    ggplot2::geom_text(
      data = pcts,
      aes(geocode_result, N, label = labels),
      size = 2.4,
      vjust = -0.3
    ) +
    ggplot2::geom_text(
      data = totais,
      aes(4, 8100, label = V1),
      size = 2.7,
      vjust = 0.7
    ) +
    ggplot2::ylim(0, 8100) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )

  # cria uma pasta e salva o grafico

  if (!dir.exists("./resultados")) dir.create("./resultados")

  if (!dir.exists("./resultados/analise_resultados_geocode"))
    dir.create("./resultados/analise_resultados_geocode")

  nome_arquivo <- paste0(
    "./resultados/analise_resultados_geocode/",
    "analise_galileo.png"
  )

  ggplot2::ggsave(
    nome_arquivo,
    width = 15,
    height = 4,
    units = "cm",
    dpi = 150
  )

}


#' @rdname analisar_resultados_geocode
#' @export
gerar_analises_geocode <- function() {
  bases <- c("address", "address_zip", "completo")
  invisible(lapply(bases, analisar_resultados_geocode))
  invisible(analisar_resultados_galileo())
}
