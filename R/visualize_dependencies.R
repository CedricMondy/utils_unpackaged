visualize_dependencies <- function(directory) {
  require(pak)
  pak::pkg_install(c("codetools", "visNetwork", "stringr"))

  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)

  analyze_defined_functions <- function(files) {
    definedFunctions <- list()

    for (file in files) {
      definedFunctions[[basename(file)]] <- readLines(file) |>
        stringr::str_extract_all(pattern = "^.* <- function") |>
        unlist() |>
        stringr::str_remove_all(pattern = " <- function") |>
        stringr::str_trim()
    }

    definedFunctions
  }

  defined_functions <- analyze_defined_functions(files)

  nodes <- data.frame(
    id = character(),
    label = character(),
    group = character(),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )

  # Ajouter les nœuds pour les fichiers
  nodes <- rbind(nodes, data.frame(
    id = basename(files),
    label = basename(files),
    group = "file",
    stringsAsFactors = FALSE
  ))

  # Ajouter les nœuds pour les fonctions et les arêtes fichier -> fonction
  for (file in basename(files)) {
    for (fn in defined_functions[[file]]) {
      nodes <- rbind(nodes, data.frame(
        id = fn,
        label = fn,
        group = "function",
        stringsAsFactors = FALSE
      ))
      edges <- rbind(edges, data.frame(
        from = file,
        to = fn,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Ajouter les arêtes fonction -> fichier (utilisation)
  for (file in files) {
    file_code <- readLines(file)
    for (def_file in basename(files)) {
      for (fn in defined_functions[[def_file]]) {
        if (any(stringr::str_detect(string = file_code, pattern = fn)) && basename(file) != def_file) {
          edges <- rbind(edges, data.frame(
            from = fn,
            to = basename(file),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  # Supprimer les doublons
  nodes <- unique(nodes)
  edges <- unique(edges)

  graph <- list(nodes = nodes, edges = edges)

  visNetwork::visNetwork(nodes = graph$nodes, edges = graph$edges) |>
    visNetwork::visNodes(shape = "ellipse") |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) |>
    visNetwork::visLayout(randomSeed = 123) |>
    visNetwork::visPhysics(stabilization = list(enabled = TRUE, iterations = 200)) |>
    visNetwork::visInteraction(dragNodes = FALSE, dragView = TRUE, zoomView = TRUE)
}

