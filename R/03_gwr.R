# --- Kodowanie UTF-8 ---
Sys.setlocale("LC_CTYPE", "pl_PL.UTF-8")
options(encoding = "UTF-8")
options(scipen = 999, digits = 3)
Sys.setenv(LANG = "en")

# Ustaw reczne pasmo GWR. Wartosc 0 oznacza automatyczny dobor pasma przez CV.
RECZNE_PASMO_GWR <- 61

# Ustalamy sciezki tak, aby skrypt dzialal z katalogu projektu i z folderu bezdrog.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
# Skrypt lezy w R/, projekt to katalog nadrzedny. Pozwalamy tez na
# uruchamianie z katalogu glownego dla zgodnosci z poprzednim ukladem.
scripts_dir <- script_dir
project_dir <- if (basename(script_dir) == "R") {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  script_dir
}
base_dir <- project_dir

data_dir <- file.path(project_dir, "data")

wydruki_dir <- file.path(base_dir, "wydruki")
wykresy_dir <- file.path(base_dir, "wykresy")
spatial_r_wykresy_dir <- file.path(wykresy_dir, "spatial_R_bez_logDrogi")
stargazer_dir <- file.path(base_dir, "stargazer", "gwr_tests_bez_logDrogi")
stargazer_tex_dir <- file.path(stargazer_dir, "tex")
tabele_dir <- file.path(base_dir, "tabele", "gwr_tests_bez_logDrogi")
overleaf_dir <- file.path(base_dir, "Overleaf")
overleaf_media_dir <- file.path(overleaf_dir, "media")
overleaf_tabele_dir <- file.path(overleaf_dir, "tabele")
overleaf_stargazer_dir <- file.path(overleaf_dir, "stargazer")

dir.create(wydruki_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(wykresy_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(spatial_r_wykresy_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(stargazer_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(stargazer_tex_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tabele_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_media_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_tabele_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(overleaf_stargazer_dir, showWarnings = FALSE, recursive = TRUE)
SAVE_PDF_PLOTS <- TRUE

# Wczytujemy liste plikow realnie uzywanych przez licm2/maintest.tex.
source(file.path(scripts_dir, "publish_helpers.R"), encoding = "UTF-8")
init_maintest_publisher(project_dir, overleaf_dir)

# Pakiety instalujemy lokalnie w projekcie.
local_r_lib <- file.path(base_dir, "Rlib")
project_r_lib <- file.path(project_dir, "Rlib")
dir.create(local_r_lib, showWarnings = FALSE, recursive = TRUE)
dir.create(project_r_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(local_r_lib, project_r_lib, .libPaths()))

requiredPackages <- c(
  "sf",
  "sp",
  "spdep",
  "GWmodel",
  "spgwr",
  "gwrr",
  "dplyr",
  "stringr",
  "readxl",
  "stargazer",
  "GGally",
  "mice"
)

# Wczytujemy pakiety potrzebne do GWR, map i tabel.
for (pkg in requiredPackages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, lib = local_r_lib, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

MICE_IMPUTE_COLS <- c("logMieszkaniaOddaneNa1000", "Przychodnie")
MICE_PREDICTOR_COLS <- unique(c(
  "ySaldo",
  "logPrzystankiNa1000",
  "logWskaznikG",
  "logWynagrodzenia",
  "M2NaOsobe",
  "logMieszkaniaOddaneNa1000",
  "Przychodnie",
  "Bezrobocie",
  "Miejska",
  "Wiejska",
  "logMieszkaniaNa1000",
  "mieszkania",
  "MieszkaniaOddaneNa1000",
  "Populacja"
))
MICE_M <- 10
MICE_MAXIT <- 2  # No chained dependency between impute targets, so 1-2 sweeps suffice.
MICE_SEED <- 20240517
MICE_COMPLETE_ACTION <- 1
MICE_REPORT_PREFIX <- "gwr_tests_bez_logDrogi_mice"
WMNK_MICE_SCRIPT <- file.path(scripts_dir, "01_mnk_wmnk_mice.R")
WMNK_MICE_BUNDLE_PATH <- file.path(base_dir, "mice", "WMNKmice_completed_datasets.rds")
# Zmniejszamy liczbe symulacji Monte Carlo dla wariantu bez logDrogi, zeby skrocic czas obliczen.
GWR_MONTE_CARLO_NSIMS <- 19

load_wmnk_mice_bundle <- function() {
  # Wczytuje wspolne 10 imputacji z WMNKmice.R; gdy ich nie ma, tworzy je z pelnego zbioru WMNK.
  if (file.exists(WMNK_MICE_BUNDLE_PATH)) {
    return(readRDS(WMNK_MICE_BUNDLE_PATH))
  }
  wmnk_env <- new.env(parent = globalenv())
  sys.source(WMNK_MICE_SCRIPT, envir = wmnk_env)
  raw_data <- wmnk_env$load_model_data()
  wmnk_env$impute_model_missing_with_mice(raw_data, return_all = TRUE, export_all = TRUE)
}

prepare_mice_model_data <- function(bundle, model_cols) {
  # Ustala jedna wspolna probe dla wszystkich 10 imputacji i zachowuje kolejnosc Kod.
  completed <- bundle$completed_datasets
  if (is.null(completed) || length(completed) == 0) {
    stop("Brak listy completed_datasets w artefakcie WMNKmice.", call. = FALSE)
  }
  missing_cols <- setdiff(model_cols, names(completed[[1]]))
  if (length(missing_cols) > 0) {
    stop(sprintf("W imputowanym zbiorze WMNK brakuje kolumn: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  complete_codes <- lapply(completed, function(d) {
    d$Kod[complete.cases(d[, model_cols, drop = FALSE])]
  })
  common_codes <- Reduce(intersect, complete_codes)
  lapply(completed, function(d) {
    current <- d[d$Kod %in% common_codes, model_cols, drop = FALSE]
    current[match(common_codes, current$Kod), , drop = FALSE]
  })
}

is_usable_mice_predictor <- function(column) {
  # Sprawdza, czy predyktor ma wystarczajaco informacji dla modelu imputacji.
  observed <- column[!is.na(column)]
  length(observed) > 1 && length(unique(observed)) > 1
}

save_mice_imputation_report <- function(report, imputed_values, imputations = NULL) {
  # Zapisuje raport MICE, zeby mozna bylo opisac uzupelnienie brakow w pracy.
  write.csv(
    report,
    file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_imputacja_brakow.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  if (length(imputed_values) > 0) {
    write.csv(
      do.call(rbind, imputed_values),
      file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_imputowane_wartosci.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  logged_events <- imputations$loggedEvents
  if (!is.null(logged_events) && nrow(logged_events) > 0) {
    write.csv(
      logged_events,
      file.path(wydruki_dir, paste0(MICE_REPORT_PREFIX, "_logged_events.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
}

impute_model_missing_with_mice <- function(data) {
  # Uzupelnia MICE tylko te zmienne, ktore ograniczaja probe modeli GWR bez logDrogi.
  missing_required <- setdiff(MICE_IMPUTE_COLS, names(data))
  if (length(missing_required) > 0) {
    stop(sprintf("Brakuje kolumn do imputacji MICE: %s", paste(missing_required, collapse = ", ")))
  }

  missing_before <- colSums(is.na(data[, MICE_IMPUTE_COLS, drop = FALSE]))
  target_cols <- names(missing_before)[missing_before > 0]
  if (length(target_cols) == 0) {
    empty_report <- data.frame(
      variable = MICE_IMPUTE_COLS,
      missing_before = as.integer(missing_before),
      missing_after = as.integer(missing_before),
      imputed = 0L,
      method = "",
      m = MICE_M,
      maxit = MICE_MAXIT
    )
    save_mice_imputation_report(empty_report, list())
    return(data)
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  predictor_cols <- unique(c(MICE_IMPUTE_COLS, intersect(MICE_PREDICTOR_COLS, numeric_cols)))
  bad_targets <- target_cols[vapply(data[, target_cols, drop = FALSE], function(column) {
    sum(!is.na(column)) < 2
  }, logical(1))]
  if (length(bad_targets) > 0) {
    stop(sprintf("Za malo obserwacji do imputacji MICE dla: %s", paste(bad_targets, collapse = ", ")))
  }

  usable_predictors <- predictor_cols[vapply(data[, predictor_cols, drop = FALSE], function(column) {
    !any(is.na(column)) && is_usable_mice_predictor(column)
  }, logical(1))]
  predictor_cols <- unique(c(MICE_IMPUTE_COLS, usable_predictors))
  impute_input <- data[, predictor_cols, drop = FALSE]

  method <- mice::make.method(impute_input)
  method[] <- ""
  method[target_cols] <- "pmm"
  predictor_matrix <- mice::make.predictorMatrix(impute_input)
  predictor_matrix[,] <- 0
  for (target_col in target_cols) {
    predictor_matrix[target_col, setdiff(usable_predictors, target_col)] <- 1
  }

  imputations <- mice::mice(
    impute_input,
    m = MICE_M,
    maxit = MICE_MAXIT,
    method = method,
    predictorMatrix = predictor_matrix,
    printFlag = FALSE,
    seed = MICE_SEED
  )
  completed <- mice::complete(imputations, action = MICE_COMPLETE_ACTION)

  imputed_values <- list()
  for (target_col in target_cols) {
    missing_idx <- which(is.na(data[[target_col]]))
    data[missing_idx, target_col] <- completed[missing_idx, target_col]
    imputed_values[[target_col]] <- data.frame(
      Kod = data$Kod[missing_idx],
      variable = target_col,
      imputed_value = data[missing_idx, target_col]
    )
  }

  missing_after <- colSums(is.na(data[, MICE_IMPUTE_COLS, drop = FALSE]))
  report <- data.frame(
    variable = MICE_IMPUTE_COLS,
    missing_before = as.integer(missing_before[MICE_IMPUTE_COLS]),
    missing_after = as.integer(missing_after[MICE_IMPUTE_COLS]),
    imputed = as.integer(missing_before[MICE_IMPUTE_COLS] - missing_after[MICE_IMPUTE_COLS]),
    method = ifelse(MICE_IMPUTE_COLS %in% target_cols, method[MICE_IMPUTE_COLS], ""),
    m = MICE_M,
    maxit = MICE_MAXIT
  )
  save_mice_imputation_report(report, imputed_values, imputations)

  if (any(missing_after[target_cols] > 0)) {
    stop("Imputacja MICE nie uzupelnila wszystkich brakow w zmiennych modelowych.")
  }
  data
}

# Normalizujemy kody TERYT do siedmiu cyfr.
norm_code <- function(x) {
  x <- as.character(x)
  x <- gsub("\\.0$", "", x)
  stringr::str_pad(trimws(x), width = 7, side = "left", pad = "0")
}

# Wczytujemy pliki GUS z separatorem srednikiem.
read_gus <- function(name) {
  read.csv2(file.path(data_dir, name), check.names = FALSE, stringsAsFactors = FALSE)
}

latex_escape <- function(x) {
  # Zabezpiecza znaki specjalne przed bledami w LaTeX-u.
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

format_tex_value <- function(x) {
  # Formatuje wartosci przed wpisaniem ich do tabeli LaTeX.
  format_one <- function(value, column_name = "") {
    if (length(value) == 0 || is.na(value)) {
      return("")
    }
    if (is.logical(value)) {
      return(ifelse(value, "tak", "nie"))
    }
    if (is.numeric(value)) {
      if (!is.finite(value)) {
        return(as.character(value))
      }
      if (is_p_value_column(column_name)) {
        return(ifelse(value < 0.001, "<0.001", formatC(value, format = "f", digits = 3, decimal.mark = ".")))
      }
      if (is_integer_like_column(column_name) && abs(value - round(value)) < .Machine$double.eps^0.5) {
        return(as.character(as.integer(round(value))))
      }
      return(formatC(value, format = "f", digits = 3, decimal.mark = ","))
    }
    latex_escape(gsub("_", " ", as.character(value), fixed = TRUE))
  }
  column_name <- attr(x, "column_name", exact = TRUE)
  if (is.null(column_name)) {
    column_name <- ""
  }
  vapply(x, format_one, character(1), column_name = column_name)
}

is_p_value_column <- function(column_name) {
  # Rozpoznaje kolumny p-value niezaleznie od zapisu technicznego.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  normalized %in% c("p", "pvalue") || grepl("pvalue$", normalized)
}

is_integer_like_column <- function(column_name) {
  # Kolumny licznikowe zostawiamy bez miejsc po przecinku.
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  normalized %in% c("n", "na", "df", "deltadf", "liczbaobserwacji")
}

format_tex_column <- function(x, column_name) {
  # Przekazuje nazwe kolumny do formatowania wartosci.
  attr(x, "column_name") <- column_name
  format_tex_value(x)
}

polish_tex_column_name <- function(column_name) {
  # Zamienia techniczne nazwy kolumn na polskie etykiety bez podkreslen.
  mapping <- c(
    parameter = "Parametr",
    f_statistic = "Statystyka",
    numerator_df = "df1",
    denominator_df = "df2",
    p_value = "p-value",
    pvalue = "p-value",
    spatially_varying = "Zmienny przestrzennie",
    fixed_candidate = "Kandydat globalny",
    test = "Test",
    statistic = "Statystyka",
    method = "Metoda",
    bandwidth_type = "Typ pasma",
    bandwidth = "Szerokosc pasma",
    kernel = "Jadro",
    adj_R2 = "Skorygowane R2",
    R2 = "R2",
    statystyka = "Statystyka"
  )
  if (column_name %in% names(mapping)) {
    return(unname(mapping[[column_name]]))
  }
  normalized <- gsub("[^a-z0-9]", "", tolower(as.character(column_name)))
  if (normalized == "pvalue") {
    return("p-value")
  }
  label <- gsub("_", " ", as.character(column_name), fixed = TRUE)
  label <- gsub("p value", "p-value", label, fixed = TRUE)
  label
}

format_tex_dataframe <- function(df) {
  # Przygotowuje ramke danych do zapisu w LaTeX.
  formatted <- as.data.frame(Map(format_tex_column, df, names(df)), stringsAsFactors = FALSE, check.names = FALSE)
  names(formatted) <- vapply(names(df), polish_tex_column_name, character(1))
  formatted
}

format_p_value_tex <- function(value) {
  # Formatuje p-value zgodnie z wymogiem progu <0.001.
  ifelse(is.na(value), "", ifelse(value < 0.001, "<0.001", formatC(value, format = "f", digits = 3, decimal.mark = ".")))
}

format_number_tex <- function(value, digits = 3) {
  # Formatuje zwykle liczby z przecinkiem dziesietnym.
  ifelse(is.na(value), "", formatC(value, format = "f", digits = digits, decimal.mark = ","))
}

write_overleaf_table <- function(df, file_stub, caption, align = NULL) {
  # Zapisuje ramke danych jako tabele LaTeX w bezdrog i publikuje wybrane pliki do Overleaf.
  out_path <- file.path(tabele_dir, paste0(file_stub, ".tex"))
  df <- as.data.frame(df, check.names = FALSE)
  formatted <- format_tex_dataframe(df)
  if (is.null(align)) {
    align <- paste0("l", paste(rep("r", max(0, ncol(formatted) - 1)), collapse = ""))
  }
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(latex_escape(names(formatted)), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  body <- apply(formatted, 1, function(row) paste(row, collapse = " & "))
  lines <- c(lines, paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}", "\\end{table}", "")
  writeLines(lines, out_path, useBytes = TRUE)
  publish_if_maintest_uses(out_path, "tabele")
  invisible(out_path)
}

save_stargazer_or_summary <- function(models, file_stub, title) {
  # Zapisuje modele jako HTML i LaTeX; w razie bledu zapisuje zwykly tekst.
  # To zabezpiecza dlugie uruchomienie skryptu przed przerwaniem tylko dlatego,
  # ze Stargazer nie rozpoznal ktorejs klasy modelu przestrzennego.
  html_path <- file.path(stargazer_dir, paste0(file_stub, ".html"))
  tex_path <- file.path(stargazer_tex_dir, paste0(file_stub, ".tex"))
  txt_path <- file.path(stargazer_dir, paste0(file_stub, ".txt"))
  tex_txt_path <- file.path(stargazer_tex_dir, paste0(file_stub, ".txt"))
  tryCatch(
    {
      do.call(
        stargazer::stargazer,
        c(models, list(type = "html", title = title, header = FALSE, out = html_path))
      )
      do.call(
        stargazer::stargazer,
        c(models, list(type = "latex", title = title, header = FALSE, out = tex_path))
      )
      publish_if_maintest_uses(tex_path, "stargazer")
    },
    error = function(e) {
      for (txt_out_path in c(txt_path, tex_txt_path)) {
        sink(txt_out_path)
        cat("Stargazer nie obsluzyl tego zestawu modeli:\n")
        cat(conditionMessage(e), "\n\n")
        for (name in names(models)) {
          cat("\n====================\n")
          cat(name, "\n")
          cat("====================\n")
          print(models[[name]])
        }
        sink()
      }
    }
  )
}

save_spatial_plot <- function(file_stub, width = 8, height = 8, plot_expr) {
  # Zapisuje wykres jako PDF (wektor, ostre etykiety w bezjc.tex) i opcjonalnie
  # jako PNG do podgladu. Publikujemy wlasnie PDF, zeby pdflatex go zlapal.
  pdf_path <- file.path(spatial_r_wykresy_dir, paste0(file_stub, ".pdf"))
  png_path <- file.path(spatial_r_wykresy_dir, paste0(file_stub, ".png"))
  plot_code <- substitute(plot_expr)

  grDevices::cairo_pdf(pdf_path, width = width, height = height, family = "serif")
  par(family = "serif", mar = c(3.2, 1.2, 3.8, 1.2), cex = 0.95, cex.main = 1.8)
  eval(plot_code, envir = parent.frame())
  dev.off()
  publish_if_maintest_uses(pdf_path, "media")

  if (SAVE_PDF_PLOTS) {
    # SAVE_PDF_PLOTS=TRUE oznacza w nowym ukladzie "zapisz tez PNG do podgladu".
    png(png_path, width = width * 300, height = height * 300, res = 300, type = "cairo")
    par(family = "serif", mar = c(3.2, 1.2, 3.8, 1.2), cex = 0.95, cex.main = 1.8)
    eval(plot_code, envir = parent.frame())
    dev.off()
  }
}

safe_plot_name_R <- function(value) {
  # Upraszcza nazwy zmiennych do bezpiecznych nazw plikow.
  gsub("[^[:alnum:]]+", "_", value)
}

find_first_column <- function(data_names, candidates) {
  # Wybiera pierwsza pasujaca nazwe kolumny z listy kandydatow.
  # Pakiety GWR roznie nazywaja intercept i statystyki t, dlatego nie zakladamy jednej nazwy.
  hit <- intersect(candidates, data_names)
  if (length(hit) == 0) {
    NA_character_
  } else {
    hit[1]
  }
}

continuous_palette <- function(n = 100, diverging = TRUE) {
  # Tworzy palete kolorow dla map GWR.
  if (diverging) {
    colorRampPalette(c("#2b6cb0", "#f6e8c3", "#c51b29"))(n)
  } else {
    colorRampPalette(c("#edf8fb", "#6baed6", "#08306b"))(n)
  }
}

value_to_colors <- function(values, palette, vmin, vmax) {
  # Przypisuje wartosci liczbowe do kolorow z legendy.
  # Wartosci poza skala sa przycinane do krancow, zeby legenda i mapa byly spojne.
  clipped <- pmin(pmax(values, vmin), vmax)
  idx <- cut(clipped, breaks = seq(vmin, vmax, length.out = length(palette) + 1), include.lowest = TRUE, labels = FALSE)
  palette[idx]
}

draw_vertical_colorbar_R <- function(vmin, vmax, label, palette, legend_breaks = NULL) {
  # Rysuje pionowa legende dla mapy.
  breaks <- seq(vmin, vmax, length.out = length(palette) + 1)
  if (is.null(legend_breaks)) {
    # Dodajemy krance skali do podpisow legendy.
    legend_breaks <- pretty(c(vmin, vmax), n = 6)
    legend_breaks <- sort(unique(c(vmin, legend_breaks, vmax)))
  }
  legend_breaks <- legend_breaks[legend_breaks >= vmin & legend_breaks <= vmax]
  plot(NA, xlim = c(0, 1), ylim = c(vmin, vmax), axes = FALSE, xlab = "", ylab = "")
  rect(0.05, breaks[-length(breaks)], 0.72, breaks[-1], col = palette, border = NA)
  axis(
    4,
    at = legend_breaks,
    labels = formatC(legend_breaks, format = "f", digits = 1, decimal.mark = ","),
    cex.axis = 1.05,
    las = 1,
    lwd = 0,
    lwd.ticks = 0.8
  )
  mtext(label, side = 4, line = 2.85, cex = 1.08)
  box()
}

gwr_sensitive_limits_R <- function(values, significant = NULL, lower_q = 0.01, upper_q = 0.99) {
  # Wyznacza skale mapy odporna na pojedyncze skrajne wartosci.
  values <- as.numeric(values)
  if (!is.null(significant) && any(significant, na.rm = TRUE)) {
    scale_values <- values[significant & is.finite(values)]
  } else {
    scale_values <- values[is.finite(values)]
  }
  if (length(scale_values) == 0) {
    return(c(vmin = -1, vmax = 1))
  }
  robust_range <- stats::quantile(scale_values, probs = c(lower_q, upper_q), na.rm = TRUE, names = FALSE)
  limit <- max(abs(robust_range), na.rm = TRUE)
  if (!is.finite(limit) || limit == 0) {
    limit <- max(abs(scale_values), na.rm = TRUE)
  }
  if (!is.finite(limit) || limit == 0) {
    limit <- 1
  }
  c(vmin = -limit, vmax = limit)
}

plot_gwr_continuous_map_R <- function(gwr_sf, base_sf, value_col, signif_col, file_stub, title_text, vmin, vmax, diverging = TRUE, legend_label = value_col, legend_breaks = NULL) {
  # Rysuje mape lokalnych wspolczynnikow GWR z legenda.
  palette <- continuous_palette(160, diverging = diverging)
  values <- as.numeric(gwr_sf[[value_col]])
  significant <- rep(TRUE, length(values))
  if (!is.na(signif_col)) {
    significant <- abs(as.numeric(gwr_sf[[signif_col]])) >= 1.96
  }
  fill <- rep("#d9d9d9", length(values))
  fill[significant & is.finite(values)] <- value_to_colors(values[significant & is.finite(values)], palette, vmin, vmax)
  gwr_plot <- st_simplify(gwr_sf, dTolerance = 150, preserveTopology = TRUE)
  base_plot <- st_simplify(base_sf, dTolerance = 150, preserveTopology = TRUE)

  save_spatial_plot(file_stub, width = 9.7, height = 8.2, {
    layout(matrix(c(1, 2), nrow = 1), widths = c(5.6, 0.95))
    par(mar = c(1.0, 1.0, 3.0, 0.3), family = "serif")
    plot(st_geometry(base_plot), col = "#d9d9d9", border = NA, main = title_text, axes = FALSE)
    plot(st_geometry(gwr_plot), col = fill, border = NA, add = TRUE)
    plot(st_geometry(st_boundary(base_plot)), col = "grey55", lwd = 0.12, add = TRUE)
    par(mar = c(3.0, 0.4, 3.0, 4.0), family = "serif")
    draw_vertical_colorbar_R(vmin, vmax, legend_label, palette, legend_breaks)
  })
}

save_colorbar_R <- function(file_stub, vmin, vmax, label, diverging = TRUE) {
  # Zapisuje osobna pozioma legende dla map GWR.
  palette <- continuous_palette(100, diverging = diverging)
  save_spatial_plot(file_stub, width = 7.4, height = 1.15, {
    par(mar = c(2.6, 3.2, 0.4, 1.0), family = "serif")
    plot(NA, xlim = c(vmin, vmax), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
    breaks <- seq(vmin, vmax, length.out = length(palette) + 1)
    rect(breaks[-length(breaks)], 0, breaks[-1], 1, col = palette, border = NA)
    axis(1, cex.axis = 0.8)
    mtext(label, side = 1, line = 1.7, cex = 0.9)
  })
}

# ============================================================
# 1. Budowa zbioru danych do GWR
# ============================================================
# Ten blok odtwarza specyfikacje finalnego WMNK bez drog i bez odleglosci.
# Dane zrodlowe laczymy po kodzie TERYT, a potem tworzymy te same logarytmy i wskazniki.
czyn_przyst <- read_gus("CzynnePrzystanki.csv")
med_wynagr_path <- Sys.glob(file.path(data_dir, "MedianaWynagrod*Brutto2024.csv"))[1]
med_wynagr <- read.csv2(med_wynagr_path, check.names = FALSE, stringsAsFactors = FALSE)
mieszk_odd <- read_gus("MieszkaniaOddane.csv")
przych_na_10k <- read_gus("PrzychodnieNa10k.csv")
udz_bezrob <- read_gus("UdzialBezrobotnych.csv")
saldo <- read_gus("SaldoMigracjiNa1k.csv")
populacja <- read_gus("PopulacjaGmin.csv")
m2_mieszkania <- read_gus("M2NaOsobe_MieszkaniaNa1000Mieszkancow.csv")
wskaznik_g <- readxl::read_excel(file.path(data_dir, "WskaznikG.xlsx"))
wskaznik_g_kod <- norm_code(wskaznik_g[[1]])
wskaznik_g_value <- as.numeric(wskaznik_g[[3]])
if (length(wskaznik_g_kod) != length(wskaznik_g_value)) {
  stop("Kolumny kodu gminy i Wskaznika G maja rozne dlugosci.")
}

# Budujemy dane zgodne z modelem WMNK bez drog i bez odleglosci.
data_full <- data.frame(Kod = norm_code(czyn_przyst$Kod))
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(saldo$Kod), ySaldo = saldo[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$Przystanki <- as.numeric(czyn_przyst[[3]])

# Populacja jest rozbita na dwie kolumny, wiec sumujemy je do jednego mianownika.
populacja$Populacja <- as.numeric(populacja[[3]]) + as.numeric(populacja[[4]])
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(populacja$Kod), Populacja = populacja$Populacja),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logPrzystankiNa1000 <- log(data_full$Przystanki / data_full$Populacja * 1000)
data_full <- merge(
  data_full,
  data.frame(Kod = wskaznik_g_kod, WskaznikG = wskaznik_g_value),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logWskaznikG <- log(data_full$WskaznikG)

# Z miesiecznych kolumn wynagrodzen liczymy srednia roczna.
med_wynagr$srednia <- rowMeans(med_wynagr[, 3:ncol(med_wynagr)], na.rm = TRUE)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(med_wynagr$Kod), logWynagrodzenia = log(med_wynagr$srednia)),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(mieszk_odd$Kod), mieszkania = as.numeric(mieszk_odd[[3]])),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$logMieszkaniaNa1000 <- log(data_full$mieszkania / data_full$Populacja * 1000)
data_full$MieszkaniaOddaneNa1000 <- data_full$mieszkania / data_full$Populacja * 1000
data_full$logMieszkaniaOddaneNa1000 <- log(data_full$MieszkaniaOddaneNa1000)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(przych_na_10k$Kod), Przychodnie = przych_na_10k[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(udz_bezrob$Kod), Bezrobocie = udz_bezrob[[3]]),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
# Dodajemy metraz na osobe; odleglosc zostaje poza modelem GWR.
data_full <- merge(
  data_full,
  data.frame(Kod = norm_code(m2_mieszkania$Kod), M2NaOsobe = as.numeric(m2_mieszkania[[3]])),
  by = "Kod",
  all.x = TRUE,
  sort = FALSE
)
data_full$Miejska <- as.integer(substr(data_full$Kod, 7, 7) == "1")
data_full$Wiejska <- as.integer(substr(data_full$Kod, 7, 7) == "2")
numeric_cols <- setdiff(names(data_full), "Kod")
for (col in numeric_cols) {
  data_full[[col]] <- as.numeric(data_full[[col]])
  data_full[[col]][!is.finite(data_full[[col]])] <- NA
}

x_cols <- c(
  "logWskaznikG",
  "logWynagrodzenia",
  "M2NaOsobe",
  "logMieszkaniaOddaneNa1000",
  "Przychodnie",
  "Bezrobocie",
  "Miejska",
  "Wiejska"
)

# Wczytujemy imputacje z pelnego zbioru WMNK i dopiero potem ustalamy probe oraz geometrie GWR.
wmnk_mice_bundle <- load_wmnk_mice_bundle()
model_cols <- c("Kod", "ySaldo", x_cols)
mice_model_data_list <- prepare_mice_model_data(wmnk_mice_bundle, model_cols)
data_full <- if (!is.null(wmnk_mice_bundle$single)) wmnk_mice_bundle$single else wmnk_mice_bundle$completed_datasets[[1]]
data_model <- mice_model_data_list[[1]]

# ============================================================
# 2. Geometria, benchmark OLS i benchmark WMNK
# ============================================================
# Przygotowujemy obiekt przestrzenny dla pakietow GWmodel i spgwr.
gmina <- st_read(file.path(data_dir, "A03_Granice_gmin.shp"), quiet = TRUE)
gmina$JPT_KOD_JE <- norm_code(gmina$JPT_KOD_JE)
gmina <- st_make_valid(gmina)
gmina <- st_transform(gmina, 2180)
gmina <- merge(gmina, data_model, by.x = "JPT_KOD_JE", by.y = "Kod", all = FALSE, sort = FALSE)
gmina <- gmina[!st_is_empty(gmina), ]
mice_model_data_list <- lapply(mice_model_data_list, function(d) {
  # Po zbudowaniu geometrii ustawiamy identyczna kolejnosc jednostek jak w obiektach Spatial.
  matched <- d[match(gmina$JPT_KOD_JE, d$Kod), , drop = FALSE]
  if (any(is.na(matched$Kod)) || !identical(matched$Kod, gmina$JPT_KOD_JE)) {
    stop("Nie udalo sie dopasowac imputowanych danych do kolejnosci geometrii.", call. = FALSE)
  }
  row.names(matched) <- NULL
  matched
})
data_model <- mice_model_data_list[[1]]
gmina_sp <- as(gmina, "Spatial")

gmina_mice_list <- lapply(mice_model_data_list, function(d) {
  # Tworzy kopie geometrii z wartosciami z danej imputacji dla lokalnych estymacji GWR.
  current_gmina <- gmina
  for (column_name in c("ySaldo", x_cols)) {
    current_gmina[[column_name]] <- d[[column_name]]
  }
  current_gmina
})
gmina_sp_mice_list <- lapply(gmina_mice_list, function(current_gmina) as(current_gmina, "Spatial"))

# Tworzymy macierz sasiedztwa dla testu Morana reszt GWR.
cont_nb <- poly2nb(gmina, queen = TRUE)
cont_listw <- nb2listw(cont_nb, style = "W", zero.policy = TRUE)

# Tworzymy tez wersje punktowa oparta na centroidach.
gmina_point <- gmina
gmina_point$point_geometry <- st_centroid(st_geometry(st_make_valid(gmina_point)))
st_geometry(gmina_point) <- "point_geometry"
gmina_sp_point <- as(gmina_point, "Spatial")

formula_gwr <- as.formula(paste("ySaldo ~", paste(x_cols, collapse = " + ")))
model_ols_gwr <- lm(formula_gwr, data = st_drop_geometry(gmina))

calculate_vif <- function(model) {
  # Liczy VIF dla regresorow benchmarkowego modelu OLS.
  x_matrix <- model.matrix(model)
  x_matrix <- x_matrix[, colnames(x_matrix) != "(Intercept)", drop = FALSE]
  data.frame(
    regressor = colnames(x_matrix),
    vif = sapply(seq_len(ncol(x_matrix)), function(idx) {
      y_aux <- x_matrix[, idx]
      x_aux <- x_matrix[, -idx, drop = FALSE]
      if (ncol(x_aux) == 0) {
        return(NA_real_)
      }
      r2_aux <- summary(lm(y_aux ~ x_aux))$r.squared
      1 / (1 - r2_aux)
    }),
    n = nrow(x_matrix),
    row.names = NULL
  )
}

vif_gwr_ols <- calculate_vif(model_ols_gwr)
write.csv(vif_gwr_ols, file.path(wydruki_dir, "gwr_bez_logDrogi_ols_vif_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Drukujemy VIF przed dlugimi etapami estymacji GWR.
cat("\n--- VIF dla benchmarku OLS przed estymacja GWR ---\n")
print(vif_gwr_ols)
sink(file.path(wydruki_dir, "gwr_bez_logDrogi_ols_vif_R.txt"))
cat("VIF dla benchmarku OLS przed estymacja GWR\n\n")
print(vif_gwr_ols)
sink()

save_stargazer_or_summary(
  list(OLS = model_ols_gwr),
  "gwr_tests_bez_logDrogi_ols_benchmark",
  "Benchmark OLS dla GWR z finalnymi zmiennymi WMNK bez odleglosci"
)

fit_wmnk_benchmark <- function(source_data) {
  # Odtwarza WMNK dla benchmarku bez drog i bez odleglosci.
  wmnk_cols <- c(
    "logWskaznikG",
    "logWynagrodzenia",
    "M2NaOsobe",
    "logMieszkaniaOddaneNa1000",
    "Przychodnie",
    "Bezrobocie",
    "Miejska",
    "Wiejska"
  )
  wmnk_data <- na.omit(source_data[, c("Kod", "ySaldo", wmnk_cols)])
  wmnk_formula <- as.formula(paste("ySaldo ~", paste(wmnk_cols, collapse = " + ")))
  ols_initial <- lm(wmnk_formula, data = wmnk_data)
  wmnk_data$cooks <- cooks.distance(ols_initial)
  cook_threshold <- 4 * mean(wmnk_data$cooks, na.rm = TRUE)
  wmnk_cook_data <- wmnk_data[wmnk_data$cooks <= cook_threshold, ]
  ols_cook <- lm(wmnk_formula, data = wmnk_cook_data)
  wmnk_cook_data$log_u2 <- log(pmax(residuals(ols_cook)^2, .Machine$double.xmin))
  variance_formula <- as.formula(paste("log_u2 ~", paste(wmnk_cols, collapse = " + ")))
  variance_model <- lm(variance_formula, data = wmnk_cook_data)
  weights <- 1 / exp(fitted(variance_model))
  wls <- lm(wmnk_formula, data = wmnk_cook_data, weights = weights)
  list(
    cols = wmnk_cols,
    data = wmnk_data,
    cook_data = wmnk_cook_data,
    cook_threshold = cook_threshold,
    ols_initial = ols_initial,
    ols_cook = ols_cook,
    variance_model = variance_model,
    wls = wls
  )
}

wmnk_benchmark <- fit_wmnk_benchmark(data_full)
wmnk_benchmark_row <- data.frame(
  model = "WMNK_bez_odleglosci",
  regressors = paste(wmnk_benchmark$cols, collapse = ","),
  n_before_cook = nrow(wmnk_benchmark$data),
  n_after_cook = nrow(wmnk_benchmark$cook_data),
  removed_cook = nrow(wmnk_benchmark$data) - nrow(wmnk_benchmark$cook_data),
  cook_threshold = wmnk_benchmark$cook_threshold,
  aic = AIC(wmnk_benchmark$wls),
  bic = BIC(wmnk_benchmark$wls),
  r2 = summary(wmnk_benchmark$wls)$r.squared,
  adj_r2 = summary(wmnk_benchmark$wls)$adj.r.squared,
  logLik = as.numeric(logLik(wmnk_benchmark$wls))
)
wmnk_benchmark_coef <- data.frame(
  variable = rownames(summary(wmnk_benchmark$wls)$coefficients),
  coefficient = summary(wmnk_benchmark$wls)$coefficients[, "Estimate"],
  std_error = summary(wmnk_benchmark$wls)$coefficients[, "Std. Error"],
  t_value = summary(wmnk_benchmark$wls)$coefficients[, "t value"],
  p_value = summary(wmnk_benchmark$wls)$coefficients[, "Pr(>|t|)"],
  row.names = NULL
)
write.csv(
  wmnk_benchmark_row,
  file.path(wydruki_dir, "gwr_bez_logDrogi_wmnk_benchmark_bez_odleglosci_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  wmnk_benchmark_coef,
  file.path(wydruki_dir, "gwr_bez_logDrogi_wmnk_benchmark_bez_odleglosci_wspolczynniki_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
save_stargazer_or_summary(
  list(WMNK_bez_odleglosci = wmnk_benchmark$wls),
  "gwr_tests_bez_logDrogi_wmnk_benchmark_bez_odleglosci",
  "Benchmark WMNK z finalnymi zmiennymi bez odleglosci dla GWR"
)

# ============================================================
# 3. Dobor pasma i estymacja glownego GWR
# ============================================================
# Dobieramy pasmo metoda walidacji krzyzowej dla jednego kernela gaussian.
extract_gwr_diagnostic <- function(model, candidates) {
  # Wyciaga wskazana statystyke z diagnostyki GWmodel, niezaleznie od wariantu nazwy.
  diagnostic <- model$GW.diagnostic
  for (candidate in candidates) {
    if (!is.null(diagnostic[[candidate]])) {
      return(as.numeric(diagnostic[[candidate]]))
    }
  }
  NA_real_
}

save_gwr_monte_carlo_result <- function(result) {
  # Zapisuje wynik testu Monte Carlo niestacjonarnosci parametrow GWR.
  output_txt <- file.path(wydruki_dir, "gwr_bez_logDrogi_montecarlo_R.txt")
  writeLines(capture.output(print(result)), output_txt, useBytes = TRUE)
  if (inherits(result, "error")) {
    write.csv(
      data.frame(test = "gwr.montecarlo", error = conditionMessage(result)),
      file.path(wydruki_dir, "gwr_bez_logDrogi_montecarlo_R.csv"),
      row.names = FALSE
    )
    return(invisible(NULL))
  }
  result_df <- as.data.frame(result)
  result_df$parameter <- row.names(result_df)
  row.names(result_df) <- NULL
  result_df <- result_df[, c("parameter", setdiff(names(result_df), "parameter")), drop = FALSE]
  write.csv(result_df, file.path(wydruki_dir, "gwr_bez_logDrogi_montecarlo_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
}

extract_gwr_residuals <- function(gwr_model) {
  # Wyciaga reszty z obiektu GWmodel, niezaleznie od wariantu nazwy kolumny w SDF.
  sdf_data <- as.data.frame(gwr_model$SDF)
  residual_col <- find_first_column(names(sdf_data), c("residual", "Residual", "residuals", "Residuals", "resid", "Resid"))
  if (is.na(residual_col)) {
    stop("Nie znaleziono kolumny reszt w obiekcie GWR.")
  }
  as.numeric(sdf_data[[residual_col]])
}

summarize_moran_test <- function(test_name, test_obj) {
  # Zamienia wynik testu Morana na krotka tabele do zapisu i porownan.
  if (inherits(test_obj, "error")) {
    return(data.frame(
      test = test_name,
      statistic = NA_real_,
      expectation = NA_real_,
      variance = NA_real_,
      p_value = NA_real_,
      method = conditionMessage(test_obj),
      row.names = NULL
    ))
  }
  statistic <- if (!is.null(test_obj$statistic)) unname(test_obj$statistic[1]) else NA_real_
  expectation <- if (!is.null(test_obj$estimate) && "Expectation" %in% names(test_obj$estimate)) unname(test_obj$estimate["Expectation"]) else NA_real_
  variance <- if (!is.null(test_obj$estimate) && "Variance" %in% names(test_obj$estimate)) unname(test_obj$estimate["Variance"]) else NA_real_
  p_value <- if (!is.null(test_obj$p.value)) test_obj$p.value else NA_real_
  method <- if (!is.null(test_obj$method)) test_obj$method else paste(capture.output(print(test_obj)), collapse = " ")
  data.frame(test = test_name, statistic = statistic, expectation = expectation, variance = variance, p_value = p_value, method = method, row.names = NULL)
}

fit_gwr_variant <- function(kernel_name, adaptive_flag, run_f_tests = TRUE, manual_bw = NULL, sp_data = gmina_sp) {
  # Dla recznego pasma pomijamy CV; w przeciwnym razie dobieramy pasmo jak dotychczas.
  if (is.null(manual_bw)) {
    bw_value <- bw.gwr(
      formula_gwr,
      data = sp_data,
      adaptive = adaptive_flag,
      kernel = kernel_name,
      approach = "CV"
    )
  } else {
    bw_value <- manual_bw
  }

  model <- gwr.basic(
    formula_gwr,
    data = sp_data,
    bw = bw_value,
    kernel = kernel_name,
    adaptive = adaptive_flag,
    F123.test = run_f_tests
  )

  data.frame(
    kernel = kernel_name,
    bandwidth_type = ifelse(adaptive_flag, "adaptive", "fixed"),
    bandwidth = as.numeric(bw_value),
    RSS = extract_gwr_diagnostic(model, c("RSS.gw", "RSS")),
    R2 = extract_gwr_diagnostic(model, c("gw.R2", "R2")),
    adj_R2 = extract_gwr_diagnostic(model, c("gwR2.adj", "gw.R2.adj", "adj.R2")),
    AIC = extract_gwr_diagnostic(model, c("AIC")),
    AICc = extract_gwr_diagnostic(model, c("AICc")),
    BIC = extract_gwr_diagnostic(model, c("BIC")),
    stringsAsFactors = FALSE
  ) -> row

  list(bw = bw_value, model = model, row = row)
}

find_gwr_se_column <- function(gwr_names, value_col, display_name) {
  # Szuka kolumny lokalnego bledu standardowego dla danego parametru GWR.
  find_first_column(
    gwr_names,
    c(
      paste0(value_col, "_SE"),
      paste0(value_col, ".SE"),
      paste0(value_col, "_se"),
      paste0(value_col, ".se"),
      paste0(display_name, "_SE"),
      paste0(make.names(display_name), "_SE")
    )
  )
}

find_gwr_t_column <- function(gwr_names, value_col, display_name) {
  # Szuka kolumny lokalnej statystyki t, gdy GWmodel nie zapisze osobnej kolumny SE.
  find_first_column(
    gwr_names,
    c(
      paste0(value_col, "_TV"),
      paste0(value_col, ".TV"),
      paste0(value_col, "_t"),
      paste0(value_col, ".t"),
      paste0(value_col, "_T"),
      paste0(value_col, ".T"),
      paste0(display_name, "_TV"),
      paste0(make.names(display_name), "_TV")
    )
  )
}

pool_gwr_local_coefficients <- function(gwr_models, unit_ids, coef_names) {
  # Pooluje lokalne parametry GWR lokalizacja po lokalizacji zgodnie z regulami Rubina.
  m <- length(gwr_models)
  sdf_list <- lapply(gwr_models, function(model) as.data.frame(model$SDF))
  pooled_parts <- list()
  for (display_name in coef_names) {
    beta_mat <- NULL
    se_mat <- NULL
    beta_col_used <- character(0)
    se_col_used <- character(0)
    for (imp_id in seq_along(sdf_list)) {
      sdf <- sdf_list[[imp_id]]
      gwr_names <- names(sdf)
      value_col <- find_first_column(gwr_names, c(display_name, make.names(display_name), gsub("[()]", "", display_name)))
      if (is.na(value_col)) {
        stop(sprintf("Brak lokalnego wspolczynnika GWR dla parametru %s w imputacji %s.", display_name, imp_id), call. = FALSE)
      }
      se_col <- find_gwr_se_column(gwr_names, value_col, display_name)
      t_col <- find_gwr_t_column(gwr_names, value_col, display_name)
      beta_values <- as.numeric(sdf[[value_col]])
      se_values <- rep(NA_real_, length(beta_values))
      if (!is.na(se_col)) {
        se_values <- as.numeric(sdf[[se_col]])
      } else if (!is.na(t_col)) {
        t_values <- as.numeric(sdf[[t_col]])
        se_values <- abs(beta_values / t_values)
        se_values[!is.finite(se_values)] <- NA_real_
      }
      beta_mat <- cbind(beta_mat, beta_values)
      se_mat <- cbind(se_mat, se_values)
      beta_col_used <- c(beta_col_used, value_col)
      se_col_used <- c(se_col_used, ifelse(is.na(se_col), ifelse(is.na(t_col), NA_character_, paste0("z_", t_col)), se_col))
    }
    qbar <- rowMeans(beta_mat, na.rm = TRUE)
    ubar <- rowMeans(se_mat^2, na.rm = TRUE)
    b <- apply(beta_mat, 1, stats::var, na.rm = TRUE)
    b[is.na(b)] <- 0
    total <- ubar + (1 + 1 / m) * b
    pooled_parts[[display_name]] <- data.frame(
      Kod = unit_ids,
      parameter = ifelse(display_name == "(Intercept)", "Intercept", display_name),
      m = m,
      beta = qbar,
      ubar = ubar,
      b = b,
      total_var = total,
      se = sqrt(total),
      riv = ifelse(ubar > 0, ((1 + 1 / m) * b) / ubar, NA_real_),
      lambda = ifelse(total > 0, ((1 + 1 / m) * b) / total, NA_real_),
      beta_columns = paste(unique(beta_col_used), collapse = ";"),
      se_columns = paste(unique(se_col_used), collapse = ";"),
      row.names = NULL
    )
  }
  do.call(rbind, pooled_parts)
}

summarize_gwr_rubin_pooling <- function(pooled_local) {
  # Zbiera lokalne wyniki Rubina do krotkiej tabeli diagnostycznej drukowanej w konsoli.
  do.call(rbind, lapply(split(pooled_local, pooled_local$parameter), function(part) {
    data.frame(
      parameter = part$parameter[1],
      locations = nrow(part),
      mean_beta = mean(part$beta, na.rm = TRUE),
      mean_se = mean(part$se, na.rm = TRUE),
      mean_ubar = mean(part$ubar, na.rm = TRUE),
      mean_b = mean(part$b, na.rm = TRUE),
      mean_total_var = mean(part$total_var, na.rm = TRUE),
      mean_fmi_proxy = mean(part$lambda, na.rm = TRUE),
      row.names = NULL
    )
  }))
}

# Uzywamy jednego kernela gaussian i od razu liczymy testy F dla glownego GWR.
selected_kernel <- "gaussian"
cat("Selected kernel:", selected_kernel, "\n")

if (!is.numeric(RECZNE_PASMO_GWR) || length(RECZNE_PASMO_GWR) != 1 || !is.finite(RECZNE_PASMO_GWR) || RECZNE_PASMO_GWR < 0) {
  stop("RECZNE_PASMO_GWR musi byc jedna nieujemna liczba; 0 oznacza dobor CV.")
}
manual_bandwidth <- list(
  # Gdy RECZNE_PASMO_GWR jest rozne od zera, pomijamy CV i przekazujemy te wartosc do modeli GWR.
  use_manual = RECZNE_PASMO_GWR != 0,
  bandwidth = if (RECZNE_PASMO_GWR != 0) as.numeric(RECZNE_PASMO_GWR) else NULL
)
if (manual_bandwidth$use_manual) {
  cat("CV bandwidth skipped. Manual bandwidth:", manual_bandwidth$bandwidth, "\n")
} else {
  cat("CV bandwidth will be calculated.\n")
}

# Liczymy tylko pasmo adaptacyjne.
bandwidth_fits <- list(
  # F123.test=TRUE on GWmodel is the dominant cost (~60 min at n=2477) and the
  # tex tables consume spgwr's LMZ.F*GWR.test instead, so we skip it here.
  adaptive = fit_gwr_variant(selected_kernel, adaptive_flag = TRUE, run_f_tests = FALSE, manual_bw = manual_bandwidth$bandwidth)
)
bandwidth_comparison <- do.call(rbind, lapply(bandwidth_fits, `[[`, "row"))
bandwidth_comparison <- bandwidth_comparison[order(bandwidth_comparison$AICc, bandwidth_comparison$AIC), ]
write.csv(bandwidth_comparison, file.path(wydruki_dir, "gwr_bez_logDrogi_bandwidth_comparison_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

bw_adaptive <- bandwidth_fits$adaptive$bw
bw_fixed <- NA_real_
bw_adaptive_point <- if (manual_bandwidth$use_manual) {
  manual_bandwidth$bandwidth
} else {
  bw.gwr(
    formula_gwr,
    data = gmina_sp_point,
    adaptive = TRUE,
    kernel = selected_kernel,
    approach = "CV"
  )
}

cat("Bandwidth selection for gaussian kernel:\n")
print(bandwidth_comparison)
cat("Bandwidth adaptive:", bw_adaptive, "\n")
cat("Bandwidth fixed:", bw_fixed, "\n")
cat("Bandwidth adaptive point:", bw_adaptive_point, "\n")

# Estymujemy glowny model GWR w pakiecie GWmodel.
gwr_model_adaptive <- bandwidth_fits$adaptive$model
print(gwr_model_adaptive)

# Estymujemy GWR na 10 imputacjach; przy pasmie recznym uzywamy tej samej wartosci, a przy CV dobieramy pasmo w kazdej imputacji.
gwr_mice_fits <- vector("list", length(gmina_sp_mice_list))
gwr_mice_fits[[1]] <- bandwidth_fits$adaptive
if (length(gmina_sp_mice_list) > 1) {
  for (imp_id in 2:length(gmina_sp_mice_list)) {
    cat(sprintf("GWR MICE: estymacja imputacji %s/%s\n", imp_id, length(gmina_sp_mice_list)))
    flush.console()
    gwr_mice_fits[[imp_id]] <- fit_gwr_variant(
      selected_kernel,
      adaptive_flag = TRUE,
      run_f_tests = FALSE,
      manual_bw = manual_bandwidth$bandwidth,
      sp_data = gmina_sp_mice_list[[imp_id]]
    )
  }
}
gwr_mice_models <- lapply(gwr_mice_fits, `[[`, "model")
gwr_mice_bandwidths <- data.frame(
  .imp = seq_along(gwr_mice_fits),
  bandwidth = as.numeric(vapply(gwr_mice_fits, `[[`, numeric(1), "bw")),
  row.names = NULL
)
gwr_rubin_local <- pool_gwr_local_coefficients(gwr_mice_models, gmina$JPT_KOD_JE, c("(Intercept)", x_cols))
gwr_rubin_summary <- summarize_gwr_rubin_pooling(gwr_rubin_local)
write.csv(gwr_mice_bandwidths, file.path(wydruki_dir, "gwr_bez_logDrogi_mice_bandwidths_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gwr_rubin_local, file.path(wydruki_dir, "gwr_bez_logDrogi_mice_rubin_local_coefficients_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gwr_rubin_summary, file.path(wydruki_dir, "gwr_bez_logDrogi_mice_rubin_summary_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Test Monte Carlo jest kosztowny, wiec uruchamiamy go osobno w GWRmontecarlo.R.
gwr_monte_carlo <- "Pominieto w tym skrypcie; uruchom bezdrog/GWRmontecarlo.R."

# Moran I dla reszt glownego GWR pokazuje, czy po modelu zostaje autokorelacja przestrzenna.
gwr_residuals_adaptive <- tryCatch(extract_gwr_residuals(gwr_model_adaptive), error = function(e) e)
gwr_residuals_moran <- tryCatch(
  {
    if (inherits(gwr_residuals_adaptive, "error")) {
      stop(conditionMessage(gwr_residuals_adaptive))
    }
    moran.test(gwr_residuals_adaptive, cont_listw, zero.policy = TRUE, alternative = "two.sided")
  },
  error = function(e) e
)
gwr_residuals_moran_df <- summarize_moran_test("Moran I reszt GWR GWmodel adaptive", gwr_residuals_moran)
write.csv(gwr_residuals_moran_df, file.path(wydruki_dir, "gwr_bez_logDrogi_residuals_moran_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Zapisujemy tabele LaTeX z wyborem pasma i porownaniem z MNK.
kernel_bandwidth_table <- data.frame(
  Jadro = tools::toTitleCase(bandwidth_comparison$kernel),
  "Typ pasma" = ifelse(bandwidth_comparison$bandwidth_type == "adaptive", "adaptacyjne", "stale"),
  "Szerokosc pasma" = bandwidth_comparison$bandwidth,
  RSS = bandwidth_comparison$RSS,
  R2 = bandwidth_comparison$R2,
  "Skorygowane R2" = bandwidth_comparison$adj_R2,
  AICc = bandwidth_comparison$AICc,
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(kernel_bandwidth_table, "gwr_kernel_bandwidth_comparison", "Porownanie szerokosci pasma dla kernela GWR", align = "@{}lrrrrrr@{}")

gwr_vs_mnk <- data.frame(
  Statystyka = c("R2", "Skorygowane R2", "AICc", "BIC", "Log-likelihood"),
  GWR = c(
    extract_gwr_diagnostic(gwr_model_adaptive, c("gw.R2", "R2")),
    extract_gwr_diagnostic(gwr_model_adaptive, c("gwR2.adj", "gw.R2.adj", "adj.R2")),
    extract_gwr_diagnostic(gwr_model_adaptive, c("AICc")),
    extract_gwr_diagnostic(gwr_model_adaptive, c("BIC")),
    -0.5 * extract_gwr_diagnostic(gwr_model_adaptive, c("AIC"))
  ),
  "Benchmark MNK" = c(
    summary(model_ols_gwr)$r.squared,
    summary(model_ols_gwr)$adj.r.squared,
    AIC(model_ols_gwr),
    BIC(model_ols_gwr),
    as.numeric(logLik(model_ols_gwr))
  ),
  row.names = NULL
)
write.csv(gwr_vs_mnk, file.path(wydruki_dir, "gwr_tests_bez_logDrogi_gwr_mnk_wmnk_stargazer.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_overleaf_table(gwr_vs_mnk, "gwr_vs_mnk_benchmark", "Porownanie GWR z benchmarkowym MNK", align = "@{}lcc@{}")

# ============================================================
# 4. Mapy lokalnych parametrow i diagnostyka kolinearnosci
# ============================================================
# Zapisujemy mapy lokalnych wspolczynnikow i lokalnego R2.
gwr_sf <- st_as_sf(gwr_model_adaptive$SDF)
if (is.na(st_crs(gwr_sf))) {
  st_crs(gwr_sf) <- st_crs(gmina)
}
gwr_names <- names(gwr_sf)
coef_candidates <- c("Intercept", "(Intercept)", x_cols)
coef_actual <- setNames(
  sapply(coef_candidates, function(v) {
    find_first_column(gwr_names, c(v, make.names(v), gsub("[()]", "", v)))
  }),
  coef_candidates
)
coef_actual <- coef_actual[!is.na(coef_actual)]
coef_actual <- coef_actual[!duplicated(unname(coef_actual))]

# Zapisujemy lokalne wspolczynniki jako kolumny ramki danych.
wsp <- as.data.frame(lapply(coef_actual, function(value_col) {
  as.numeric(gwr_sf[[value_col]])
}))
names(wsp) <- ifelse(names(coef_actual) == "(Intercept)", "Intercept", names(coef_actual))
write.csv(
  cor(wsp, use = "pairwise.complete.obs"),
  file.path(wydruki_dir, "gwr_bez_logDrogi_korelacje_parametrow_R.csv"), fileEncoding = "UTF-8")

# Tworzymy korelogram lokalnych wspolczynnikow.
gwr_correlogram <- GGally::ggpairs(
  wsp,
  lower = list(continuous = GGally::wrap("points", size = 0.25, alpha = 0.18, colour = "#333333")),
  diag = list(continuous = GGally::wrap("densityDiag", colour = "#1f3a93", fill = "#1f3a93", alpha = 0.25))
)
if (SAVE_PDF_PLOTS) {
  grDevices::cairo_pdf(
    file.path(spatial_r_wykresy_dir, "gwr_parametry_korelogram_R.pdf"),
    width = 12,
    height = 12,
    family = "serif"
  )
  print(gwr_correlogram)
  dev.off()
}
png(
  file.path(spatial_r_wykresy_dir, "gwr_parametry_korelogram_R.png"),
  width = 3600,
  height = 3600,
  res = 300,
  type = "cairo"
)
print(gwr_correlogram)
dev.off()
publish_if_maintest_uses(file.path(spatial_r_wykresy_dir, "gwr_parametry_korelogram_R.png"), "media")

# Liczymy lokalna kolinearnosc w modelu GWR.
diag_gwr <- gwr.collin.diagno(
  formula_gwr,
  data = gmina_sp,
  bw = bw_adaptive,
  kernel = "gaussian",
  adaptive = TRUE
)
diag_gwr_sf <- st_as_sf(diag_gwr$SDF)
if (is.na(st_crs(diag_gwr_sf))) {
  st_crs(diag_gwr_sf) <- st_crs(gmina)
}
write.csv(
  st_drop_geometry(diag_gwr_sf),
  file.path(wydruki_dir, "gwr_bez_logDrogi_lokalna_kolinearnosc_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
sink(file.path(wydruki_dir, "gwr_bez_logDrogi_lokalna_kolinearnosc_R.txt"))
print(diag_gwr)
cat("\nSummary diag_gwr:\n")
print(summary(diag_gwr))
sink()

# Mapujemy lokalne korelacje parametrow z diagnostyki GWR.
diag_gwr_names <- names(diag_gwr_sf)
diag_corr_cols <- grep("^Corr_", diag_gwr_names, value = TRUE)
if (FALSE && length(diag_corr_cols) > 0) {
  for (corr_col in diag_corr_cols) {
    corr_slug <- safe_plot_name_R(corr_col)
    corr_label <- sub("^Corr_", "", corr_col)
    plot_gwr_continuous_map_R(
      diag_gwr_sf,
      gmina,
      corr_col,
      NA_character_,
      paste0("gwr_lokalna_korelacja_", corr_slug, "_R"),
      paste("Lokalna korelacja GWR:", corr_label),
      -1,
      1,
      diverging = TRUE,
      legend_label = "Korelacja",
      legend_breaks = seq(-1, 1, by = 0.5)
    )
  }
}

# Histogram pokazuje rozklad lokalnych wartosci VIF.
diag_vif_cols <- grep("_VIF$", diag_gwr_names, value = TRUE)
if (length(diag_vif_cols) > 0) {
  vif_values <- as.numeric(unlist(st_drop_geometry(diag_gwr_sf[, diag_vif_cols, drop = FALSE]), use.names = FALSE))
  vif_values <- vif_values[is.finite(vif_values)]
  if (length(vif_values) > 0) {
    save_spatial_plot("gwr_lokalne_vif_histogram_R", width = 8.4, height = 5.8, {
      # Ograniczamy liczbe podpisow osi X, zeby histogram byl czytelny.
      vif_xlim <- range(vif_values, na.rm = TRUE)
      vif_x_ticks <- pretty(vif_xlim, n = 6)
      vif_breaks <- pretty(vif_xlim, n = 35)
      par(mar = c(4.6, 4.8, 3.2, 1.2), family = "serif")
      hist(
        vif_values,
        breaks = vif_breaks,
        col = "#6baed6",
        border = "white",
        main = "Histogram lokalnych VIF w modelu GWR",
        xlab = "Wartość statystyki VIF",
        ylab = "Częstość",
        xaxt = "n",
        xlim = c(min(vif_breaks, na.rm = TRUE), max(5.2, max(vif_breaks, na.rm = TRUE)))
      )
      vif_x_ticks_ext <- pretty(c(min(vif_breaks, na.rm = TRUE), max(5.2, max(vif_breaks, na.rm = TRUE))), n = 6)
      axis(1, at = vif_x_ticks_ext, labels = formatC(vif_x_ticks_ext, format = "f", digits = 1, decimal.mark = ","))
      abline(v = 5, col = "#fdae61", lwd = 1.4, lty = 2)
      legend(
        "topright",
        legend = "VIF = 5 (próg ostrzegawczy)",
        col = "#fdae61",
        lwd = 1.4,
        lty = 2,
        bty = "n"
      )
    })
  }
}

local_significance_rows <- list()
for (display_name in names(coef_actual)) {
  # Dla kazdego parametru zapisujemy udzial istotnych lokalnych wspolczynnikow
  # oraz osobna mape przestrzennego rozkladu oszacowan.
  value_col <- unname(coef_actual[[display_name]])
  signif_col <- find_first_column(
    gwr_names,
    c(
      paste0(value_col, "_TV"),
      paste0(value_col, ".TV"),
      paste0(value_col, "_t"),
      paste0(value_col, ".t"),
      paste0(value_col, "_T"),
      paste0(value_col, ".T")
    )
  )
  coeff_values <- as.numeric(gwr_sf[[value_col]])
  coeff_significant <- rep(TRUE, length(coeff_values))
  if (!is.na(signif_col)) {
    coeff_significant <- abs(as.numeric(gwr_sf[[signif_col]])) >= 1.96
  }
  # Zapisujemy udzial lokalnie istotnych wspolczynnikow.
  local_significance_rows[[length(local_significance_rows) + 1]] <- data.frame(
    parameter = ifelse(display_name == "(Intercept)", "Intercept", display_name),
    share_significant = mean(coeff_significant, na.rm = TRUE),
    mean_beta = mean(coeff_values, na.rm = TRUE),
    std_beta = stats::sd(coeff_values, na.rm = TRUE),
    t_column = ifelse(is.na(signif_col), NA_character_, signif_col),
    row.names = NULL
  )
  coeff_limits <- gwr_sensitive_limits_R(coeff_values, coeff_significant)
  vmin <- coeff_limits[["vmin"]]
  vmax <- coeff_limits[["vmax"]]
  # Ustawiamy symetryczna skale kolorow wzgledem zera.
  lim_abs <- max(abs(c(vmin, vmax)), na.rm = TRUE)
  vmin <- -lim_abs
  vmax <- lim_abs
  legend_breaks <- pretty(c(vmin, vmax), n = 6)
  legend_breaks <- sort(unique(c(vmin, legend_breaks, vmax)))
  legend_breaks <- legend_breaks[legend_breaks >= vmin & legend_breaks <= vmax]
  coeff_slug <- safe_plot_name_R(ifelse(display_name == "(Intercept)", "Intercept", display_name))
  plot_gwr_continuous_map_R(
    gwr_sf,
    gmina,
    value_col,
    signif_col,
    paste0("gwr_coef_", coeff_slug, "_R"),
    paste("Lokalny współczynnik GWR:", display_name),
    vmin,
    vmax,
    diverging = TRUE,
    legend_label = display_name,
    legend_breaks = legend_breaks
  )
}
write.csv(
  do.call(rbind, local_significance_rows),
  file.path(wydruki_dir, "gwr_local_significance_bez_logDrogi_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")

local_r2_col <- find_first_column(gwr_names, c("Local_R2", "localR2", "LocalR2", "local_R2"))
if (!is.na(local_r2_col)) {
  # Local R2 pokazuje, gdzie GWR poprawia lokalne dopasowanie.
  local_r2_range <- range(as.numeric(gwr_sf[[local_r2_col]]), na.rm = TRUE)
  plot_gwr_continuous_map_R(
    gwr_sf,
    gmina,
    local_r2_col,
    NA_character_,
    "gwr_local_r2_R",
    "Lokalne R² modelu GWR",
    local_r2_range[1],
    local_r2_range[2],
    diverging = FALSE,
    legend_label = "Lokalne R²"
  )
}

# ============================================================
# 5. Obiekt spgwr i testy diagnostyczne GWR
# ============================================================
# Estymujemy drugi obiekt GWR w pakiecie spgwr, potrzebny do testow.
# To nie jest powtorzenie glownego wyniku: testy LMZ/BFC wymagaja klasy z pakietu spgwr.
selected_gweight <- if (selected_kernel == "bisquare") {
  gwr.bisquare
} else {
  gwr.Gauss
}
bw_spgwr_adaptive <- if (manual_bandwidth$use_manual) {
  # spgwr przy pasmie adaptacyjnym oczekuje udzialu obserwacji, a nie liczby sasiadow.
  min(1, manual_bandwidth$bandwidth / nrow(gmina_sp))
} else {
  gwr.sel(
    formula_gwr,
    data = gmina_sp,
    adapt = TRUE,
    RMSE = TRUE,
    gweight = selected_gweight
  )
}
gwr_model_spgwr <- gwr(
  formula_gwr,
  data = gmina_sp,
  adapt = bw_spgwr_adaptive,
  hatmatrix = TRUE,
  gweight = selected_gweight
)
save_stargazer_or_summary(
  list(
    OLS = model_ols_gwr,
    GWR_GWmodel_adaptive = gwr_model_adaptive,
    GWR_spgwr_adaptive = gwr_model_spgwr
  ),
  "gwr_tests_bez_logDrogi_all_models",
  "Modele GWR z finalnymi zmiennymi WMNK bez odleglosci"
)
save_stargazer_or_summary(
  list(GWR_GWmodel_adaptive = gwr_model_adaptive),
  "gwr_tests_bez_logDrogi_gwmodel_adaptive",
  "Model GWR adaptive z finalnymi zmiennymi WMNK bez odleglosci"
)
save_stargazer_or_summary(
  list(GWR_spgwr_adaptive = gwr_model_spgwr),
  "gwr_tests_bez_logDrogi_spgwr_adaptive",
  "Model GWR adaptive z finalnymi zmiennymi WMNK bez odleglosci z spgwr"
)

# Liczymy testy diagnostyczne GWR na obiekcie z pakietu spgwr.
# F3 jest dalej uzywany do decyzji, ktore parametry moga byc globalne w modelu mieszanym.
test_outputs <- list(
  LMZ_F1 = tryCatch(LMZ.F1GWR.test(gwr_model_spgwr), error = function(e) e),
  LMZ_F2 = tryCatch(LMZ.F2GWR.test(gwr_model_spgwr), error = function(e) e),
  LMZ_F3 = tryCatch(LMZ.F3GWR.test(gwr_model_spgwr), error = function(e) e),
  BFC99 = tryCatch(BFC99.gwr.test(gwr_model_spgwr), error = function(e) e),
  BFC02 = tryCatch(BFC02.gwr.test(gwr_model_spgwr), error = function(e) e)
)

test_to_row <- function(test_name, test_obj) {
  # Zamienia wynik testu GWR na jeden wiersz tabeli.
  if (inherits(test_obj, "error")) {
    return(data.frame(test = test_name, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_, method = conditionMessage(test_obj)))
  }
  if (is.matrix(test_obj)) {
    p_values <- as.numeric(test_obj[, "Pr(>)"])
    return(data.frame(
      test = test_name,
      statistic = NA_real_,
      df1 = nrow(test_obj),
      df2 = NA_real_,
      p_value = min(p_values, na.rm = TRUE),
      method = "Macierz testow dla parametrow lokalnych; pelne wyniki w osobnej tabeli",
      row.names = NULL
    ))
  }
  statistic <- if (!is.null(test_obj$statistic)) unname(test_obj$statistic[1]) else NA_real_
  parameters <- if (!is.null(test_obj$parameter)) unname(test_obj$parameter) else c(NA_real_, NA_real_)
  df1 <- if (length(parameters) >= 1) parameters[1] else NA_real_
  df2 <- if (length(parameters) >= 2) parameters[2] else NA_real_
  p_value <- if (!is.null(test_obj$p.value)) test_obj$p.value else NA_real_
  method <- if (!is.null(test_obj$method)) test_obj$method else paste(capture.output(print(test_obj)), collapse = " ")
  data.frame(test = test_name, statistic = statistic, df1 = df1, df2 = df2, p_value = p_value, method = method, row.names = NULL)
}

f3_to_parameter_table <- function(test_obj, alpha = 0.05) {
  # Rozbija test F(3) na parametry i oznacza wspolczynniki bez istotnej zmiennosci przestrzennej.
  if (inherits(test_obj, "error")) {
    return(data.frame(
      parameter = character(),
      f_statistic = numeric(),
      numerator_df = numeric(),
      denominator_df = numeric(),
      p_value = numeric(),
      alpha = numeric(),
      spatially_varying = logical(),
      fixed_candidate = logical(),
      row.names = NULL
    ))
  }
  if (!is.matrix(test_obj)) {
    stop("Test F(3) powinien zwracac macierz wynikow dla parametrow.")
  }
  f3_df <- as.data.frame(test_obj, check.names = FALSE)
  f3_df$parameter <- rownames(test_obj)
  rownames(f3_df) <- NULL
  f3_df <- f3_df[, c("parameter", "F statistic", "Numerator d.f.", "Denominator d.f.", "Pr(>)")]
  names(f3_df) <- c("parameter", "f_statistic", "numerator_df", "denominator_df", "p_value")
  f3_df$alpha <- alpha
  f3_df$spatially_varying <- !is.na(f3_df$p_value) & f3_df$p_value < alpha
  f3_df$fixed_candidate <- !is.na(f3_df$p_value) & f3_df$p_value >= alpha
  f3_df
}

gwr_test_rows <- do.call(rbind, Map(test_to_row, names(test_outputs), test_outputs))
gwr_f3_parameter_table <- f3_to_parameter_table(test_outputs$LMZ_F3, alpha = 0.05)
# Parametry z p-value >= 0,05 traktujemy jako kandydatow do czesci globalnej w gwr.mixed().
gwr_f3_fixed_candidates <- gwr_f3_parameter_table[gwr_f3_parameter_table$fixed_candidate, , drop = FALSE]
gwr_mixed_intercept_fixed <- any(gwr_f3_fixed_candidates$parameter %in% c("(Intercept)", "Intercept", "X.Intercept."))
gwr_mixed_fixed_vars <- setdiff(gwr_f3_fixed_candidates$parameter, c("(Intercept)", "Intercept", "X.Intercept."))
gwr_mixed_fixed_vars <- intersect(gwr_mixed_fixed_vars, x_cols)

write.csv(
  gwr_f3_parameter_table,
  file.path(wydruki_dir, "gwr_bez_logDrogi_f3_zmiennosc_parametrow_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  gwr_f3_fixed_candidates,
  file.path(wydruki_dir, "gwr_bez_logDrogi_f3_fixed_vars_R.csv"),
  row.names = FALSE, fileEncoding = "UTF-8")

gwr_test_label <- function(test_name) {
  # Zamienia techniczna nazwe testu na podpis tabelaryczny.
  mapping <- c(LMZ_F1 = "LMZ F(1)", LMZ_F2 = "LMZ F(2)", BFC99 = "BFC99", BFC02 = "BFC02")
  if (test_name %in% names(mapping)) {
    unname(mapping[[test_name]])
  } else {
    gsub("_", " ", test_name, fixed = TRUE)
  }
}

gwr_test_conclusion <- function(test_name, p_value) {
  # Dopisuje krotki wniosek do testow diagnostycznych GWR.
  if (is.na(p_value)) {
    return("")
  }
  if (test_name == "LMZ_F1") {
    return(ifelse(p_value < 0.05, "GWR lepszy od MNK", "brak istotnej przewagi GWR nad MNK"))
  }
  ifelse(p_value < 0.05, "istotna poprawa GWR", "brak istotnej poprawy GWR")
}

gwr_test_tex <- gwr_test_rows[gwr_test_rows$test != "LMZ_F3", , drop = FALSE]
gwr_test_tex <- data.frame(
  Test = vapply(gwr_test_tex$test, gwr_test_label, character(1)),
  Statystyka = gwr_test_tex$statistic,
  df1 = gwr_test_tex$df1,
  df2 = gwr_test_tex$df2,
  "p-value" = gwr_test_tex$p_value,
  Wniosek = mapply(gwr_test_conclusion, gwr_test_tex$test, gwr_test_tex$p_value),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(gwr_test_tex, "gwr_testy_diagnostyczne", "Testy diagnostyczne modelu GWR", align = "@{}lrrrrl@{}")

gwr_parameter_label <- function(parameter_name) {
  # Ujednolica nazwy parametrow w tabeli F(3).
  ifelse(parameter_name %in% c("(Intercept)", "Intercept", "X.Intercept."), "Wyraz wolny", parameter_name)
}

gwr_f3_tex <- data.frame(
  Zmienna = vapply(gwr_f3_parameter_table$parameter, gwr_parameter_label, character(1)),
  Statystyka = gwr_f3_parameter_table$f_statistic,
  df1 = gwr_f3_parameter_table$numerator_df,
  df2 = gwr_f3_parameter_table$denominator_df,
  "p-value" = gwr_f3_parameter_table$p_value,
  Wniosek = ifelse(gwr_f3_parameter_table$spatially_varying, "parametr zmienny przestrzennie", "brak istotnej zmienności przestrzennej"),
  row.names = NULL,
  check.names = FALSE
)
write_overleaf_table(gwr_f3_tex, "gwr_test_lmz_f3", "Test LMZ F(3) lokalnej zmienności parametrów modelu GWR", align = "@{}lrrrrp{4.6cm}@{}")

# ============================================================
# 6. Model mieszany GWR
# ============================================================
# Model mieszany zostawia przestrzennie zmienne tylko te parametry,
# dla ktorych test F3 wskazuje istotna zmiennosc lokalna.
gwr_mixed_model <- NULL
if (length(gwr_mixed_fixed_vars) > 0 || gwr_mixed_intercept_fixed) {
  # Estymujemy model mieszany GWR: parametry bez istotnej zmiennosci F(3) traktujemy jako globalne.
  gwr_mixed_model <- gwr.mixed(
    formula_gwr,
    data = gmina_sp,
    fixed.vars = gwr_mixed_fixed_vars,
    intercept.fixed = gwr_mixed_intercept_fixed,
    bw = bw_adaptive,
    kernel = "gaussian",
    adaptive = TRUE,
    diagnostic = TRUE
  )
  writeLines(
    capture.output(print(gwr_mixed_model)),
    file.path(wydruki_dir, "gwr_bez_logDrogi_mixed_model_R.txt"),
    useBytes = TRUE
  )
  gwr_mixed_sdf <- st_as_sf(gwr_mixed_model$SDF)
  if (is.na(st_crs(gwr_mixed_sdf))) {
    st_crs(gwr_mixed_sdf) <- st_crs(gmina)
  }
  write.csv(
    st_drop_geometry(gwr_mixed_sdf),
    file.path(wydruki_dir, "gwr_bez_logDrogi_mixed_model_coefficients_R.csv"),
    row.names = FALSE, fileEncoding = "UTF-8")
} else {
  # Gdy F(3) wskazuje zmiennosc wszystkich parametrow, model mieszany nie ma zmiennych globalnych.
  writeLines(
    c(
      "Nie oszacowano gwr.mixed().",
      "Test F(3) nie wskazal zadnych parametrow bez istotnej zmiennosci przestrzennej przy alpha = 0.05."
    ),
    file.path(wydruki_dir, "gwr_bez_logDrogi_mixed_model_R.txt"),
    useBytes = TRUE
  )
}

gwr_mixed_summary <- data.frame(
  # Ta tabela jest krotkim slownikiem ustawien i wyniku estymacji modelu mieszanego.
  Model = "gwr.mixed",
  Jadro = "gaussian",
  "Typ pasma" = "adaptacyjne",
  "Szerokosc pasma" = as.numeric(bw_adaptive),
  "Zmienne globalne" = ifelse(length(gwr_mixed_fixed_vars) > 0, paste(gwr_mixed_fixed_vars, collapse = ", "), "brak"),
  "Wyraz wolny globalny" = ifelse(gwr_mixed_intercept_fixed, "tak", "nie"),
  Oszacowany = ifelse(!is.null(gwr_mixed_model), "tak", "nie"),
  AIC = if (!is.null(gwr_mixed_model)) as.numeric(gwr_mixed_model$aic) else NA_real_,
  BIC = if (!is.null(gwr_mixed_model)) as.numeric(gwr_mixed_model$bic) else NA_real_,
  RSS = if (!is.null(gwr_mixed_model)) as.numeric(gwr_mixed_model$r.ss) else NA_real_,
  df = if (!is.null(gwr_mixed_model)) as.numeric(gwr_mixed_model$df.used) else NA_real_,
  row.names = NULL,
  check.names = FALSE
)
write.csv(gwr_mixed_summary, file.path(wydruki_dir, "gwr_bez_logDrogi_mixed_model_summary_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_overleaf_table(gwr_mixed_summary, "gwr_mixed_model_summary", "Model mieszany GWR z parametrami globalnymi wedlug testu F3", align = "@{}lllrlllrrrr@{}")

# Wypisujemy testy w konsoli i zapisujemy je do pliku.
cat("\n--- GWR po 10 imputacjach MICE - reguly Rubina ---\n")
cat("Dla kazdej lokalizacji i parametru: qbar=mean(beta_i), ubar=mean(SE_i^2), b=var(beta_i), total_var=ubar+(1+1/m)*b, SE=sqrt(total_var).\n")
print(gwr_rubin_summary)
cat("\n--- GWR spgwr model ---\n")
print(gwr_model_spgwr)
cat("\n--- GWR diagnostic tests ---\n")
for (name in names(test_outputs)) {
  cat("\n", name, "\n", sep = "")
  print(test_outputs[[name]])
}
cat("\n--- GWR F3 fixed candidates ---\n")
print(gwr_f3_fixed_candidates)
cat("\n--- GWR mixed model ---\n")
if (!is.null(gwr_mixed_model)) {
  print(gwr_mixed_model)
} else {
  cat("Nie oszacowano gwr.mixed(), bo nie ma parametrow globalnych wedlug F(3).\n")
}

# Zapisujemy pelny raport tekstowy z wyboru pasma, testow i modelu mieszanego.
sink(file.path(wydruki_dir, "gwr_bez_logDrogi_tests_R.txt"))
cat("GWR z finalnymi zmiennymi WMNK bez drog oraz bez Odleglosc i Odleglosc2 - testy wedlug wzorca R\n\n")
cat("Regresory:", paste(x_cols, collapse = ", "), "\n\n")
cat("VIF dla benchmarku OLS przed estymacja GWR:\n")
print(vif_gwr_ols)
cat("\n")
cat("Wybor pasma dla kernela:", selected_kernel, "\n")
print(bandwidth_comparison)
cat("\n")
cat("Selected kernel:", selected_kernel, "\n")
cat("Bandwidth adaptive:", bw_adaptive, "\n")
cat("Bandwidth fixed:", bw_fixed, "\n\n")
cat("Bandwidth adaptive point:", bw_adaptive_point, "\n")
cat("Bandwidth adaptive spgwr:", bw_spgwr_adaptive, "\n\n")
cat("Model adaptive:\n")
print(gwr_model_adaptive)
cat("\nGWR po 10 imputacjach MICE - reguly Rubina:\n")
cat("Dla kazdej lokalizacji i parametru: qbar=mean(beta_i), ubar=mean(SE_i^2), b=var(beta_i), total_var=ubar+(1+1/m)*b, SE=sqrt(total_var).\n")
cat("Ponizej srednie po lokalizacjach; pelne wyniki sa w gwr_bez_logDrogi_mice_rubin_local_coefficients_R.csv.\n")
print(gwr_mice_bandwidths)
print(gwr_rubin_summary)
cat("\nTest Monte Carlo niestacjonarnosci parametrow GWR:\n")
print(gwr_monte_carlo)
cat("\nMoran I dla reszt glownego GWR:\n")
print(gwr_residuals_moran)
cat("\nModel fixed:\n")
cat("Pominieto estymacje fixed bandwidth; zapisano tylko bw_fixed do porownania.\n")
cat("\nModel spgwr adaptive:\n")
print(gwr_model_spgwr)
cat("\nTesty diagnostyczne:\n")
for (name in names(test_outputs)) {
  cat("\n", name, "\n", sep = "")
  print(test_outputs[[name]])
}
cat("\nParametry bez istotnej zmiennosci przestrzennej wedlug F(3):\n")
print(gwr_f3_fixed_candidates)
cat("\nfixed.vars dla gwr.mixed():", paste(gwr_mixed_fixed_vars, collapse = ", "), "\n")
cat("intercept.fixed dla gwr.mixed():", gwr_mixed_intercept_fixed, "\n")
cat("\nModel mieszany GWR:\n")
if (!is.null(gwr_mixed_model)) {
  print(gwr_mixed_model)
} else {
  cat("Nie oszacowano gwr.mixed(), bo nie ma parametrow globalnych wedlug F(3).\n")
}
sink()

# ============================================================
# 7. Selekcja zmiennych GWR
# ============================================================
# Selekcjonujemy zmienne GWR i sortujemy modele wedlug kryteriow GWmodel.
model_sel <- model.selection.gwr(
  DeVar = "ySaldo",
  InDeVars = x_cols,
  data = gmina_sp_point,
  kernel = selected_kernel,
  adaptive = TRUE,
  bw = bw_adaptive_point
)

sorted_models <- model.sort.gwr(
  model_sel,
  numVars = length(x_cols),
  ruler.vector = model_sel[[2]][, 3]
)

model_list <- sorted_models[[1]]
write.csv(sorted_models[[2]], file.path(wydruki_dir, "gwr_bez_logDrogi_model_selection_sorted_R.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Zapisujemy wykres krokow selekcji zmiennych.
model_view_exts <- if (SAVE_PDF_PLOTS) c("pdf", "png") else c("png")
for (ext in model_view_exts) {
  output_path <- file.path(wydruki_dir, paste0("gwr_bez_logDrogi_model_selection_view_R.", ext))
  if (ext == "pdf") {
    pdf(output_path, width = 9, height = 6)
  } else {
    png(output_path, width = 2700, height = 1800, res = 300)
  }
  model.view.gwr("ySaldo", x_cols, model.list = model_list)
  dev.off()
}

# Wypisujemy wyniki selekcji zmiennych w konsoli i pliku.
cat("\n--- GWR variable selection ---\n")
print(model_sel)
cat("\n--- GWR sorted models ---\n")
print(sorted_models[[2]])
cat("\n--- GWR model list for model.view.gwr ---\n")
print(model_list)
cat("\n--- GWR Monte Carlo non-stationarity test ---\n")
print(gwr_monte_carlo)
cat("\n--- Moran I for GWR residuals ---\n")
print(gwr_residuals_moran)

sink(file.path(wydruki_dir, "gwr_bez_logDrogi_model_selection_R.txt"))
cat("GWR z finalnymi zmiennymi WMNK bez drog oraz bez Odleglosc i Odleglosc2 - selekcja zmiennych\n\n")
cat("Regresory:", paste(x_cols, collapse = ", "), "\n\n")
print(model_sel)
cat("\nModele posortowane:\n")
print(sorted_models[[2]])
cat("\nLista modeli dla model.view.gwr:\n")
print(model_list)
cat("\nTest Monte Carlo niestacjonarnosci parametrow GWR:\n")
print(gwr_monte_carlo)
cat("\nMoran I dla reszt glownego GWR:\n")
print(gwr_residuals_moran)
sink()

print(sorted_models[[2]])
cat("\n--- FINAL GWR Monte Carlo non-stationarity test ---\n")
print(gwr_monte_carlo)
cat("\n--- FINAL Moran I for GWR residuals ---\n")
print(gwr_residuals_moran)
cat("\n--- FINAL GWR po 10 imputacjach MICE - reguly Rubina ---\n")
cat("qbar=mean(beta_i), ubar=mean(SE_i^2), b=var(beta_i), total_var=ubar+(1+1/m)*b, SE=sqrt(total_var).\n")
print(gwr_rubin_summary)

print("DONE FINITO")
