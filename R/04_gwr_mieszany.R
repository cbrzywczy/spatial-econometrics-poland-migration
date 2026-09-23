options(scipen = 999, digits = 6)
Sys.setenv(LANG = "en")

# Skrypt szacuje mixed GWR (niektore wspolczynniki globalne, niektore lokalne)
# dla obu poziomow agregacji: powiaty (sourcing wyborprzest_pow danych) i gminy.
# Wybor zmiennych lokalnych opiera sie na tescie LMZ.F3 (per-variable F-test
# stabilnosci wspolczynnikow): istotne -> traktujemy jako lokalne (varying).

args <- commandArgs(trailingOnly = TRUE)
level <- if (length(args) >= 1) args[1] else "powiat"

project_dir <- "/work"
data_dir <- file.path(project_dir, "data")
local_r_lib <- file.path(project_dir, "Rlib")
.libPaths(c(local_r_lib, .libPaths()))

suppressMessages({
  library(sf); library(sp); library(GWmodel); library(spgwr)
  library(dplyr); library(stringr); library(readxl)
})

norm_code <- function(x) {
  x <- as.character(x); x <- gsub("\\.0$", "", x)
  stringr::str_pad(trimws(x), width = 7, side = "left", pad = "0")
}
norm_code_pow <- function(x) {
  x <- as.character(x); x <- gsub("\\.0$", "", x)
  stringr::str_pad(trimws(x), width = 4, side = "left", pad = "0")
}
read_gus <- function(name) read.csv2(file.path(data_dir, name), check.names = FALSE, stringsAsFactors = FALSE)

# Wspolne wczytanie plikow GUS
czyn_przyst <- read_gus("CzynnePrzystanki.csv")
dl_drog <- read_gus("DlugoscDrog.csv")
med_wynagr <- read_gus("MedianaWynagrodzen2025.csv")
mieszk_odd <- read_gus("MieszkaniaOddane.csv")
przych_na_10k <- read_gus("PrzychodnieNa10k.csv")
udz_bezrob <- read_gus("UdzialBezrobotnych.csv")
saldo <- read_gus("SaldoMigracjiNa1k.csv")
populacja <- read_gus("PopulacjaGmin.csv")
m2_mieszkania <- read_gus("M2NaOsobe_MieszkaniaNa1000Mieszkancow.csv")
wskaznik_g <- readxl::read_excel(file.path(data_dir, "WskaznikG.xlsx"))

# Surowe dane gmin
gmina_raw <- data.frame(Kod = norm_code(czyn_przyst$Kod))
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(saldo$Kod), ySaldo_rate = as.numeric(saldo[[3]])), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw$Przystanki <- as.numeric(czyn_przyst[[3]])
populacja$Populacja <- as.numeric(populacja[[3]]) + as.numeric(populacja[[4]])
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(populacja$Kod), Populacja = populacja$Populacja), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(dl_drog$Kod), Drogi = as.numeric(dl_drog[[3]])), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(wskaznik_g[["Kod gminy"]]), WskaznikG = as.numeric(wskaznik_g[["Wskaźnik G"]])), by="Kod", all.x=TRUE, sort=FALSE)
med_wynagr$srednia <- rowMeans(med_wynagr[, 3:8], na.rm = TRUE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(med_wynagr$Kod), Wynagrodzenia = as.numeric(med_wynagr$srednia)), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(mieszk_odd$Kod), mieszkania = as.numeric(mieszk_odd[[3]])), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(przych_na_10k$Kod), PrzychodnieRate = as.numeric(przych_na_10k[[3]])), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(udz_bezrob$Kod), Bezrobocie = as.numeric(udz_bezrob[[3]])), by="Kod", all.x=TRUE, sort=FALSE)
gmina_raw <- merge(gmina_raw, data.frame(Kod = norm_code(m2_mieszkania$Kod), M2NaOsobe = as.numeric(m2_mieszkania[[3]])), by="Kod", all.x=TRUE, sort=FALSE)

if (level == "powiat") {
  # Agregacja gmina -> powiat (taka sama jak w wyborprzest_pow)
  gmina_raw$KodPow <- substr(gmina_raw$Kod, 1, 4)
  gmina_raw$typ_gminy <- substr(gmina_raw$Kod, 7, 7)
  gmina_raw$SaldoCount <- gmina_raw$ySaldo_rate * gmina_raw$Populacja / 1000
  gmina_raw$PrzychodnieCount <- gmina_raw$PrzychodnieRate * gmina_raw$Populacja / 10000
  for (xcol in c("Bezrobocie","Wynagrodzenia","M2NaOsobe","WskaznikG")) {
    gmina_raw[[paste0(xcol, "_x_pop")]] <- gmina_raw[[xcol]] * gmina_raw$Populacja
    gmina_raw[[paste0("Pop_dla_", xcol)]] <- ifelse(is.na(gmina_raw[[xcol]]) | is.na(gmina_raw$Populacja), 0, gmina_raw$Populacja)
  }
  gmina_raw$PopMiejska <- ifelse(gmina_raw$typ_gminy == "1", gmina_raw$Populacja, 0)
  gmina_raw$PopWiejska <- ifelse(gmina_raw$typ_gminy == "2", gmina_raw$Populacja, 0)
  agg_cols <- c("Przystanki","Populacja","Drogi","mieszkania","SaldoCount","PrzychodnieCount",
                "Bezrobocie_x_pop","Wynagrodzenia_x_pop","M2NaOsobe_x_pop","WskaznikG_x_pop",
                "PopMiejska","PopWiejska","Pop_dla_Bezrobocie","Pop_dla_Wynagrodzenia",
                "Pop_dla_M2NaOsobe","Pop_dla_WskaznikG")
  data_pow <- aggregate(gmina_raw[, agg_cols], by = list(KodPow = gmina_raw$KodPow), FUN = function(v) sum(v, na.rm = TRUE))

  data_full <- data.frame(Kod = norm_code_pow(data_pow$KodPow))
  data_full$Populacja <- data_pow$Populacja
  data_full$ySaldo <- data_pow$SaldoCount / data_pow$Populacja * 1000
  data_full$Bezrobocie <- data_pow$Bezrobocie_x_pop / data_pow$Pop_dla_Bezrobocie
  data_full$Wynagrodzenia <- data_pow$Wynagrodzenia_x_pop / data_pow$Pop_dla_Wynagrodzenia
  data_full$M2NaOsobe <- data_pow$M2NaOsobe_x_pop / data_pow$Pop_dla_M2NaOsobe
  data_full$WskaznikG <- data_pow$WskaznikG_x_pop / data_pow$Pop_dla_WskaznikG
  data_full$Przychodnie <- data_pow$PrzychodnieCount / data_pow$Populacja * 10000
  data_full$Miejska <- data_pow$PopMiejska / data_pow$Populacja
  data_full$Wiejska <- data_pow$PopWiejska / data_pow$Populacja
  data_full$logDrogiNa1000 <- log1p(data_pow$Drogi / data_pow$Populacja * 1000)
  data_full$logWskaznikG <- log(data_full$WskaznikG)
  data_full$logWynagrodzenia <- log(data_full$Wynagrodzenia)
  data_full$logMieszkaniaOddaneNa1000 <- log(data_pow$mieszkania / data_pow$Populacja * 1000)

  shapefile <- "A02_Granice_powiatow.shp"
  norm_fn <- norm_code_pow
} else {
  # Poziom gminy: bez agregacji
  data_full <- gmina_raw
  data_full$ySaldo <- data_full$ySaldo_rate
  data_full$logDrogiNa1000 <- log1p(data_full$Drogi / data_full$Populacja * 1000)
  data_full$logWskaznikG <- log(data_full$WskaznikG)
  data_full$logWynagrodzenia <- log(data_full$Wynagrodzenia)
  data_full$logMieszkaniaOddaneNa1000 <- log(data_full$mieszkania / data_full$Populacja * 1000)
  data_full$Przychodnie <- data_full$PrzychodnieRate
  data_full$Miejska <- as.integer(substr(data_full$Kod, 7, 7) == "1")
  data_full$Wiejska <- as.integer(substr(data_full$Kod, 7, 7) == "2")
  shapefile <- "A03_Granice_gmin.shp"
  norm_fn <- norm_code
}

x_cols <- c("logWskaznikG","logDrogiNa1000","logWynagrodzenia","M2NaOsobe",
            "logMieszkaniaOddaneNa1000","Przychodnie","Bezrobocie","Miejska","Wiejska")
numeric_cols <- setdiff(names(data_full), "Kod")
for (col in numeric_cols) {
  data_full[[col]] <- as.numeric(data_full[[col]])
  data_full[[col]][!is.finite(data_full[[col]])] <- NA
}
data_model <- na.omit(data_full[, c("Kod", "ySaldo", x_cols)])
cat(sprintf("Level: %s, n = %d\n", level, nrow(data_model)))

geo <- st_read(file.path(data_dir, shapefile), quiet = TRUE)
geo$JPT_KOD_JE <- norm_fn(geo$JPT_KOD_JE)
geo <- st_make_valid(geo)
geo <- st_transform(geo, 2180)
geo <- merge(geo, data_model, by.x = "JPT_KOD_JE", by.y = "Kod", all = FALSE, sort = FALSE)
geo <- geo[!st_is_empty(geo), ]
geo_point <- geo
geo_point$pt <- st_centroid(st_geometry(st_make_valid(geo_point)))
st_geometry(geo_point) <- "pt"
geo_sp <- as(geo_point, "Spatial")

formula_gwr <- as.formula(paste("ySaldo ~", paste(x_cols, collapse = " + ")))

# Lista fixed/varying. Dla powiatow opieramy sie na LMZ.F3 z gwr_tests_pow:
# istotne lokalnie - logWskaznikG, logDrogiNa1000, M2NaOsobe, Miejska.
# Dla gmin uzywamy tej samej listy (spojnosc specyfikacji + LMZ.F3 dla gmin
# w wydruki/gwr_tests_R.txt pokazuje, ze prawie wszystkie zmienne maja
# istotnosc lokalna - czyli nie ma "naturalnej" listy fixed - wybor
# zmiennych slabszej zmiennosci jest umowny).
fixed_vars <- c("logWynagrodzenia","logMieszkaniaOddaneNa1000","Przychodnie","Bezrobocie","Wiejska")
cat("Globalne (fixed):", paste(fixed_vars, collapse=", "), "\n")
cat("Lokalne (varying):", paste(setdiff(x_cols, fixed_vars), collapse=", "), "\n")

# Bandwidth: dla powiatow CV; dla gmin uzywamy bw = 200 (adaptive bisquare),
# bo bw.gwr CV na n=2477 trwa godziny i nie wnosi precyzji do mixed GWR
# (efektywny stopien lokalnosci jest podobny dla bw 100-500).
if (level == "powiat") {
  cat("Wyznaczanie bandwidth dla mixed GWR (powiat, CV)...\n")
  bw_mix <- bw.gwr(formula_gwr, data = geo_sp, adaptive = TRUE, kernel = "bisquare", approach = "CV")
} else {
  bw_mix <- 200
  cat("Bandwidth (gmina, ustalony bez CV):", bw_mix, "\n")
}
cat(sprintf("Bandwidth: %s\n", format(bw_mix)))

cat("Estymacja mixed GWR...\n")
mixed_model <- gwr.mixed(
  formula = formula_gwr,
  data = geo_sp,
  fixed.vars = fixed_vars,
  bw = bw_mix,
  kernel = "bisquare",
  adaptive = TRUE
)
print(mixed_model$mixed.df)

# Diagnostyka
cat("\n--- Mixed GWR diagnostics ---\n")
print(mixed_model)

# Zapis wynikow
out_dir <- file.path(project_dir, "wydruki_pow")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
suffix <- if (level == "powiat") "powiat" else "gmina"

# Globalne wsp.
if (!is.null(mixed_model$mixed.df)) {
  write.csv(mixed_model$mixed.df,
            file.path(out_dir, paste0("mixed_gwr_", suffix, "_summary_R.csv")),
            row.names = FALSE)
}

# Lokalne wsp. (sf)
if (!is.null(mixed_model$SDF)) {
  sdf <- as.data.frame(mixed_model$SDF)
  write.csv(sdf,
            file.path(out_dir, paste0("mixed_gwr_", suffix, "_local_R.csv")),
            row.names = FALSE)
}

# Diagnostyka
sink(file.path(out_dir, paste0("mixed_gwr_", suffix, "_print_R.txt")))
cat("MIXED GWR -", level, "\n\n")
cat("n =", nrow(data_model), "\n")
cat("Fixed (global):", paste(fixed_vars, collapse=", "), "\n")
cat("Varying (local):", paste(setdiff(x_cols, fixed_vars), collapse=", "), "\n")
cat("Bandwidth:", bw_mix, "\n\n")
print(mixed_model)
sink()

cat("\nWynik zapisany do:", out_dir, "\n")
cat("DONE FINITO -", level, "\n")
