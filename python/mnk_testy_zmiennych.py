from pathlib import Path
import os
import re
import shutil

# Ustawiamy lokalny cache Matplotliba w folderze projektu.
_SCRIPT_DIR = Path(__file__).resolve().parent
# Skrypt mieszka w python/; projekt to katalog nadrzedny. Dopuszczamy
# rowniez uruchamianie z korzenia projektu (zgodnosc z poprzednim ukladem).
PROJECT_DIR = _SCRIPT_DIR.parent if _SCRIPT_DIR.name == "python" else _SCRIPT_DIR
BASE_DIR = PROJECT_DIR
_MPL_CACHE_DIR = BASE_DIR / ".matplotlib-cache"
_MPL_CACHE_DIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(_MPL_CACHE_DIR))

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy import stats
from scipy.linalg import qr
from statsmodels.compat import lzip
from statsmodels.stats.diagnostic import het_breuschpagan, linear_reset
from statsmodels.stats.outliers_influence import variance_inflation_factor
import statsmodels.stats.api as sms
try:
    import geopandas as gpd
except ImportError:
    gpd = None

try:
    from stargazer.stargazer import Stargazer
except ImportError:
    Stargazer = None


# Ustawiamy katalogi danych, wynikow i plikow do Overleaf.
DATA_DIR = PROJECT_DIR / "data"
GRANICE_GMIN_PATH = DATA_DIR / "A03_Granice_gmin.shp"
WYDRUKI_DIR = BASE_DIR / "wydruki"
WYKRESY_DIR = BASE_DIR / "wykresy" / "WMNKtestyzmiennych"
STARGAZER_DIR = BASE_DIR / "stargazer" / "WMNKtestyzmiennych"
STARGAZER_TEX_DIR = STARGAZER_DIR / "tex"
TABELE_DIR = BASE_DIR / "tabele" / "WMNKtestyzmiennych"
# "Overleaf" publication targets now point at the consolidated repo
# locations consumed directly by bezjc.tex (no separate Overleaf/ tree).
OVERLEAF_DIR = BASE_DIR
OVERLEAF_MEDIA_DIR = BASE_DIR / "media"
OVERLEAF_TABELE_DIR = BASE_DIR / "tabele"
OVERLEAF_STARGAZER_DIR = BASE_DIR / "stargazer"
PAPER_DIR = PROJECT_DIR / "licm2"
WYDRUKI_DIR.mkdir(parents=True, exist_ok=True)
WYKRESY_DIR.mkdir(parents=True, exist_ok=True)
STARGAZER_DIR.mkdir(parents=True, exist_ok=True)
STARGAZER_TEX_DIR.mkdir(parents=True, exist_ok=True)
TABELE_DIR.mkdir(parents=True, exist_ok=True)
OVERLEAF_MEDIA_DIR.mkdir(parents=True, exist_ok=True)
OVERLEAF_TABELE_DIR.mkdir(parents=True, exist_ok=True)
OVERLEAF_STARGAZER_DIR.mkdir(parents=True, exist_ok=True)

# Ustawienia zgodne z reszta projektu, ale bez wymuszania LaTeX-a gdy go nie ma.
mpl.rcParams.update(
    {
        "text.usetex": shutil.which("latex") is not None,
        "font.family": "serif",
        "font.serif": ["Latin Modern Roman", "Computer Modern Roman", "DejaVu Serif"],
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)

SALDO_COL = "saldo migracji wewnętrznych na 1000 ludności;ogółem;2024;[osoba]"
PRZYSTANKI_COL = (
    "ogółem (przystanki autobusowe (z trolejbusowymi) i tramwajowe, "
    "przystanki wspólne dla tramwajów i autobusów);ogółem;2024;[szt.]"
)
DROGI_COL = "o nawierzchni twardej;2024;[km]"
MIESZKANIA_COL = "ogółem;mieszkania;2024;[-]"
PRZYCHODNIE_COL = "przychodnie na 10 tys. ludności;2024;[ob.]"
BEZROBOCIE_COL = "ogółem;2024;[%]"
POWIERZCHNIA_COL = "ogółem w km2;2024;[km2]"
POP_GMINY_COL = "gminy bez miast na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;ogółem;2024;[osoba]"
POP_MNPP_COL = "miasta na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;ogółem;2024;[osoba]"
WSKAZNIK_G_COL = "Wskaźnik G"
M2_NA_OSOBE_COL = "M2NaOsobe"

INITIAL_OLS_COLS = [
    "logPrzystankiNa1000",
    "logWskaznikG",
    "logWynagrodzenia",
    "M2NaOsobe",
    "logMieszkaniaOddaneNa1000",
    "Przychodnie",
    "Bezrobocie",
    "Miejska",
    "Wiejska",
    "Odleglosc",
    "Odleglosc2",
]

FINAL_WMNK_COLS = [column for column in INITIAL_OLS_COLS if column != "logPrzystankiNa1000"]
SAVE_PDF_PLOTS = True
TABLE_FLOAT_FORMAT = lambda value: f"{value:.3f}"

POLISH_VARIABLE_LABELS = {
    "const": "Wyraz wolny",
    "ySaldo": "Saldo migracji",
    "logPrzystankiNa1000": "log przystanków na 1000 ludności",
    "logWskaznikG": "log Wskaźnika G",
    "WskaznikG": "Wskaźnik G",
    "logWynagrodzenia": "log wynagrodzenia",
    "M2NaOsobe": "m2 na osobę",
    "logM2NaOsobe": "log m2 na osobę",
    "logMieszkaniaOddaneNa1000": "log mieszkań oddanych na 1000 ludności",
    "logMieszkaniaNa1000": "log mieszkań na 1000 ludności",
    "MieszkaniaOddane": "Mieszkania oddane",
    "Przychodnie": "Przychodnie",
    "Bezrobocie": "Bezrobocie",
    "Miejska": "Gmina miejska",
    "Wiejska": "Gmina wiejska",
    "Odleglosc2": "Odległość^2",
    "Odleglosc": "Odległość",
}

POLISH_TEXT_REPLACEMENTS = {
    "Zmienna objasniana:": "Zmienna objaśniana:",
    "Blad standardowy reszt": "Błąd standardowy reszt",
    "Blad standardowy": "Błąd standardowy",
    "Wspolczynniki": "Współczynniki",
    "Wspolczynnik": "Współczynnik",
    "Srednia": "Średnia",
    "srednia": "średnia",
    "Pelny": "Pełny",
    "pelny": "pełny",
    "przystankow": "przystanków",
}


def polish_output_text(value: str) -> str:
    # Zamienia techniczne lub bezogonowe etykiety na wersje gotowe do tabel.
    text = str(value)
    for key in sorted(POLISH_VARIABLE_LABELS, key=len, reverse=True):
        text = text.replace(key, POLISH_VARIABLE_LABELS[key])
    for key in sorted(POLISH_TEXT_REPLACEMENTS, key=len, reverse=True):
        text = text.replace(key, POLISH_TEXT_REPLACEMENTS[key])
    return text


def polish_display_label(value: str) -> str:
    # Zwraca pojedyncza etykiete zmiennej z polskimi znakami.
    key = str(value)
    return POLISH_VARIABLE_LABELS.get(key, polish_output_text(key.replace("_", " ")))


def is_p_value_column(column: str) -> bool:
    # Rozpoznaje kolumny p-value niezaleznie od zapisu z podkresleniem albo kropka.
    normalized = re.sub(r"[^a-z0-9]", "", str(column).lower())
    return normalized in {"pvalue", "p"} or normalized.endswith("pvalue")


def is_integer_like_column(column: str) -> bool:
    # Kolumny licznikowe zostawiamy bez miejsc po przecinku, jak w tabelach w pracy.
    normalized = re.sub(r"[^a-z0-9]", "", str(column).lower())
    return normalized in {"na", "n", "df", "dfnum", "dfden", "deltadf", "liczbaobserwacji"}


def format_tex_number(value, column: str) -> str:
    # Formatuje liczby do tabel LaTeX; p-value ma progowy zapis <0.001.
    if pd.isna(value):
        return ""
    number = float(value)
    if is_p_value_column(column):
        return "<0.001" if number < 0.001 else f"{number:.3f}"
    if is_integer_like_column(column) and float(number).is_integer():
        return str(int(number))
    return f"{number:.3f}".replace(".", ",")


def format_tex_value(value, column: str) -> str:
    # Zamienia pojedyncza wartosc na tekst gotowy do tabeli LaTeX.
    if isinstance(value, (int, float, np.integer, np.floating)) and not isinstance(value, bool):
        return format_tex_number(value, column)
    if pd.isna(value):
        return ""
    return polish_output_text(str(value).replace("_", " "))


def polish_tex_column_name(column: str) -> str:
    # Ujednolica naglowki: bez podkreslen i z zapisem p-value.
    mapping = {
        "p_value": "p-value",
        "pvalue": "p-value",
        "std_error": "Błąd standardowy",
        "z_value": "Statystyka z",
        "t_value": "Statystyka t",
        "coefficient": "Współczynnik",
        "estimate": "Współczynnik",
        "statistic": "Statystyka",
        "variable": "Zmienna",
        "srednia": "Średnia",
        "n_obs": "Liczba obserwacji",
        "regressors": "Regresory",
        "adj_r2": "Skorygowane $R^2$",
        "bp_pvalue": "p-value Breuscha-Pagana",
        "white_pvalue": "p-value White'a",
    }
    key = str(column)
    normalized = re.sub(r"[^a-z0-9]", "", key.lower())
    if key in mapping:
        return mapping[key]
    if normalized in mapping:
        return mapping[normalized]
    label = key.replace("_", " ")
    label = re.sub(r"\bp value\b", "p-value", label, flags=re.IGNORECASE)
    return polish_output_text(label)


def format_dataframe_for_latex(df: pd.DataFrame) -> pd.DataFrame:
    # Przygotowuje cala ramke do stabilnego eksportu LaTeX.
    formatted = pd.DataFrame(index=df.index)
    for column in df.columns:
        formatted[column] = df[column].map(lambda value, col=column: format_tex_value(value, col))
    formatted.columns = [polish_tex_column_name(column) for column in df.columns]
    return formatted


def read_maintest_refs() -> dict[str, set[str]]:
    # Buduje liste plikow realnie wczytywanych przez aktywny plik pracy (bezjc.tex).
    # Plik bez rozszerzenia matchuje wszystkie rozszerzenia (pdflatex picks pdf
    # ponad png ponad jpg) — zwracamy stem zeby wskazac kazdy wariant.
    maintest_path = PROJECT_DIR / "bezjc.tex"
    refs = {"media": set(), "tabele": set(), "stargazer": set()}
    if not maintest_path.exists():
        return refs

    text = "\n".join(line.split("%", 1)[0] for line in maintest_path.read_text(encoding="utf-8").splitlines())
    graphics_refs = re.findall(r"\\includegraphics(?:\[[^\]]*\])?\{([^}]*)\}", text)
    input_refs = re.findall(r"\\input(?:\[[^\]]*\])?\{([^}]*)\}", text)

    for ref in graphics_refs:
        normalized = ref.replace("\\", "/")
        if normalized.startswith("media/"):
            # Zachowujemy zarowno nazwe bez rozszerzenia jak i przyklady z .pdf/.png/.jpg
            name = Path(normalized).name
            refs["media"].add(name)
            stem = Path(name).stem
            for ext in (".pdf", ".png", ".jpg", ".jpeg"):
                refs["media"].add(stem + ext)

    for ref in input_refs:
        normalized = ref.replace("\\", "/")
        if not Path(normalized).suffix:
            normalized = f"{normalized}.tex"
        if normalized.startswith("tabele/"):
            refs["tabele"].add(Path(normalized).name)
        elif normalized.startswith("stargazer/"):
            refs["stargazer"].add(Path(normalized).name)
    return refs


MAINTEST_REFS = read_maintest_refs()
PUBLISH_DIRS = {
    "media": OVERLEAF_MEDIA_DIR,
    "tabele": OVERLEAF_TABELE_DIR,
    "stargazer": OVERLEAF_STARGAZER_DIR,
}


def maintest_uses(subdir: str, filename: str) -> bool:
    # Sprawdza, czy plik jest zaleznoscia glownego pliku pracy.
    return filename in MAINTEST_REFS.get(subdir, set())


def publish_if_maintest_uses(source_path: Path, subdir: str) -> bool:
    # Publikuje do Overleaf tylko nowo zapisane pliki wczytywane przez maintest.tex.
    if not source_path.exists() or not maintest_uses(subdir, source_path.name):
        return False
    target_dir = PUBLISH_DIRS[subdir]
    target_dir.mkdir(parents=True, exist_ok=True)
    target_path = target_dir / source_path.name
    if source_path.resolve() != target_path.resolve():
        shutil.copy2(source_path, target_path)
    return True


def save_current_plot(name: str) -> None:
    # Zapisuje wykres jako PDF (wektor — ostre etykiety w bezjc.tex) i opcjonalnie
    # jako PNG do szybkiego podgladu. Publikujemy PDF.
    pdf_path = WYKRESY_DIR / f"{name}.pdf"
    plt.savefig(pdf_path, bbox_inches="tight")
    publish_if_maintest_uses(pdf_path, "media")
    if SAVE_PDF_PLOTS:
        png_path = WYKRESY_DIR / f"{name}.png"
        plt.savefig(png_path, bbox_inches="tight", dpi=300)
    plt.close("all")


def save_current_raster_plot(name: str, dpi: int = 220) -> None:
    # Mapa rastrowa (np. choropleta) — zapisujemy najpierw PDF (wektor z osadzona
    # warstwa rastrowa), opcjonalnie PNG do szybkiego podgladu.
    pdf_path = WYKRESY_DIR / f"{name}.pdf"
    plt.savefig(pdf_path, bbox_inches="tight", dpi=dpi)
    publish_if_maintest_uses(pdf_path, "media")
    if SAVE_PDF_PLOTS:
        png_path = WYKRESY_DIR / f"{name}.png"
        plt.savefig(png_path, bbox_inches="tight", dpi=dpi)
    plt.close("all")


def safe_plot_name(value: str) -> str:
    # Upraszcza nazwy zmiennych do bezpiecznych nazw plikow graficznych.
    return "".join(char if char.isalnum() else "_" for char in value)


def save_overleaf_table(filename: str, latex: str) -> None:
    # Zapisuje tabele w bezdrog i publikuje ja do Overleaf, jesli uzywa jej maintest.tex.
    tex_path = TABELE_DIR / f"{filename}.tex"
    tex_path.write_text(latex, encoding="utf-8")
    publish_if_maintest_uses(tex_path, "tabele")


def dataframe_to_latex_table(df: pd.DataFrame, caption: str, column_format: str | None = None) -> str:
    # Tworzy prosta tabele LaTeX z ramki danych.
    formatted = format_dataframe_for_latex(df)
    latex = formatted.to_latex(index=False, escape=False, column_format=column_format)
    return "\n".join(
        [
            "\\begin{table}[H]",
            "\\centering",
            f"\\caption{{{polish_output_text(caption)}}}",
            latex.rstrip(),
            "\\end{table}",
            "",
        ]
    )


def save_stargazer_models(
    models: list,
    filename: str,
    covariate_order: list[str] | None = None,
    title: str | None = None,
    column_labels: list[str] | None = None,
) -> None:
    # Zapisuje modele jako HTML i LaTeX; w razie bledu zapisuje zwykly tekst.
    if Stargazer is None:
        lines = ["Pakiet stargazer nie jest dostepny w tym interpreterze.", ""]
        for idx, model in enumerate(models, start=1):
            lines.extend([f"MODEL {idx}", str(model.summary()), ""])
        (STARGAZER_DIR / f"{filename}.txt").write_text("\n".join(lines), encoding="utf-8")
        (STARGAZER_TEX_DIR / f"{filename}.txt").write_text("\n".join(lines), encoding="utf-8")
        return
    try:
        stargazer = Stargazer(models)
        if covariate_order is not None:
            stargazer.covariate_order(covariate_order)
        stargazer.rename_covariates({"const": "Wyraz wolny", **{column: polish_display_label(column) for column in (covariate_order or [])}})
        if column_labels is not None:
            stargazer.custom_columns(column_labels, [1] * len(column_labels))
        if title is not None:
            stargazer.title(polish_output_text(title))
        (STARGAZER_DIR / f"{filename}.html").write_text(stargazer.render_html(), encoding="utf-8")
        latex = stargazer.render_latex()
        # Poprawiamy fragment naglowka, ktory czasem psuje sklad LaTeX.
        latex = latex.replace("} \\\n\\cr \\cline", "} \\\\\n\\cline")
        # Spolszczamy stale etykiety generowane przez pakiet stargazer.
        latex = latex.replace("Dependent variable:", "Zmienna objaśniana:")
        latex = latex.replace(" Observations &", " Liczba obserwacji &")
        latex = latex.replace(" Adjusted $R^2$ &", " Skorygowane $R^2$ &")
        latex = latex.replace(" Residual Std. Error &", " Błąd standardowy reszt &")
        latex = latex.replace(" F Statistic &", " Statystyka F &")
        latex = latex.replace("\\textit{Note:}", "\\textit{Uwaga:}")
        latex = polish_output_text(latex)
        latex = latex.replace(
            "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
            "$^{*}$p-value$<$0.100; $^{**}$p-value$<$0.050; $^{***}$p-value$<$0.010",
        )
        if filename in {"modele_chow", "WydrukiMNKfull"}:
            latex = latex.replace("\\begin{table}[!htbp]", "\\begin{table}[H]")
            latex = latex.replace("\\begin{tabular}{", "\\resizebox{\\textwidth}{!}{%\n\\begin{tabular}{", 1)
            latex = latex.replace("\\end{tabular}\n\\end{table}", "\\end{tabular}\n}\n\\end{table}", 1)
        tex_path = STARGAZER_TEX_DIR / f"{filename}.tex"
        tex_path.write_text(latex, encoding="utf-8")
        publish_if_maintest_uses(tex_path, "stargazer")
    except Exception as exc:
        lines = [f"Stargazer nie obsluzyl modeli: {exc}", ""]
        for idx, model in enumerate(models, start=1):
            lines.extend([f"MODEL {idx}", str(model.summary()), ""])
        (STARGAZER_DIR / f"{filename}.txt").write_text("\n".join(lines), encoding="utf-8")
        (STARGAZER_TEX_DIR / f"{filename}.txt").write_text("\n".join(lines), encoding="utf-8")


def full_rank_exog(exog):
    # Usuwa liniowo zalezne kolumny pomocniczej macierzy testowej.
    exog = np.asarray(exog)
    _, _, independent_columns = qr(exog, mode="economic", pivoting=True)
    rank = np.linalg.matrix_rank(exog)
    return exog[:, np.sort(independent_columns[:rank])]


def safe_white(resid, exog):
    # Test White'a wymaga macierzy bez liniowo zaleznych kolumn.
    try:
        return sms.het_white(resid, full_rank_exog(exog))
    except (AssertionError, ValueError):
        return (np.nan, np.nan, np.nan, np.nan)


def load_model_data() -> pd.DataFrame:
    # Wczytuje dane z BDL, GUS i Ministerstwa Finansow.
    czyn_przyst = pd.read_csv(DATA_DIR / "CzynnePrzystanki.csv", sep=";", decimal=",")
    dl_drog = pd.read_csv(DATA_DIR / "DlugoscDrog.csv", sep=";", decimal=",")
    med_wynagr_path = next(DATA_DIR.glob("MedianaWynagrod*Brutto2024.csv"))
    med_wynagr = pd.read_csv(med_wynagr_path, sep=";", decimal=",")
    mieszk_odd = pd.read_csv(DATA_DIR / "MieszkaniaOddane.csv", sep=";", decimal=",")
    przych_na_10k = pd.read_csv(DATA_DIR / "PrzychodnieNa10k.csv", sep=";", decimal=",")
    udz_bezrob = pd.read_csv(DATA_DIR / "UdzialBezrobotnych.csv", sep=";", decimal=",")
    odleglosci = pd.read_csv(DATA_DIR / "odleglosci_km.csv", sep=",")
    saldo = pd.read_csv(DATA_DIR / "SaldoMigracjiNa1k.csv", sep=";", decimal=",")
    populacja = pd.read_csv(DATA_DIR / "PopulacjaGmin.csv", sep=";", decimal=",")
    powierzchnia = pd.read_csv(DATA_DIR / "PowierzchniaGmin.csv", sep=";", decimal=",")
    m2_mieszkania = pd.read_csv(DATA_DIR / "M2NaOsobe_MieszkaniaNa1000Mieszkancow.csv", sep=";", decimal=",")
    wskaznik_g = pd.read_excel(DATA_DIR / "WskaznikG.xlsx")

    data = pd.DataFrame({"Kod": czyn_przyst["Kod"]})
    data = data.merge(saldo.loc[:, ["Kod", SALDO_COL]], on="Kod", how="left")
    data = data.merge(czyn_przyst.loc[:, ["Kod", PRZYSTANKI_COL]], on="Kod", how="left")
    data = data.merge(dl_drog.loc[:, ["Kod", DROGI_COL]], on="Kod", how="left")
    data = data.merge(przych_na_10k.loc[:, ["Kod", PRZYCHODNIE_COL]], on="Kod", how="left")
    data = data.merge(udz_bezrob.loc[:, ["Kod", BEZROBOCIE_COL]], on="Kod", how="left")
    data = data.merge(odleglosci.loc[:, ["Kod", "odleglosc"]], on="Kod", how="left")
    data = data.merge(powierzchnia.loc[:, ["Kod", POWIERZCHNIA_COL]], on="Kod", how="left")

    # Populacja jest rozbita na dwie kolumny, wiec sumujemy je do jednego mianownika.
    populacja = populacja.loc[:, ["Kod", POP_GMINY_COL, POP_MNPP_COL]].copy()
    populacja["Populacja"] = populacja[POP_GMINY_COL] + populacja[POP_MNPP_COL]
    data = data.merge(populacja.loc[:, ["Kod", "Populacja"]], on="Kod", how="left")

    # Z miesiecznych kolumn wynagrodzen liczymy srednia roczna.
    med_wynagr["srednia"] = med_wynagr.iloc[:, 2:].mean(axis=1)
    data = data.merge(med_wynagr.loc[:, ["Kod", "srednia"]], on="Kod", how="left")
    data = data.merge(mieszk_odd.loc[:, ["Kod", MIESZKANIA_COL]], on="Kod", how="left")
    # Z pliku bierzemy tylko powierzchnie mieszkan na osobe.
    m2_mieszkania = m2_mieszkania.rename(
        columns={
            m2_mieszkania.columns[2]: M2_NA_OSOBE_COL,
        }
    )
    data = data.merge(m2_mieszkania.loc[:, ["Kod", M2_NA_OSOBE_COL]], on="Kod", how="left")

    data["Kod"] = data["Kod"].astype(str).str.strip().str.zfill(7)
    # Normalizujemy kody TERYT we Wskazniku G przed scaleniem.
    wskaznik_g = wskaznik_g.rename(columns={"Kod gminy": "Kod"}).copy()
    wskaznik_g["Kod"] = wskaznik_g["Kod"].astype(str).str.strip().str.zfill(7)
    wskaznik_g["Kod6"] = wskaznik_g["Kod"].str[:6]
    data = data.merge(wskaznik_g.loc[:, ["Kod", WSKAZNIK_G_COL]], on="Kod", how="left")

    # Braki Wskaznika G uzupelniamy po pierwszych szesciu cyfrach TERYT.
    data["Kod6"] = data["Kod"].str[:6]
    g_fallback = (
        wskaznik_g.loc[:, ["Kod6", WSKAZNIK_G_COL]]
        .dropna(subset=[WSKAZNIK_G_COL])
        .drop_duplicates(subset=["Kod6"])
        .rename(columns={WSKAZNIK_G_COL: f"{WSKAZNIK_G_COL}_po_Kod6"})
    )
    data = data.merge(g_fallback, on="Kod6", how="left")
    g_missing = data[WSKAZNIK_G_COL].isna()
    data.loc[g_missing, WSKAZNIK_G_COL] = data.loc[g_missing, f"{WSKAZNIK_G_COL}_po_Kod6"]
    data = data.drop(columns=["Kod6", f"{WSKAZNIK_G_COL}_po_Kod6"])
    data["ySaldo"] = data[SALDO_COL]
    data["logPrzystankiNaKm2"] = np.log(data[PRZYSTANKI_COL] / data[POWIERZCHNIA_COL])
    data["logPrzystankiNa1000"] = np.log(data[PRZYSTANKI_COL] / data["Populacja"] * 1000)
    data["logDrogi"] = np.log1p(data[DROGI_COL])
    data["logDrogiNaKm2"] = np.log1p(data[DROGI_COL] / data[POWIERZCHNIA_COL])
    data["logWynagrodzenia"] = np.log(data["srednia"])
    data["logMieszkania"] = np.log(data[MIESZKANIA_COL])
    # Liczbe mieszkan oddanych zachowujemy tez w wartosciach bezwzglednych.
    data["MieszkaniaOddane"] = data[MIESZKANIA_COL]
    # Dla dodatnich wartosci stosujemy log(x), bez dodawania jedynki.
    data["logMieszkaniaOddane"] = np.log(data["MieszkaniaOddane"])
    data["logMieszkaniaNaKm2"] = np.log(data[MIESZKANIA_COL] / data[POWIERZCHNIA_COL])
    data["logMieszkaniaNa1000"] = np.log(data[MIESZKANIA_COL] / data["Populacja"] * 1000)
    # Tworzymy log mieszkan oddanych na 1000 mieszkancow.
    data["logMieszkaniaOddaneNa1000"] = np.log(data[MIESZKANIA_COL] / data["Populacja"] * 1000)
    data["M2NaOsobe"] = data[M2_NA_OSOBE_COL]
    # Logarytm powierzchni na osobe zapisujemy do osobnej mapy.
    data["logM2NaOsobe"] = np.log(data["M2NaOsobe"])
    data["Przychodnie"] = data[PRZYCHODNIE_COL]
    data["Bezrobocie"] = data[BEZROBOCIE_COL]
    data["Miejska"] = (data["Kod"].str[-1] == "1").astype(int)
    data["Wiejska"] = (data["Kod"].str[-1] == "2").astype(int)
    data["Odleglosc"] = data["odleglosc"]
    data["Odleglosc2"] = data["Odleglosc"] ** 2
    data["WskaznikG"] = data[WSKAZNIK_G_COL]
    data["logWskaznikG"] = np.log(data["WskaznikG"])
    return data.replace([np.inf, -np.inf], np.nan)


def fit_variant(
    data: pd.DataFrame,
    model_name: str,
    include_roads: bool = True,
    g_variant: str = "base_log",
    housing_variant: str = "base_m2",
    sample_label: str = "pelna_proba",
) -> dict:
    # Estymuje wariant modelu: OLS i WLS na tej samej probie bez usuwania obserwacji.
    x_cols = build_x_cols(include_roads, g_variant, housing_variant)
    # Porownania modeli liczymy na tej samej probie.
    required_cols = ["Kod", "ySaldo", *x_cols]
    model_data = data.loc[:, required_cols].dropna().copy()
    x_ols = sm.add_constant(model_data[x_cols])
    y = model_data["ySaldo"]
    ols = sm.OLS(y, x_ols).fit()

    model_data["log_u2"] = np.log(np.maximum(ols.resid**2, np.finfo(float).tiny))
    formula = "log_u2 ~ " + " + ".join(x_cols)
    variance_model = smf.ols(formula=formula, data=model_data).fit()
    weights = 1 / np.exp(variance_model.fittedvalues)
    wls = sm.WLS(y, x_ols, weights=weights).fit()

    bp = het_breuschpagan(wls.resid, wls.model.exog)
    white = safe_white(wls.resid, wls.model.exog)
    max_pvalue_without_const = float(wls.pvalues.drop(labels=["const"], errors="ignore").max())
    insignificant_variables_5pct = ",".join(
        variable for variable, pvalue in wls.pvalues.drop(labels=["const"], errors="ignore").items() if pvalue >= 0.05
    )
    return {
        "model": model_name,
        "sample": sample_label,
        "variable": "bez_drog",
        "wskaznik_g": g_variant,
        "housing": housing_variant,
        "n_obs": int(len(model_data)),
        "ols_aic": float(ols.aic),
        "ols_bic": float(ols.bic),
        "ols_r2": float(ols.rsquared),
        "wls_aic": float(wls.aic),
        "wls_bic": float(wls.bic),
        "wls_r2": float(wls.rsquared),
        "wls_adj_r2": float(wls.rsquared_adj),
        "bp_pvalue": float(bp[1]),
        "white_pvalue": float(white[1]) if not np.isnan(white[1]) else np.nan,
        "all_variables_significant_5pct": bool(max_pvalue_without_const < 0.05),
        "max_pvalue_without_const": max_pvalue_without_const,
        "insignificant_variables_5pct": insignificant_variables_5pct,
        "coef_przystanki_na_1000": float(wls.params["logPrzystankiNa1000"]),
        "pvalue_przystanki_na_1000": float(wls.pvalues["logPrzystankiNa1000"]),
        "coef_drogi_na_1000": np.nan,
        "pvalue_drogi_na_1000": np.nan,
        "coef_mieszkania_na_1000": float(wls.params["logMieszkaniaNa1000"]),
        "pvalue_mieszkania_na_1000": float(wls.pvalues["logMieszkaniaNa1000"]),
        "coef_wskaznik_g": np.nan,
        "pvalue_wskaznik_g": np.nan,
        "coef_log_wskaznik_g": float(wls.params["logWskaznikG"]),
        "pvalue_log_wskaznik_g": float(wls.pvalues["logWskaznikG"]),
        "coef_m2_na_osobe": float(wls.params["M2NaOsobe"]),
        "pvalue_m2_na_osobe": float(wls.pvalues["M2NaOsobe"]),
        "model_object": wls,
    }


def fit_wls_for_columns(data: pd.DataFrame, x_cols: list[str]) -> dict:
    # Estymuje pojedynczy model WMNK dla zadanej listy zmiennych bez usuwania obserwacji.
    required_cols = ["Kod", "ySaldo", *x_cols]
    model_data = data.loc[:, required_cols].dropna().copy()
    x_ols = sm.add_constant(model_data[x_cols])
    y = model_data["ySaldo"]
    ols = sm.OLS(y, x_ols).fit()

    model_data["log_u2"] = np.log(np.maximum(ols.resid**2, np.finfo(float).tiny))
    variance_formula = "log_u2 ~ " + " + ".join(x_cols)
    variance_model = smf.ols(formula=variance_formula, data=model_data).fit()
    weights = 1 / np.exp(variance_model.fittedvalues)
    wls = sm.WLS(y, x_ols, weights=weights).fit()
    bp = het_breuschpagan(wls.resid, wls.model.exog)
    white = safe_white(wls.resid, wls.model.exog)

    return {
        "x_cols": x_cols,
        "model_data": model_data,
        "ols": ols,
        "variance_model": variance_model,
        "wls": wls,
        "bp": bp,
        "white": white,
    }


def load_gminy_for_map(message_filename: str, map_description: str):
    # Wczytuje geometrie gmin; gdy brakuje silnika GIS, nie przerywa eksportu tabel.
    if gpd is None:
        message = (
            f"Nie zapisano {map_description}, bo w srodowisku Pythona "
            "nie jest dostepny pakiet geopandas."
        )
        (WYDRUKI_DIR / message_filename).write_text(message, encoding="utf-8")
        return None
    try:
        return gpd.read_file(GRANICE_GMIN_PATH)
    except ImportError as exc:
        message = (
            f"Nie zapisano {map_description}, bo geopandas nie ma silnika "
            f"do wczytania pliku SHP: {exc}"
        )
        (WYDRUKI_DIR / message_filename).write_text(message, encoding="utf-8")
        return None


def save_ysaldo_heatmap(data: pd.DataFrame) -> None:
    # Zapisuje mape ciepla salda migracji.
    gminy = load_gminy_for_map("mapa_ciepla_ysaldo_brak_geopandas.txt", "mapy ciepla YSaldo")
    if gminy is None:
        return

    saldo_data = data.loc[:, ["Kod", "ySaldo"]].dropna().copy()
    saldo_data["Kod"] = saldo_data["Kod"].astype(str).str.strip().str.zfill(7)

    gminy["Kod"] = gminy["JPT_KOD_JE"].astype(str).str.strip().str.zfill(7)
    map_data = gminy.merge(saldo_data, on="Kod", how="left")
    ysaldo = map_data["ySaldo"].dropna()
    norm = None
    if float(ysaldo.min()) < 0 < float(ysaldo.max()):
        norm = TwoSlopeNorm(vmin=float(ysaldo.min()), vcenter=0, vmax=float(ysaldo.max()))

    fig, ax = plt.subplots(figsize=(9, 9))
    map_data.plot(
        column="ySaldo",
        ax=ax,
        cmap="coolwarm",
        norm=norm,
        linewidth=0.08,
        edgecolor="white",
        legend=True,
        legend_kwds={"label": "YSaldo", "shrink": 0.72},
        missing_kwds={"color": "#eeeeee", "edgecolor": "white", "hatch": "///", "label": "brak danych"},
    )
    ax.set_axis_off()
    ax.set_title("Mapa ciepla YSaldo", fontsize=12)
    fig.tight_layout()
    save_current_raster_plot("mapa_ciepla_ysaldo")


def save_single_variable_heatmap(data: pd.DataFrame, column: str, filename: str, title: str) -> None:
    # Zapisuje osobna mape ciepla dla jednej zmiennej.
    gminy = load_gminy_for_map(f"{filename}_brak_geopandas.txt", f"mapy ciepla {column}")
    if gminy is None:
        return

    variable_data = data.loc[:, ["Kod", column]].dropna().copy()
    variable_data["Kod"] = variable_data["Kod"].astype(str).str.strip().str.zfill(7)

    gminy["Kod"] = gminy["JPT_KOD_JE"].astype(str).str.strip().str.zfill(7)
    map_data = gminy.merge(variable_data, on="Kod", how="left")
    values = map_data[column].dropna()
    norm = None
    if not values.empty and float(values.min()) < 0 < float(values.max()):
        norm = TwoSlopeNorm(vmin=float(values.min()), vcenter=0, vmax=float(values.max()))

    fig, ax = plt.subplots(figsize=(9, 9))
    map_data.plot(
        column=column,
        ax=ax,
        cmap="coolwarm",
        norm=norm,
        linewidth=0.08,
        edgecolor="white",
        legend=True,
        legend_kwds={"label": column, "shrink": 0.72},
        missing_kwds={"color": "#eeeeee", "edgecolor": "white", "hatch": "///", "label": "brak danych"},
    )
    ax.set_axis_off()
    ax.set_title(title, fontsize=12)
    fig.tight_layout()
    save_current_raster_plot(filename)


def save_final_wmnk_residual_map(fit: dict) -> None:
    # Zapisuje mape reszt finalnego modelu WMNK.
    gminy = load_gminy_for_map("mapa_reszt_finalnego_wmnk_brak_geopandas.txt", "mapy reszt finalnego WMNK")
    if gminy is None:
        return

    residual_data = fit["model_data"].loc[:, ["Kod"]].copy()
    residual_data["Kod"] = residual_data["Kod"].astype(str).str.strip().str.zfill(7)
    residual_data["reszty_wmnk"] = np.asarray(fit["wls"].resid, dtype=float)

    gminy["Kod"] = gminy["JPT_KOD_JE"].astype(str).str.strip().str.zfill(7)
    map_data = gminy.merge(residual_data, on="Kod", how="left")
    residuals = map_data["reszty_wmnk"].dropna()
    vmax = float(np.nanmax(np.abs(residuals))) if not residuals.empty else 1.0
    norm = TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)

    fig, ax = plt.subplots(figsize=(9, 9))
    map_data.plot(
        column="reszty_wmnk",
        ax=ax,
        cmap="coolwarm",
        norm=norm,
        linewidth=0.08,
        edgecolor="white",
        legend=True,
        legend_kwds={"label": "Reszty WMNK", "shrink": 0.72},
        missing_kwds={"color": "#eeeeee", "edgecolor": "white", "hatch": "///", "label": "brak danych"},
    )
    ax.set_axis_off()
    ax.set_title("Mapa reszt finalnego modelu WMNK", fontsize=12)
    fig.tight_layout()
    save_current_raster_plot("mapa_reszt_finalnego_wmnk")


def save_variable_heatmap_panel(data: pd.DataFrame, variable_cols: list[str]) -> None:
    # Tworzy plansze map ciepla dla zmiennej objasnianej i regresorow.
    gminy = load_gminy_for_map("plansza_map_ciepla_zmiennych_brak_geopandas.txt", "planszy map ciepla zmiennych")
    if gminy is None:
        return

    map_cols = ["Kod", *variable_cols]
    variable_data = data.loc[:, map_cols].copy()
    variable_data["Kod"] = variable_data["Kod"].astype(str).str.strip().str.zfill(7)

    gminy["Kod"] = gminy["JPT_KOD_JE"].astype(str).str.strip().str.zfill(7)
    map_data = gminy.merge(variable_data, on="Kod", how="left")

    fig, axes = plt.subplots(4, 3, figsize=(15, 18))
    for ax, column in zip(axes.ravel(), variable_cols):
        values = map_data[column].dropna()
        if values.empty:
            ax.set_axis_off()
            ax.set_title(column, fontsize=10)
            continue

        vmin = float(values.min())
        vmax = float(values.max())
        norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=vmax) if vmin < 0 < vmax else mpl.colors.Normalize(vmin=vmin, vmax=vmax)
        map_data.plot(
            column=column,
            ax=ax,
            cmap="coolwarm",
            norm=norm,
            linewidth=0.03,
            edgecolor="white",
            rasterized=True,
            missing_kwds={"color": "#eeeeee", "edgecolor": "white"},
        )
        ax.set_axis_off()
        ax.set_title(column, fontsize=10)
        scalar_map = mpl.cm.ScalarMappable(norm=norm, cmap="coolwarm")
        scalar_map.set_array([])
        colorbar = fig.colorbar(scalar_map, ax=ax, fraction=0.035, pad=0.01)
        colorbar.ax.tick_params(labelsize=7)

    for ax in axes.ravel()[len(variable_cols):]:
        ax.set_axis_off()

    fig.suptitle("Mapy ciepla zmiennych modelu", fontsize=14)
    fig.tight_layout(rect=(0, 0, 1, 0.98))
    save_current_raster_plot("mapy_ciepla_zmiennych_4x3")


def save_final_wmnk_plots(data: pd.DataFrame, fit: dict, x_cols: list[str]) -> None:
    # Generuje wykresy diagnostyczne finalnego WMNK.
    model_data = fit["model_data"]
    model_wls_data = fit["model_data"].copy()
    wls = fit["wls"]

    # W korelacjach pomijamy kwadrat odleglosci, zeby nie dublowac zmiennej.
    corr_cols = ["logPrzystankiNa1000", *(col for col in x_cols if col != "Odleglosc2")]
    corr_data = data.loc[:, corr_cols].dropna()
    for corr_method in ["pearson", "spearman"]:
        # Rysuje osobna macierz korelacji dla wybranej metody.
        corr_df = corr_data.corr(method=corr_method)
        fig, ax = plt.subplots(figsize=(7.2, 6.4))
        image = ax.imshow(corr_df.to_numpy(dtype=float), cmap="coolwarm", vmin=-1, vmax=1)
        ax.set_xticks(np.arange(len(corr_df.columns)))
        ax.set_yticks(np.arange(len(corr_df.index)))
        ax.set_xticklabels(corr_df.columns, rotation=45, ha="right", fontsize=10)
        ax.set_yticklabels(corr_df.index, fontsize=10)
        fig.colorbar(image, ax=ax, fraction=0.046, pad=0.04)
        ax.set_title(f"Korelacje zmiennych ({corr_method})", fontsize=11)
        fig.tight_layout()
        save_current_plot(f"korelacje_zmiennych_{corr_method}")

    residuals = pd.Series(wls.resid, index=model_wls_data.index, name="residuals")
    fitted = pd.Series(wls.fittedvalues, index=model_wls_data.index, name="predictions")
    model_wls_data["residuals"] = residuals
    model_wls_data["predictions"] = fitted
    model_wls_data["sqrt_abs_resid"] = np.sqrt(np.abs(residuals))

    # Laczymy cztery wykresy diagnostyczne reszt w jedna plansze.
    fig, axes = plt.subplots(2, 2, figsize=(12, 9))
    ax_hist, ax_resid, ax_qq, ax_scale = axes.ravel()

    ax_hist.hist(residuals, bins=30, density=True, edgecolor="black", alpha=0.75)
    if residuals.std(ddof=1) > 0:
        x_grid = np.linspace(residuals.min(), residuals.max(), 200)
        normal_density = (
            1
            / (residuals.std(ddof=1) * np.sqrt(2 * np.pi))
            * np.exp(-0.5 * ((x_grid - residuals.mean()) / residuals.std(ddof=1)) ** 2)
        )
        ax_hist.plot(x_grid, normal_density, color="black", linewidth=1.2)
    ax_hist.set_title("Histogram reszt WMNK")
    ax_hist.set_xlabel("reszty")
    ax_hist.set_ylabel("gestosc")

    ax_resid.scatter(fitted, residuals, s=14, alpha=0.65)
    ax_resid.axhline(0, color="black", linewidth=1)
    if len(fitted) > 1:
        smooth = sm.nonparametric.lowess(residuals, fitted, frac=0.25, return_sorted=True)
        ax_resid.plot(smooth[:, 0], smooth[:, 1], color="green", linewidth=1.4)
    ax_resid.set_title("Reszty vs wartosci dopasowane")
    ax_resid.set_xlabel("wartosci dopasowane")
    ax_resid.set_ylabel("reszty")

    stats.probplot(residuals, dist="norm", plot=ax_qq)
    ax_qq.set_title("QQ plot reszt WMNK")

    ax_scale.scatter(fitted, model_wls_data["sqrt_abs_resid"], s=14, alpha=0.65)
    if len(fitted) > 1:
        smooth = sm.nonparametric.lowess(model_wls_data["sqrt_abs_resid"], fitted, frac=0.25, return_sorted=True)
        ax_scale.plot(smooth[:, 0], smooth[:, 1], color="green", linewidth=1.4)
    ax_scale.set_title("Scale-location")
    ax_scale.set_xlabel("wartosci dopasowane")
    ax_scale.set_ylabel("sqrt(abs(reszty))")

    fig.tight_layout()
    save_current_plot("diagnostyka_reszt_2x2_final")

    # Laczymy wykresy reszt wzgledem regresorow w jedna plansze.
    fig, axes = plt.subplots(4, 3, figsize=(15, 18))
    axes_flat = axes.ravel()
    plot_positions = [*axes_flat[:9], axes[3, 0], axes[3, 2]]
    plot_cols = [column for column in x_cols if column not in {"Miejska", "Wiejska"}]
    plot_cols.extend(column for column in ["Miejska", "Wiejska"] if column in x_cols)

    for ax, column in zip(plot_positions, plot_cols):
        ax.scatter(model_wls_data[column], residuals, s=10, alpha=0.6)
        ax.axhline(0, color="black", linewidth=0.9)
        ax.set_xlabel(column, fontsize=9)
        ax.set_ylabel("reszty", fontsize=9)
        ax.set_title(f"Reszty vs {column}", fontsize=10)
        ax.tick_params(axis="both", labelsize=8)

    for ax in axes_flat:
        if not ax.has_data():
            ax.axis("off")
    axes[3, 1].axis("off")

    fig.tight_layout()
    save_current_plot("reszty_vs_zmienne_4x3_final")

    for column in x_cols:
        fig, ax = plt.subplots(figsize=(7, 5))
        ax.scatter(model_wls_data[column], residuals, s=14, alpha=0.65)
        ax.axhline(0, color="black", linewidth=1)
        ax.set_xlabel(column)
        ax.set_ylabel("reszty")
        ax.set_title(f"Reszty WMNK vs {column}")
        save_current_plot(f"reszty_vs_zmienna_{safe_plot_name(column)}")

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(fitted, residuals, s=14, alpha=0.65)
    ax.axhline(0, color="black", linewidth=1)
    if len(fitted) > 1:
        smooth = sm.nonparametric.lowess(residuals, fitted, frac=0.25, return_sorted=True)
        ax.plot(smooth[:, 0], smooth[:, 1], color="green", linewidth=1.4)
    ax.set_xlabel("wartosci dopasowane")
    ax.set_ylabel("reszty")
    ax.set_title("Reszty vs wartosci dopasowane")
    save_current_plot("reszty_vs_wartosci_dopasowane_final")

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(fitted, model_wls_data["sqrt_abs_resid"], s=14, alpha=0.65)
    if len(fitted) > 1:
        smooth = sm.nonparametric.lowess(model_wls_data["sqrt_abs_resid"], fitted, frac=0.25, return_sorted=True)
        ax.plot(smooth[:, 0], smooth[:, 1], color="green", linewidth=1.4)
    ax.set_xlabel("wartosci dopasowane")
    ax.set_ylabel("sqrt(abs(reszty))")
    ax.set_title("Scale-location")
    save_current_plot("scale_location_final")

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.hist(residuals, bins=30, density=True, edgecolor="black", alpha=0.75)
    if residuals.std(ddof=1) > 0:
        x_grid = np.linspace(residuals.min(), residuals.max(), 200)
        normal_density = (
            1
            / (residuals.std(ddof=1) * np.sqrt(2 * np.pi))
            * np.exp(-0.5 * ((x_grid - residuals.mean()) / residuals.std(ddof=1)) ** 2)
        )
        ax.plot(x_grid, normal_density, color="black", linewidth=1.2)
    ax.set_title("Histogram reszt WMNK")
    ax.set_xlabel("reszty")
    save_current_plot("histogram_reszt_final")

    sm.qqplot(residuals, line="r")
    plt.title("QQ plot reszt WMNK")
    save_current_plot("qq_plot_final")


def save_final_wmnk_stargazer(fit: dict, x_cols: list[str], initial_ols) -> None:
    # Zapisuje zestawy modeli do tabel Stargazer.
    order = ["const", *x_cols]
    save_stargazer_models(
        [fit["ols"], fit["wls"]],
        "modele_ols_podstawowe",
        covariate_order=order,
        title="Model 2 OLS i finalny WMNK bez usuwania obserwacji",
    )
    save_stargazer_models(
        [fit["variance_model"]],
        "model_funkcji_wariancji",
        title="Funkcja wariancji dla finalnego WMNK bez drog",
    )


def run_chow_test_for_final_model(fit: dict, x_cols: list[str]) -> dict:
    # Liczy test Chowa dla finalnej proby modelu bez usuwania obserwacji.
    model_data = fit["model_data"].copy()
    model_data["Kod"] = model_data["Kod"].astype(str)

    chow_cols = [col for col in x_cols if col not in {"Miejska", "Wiejska"}]
    chow_data = model_data.loc[:, ["Kod", "ySaldo", *chow_cols]].dropna().copy()

    groups = {
        "wiejskie": chow_data[chow_data["Kod"].str.endswith("2")],
        "miejsko_wiejskie": chow_data[chow_data["Kod"].str.endswith("3")],
        "miejskie": chow_data[chow_data["Kod"].str.endswith("1")],
    }

    x_full = sm.add_constant(chow_data[chow_cols])
    y_full = chow_data["ySaldo"]
    model_full = sm.OLS(y_full, x_full).fit()

    group_models = {}
    for name, group_data in groups.items():
        if len(group_data) == 0:
            continue
        x_group = sm.add_constant(group_data[chow_cols])
        y_group = group_data["ySaldo"]
        group_models[name] = sm.OLS(y_group, x_group).fit()

    rss_full = float(np.sum(model_full.resid**2))
    rss_groups = float(sum(np.sum(model.resid**2) for model in group_models.values()))
    n_groups = int(sum(model.nobs for model in group_models.values()))
    group_count = len(group_models)
    k = int(model_full.df_model) + 1
    df_num = k * (group_count - 1)
    df_den = n_groups - group_count * k
    f_chow = ((rss_full - rss_groups) / df_num) / (rss_groups / df_den)
    p_value = float(stats.f.sf(f_chow, df_num, df_den))

    result = {
        "F_chow": float(f_chow),
        "p_value": p_value,
        "df_num": int(df_num),
        "df_den": int(df_den),
        "n_full": int(model_full.nobs),
        "n_wiejskie": int(group_models["wiejskie"].nobs) if "wiejskie" in group_models else 0,
        "n_miejsko_wiejskie": int(group_models["miejsko_wiejskie"].nobs) if "miejsko_wiejskie" in group_models else 0,
        "n_miejskie": int(group_models["miejskie"].nobs) if "miejskie" in group_models else 0,
    }
    pd.DataFrame([result]).to_csv(WYDRUKI_DIR / "WMNKtestyzmiennych_chow_test.csv", index=False)

    model_order = [model_full]
    for group_name in ["wiejskie", "miejsko_wiejskie", "miejskie"]:
        if group_name in group_models:
            model_order.append(group_models[group_name])
    save_stargazer_models(
        model_order,
        "modele_chow",
        covariate_order=["const", *chow_cols],
        title="Modele do testu Chowa dla finalnego modelu WMNK",
    )
    return {"result": result, "models": model_order, "x_cols": chow_cols}


def print_final_wmnk_diagnostics(
    data: pd.DataFrame,
    fit: dict,
    x_cols: list[str],
    model_row: pd.DataFrame,
    coef_df: pd.DataFrame,
    vif_df: pd.DataFrame,
    initial_ols=None,
) -> None:
    # Wypisuje na konsole statystyki, testy i wyniki hipotez.
    model_data = fit["model_data"]
    ols = fit["ols"]
    variance_model = fit["variance_model"]
    wls = fit["wls"]
    diagnostic_cols = ["ySaldo", *x_cols]
    bp_white_names = ["lagrange multiplier statistic", "p-value", "f-value", "f p-value"]
    jb_names = ["The Jarque-Bera test statistic", "p-value", "skewness", "kurtosis"]

    print("Braki danych")
    print(data.loc[:, diagnostic_cols].isna().sum())
    print("Liczba obserwacji wykorzystanych w modelu to:", len(model_data))
    print("średnia")
    print(model_data.loc[:, diagnostic_cols].mean(numeric_only=True))
    print("odchylenie standardowe")
    print(model_data.loc[:, diagnostic_cols].std(numeric_only=True))
    print("min")
    print(model_data.loc[:, diagnostic_cols].min(numeric_only=True))
    print("max")
    print(model_data.loc[:, diagnostic_cols].max(numeric_only=True))

    if initial_ols is not None:
        print("\nMODEL OLS POCZATKOWY")
        print(initial_ols.summary())
        print("\nMODEL OLS BEZ logPrzystankiNa1000")
    else:
        print("\nMODEL OLS POCZATKOWY")
    print(ols.summary())
    try:
        reset_ols = linear_reset(ols, power=3, test_type="fitted")
        print("\n TEST RESET (fitted) ")
        print("p-value:", reset_ols.pvalue)
    except Exception as exc:
        print("\n TEST RESET (fitted) pominiety:", exc)

    print("Test Breusha-Pagana:")
    print(lzip(bp_white_names, het_breuschpagan(ols.resid, ols.model.exog)))
    print("Test White'a:")
    print(lzip(bp_white_names, safe_white(ols.resid, ols.model.exog)))
    print("ZMIENNE HETEROSKEDASTYCZNOSC")
    print(variance_model.summary())
    print("Model z macierzą odporną White'a HC0")
    print(ols.get_robustcov_results(cov_type="HC0").summary())
    print("Test Jarque-Bera", lzip(jb_names, sms.jarque_bera(ols.resid)))
    print("Test na łączną nieistotność")
    print("F-stat:", ols.fvalue, "p-value:", ols.f_pvalue)
    print("Zmienne łącznie istotne")
    print(" VIF ")
    print(vif_df.rename(columns={"vif": "VIF"}).sort_values("VIF", ascending=False).to_string(index=False))

    print("\nFINALNY MODEL WMNK")
    print(wls.summary())
    for name, val in wls.params.items():
        print(f"{name}: {val:.20f}")

    try:
        reset_wls = linear_reset(wls, power=3, test_type="fitted")
        print("\n TEST RESET (fitted) best ")
        print("p-value:", reset_wls.pvalue)
    except Exception as exc:
        print("\n TEST RESET (fitted) best pominiety:", exc)

    print("Test Breusha-Pagana best:")
    print(lzip(bp_white_names, fit["bp"]))
    print("Test White'a best:")
    print(lzip(bp_white_names, fit["white"]))
    print("Test Jarque-Bera best", lzip(jb_names, sms.jarque_bera(wls.resid)))
    print("Test na łączną nieistotność 1")
    print("F-stat best:", wls.fvalue, "p-value:", wls.f_pvalue)
    print("Zmienne łącznie istotne")

    # Testuje hipotezy na finalnej specyfikacji bez drog.
    print("\n=== H2: Test Wiejska > 0 ===")
    if "Wiejska" in wls.params.index:
        beta_wiejska = float(wls.params["Wiejska"])
        se_wiejska = float(wls.bse["Wiejska"])
        t_stat_wiejska = beta_wiejska / se_wiejska
        p_value_wiejska = float(1 - stats.t.cdf(t_stat_wiejska, df=wls.df_resid))
        print(f"Współczynnik Wiejska: {beta_wiejska:.6f}")
        print(f"Błąd standardowy: {se_wiejska:.6f}")
        print(f"t-statystyka: {t_stat_wiejska:.4f}")
        print(f"p-value (test jednostronny >0): {p_value_wiejska:.6f}")
        print("Hipoteza H2 potwierdzona: Wiejska > 0 (istotnie)" if p_value_wiejska < 0.05 else "Hipoteza H2 NIE potwierdzona")

    print("\n=== H3: Test logWynagrodzenia istotne i > 0 ===")
    if "logWynagrodzenia" in wls.params.index:
        beta_wyn = float(wls.params["logWynagrodzenia"])
        se_wyn = float(wls.bse["logWynagrodzenia"])
        t_stat_wyn = beta_wyn / se_wyn
        p_value_wyn = float(wls.pvalues["logWynagrodzenia"])
        p_value_wyn_one_sided = float(1 - stats.t.cdf(t_stat_wyn, df=wls.df_resid))
        print(f"Współczynnik logWynagrodzenia: {beta_wyn:.6f}")
        print(f"Błąd standardowy: {se_wyn:.6f}")
        print(f"t-statystyka: {t_stat_wyn:.4f}")
        print(f"p-value (test dwustronny): {p_value_wyn:.6f}")
        print(f"p-value (test jednostronny >0): {p_value_wyn_one_sided:.6f}")
        print("Hipoteza H3 potwierdzona: logWynagrodzenia > 0 (istotnie)" if p_value_wyn_one_sided < 0.05 else "Hipoteza H3 NIE potwierdzona")
        print("\nInterpretacja: Wzrost wynagrodzeń o 1% powoduje wzrost salda migracji")
        print(f"o {beta_wyn / 100:.6f} na 1000 ludności")
        print(f"Wzrost wynagrodzeń o 10% -> wzrost salda o {beta_wyn * 10 / 100:.4f} na 1000 ludności")

    print("\n" + "=" * 60)
    print("TESTOWANIE HIPOTEZY H1 (ZŁOŻONEJ)")
    print("=" * 60)
    if {"Odleglosc", "Odleglosc2"}.issubset(wls.params.index):
        beta_odl = float(wls.params["Odleglosc"])
        beta_odl2 = float(wls.params["Odleglosc2"])
        odl_mean = float(model_data["Odleglosc"].mean())
        marginal_effect = beta_odl + 2 * beta_odl2 * odl_mean
        marginal_effects = beta_odl + 2 * beta_odl2 * model_data["Odleglosc"]
        pct_negative = float((marginal_effects < 0).mean() * 100)
        print("\n=== H1a: Wpływ odległości jest ujemny ===")
        print(f"Średnia odległość: {odl_mean:.2f} km")
        print(f"Efekt krańcowy w średniej odległości: {marginal_effect:.6f}")
        print(f"Procent obserwacji z ujemnym efektem krańcowym: {pct_negative:.2f}%")
        h1a_confirmed = marginal_effect < 0 and pct_negative > 50
        print("H1a potwierdzona: Wpływ odległości jest ujemny (w średniej)" if h1a_confirmed else "H1a NIE potwierdzona")

        print("\n=== H1b: Wpływ odległości jest nieliniowy ===")
        f_test_nonlinear = wls.f_test("Odleglosc2 = 0")
        print("Test F dla nieliniowości (^2):")
        print(f"F-statystyka: {float(f_test_nonlinear.fvalue):.4f}")
        print(f"p-value: {float(f_test_nonlinear.pvalue):.6f}")
        h1b_confirmed = float(f_test_nonlinear.pvalue) < 0.05
        print("H1b potwierdzona: Zależność jest nieliniowa (istotnie)" if h1b_confirmed else "H1b NIE potwierdzona")

        print("\n" + "=" * 60)
        print("PODSUMOWANIE HIPOTEZY H1 (ZŁOŻONEJ)")
        print("=" * 60)
        print("\nH1a: Wpływ odległości jest UJEMNY")
        print(f"  - Efekt krańcowy w średniej odległości: {marginal_effect:.6f}")
        print(f"  - Procent obserwacji z ujemnym efektem: {pct_negative:.2f}%")
        print(f"  - Wniosek: {'POTWIERDZONA' if h1a_confirmed else 'ODRZUCONA'}")
        print("\nH1b: Wpływ odległości jest NIELINIOWY")
        print(f"  - F-test (^2 = 0): p-value = {float(f_test_nonlinear.pvalue):.6f}")
        print(f"  - Wniosek: {'POTWIERDZONA' if h1b_confirmed else 'ODRZUCONA'}")
        print("\n" + "-" * 60)
        print("H1 (ŁĄCZNIE): Wpływ odległości jest UJEMNY i NIELINIOWY")
        print(f"Wniosek końcowy: {'HIPOTEZA H1 POTWIERDZONA' if h1a_confirmed and h1b_confirmed else 'HIPOTEZA H1 ODRZUCONA'}")
        print("=" * 60)

    print("\nPODSUMOWANIE FINALNE")
    print(model_row.to_string(index=False))
    print("\nWspółczynniki:")
    print(coef_df.to_string(index=False))


def save_descriptive_statistics_tex(data: pd.DataFrame, x_cols: list[str]) -> None:
    # Zapisuje statystyki opisowe do tabeli LaTeX.
    diagnostic_cols = ["ySaldo", "logPrzystankiNa1000", *x_cols]
    rows = []
    for column in diagnostic_cols:
        series = data[column]
        rows.append(
            {
                "Zmienna": column,
                "Min": series.min(skipna=True),
                "Średnia": series.mean(skipna=True),
                "Max": series.max(skipna=True),
                "Odchylenie standardowe": series.std(skipna=True),
                "NA": int(series.isna().sum()),
            }
        )
    stats_df = pd.DataFrame(rows)
    save_overleaf_table(
        "statystyki_opisowe_zmiennych_przed_MNK",
        dataframe_to_latex_table(stats_df, "Statystyki opisowe zmiennych", column_format="lrrrrr"),
    )


def save_wmnk_diagnostic_tests_tex(fit: dict) -> None:
    # Zapisuje testy diagnostyczne do tabeli LaTeX.
    def scalar_stat(value) -> float:
        # Sprowadza skalar lub jednoelementowa tablice statystyki do liczby.
        try:
            return float(np.asarray(value).ravel()[0])
        except Exception:
            return np.nan

    rows = []
    for model_label, model, bp_result, white_result in [
        ("OLS bez logPrzystankiNa1000", fit["ols"], het_breuschpagan(fit["ols"].resid, fit["ols"].model.exog), safe_white(fit["ols"].resid, fit["ols"].model.exog)),
        ("Finalny WMNK", fit["wls"], fit["bp"], fit["white"]),
    ]:
        jb_result = sms.jarque_bera(model.resid)
        try:
            reset_result = linear_reset(model, power=3, test_type="fitted")
            reset_stat = scalar_stat(getattr(reset_result, "fvalue", getattr(reset_result, "statistic", np.nan)))
            reset_pvalue = float(reset_result.pvalue)
        except Exception:
            reset_stat = np.nan
            reset_pvalue = np.nan
        # Dla WMNK nie pokazujemy testow BP i White'a, bo tabela ma zawierac tylko testy reszt po korekcie wagami.
        if model_label != "Finalny WMNK":
            rows.extend(
                [
                    {"Model": model_label, "Test": "Breusch-Pagan", "Statystyka": float(bp_result[0]), "p-value": float(bp_result[1])},
                    {
                        "Model": model_label,
                        "Test": "White",
                        "Statystyka": float(white_result[0]) if not np.isnan(white_result[0]) else np.nan,
                        "p-value": float(white_result[1]) if not np.isnan(white_result[1]) else np.nan,
                    },
                ]
            )
        rows.extend(
            [
                {"Model": model_label, "Test": "Jarque-Bera", "Statystyka": float(jb_result[0]), "p-value": float(jb_result[1])},
                {"Model": model_label, "Test": "RESET", "Statystyka": reset_stat, "p-value": reset_pvalue},
            ]
        )
    tests_df = pd.DataFrame(rows)
    save_overleaf_table(
        "testy_diagnostyczne_model2_wmnk",
        dataframe_to_latex_table(tests_df, "Testy diagnostyczne modelu MNK i WMNK", column_format="llrr"),
    )


def backward_elimination_wmnk_bez_drog(data: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame, object]:
    # Usuwa kolejno najmniej istotne zmienne z modelu WMNK.
    current_cols = build_x_cols(include_roads=True, g_variant="base_log")
    step_rows = []
    coef_rows = []
    step = 0

    while True:
        fit = fit_wls_for_columns(data, current_cols)
        wls = fit["wls"]
        pvalues_without_const = wls.pvalues.drop(labels=["const"], errors="ignore")
        max_pvalue = float(pvalues_without_const.max())
        variable_to_remove = str(pvalues_without_const.idxmax())
        all_significant = bool(max_pvalue < 0.05)

        step_rows.append(
            {
                "step": step,
                "removed_variable": "" if all_significant else variable_to_remove,
                "removed_variable_pvalue": np.nan if all_significant else max_pvalue,
                "n_obs": int(len(fit["model_data"])),
                "num_regressors": int(len(current_cols)),
                "regressors": ",".join(current_cols),
                "aic": float(wls.aic),
                "bic": float(wls.bic),
                "r2": float(wls.rsquared),
                "adj_r2": float(wls.rsquared_adj),
                "bp_pvalue": float(fit["bp"][1]),
                "white_pvalue": float(fit["white"][1]) if not np.isnan(fit["white"][1]) else np.nan,
                "max_pvalue_without_const": max_pvalue,
                "all_variables_significant_5pct": all_significant,
            }
        )
        for variable in wls.params.index:
            coef_rows.append(
                {
                    "step": step,
                    "variable": variable,
                    "coefficient": float(wls.params[variable]),
                    "std_error": float(wls.bse[variable]),
                    "t_value": float(wls.tvalues[variable]),
                    "p_value": float(wls.pvalues[variable]),
                }
            )

        if all_significant or len(current_cols) == 1:
            return pd.DataFrame(step_rows), pd.DataFrame(coef_rows), wls

        current_cols = [column for column in current_cols if column != variable_to_remove]
        step += 1


def compare_housing_stock_and_new_flats(data: pd.DataFrame) -> pd.DataFrame:
    # Porownuje warianty zmiennych mieszkaniowych w tej samej specyfikacji.
    base_cols = [
        "logPrzystankiNa1000",
        "logWynagrodzenia",
        "M2NaOsobe",
        "Przychodnie",
        "Bezrobocie",
        "Miejska",
        "Wiejska",
        "Odleglosc",
        "Odleglosc2",
        "logWskaznikG",
    ]
    housing_variants = [
        ("bez_mieszkan", []),
        ("oddane_raw", ["MieszkaniaOddane"]),
        ("oddane_log", ["logMieszkaniaOddane"]),
    ]
    rows = []
    coef_rows = []
    vif_rows = []
    for variant_name, housing_cols in housing_variants:
        x_cols = [*base_cols, *housing_cols]
        fit = fit_wls_for_columns(data, x_cols)
        wls = fit["wls"]
        pvalues_without_const = wls.pvalues.drop(labels=["const"], errors="ignore")
        rows.append(
            {
                "variant": variant_name,
                "housing_variables": ",".join(housing_cols),
                "n_obs": int(len(fit["model_data"])),
                "aic": float(wls.aic),
                "bic": float(wls.bic),
                "r2": float(wls.rsquared),
                "adj_r2": float(wls.rsquared_adj),
                "bp_pvalue": float(fit["bp"][1]),
                "white_pvalue": float(fit["white"][1]) if not np.isnan(fit["white"][1]) else np.nan,
                "max_pvalue_without_const": float(pvalues_without_const.max()),
                "insignificant_variables_5pct": ",".join(
                    variable for variable, pvalue in pvalues_without_const.items() if pvalue >= 0.05
                ),
            }
        )
        for variable in wls.params.index:
            coef_rows.append(
                {
                    "variant": variant_name,
                    "variable": variable,
                    "coefficient": float(wls.params[variable]),
                    "std_error": float(wls.bse[variable]),
                    "t_value": float(wls.tvalues[variable]),
                    "p_value": float(wls.pvalues[variable]),
                }
            )
        x_vif = sm.add_constant(fit["model_data"][x_cols])
        for idx, column in enumerate(x_vif.columns):
            vif_rows.append(
                {
                    "variant": variant_name,
                    "regressor": column,
                    "vif": float(variance_inflation_factor(x_vif.to_numpy(dtype=float), idx)),
                    "n": int(len(fit["model_data"])),
                }
            )

    comparison = pd.DataFrame(rows).sort_values(["aic", "bic"], ascending=True).reset_index(drop=True)
    comparison.to_csv(WYDRUKI_DIR / "WMNK_mieszkania_oddane_zasob_porownanie.csv", index=False)
    comparison.to_html(
        WYDRUKI_DIR / "WMNK_mieszkania_oddane_zasob_porownanie.html",
        index=False,
        float_format=TABLE_FLOAT_FORMAT,
    )
    pd.DataFrame(coef_rows).to_csv(WYDRUKI_DIR / "WMNK_mieszkania_oddane_zasob_wspolczynniki.csv", index=False)
    pd.DataFrame(vif_rows).to_csv(WYDRUKI_DIR / "WMNK_mieszkania_oddane_zasob_vif.csv", index=False)
    return comparison


def add_g_comparison_to_baseline(comparison: pd.DataFrame) -> pd.DataFrame:
    # Dodaje porownanie z modelem bez Wskaznika G.
    group_cols = ["sample", "variable", "housing"]
    baseline = comparison.loc[
        comparison["wskaznik_g"] == "none",
        group_cols + ["wls_aic", "wls_bic", "wls_r2"],
    ].rename(
        columns={
            "wls_aic": "baseline_wls_aic",
            "wls_bic": "baseline_wls_bic",
            "wls_r2": "baseline_wls_r2",
        }
    )
    comparison = comparison.merge(baseline, on=group_cols, how="left")
    comparison["delta_aic_vs_baseline"] = comparison["wls_aic"] - comparison["baseline_wls_aic"]
    comparison["delta_bic_vs_baseline"] = comparison["wls_bic"] - comparison["baseline_wls_bic"]
    comparison["delta_r2_vs_baseline"] = comparison["wls_r2"] - comparison["baseline_wls_r2"]
    return comparison


def add_housing_comparison_to_baseline(comparison: pd.DataFrame) -> pd.DataFrame:
    # Dodaje porownanie z modelem bez dodatkowej zmiennej mieszkaniowej.
    group_cols = ["sample", "variable", "wskaznik_g"]
    baseline = comparison.loc[
        comparison["housing"] == "none",
        group_cols + ["wls_aic", "wls_bic", "wls_r2", "wls_adj_r2"],
    ].rename(
        columns={
            "wls_aic": "baseline_no_housing_wls_aic",
            "wls_bic": "baseline_no_housing_wls_bic",
            "wls_r2": "baseline_no_housing_wls_r2",
            "wls_adj_r2": "baseline_no_housing_wls_adj_r2",
        }
    )
    comparison = comparison.merge(baseline, on=group_cols, how="left")
    comparison["delta_aic_vs_no_housing"] = comparison["wls_aic"] - comparison["baseline_no_housing_wls_aic"]
    comparison["delta_bic_vs_no_housing"] = comparison["wls_bic"] - comparison["baseline_no_housing_wls_bic"]
    comparison["delta_r2_vs_no_housing"] = comparison["wls_r2"] - comparison["baseline_no_housing_wls_r2"]
    comparison["delta_adj_r2_vs_no_housing"] = comparison["wls_adj_r2"] - comparison["baseline_no_housing_wls_adj_r2"]
    return comparison


def build_x_cols(
    include_roads: bool,
    g_variant: str = "base_log",
    housing_variant: str = "base_m2",
) -> list[str]:
    # Buduje wspolna liste regresorow dla estymacji i VIF.
    x_cols = [
        "logPrzystankiNa1000",
        "logWynagrodzenia",
        "M2NaOsobe",
        "logWskaznikG",
        "logMieszkaniaNa1000",
        "MieszkaniaOddane",
        "Przychodnie",
        "Bezrobocie",
        "Miejska",
        "Wiejska",
        "Odleglosc",
        "Odleglosc2",
    ]
    if g_variant not in {"base_log", "log", "none"}:
        raise ValueError(f"Nieznany wariant Wskaznika G: {g_variant}")
    if housing_variant not in {"base_m2", "none"}:
        raise ValueError(f"Nieznany wariant zasobow mieszkaniowych: {housing_variant}")
    return x_cols


def save_correlation_and_vif(data: pd.DataFrame, base_variants: list[dict]) -> None:
    # Zapisuje korelacje i VIF dla porownywanych specyfikacji.
    correlation_cols = [
        "ySaldo",
        "logPrzystankiNa1000",
        "logWynagrodzenia",
        "logMieszkaniaNa1000",
        "Przychodnie",
        "Bezrobocie",
        "Miejska",
        "Wiejska",
        "Odleglosc",
        "Odleglosc2",
        "WskaznikG",
        "logWskaznikG",
        "M2NaOsobe",
    ]
    corr_data = data.loc[:, correlation_cols].dropna().copy()
    for corr_method in ["pearson", "spearman"]:
        # Zapisuje korelacje dla wybranej metody.
        corr_df = corr_data.corr(method=corr_method)
        corr_df.to_csv(WYDRUKI_DIR / f"WMNKtestyzmiennych_korelacje_{corr_method}.csv")
        corr_df.to_html(
            WYDRUKI_DIR / f"WMNKtestyzmiennych_korelacje_{corr_method}.html",
            float_format=TABLE_FLOAT_FORMAT,
        )

    vif_rows = []
    for variant in base_variants:
        for g_variant in ["base_log"]:
            for housing_variant in ["base_m2"]:
                x_cols = build_x_cols(
                    variant["include_roads"],
                    g_variant,
                    housing_variant,
                )
                required_cols = [*x_cols]
                model_data = variant["data"].loc[:, required_cols].dropna().copy()
                x_vif = sm.add_constant(model_data[x_cols])
                for idx, column in enumerate(x_vif.columns):
                    vif_rows.append(
                        {
                            "model": variant["model_name"],
                            "sample": variant["sample_label"],
                            "variable_group": "bez_drog",
                            "wskaznik_g": g_variant,
                            "housing": housing_variant,
                            "regressor": column,
                            "vif": float(variance_inflation_factor(x_vif.to_numpy(dtype=float), idx)),
                            "n": int(len(model_data)),
                        }
                    )
    vif_df = pd.DataFrame(vif_rows)
    vif_df.to_csv(WYDRUKI_DIR / "WMNKtestyzmiennych_vif.csv", index=False)
    vif_df.to_html(
        WYDRUKI_DIR / "WMNKtestyzmiennych_vif.html",
        index=False,
        float_format=TABLE_FLOAT_FORMAT,
    )


def main() -> None:
    data = load_model_data()
    # Najpierw liczymy pelny OLS, a potem finalny WMNK bez przystankow.
    initial_model_data = data.loc[:, ["Kod", "ySaldo", *INITIAL_OLS_COLS]].dropna().copy()
    initial_ols = sm.OLS(
        initial_model_data["ySaldo"],
        sm.add_constant(initial_model_data[INITIAL_OLS_COLS]),
    ).fit()
    fit = fit_wls_for_columns(data, FINAL_WMNK_COLS)
    wls = fit["wls"]

    model_row = pd.DataFrame(
        [
            {
                "model": "WMNK_final_bez_logPrzystankiNa1000",
                "regressors": ",".join(FINAL_WMNK_COLS),
                "n_obs": int(len(fit["model_data"])),
                "aic": float(wls.aic),
                "bic": float(wls.bic),
                "r2": float(wls.rsquared),
                "adj_r2": float(wls.rsquared_adj),
                "bp_pvalue": float(fit["bp"][1]),
                "white_pvalue": float(fit["white"][1]) if not np.isnan(fit["white"][1]) else np.nan,
            }
        ]
    )
    coef_df = pd.DataFrame(
        [
            {
                "variable": variable,
                "coefficient": float(wls.params[variable]),
                "std_error": float(wls.bse[variable]),
                "t_value": float(wls.tvalues[variable]),
                "p_value": float(wls.pvalues[variable]),
            }
            for variable in wls.params.index
        ]
    )
    x_vif = sm.add_constant(fit["model_data"][FINAL_WMNK_COLS])
    vif_df = pd.DataFrame(
        [
            {
                "regressor": column,
                "vif": float(variance_inflation_factor(x_vif.to_numpy(dtype=float), idx)),
                "n": int(len(fit["model_data"])),
            }
            for idx, column in enumerate(x_vif.columns)
        ]
    )
    save_descriptive_statistics_tex(data, FINAL_WMNK_COLS)
    save_wmnk_diagnostic_tests_tex(fit)

    save_stargazer_models(
        [initial_ols, fit["ols"]],
        "modele_ols_podstawowy_i_bez_przystankow",
        covariate_order=["const", *INITIAL_OLS_COLS],
        title="Model OLS podstawowy i model bez logPrzystankiNa1000",
    )

    model_row.to_csv(WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000.csv", index=False)
    model_row.to_html(
        WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000.html",
        index=False,
        float_format=TABLE_FLOAT_FORMAT,
    )
    coef_df.to_csv(WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000_wspolczynniki.csv", index=False)
    coef_df.to_html(
        WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000_wspolczynniki.html",
        index=False,
        float_format=TABLE_FLOAT_FORMAT,
    )
    vif_df.to_csv(WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000_vif.csv", index=False)
    (WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000_summary.txt").write_text(wls.summary().as_text(), encoding="utf-8")
    (WYDRUKI_DIR / "WMNK_final_bez_logPrzystankiNa1000_summary.html").write_text(wls.summary().as_html(), encoding="utf-8")
    save_ysaldo_heatmap(data)
    save_variable_heatmap_panel(data, ["ySaldo", *(column for column in FINAL_WMNK_COLS if column != "Odleglosc2")])
    save_single_variable_heatmap(data, "logM2NaOsobe", "mapa_ciepla_logM2NaOsobe", "Mapa ciepla logM2NaOsobe")
    save_final_wmnk_residual_map(fit)
    save_final_wmnk_plots(data, fit, FINAL_WMNK_COLS)
    save_final_wmnk_stargazer(fit, FINAL_WMNK_COLS, initial_ols)
    # Zapisuje trzy etapy estymacji do jednej tabeli LaTeX.
    save_stargazer_models(
        [initial_ols, fit["ols"], fit["wls"]],
        "WydrukiMNKfull",
        covariate_order=["const", *INITIAL_OLS_COLS],
        title="Pełny OLS, OLS bez przystanków i finalny WMNK",
    )
    chow = run_chow_test_for_final_model(fit, FINAL_WMNK_COLS)
    print_final_wmnk_diagnostics(data, fit, FINAL_WMNK_COLS, model_row, coef_df.copy(), vif_df, initial_ols=initial_ols)
    print(
        "Test Chowa dla finalnego modelu WMNK: "
        f"F={chow['result']['F_chow']:.4f}, "
        f"p-value={chow['result']['p_value']:.6f}, "
        f"df=({chow['result']['df_num']}; {chow['result']['df_den']})"
    )

    # Nadpisuje stare nazwy plikow wynikami modelu finalnego.
    model_row.rename(columns={"model": "step"}).to_csv(WYDRUKI_DIR / "WMNK_bez_logPrzystankiNa1000_eliminacja_5pct.csv", index=False)
    coef_df.insert(0, "step", "final")
    coef_df.to_csv(WYDRUKI_DIR / "WMNK_bez_logPrzystankiNa1000_eliminacja_5pct_wspolczynniki.csv", index=False)
    (WYDRUKI_DIR / "WMNK_bez_logPrzystankiNa1000_final_5pct_summary.txt").write_text(wls.summary().as_text(), encoding="utf-8")
    (WYDRUKI_DIR / "WMNK_bez_logPrzystankiNa1000_final_5pct_summary.html").write_text(wls.summary().as_html(), encoding="utf-8")

    lines = [
        "FINALNY MODEL WMNK BEZ LOGPRZYSTANKINA1000",
        "",
        "Regresory:",
        ", ".join(FINAL_WMNK_COLS),
        "",
        model_row.to_string(index=False),
        "",
        "Współczynniki:",
        coef_df.to_string(index=False),
    ]
    (WYDRUKI_DIR / "WMNKtestyzmiennych_porownanie.txt").write_text("\n".join(lines), encoding="utf-8")


if __name__ == "__main__":
    main()
