from pathlib import Path
import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "data"
#Wczytanie pliku z granicami gmin
gminy = gpd.read_file(DATA_DIR / "A03_Granice_gmin.shp", encoding="utf-8")
#Wybór projekcji i koordynatów
gminy_proj = gminy.to_crs(epsg=2180)
#Wyznaczenie centroidów gmin
gminy_proj["centroid"] = gminy_proj.geometry.centroid
#wgranie listy miast powiatowych
mpowiatowe = [
    "Jelenia Góra", "Legnica", "Wałbrzych", "Wrocław", "Bydgoszcz", "Grudziądz",
    "Toruń", "Włocławek", "Biała Podlaska", "Chełm", "Lublin", "Zamość",
    "Gorzów Wielkopolski", "Zielona Góra", "Łódź", "Piotrków Trybunalski",
    "Skierniewice", "Kraków", "Nowy Sącz", "Tarnów", "Ostrołęka", "Płock",
    "Radom", "Siedlce", "Warszawa", "Opole", "Krosno", "Przemyśl", "Rzeszów",
    "Tarnobrzeg", "Białystok", "Łomża", "Suwałki", "Gdańsk", "Gdynia", "Słupsk",
    "Sopot", "Bielsko-Biała", "Bytom", "Chorzów", "Częstochowa", "Dąbrowa Górnicza",
    "Gliwice", "Jastrzębie-Zdrój", "Jaworzno", "Katowice", "Mysłowice",
    "Piekary Śląskie", "Ruda Śląska", "Rybnik", "Siemianowice Śląskie",
    "Sosnowiec", "Świętochłowice", "Tychy", "Zabrze", "Żory", "Kielce",
    "Elbląg", "Olsztyn", "Kalisz", "Konin", "Leszno", "Poznań", "Koszalin",
    "Szczecin", "Świnoujście"]

# Wyznaczenie siedzib gmin miast powiatowych na potrzebę wizualizacji
# siedziba = 1 dla siedziby gminy miasta powiatowego (najmniejsza powierzchnia), 0 dla pozostałych
def oznacz_siedzibe(df, mpowiatowe):
    df["siedziba"] = 0
    maska_miasta = df["JPT_NAZWA_"].isin(mpowiatowe)
    df_miasta = df[maska_miasta].copy()
#obserwacja z najmniejszą powierzchnią = siedziba (=1)
    idx_min_pow = df_miasta.groupby("JPT_NAZWA_")["JPT_POWIER"].idxmin()
    df.loc[idx_min_pow, "siedziba"] = 1
    return df

gminy_proj = oznacz_siedzibe(gminy_proj, mpowiatowe)

# Utworzenie GeoDataFrame z centroidami miast na prawach powiatu
centroidy = gminy_proj[gminy_proj["siedziba"] == 1].set_geometry("centroid")

wyniki = []

#wczytanie danych o populacji
popGmin = pd.read_csv(DATA_DIR / "PopulacjaGmin.csv",sep = ";", decimal="," )
popGmin["poprazem"] = popGmin["gminy bez miast na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;ogółem;2024;[osoba]"] + popGmin["miasta na prawach powiatu;miejsce zamieszkania;stan na 31 grudnia;ogółem;2024;[osoba]"]
pop = popGmin.loc[:, ["Kod", "poprazem"]].copy()
pop.columns = ["JPT_KOD_JE", "populacja"]
gminy_proj["JPT_KOD_JE"] = gminy_proj["JPT_KOD_JE"].astype(str).str.zfill(7)
pop["JPT_KOD_JE"] = pop["JPT_KOD_JE"].astype(str).str.zfill(7)
gminy_proj = gminy_proj.merge(pop, on="JPT_KOD_JE", how="left")

duze_miasta = gminy_proj[(gminy_proj["populacja"] > 100000)].copy()
duze_miasta = duze_miasta.set_geometry("centroid")
#Wyznaczenie najbliższego dużego miasta i odległości od niego dla każdej gminy
for idx, gmina in gminy_proj.iterrows():  # iterujemy po wszystkich gminach
    # Jeśli gmina sama jest dużym miastem (pop > 100000), odległość = 0
    if gmina["populacja"] > 100000:
        wyniki.append({
            "TERYT": gmina["JPT_KOD_JE"],
            "gmina": gmina["JPT_NAZWA_"],
            "najblizsze_duze_miasto": gmina["JPT_NAZWA_"],
            "odleglosc": 0
        })
        continue
    
    # Dla pozostałych gmin obliczamy odległość do najbliższego dużego miasta
    gmina_centroid = gmina["centroid"]
    odleglosci = duze_miasta["centroid"].distance(gmina_centroid)
    min_idx = odleglosci.idxmin()
    najblizsze_duze_miasto = duze_miasta.loc[min_idx, "JPT_NAZWA_"]
    min_distance = odleglosci[min_idx]
    
    wyniki.append({
        "TERYT": gmina["JPT_KOD_JE"],
        "gmina": gmina["JPT_NAZWA_"],
        "najblizsze_duze_miasto": najblizsze_duze_miasto,
        "odleglosc": min_distance
    })

#Zamiana wyników na df
df_odleglosci = pd.DataFrame(wyniki)

#sprawdzenie przykładowych miast
print(df_odleglosci[df_odleglosci['gmina'].str.contains('Olsztyn', case=False, na=False)])

#Nowa ramka danych z istotnymi kolumnami do eksportu
df_odleglosci_km = pd.DataFrame()
df_odleglosci_km["Kod"] = df_odleglosci["TERYT"]
df_odleglosci_km["gmina"] = df_odleglosci["gmina"]
df_odleglosci_km["odleglosc"] = df_odleglosci["odleglosc"]/1000
#Eksport danych o odległościach do CSV
df_odleglosci_km.to_csv(DATA_DIR / "odleglosci_km.csv", index=False, encoding="utf-8")
