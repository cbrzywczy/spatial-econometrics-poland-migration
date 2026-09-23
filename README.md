# Ekonometria przestrzenna: migracje wewnętrzne w gminach Polski

<img src="charts/lisa_klastry_saldo_migracji.png" alt="Mapa klastrów LISA dla salda migracji" align="right" width="340">

Dlaczego jedne gminy zyskują mieszkańców, a inne ich tracą? Praca licencjacka bada, jak saldo migracji wewnętrznych zależy od cech ekonomicznych i położenia gminy oraz od sytuacji w gminach sąsiednich.

**Dane:** 2477 gmin, rok 2024: saldo migracji, wynagrodzenia, bezrobocie, mieszkania, przychodnie, dochody podatkowe, odległość od dużego miasta

**Modele:** MNK / WMNK z imputacją MICE · SAR, SEM, SLX, SDM, SDEM, SAC, GNS · GWR i mixed GWR

**Testy:** Chow, RESET · I Morana, C Geary’ego, join-count, LISA · LM · F(3) dla GWR

<img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/r/r-original.svg" width="16" alt="R"> &nbsp;`spdep` · `spatialreg` · `GWmodel` · `mice` · `sf`<br>
<img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/python/python-original.svg" width="16" alt="Python"> &nbsp;`pandas` · `geopandas` · `statsmodels` · `scipy`

<sub>Praca licencjacka, Informatyka i Ekonometria, WNE UW, 2026. Na mapie: klastry LISA salda migracji.</sub>

<br clear="right">

## Najważniejsze wyniki

- Model WMNK wyjaśnia **43,4%** zmienności salda migracji. Najsilniej przyciąga **wynagrodzenie**. Najsilniej wypychają **bezrobocie** i miejski charakter gminy.
- Saldo migracji maleje nieliniowo wraz z odległością od dużego miasta. Efekt krańcowy jest ujemny w przedziale 0–107 km dla 98% gmin.
- Reszty MNK mają dodatnią, istotną autokorelację przestrzenną. Model bez części przestrzennej pomija interakcje między sąsiednimi gminami.
- Pierścienie podmiejskie największych miast tworzą klastry **High-High**, zwykle bez samego rdzenia aglomeracji (Low-High). Gminy peryferyjne na wschodzie tworzą klastry **Low-Low**.
- Model **SDEM** usuwa autokorelację reszt (I = −0,004; p = 0,617) i wykazuje istotne **efekty spillover**. Czynniki przyciągające w gminach sąsiednich są ujemnie powiązane z saldem danej gminy: gminy konkurują o migrantów.
- **GWR** dopasowuje się lepiej niż MNK. Wszystkie parametry zmieniają się istotnie w przestrzeni (test F(3)). Najwyższe lokalne R² model osiąga dla aglomeracji Warszawy, Poznania, Wrocławia, Łodzi i Krakowa.

## Wykresy

| Lokalne R² modelu GWR | Lokalny współczynnik GWR dla log wynagrodzeń |
|---|---|
| ![Lokalne R² modelu GWR](charts/gwr_lokalne_r2.png) | ![Lokalny współczynnik GWR dla wynagrodzeń](charts/gwr_wspolczynnik_wynagrodzenia.png) |
| **Wykres rozrzutu I Morana dla salda migracji** | **Mapy zmiennych modelu** |
| ![Wykres rozrzutu Morana](charts/moran_wykres_rozrzutu.png) | ![Mapy zmiennych modelu](charts/mapy_zmiennych.png) |

## Kod

| Plik | Zawartość |
|---|---|
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/r/r-original.svg" width="14"> [`R/01_mnk_wmnk_mice.R`](R/01_mnk_wmnk_mice.R) | statystyki opisowe, MNK / WMNK, diagnostyka, test Chowa, imputacja MICE, reguły Rubina |
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/r/r-original.svg" width="14"> [`R/02_modele_przestrzenne.R`](R/02_modele_przestrzenne.R) | macierz wag, statystyki autokorelacji, testy LM, estymacja i wybór modelu przestrzennego, efekty bezpośrednie i pośrednie |
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/r/r-original.svg" width="14"> [`R/03_gwr.R`](R/03_gwr.R) | wybór pasma, GWR, test F(3), lokalna współliniowość, mapy współczynników |
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/r/r-original.svg" width="14"> [`R/04_gwr_mieszany.R`](R/04_gwr_mieszany.R) | mixed GWR: parametry globalne i lokalne |
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/python/python-original.svg" width="14"> [`python/mnk_testy_zmiennych.py`](python/mnk_testy_zmiennych.py) | przygotowanie i transformacje zmiennych, eliminacja zmiennych, testy MNK |
| <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.16.0/icons/python/python-original.svg" width="14"> [`python/odleglosci_km.py`](python/odleglosci_km.py) | centroidy gmin (EPSG:2180), odległość od najbliższego dużego miasta |

## Dane

Dane nie są częścią repozytorium. Skrypty odczytują pliki z katalogu `data/`.

- [Bank Danych Lokalnych GUS](https://bdl.stat.gov.pl/bdl/start), 2024: saldo migracji na 1000 osób, bezrobocie, mediany wynagrodzeń, populacja, przystanki, przychodnie, mieszkania oddane, powierzchnia mieszkań na osobę
- [Ministerstwo Finansów](https://www.gov.pl/web/finanse/wskazniki-dochodow-podatkowych-gmin-powiatow-i-wojewodztw-na-2024-r), 2024: wskaźnik dochodów podatkowych gmin G
- [Państwowy Rejestr Granic, GUGiK](https://dane.gov.pl/pl/dataset/726,panstwowy-rejestr-granic-i-powierzchni-jednostek-podziaow-terytorialnych-kraju): granice gmin

## Licencja

Kod: [MIT](LICENSE). Wykresy: CC BY 4.0.
