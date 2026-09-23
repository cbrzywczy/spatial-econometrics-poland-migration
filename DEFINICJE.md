# Definicje

Pojęcia i metody użyte w pracy. [← powrót do README](README.md)

- [Zjawisko](#zjawisko)
- [Dane i zmienne](#dane-i-zmienne)
- [Model MNK](#model-mnk)
- [Analiza przestrzenna](#analiza-przestrzenna)
- [Modele przestrzenne](#modele-przestrzenne)
- [Regresja geograficznie ważona (GWR)](#regresja-geograficznie-ważona-gwr)

## Zjawisko

**Migracje wewnętrzne** to zmiana miejsca zamieszkania w obrębie jednego kraju. W pracy zdefiniowano je jako rejestrowane przemieszczenie się na pobyt stały do innej gminy.

**Saldo migracji** to różnica między napływem a odpływem ludności, przeliczona na 1000 mieszkańców gminy. Dodatnie saldo oznacza, że więcej osób do gminy przybywa, niż ją opuszcza.

**Suburbanizacja** to wzrost znaczenia strefy podmiejskiej na skutek odpływu ludności z rdzenia aglomeracji do jego otoczenia. W Polsce proces przyspieszył od drugiej połowy lat 90. i skupia się wokół Warszawy, Krakowa, Poznania, Wrocławia i Trójmiasta.

**Czynniki push-pull** to cechy ekonomiczne, geograficzne i infrastrukturalne gminy i jej otoczenia. Czynniki *pull* (przyciągające) zwiększają atrakcyjność gminy: przyciągają napływ ludności i ograniczają jej odpływ. Czynniki *push* (wypychające) działają odwrotnie.

**Efekt spillover** to rozlewanie się efektów z gmin sąsiednich na daną gminę. Saldo migracji w jednej gminie może być powiązane z cechami gmin sąsiednich, na przykład w sposób konkurencyjny.

## Dane i zmienne

**Typ gminy** wynika z ostatniej cyfry kodu TERYT: 1 dla gmin miejskich, 2 dla wiejskich, 3 dla miejsko-wiejskich. Zmienne *Miejska* i *Wiejska* przyjmują wartość 1 dla odpowiedniego typu. Gminy miejsko-wiejskie są kategorią bazową.

**Duże miasto** to, zgodnie z definicją GUS, miejscowość o populacji co najmniej 100 000 mieszkańców. W 2024 roku takich miast było 34.

**Odległość** to dystans w kilometrach między centroidem gminy a centroidem najbliższego dużego miasta, liczony w układzie PL-1992. Dużym miastom przypisano odległość 0.

**Wskaźnik G** to dochody podatkowe gminy na jednego mieszkańca, publikowane przez Ministerstwo Finansów. W modelu przybliża fiskalną zamożność gminy.

**Przeliczenie na mieszkańca.** Saldo migracji, przystanki i mieszkania przeliczono na 1000 mieszkańców, a przychodnie na 10 tys. mieszkańców. Dzięki temu model nie mierzy pośrednio wielkości gminy. Wadą jest ryzyko przeszacowania wskaźników dla bardzo małych gmin.

**Logarytmizacja.** Wynagrodzenia, mieszkania i przystanki zlogarytmowano z powodu heteroskedastyczności.

## Model MNK

**Postać modelu.** Zmienną zależną jest saldo migracji wewnętrznych na 1000 mieszkańców w 2024 roku:

```math
y_i = \beta_0 + \sum_{k=1}^{K} \beta_k x_{k,i} + \beta_{d}\, d_i + \beta_{d^2}\, d_i^{2} + \varepsilon_i
```

gdzie $y_i$ to saldo migracji w gminie $i$, $x_{k,i}$ to cechy gminy, $d_i$ to odległość od najbliższego dużego miasta, a $\varepsilon_i$ to składnik losowy. Kwadrat odległości pozwala uchwycić nieliniowy efekt odległości.

**MICE i PMM.** Dla oddanych mieszkań brakowało 19 obserwacji, a dla przychodni 45. Braki uzupełniono metodą dopasowania średnich predykcyjnych (PMM) w ramach wielokrotnej imputacji równaniami łańcuchowymi (MICE). Powstało 10 zbiorów z imputowanymi danymi. Każdy model oszacowano osobno na każdym zbiorze.

**Reguły Rubina** łączą wyniki z wielu imputowanych zbiorów w jedno oszacowanie. Uwzględniają zarówno niepewność wewnątrz zbiorów, jak i między nimi.

**WMNK (ważona metoda najmniejszych kwadratów)** ogranicza problem heteroskedastyczności. Użyto metody feasible WLS: zlogarytmowane reszty MNK stają się zmienną objaśnianą w pomocniczym modelu, a każda obserwacja dostaje wagę równą odwrotności przewidywanej wariancji.

**Heteroskedastyczność** oznacza, że wariancja składnika losowego nie jest stała. Oszacowania pozostają nieobciążone, ale błędy standardowe są niewiarygodne.

**Test RESET** sprawdza poprawność formy funkcyjnej modelu. Odrzucenie hipotezy zerowej sugeruje pominięte nieliniowości lub interakcje.

**Test Jarque-Bera** sprawdza normalność reszt.

**Test Chowa** sprawdza stabilność parametrów między podpróbami. Hipoteza zerowa mówi o równości parametrów. W pracy oznacza to sprawdzenie, czy wpływ zmiennych na saldo migracji jest jednakowy dla gmin wiejskich, miejsko-wiejskich i miejskich.

## Analiza przestrzenna

**Macierz wag przestrzennych $W$** opisuje, które gminy są sąsiadami. Użyto sąsiedztwa typu **Queen**: gminy są sąsiadami, jeżeli mają jakikolwiek wspólny punkt styku, granicę lub wierzchołek. Sąsiedztwo **Rook** uwzględnia tylko wspólne granice. Queen obejmuje całe lokalne otoczenie, a większa liczba sąsiadów daje stabilniejsze oszacowania.

**Autokorelacja przestrzenna.** Dodatnia autokorelacja oznacza, że w danej gminie występują podobne wartości co u jej sąsiadów, czyli tworzą się skupiska o podobnych wartościach. Brak autokorelacji oznacza losowe rozmieszczenie wartości w przestrzeni.

**Statystyka I Morana** mierzy globalną autokorelację przestrzenną. Hipoteza zerowa mówi, że wartości cechy są rozmieszczone losowo.

```math
I = \frac{n}{S_0} \cdot \frac{\sum_{i=1}^{n}\sum_{j=1}^{n} w_{ij}(x_i-\bar{x})(x_j-\bar{x})}{\sum_{i=1}^{n}(x_i-\bar{x})^2}
```

gdzie $n$ to liczba gmin, $x_i$ i $x_j$ to wartości zmiennej, $\bar{x}$ to średnia, $w_{ij}$ to element macierzy wag, a $S_0$ to suma wszystkich wag. Wartość dodatnia oznacza skupiska podobnych wartości.

**Statystyka C Geary’ego** to druga miara globalnej autokorelacji, bardziej czuła na różnice między bezpośrednimi sąsiadami. Wartość poniżej 1 oznacza dodatnią autokorelację.

**Wykres rozrzutu Morana** zestawia wartość zmiennej w gminie (oś X) ze średnią wartością u jej sąsiadów (oś Y). Nachylenie linii regresji odpowiada statystyce I Morana.

**LISA (Local Indicators of Spatial Association)** to lokalna miara autokorelacji przestrzennej. Użyto lokalnej statystyki Morana. Jej wizualizacja pokazuje, gdzie tworzą się najsilniejsze skupienia podobnych i odmiennych wartości. Pierwszy człon nazwy klastra odnosi się do gminy, a drugi do jej otoczenia:

- **High-High:** gmina o wysokiej wartości otoczona gminami o wysokich wartościach (*hotspot*),
- **Low-Low:** gmina o niskiej wartości otoczona gminami o niskich wartościach,
- **High-Low** i **Low-High:** gminy odstające od swojego otoczenia.

Na mapie LISA szare gminy mają nieistotną wartość statystyki.

**Autokorelacja reszt MNK** oznacza, że model „myli się” w podobny sposób w sąsiednich gminach. To naruszenie założenia MNK o niezależności obserwacji i przesłanka do użycia modeli przestrzennych.

## Modele przestrzenne

**Opóźnienie przestrzenne** powstaje przez przemnożenie składnika modelu przez macierz wag $W$. Modele mogą opóźniać:

- **zmienną zależną ($\rho Wy$):** saldo w gminie jest powiązane z saldem w gminach sąsiednich,
- **zmienne objaśniające ($WX\theta$):** saldo w gminie jest powiązane z cechami gmin sąsiednich,
- **składnik losowy ($\lambda Wu$):** błąd modelu ma strukturę przestrzenną.

| Model | Skrót | $\rho Wy$ | $WX\theta$ | $\lambda Wu$ |
|---|---|:-:|:-:|:-:|
| General Nested Spatial Model | GNS | ✓ | ✓ | ✓ |
| Spatial Durbin Model | SDM | ✓ | ✓ | |
| Spatial Durbin Error Model | SDEM | | ✓ | ✓ |
| Spatial Autoregressive Combined Model | SAC | ✓ | | ✓ |
| Spatial Autoregressive Model | SAR | ✓ | | |
| Spatial Lag of X Model | SLX | | ✓ | |
| Spatial Error Model | SEM | | | ✓ |

**SDEM (Spatial Durbin Error Model)** to model wybrany w pracy:

```math
\mathbf{y} = \mathbf{X}\boldsymbol{\beta} + \mathbf{W}\mathbf{X}\boldsymbol{\theta} + \mathbf{u}, \qquad \mathbf{u} = \lambda\,\mathbf{W}\mathbf{u} + \boldsymbol{\varepsilon}
```

**Testy LM (Rao score)** wskazują, jakiego opóźnienia potrzebuje model. Test SARMA sprawdza, czy MNK w ogóle wystarcza. Wersje odporne (adjRSerr, adjRSlag) oddzielają opóźnienie błędu od opóźnienia zmiennej zależnej.

**Kryteria AIC, BIC i log-likelihood** porównują dopasowanie modeli. BIC mocniej karze za liczbę parametrów.

**Test LR (ilorazu wiarygodności)** sprawdza, czy uproszczenie bardziej rozbudowanego modelu istotnie pogarsza dopasowanie.

**Overfitting** to nadmierne dopasowanie do danych. Najbardziej rozbudowany model GNS jest na nie podatny, dlatego w pracy wybrano prostszy SDEM.

**Test Walda** sprawdza, czy współczynniki $\theta$ przy $WX$ są łącznie równe 0. Odrzucenie hipotezy zerowej oznacza istotny efekt spillover cech gmin sąsiednich.

**Efekty w modelu SDEM:**

- **bezpośredni ($\beta$):** przeciętny wpływ zmiennej na saldo migracji w tej samej gminie,
- **pośredni ($\theta$):** wpływ zmiennej w gminach sąsiednich na saldo w danej gminie, czyli spillover,
- **całkowity:** suma efektu bezpośredniego i pośredniego.

Gdy efekty bezpośredni i pośredni mają przeciwne znaki, częściowo się znoszą i efekt całkowity traci istotność.

**Test Breuscha-Pagana** sprawdza heteroskedastyczność reszt.

## Regresja geograficznie ważona (GWR)

**GWR** szacuje osobną regresję dla każdej gminy, z wagami malejącymi wraz z odległością od niej. Dzięki temu współczynniki mogą się zmieniać w przestrzeni. Służy do eksploracyjnego badania, gdzie dany czynnik działa silniej, a gdzie słabiej.

**Pasmo (bandwidth)** to zasięg obserwacji używanych przy estymacji lokalnych współczynników. Im mniejsze pasmo, tym większe znaczenie ma najbliższe sąsiedztwo i tym silniej zróżnicowane są efekty. Duże pasmo wygładza efekty i tworzy bardziej globalny model.

- **Pasmo stałe (fixed)** to stały promień. Sprawdza się przy regularnie rozłożonych jednostkach.
- **Pasmo adaptacyjne (adaptive)** to stała liczba najbliższych jednostek. Lepiej działa przy nierównomiernie rozmieszczonych gminach. W pracy optymalne pasmo to 61 gmin.

**Walidacja krzyżowa (CV)** to kryterium wyboru pasma: każdą gminę przewiduje się modelem oszacowanym bez niej i wybiera pasmo z najmniejszym błędem.

**Jądro Gaussa** określa, jak szybko waga obserwacji maleje wraz z odległością.

**Model benchmarkowy** to globalny model MNK, z którym porównuje się GWR.

**Testy F(1), F(2), BFC99, BFC02** sprawdzają, czy GWR dopasowuje się istotnie lepiej niż globalny MNK. Hipoteza zerowa zakłada brak poprawy.

**Test F(3)** sprawdza, czy poszczególne współczynniki rzeczywiście zmieniają się w przestrzeni.

**Lokalny VIF** mierzy współliniowość zmiennych w każdej lokalnej regresji. Wartości poniżej 5 oznaczają, że współliniowość nie jest silna.

**Lokalne R²** pokazuje, jak dobrze model wyjaśnia saldo migracji w każdej gminie.

**Mapy współczynników β** pokazują znak i siłę efektu w gminach, w których parametr jest istotny. Gminy z nieistotnym współczynnikiem na poziomie 5% są szare.
