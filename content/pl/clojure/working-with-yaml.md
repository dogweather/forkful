---
title:                "Clojure: Praca z formatem yaml"
simple_title:         "Praca z formatem yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pracować z YAML w programowaniu w języku Clojure? Ponieważ YAML jest językiem znanym przez wielu programistów jako intuicyjny sposób kodowania struktury danych. Pozwala on na czytelność, prostotę i elastyczność w naszym kodzie.

## Jak

### 1. Tworzenie pliku YAML

Aby rozpocząć pracę z YAML, musimy najpierw utworzyć plik YAML. W tym celu będziemy korzystać z biblioteki YAML dla Clojure - `clj-yaml`. Następnie wykonujemy polecenie `require` aby zaimportować bibliotekę:

```Clojure
(require '[clj-yaml.core :as yaml])
```

Teraz możemy utworzyć prosty plik YAML zawierający listę pięciu elementów:

```Clojure
(def sample-yaml
  "colors:
    - red
    - blue
    - green
    - yellow
    - purple")
```

### 2. Przetwarzanie pliku YAML

Aby przetworzyć nasz plik YAML, użyjemy funkcji `load`, która załaduje zawartość pliku i przekonwertuje ją na odpowiednią strukturę danych w Clojure:

```Clojure
(def yaml-data (yaml/load sample-yaml))
```

Wynikiem będzie mapa Clojure zawierająca klucz `colors` oraz listę pięciu kolorów.

### 3. Dostęp do danych

Możemy uzyskać dostęp do danych w naszym pliku YAML poprzez odwołanie się do kluczy i indeksów w mapie Clojure:

```Clojure
(:colors yaml-data) ; zwróci listę kolorów
(:colors yaml-data 0) ; zwróci pierwszy kolor (czerwony)
```

## Deep Dive

Warto zauważyć, że używając biblioteki `clj-yaml`, możemy również wykorzystać wszystkie funkcje Clojure przy przetwarzaniu danych. Na przykład, jeśli chcemy dodać nowy kolor do naszego pliku YAML, możemy użyć funkcji `conj`:

```Clojure
(update yaml-data :colors conj "orange") ; doda 'orange' na koniec listy kolorów
```

## Zobacz także

- [Dokumentacja biblioteki YAML dla Clojure](https://github.com/clj-yaml/clj-yaml)
- [Tutorial dotyczący pracy z YAML w Clojure](https://practicalli.github.io/clojure-data/read/yaml.html)
- [Przykładowy projekt wykorzystujący YAML i Clojure](https://github.com/bbatsov/clojure-style-guide/blob/master/guide_pl_PL.md)