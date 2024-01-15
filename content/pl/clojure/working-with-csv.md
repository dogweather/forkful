---
title:                "Praca z plikami CSV"
html_title:           "Clojure: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z dużymi zbiorami danych, prawdopodobnie miałaś lub będziesz miała do czynienia z plikami CSV. CSV (Comma-Separated Values) jest to format pliku, który pozwala przechowywać dane w postaci tabelarycznej. Dzięki swojej prostocie i powszechności, jest to jeden z najpopularniejszych formatów używanych w projektach związanych z analizą danych.

## Jak to zrobić

Aby przetwarzać pliki CSV w języku Clojure, możesz skorzystać z biblioteki core/csv, która jest dostarczana razem z Clojure. Pierwszym krokiem jest zaimportowanie tej biblioteki poprzez dodanie poniższego kodu na początku Twojego pliku:

```Clojure
(require '[clojure.core.csv :as csv])
```

Następnie, aby odczytać dane z pliku csv, możesz skorzystać z funkcji `read-csv`, która przyjmuje jeden argument - ścieżkę do pliku csv.

```Clojure
(def csv-data (csv/read-csv "sciezka/do/pliku.csv"))
```

Powyższy kod odczyta dane z pliku csv i zwróci je w postaci listy list. Każdy wiersz pliku odpowiada jednemu wierszowi w liście, a każda kolumna odpowiada jednemu elementowi w wierszu. Przykładowo, jeśli plik csv zawiera dane o pracownikach - imię, nazwisko i pensję - to możemy odczytać je w ten sposób:

```Clojure
(def csv-data [["Anna" "Kowalska" 5000]
               ["Jan" "Nowak" 6000]])
```

Aby zapisać dane do pliku CSV, możesz wykorzystać funkcję `write-csv`, która przyjmuje dwa argumenty - ścieżkę do pliku i dane w formie listy list.

```Clojure
(csv/write-csv "sciezka/do/pliku.csv" csv-data)
```

Jeśli chcesz zmienić separator w pliku CSV (domyślnie jest to przecinek), możesz wykorzystać argument `:separator` w funkcji `write-csv`.

## Gleboki zanurzenie

Aby bardziej efektywnie poradzić sobie z przetwarzaniem plików CSV, warto zapoznać się z innymi bibliotekami, takimi jak clojure-csv czy La Clojure, które oferują dodatkowe funkcjonalności, np. obsługę nagłówków w pliku CSV czy parsowanie zaawansowanych typów danych.

W języku Clojure istnieje również biblioteka clojure.data.csv, która udostępnia zestaw funkcji do konwersji danych z pliku CSV na inne struktury danych, takie jak m.in. mapy czy sekwencje.

Jeśli chcesz przetwarzać duże pliki CSV, warto również zapoznać się z biblioteką clojure-csv-clj, która jest zoptymalizowana pod kątem wydajności i wykorzystuje techniki podziału danych na mniejsze części oraz równoległego przetwarzania.

Niezależnie od tego, jakiej biblioteki użyjesz, ważne jest, aby wybrać tę, która najlepiej odpowiada Twoim potrzebom i charakterystyce przetwarzanych danych.

## Zobacz również

- [Dokumentacja biblioteki core/csv](https://clojuredocs.org/clojure.core/csv)
- [Dokumentacja biblioteki clojure-csv](https://github.com/clojure/data.csv)
- [Dokumentacja biblioteki clojure-csv-clj](https://github.com/AlexBaranosky/clojure-csv-clj)