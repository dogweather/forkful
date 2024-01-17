---
title:                "Praca z plikami csv"
html_title:           "Clojure: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?
Praca z plikami CSV jest często wykonywana przez programistów, ponieważ są one powszechnie używane do przechowywania i przetwarzania dużych zbiorów danych w formacie tabelarycznym. CSV (ang. Comma-Separated Values) to prosty format przechowywania danych, w którym wartości są oddzielane przecinkami. Umożliwia to łatwy odczyt i zapis danych przez komputery.

## Jak to zrobić:
```Clojure
(require '[clojure-csv.core :as csv])

;; Odczytywanie pliku CSV
(with-open [csv-file (clojure.java.io/reader "dane.csv")]
  (doall (csv/read-csv csv-file)))

;; Zapisywanie danych do pliku CSV
(with-open [csv-file (clojure.java.io/writer "dane.csv")]
  (csv/write-csv csv-file [["Imię" "Nazwisko" "Wiek"]
                           ["Anna" "Kowalska" "26"]
                           ["Jan" "Nowak" "32"]]))
```
Powyższe przykłady wykorzystują bibliotekę `clojure-csv` do odczytu i zapisu plików CSV. Funkcja `read-csv` odczytuje dane z pliku w formacie tabelarycznym, a `write-csv` zapisuje dane w odpowiednim formacie. Należy zwrócić uwagę na użycie funkcji `doall` w celu wymuszenia wykonania operacji odczytu pliku.

## Głębsza analiza:
1. CSV został stworzony w latach 70. jako prosty format do przechowywania danych w arkuszach kalkulacyjnych. Obecnie jest szeroko wykorzystywany w wielu dziedzinach, w tym w programowaniu.
2. Alternatywne sposoby przechowywania danych tabelarycznych to m.in. XML i JSON.
3. Przy pracy z większymi zbiorami danych, należy uważać na wydajność operacji odczytu i zapisu plików CSV. Można to osiągnąć poprzez wykorzystanie bibliotek zoptymalizowanych pod kątem tego formatu, takich jak `clojure-csv`.

## Zobacz także:
1. Oficjalna dokumentacja Clojure dotycząca pracy z CSV: https://clojuredocs.org/clojure-csv.
2. Poradnik na temat przetwarzania plików CSV w Clojure: https://www.braveclojure.com/reading-and-writing-csv/.
3. Biblioteka `clojure.data.csv` w standardowej bibliotece Clojure: https://clojure.github.io/clojure/clojure.data.csv-api.html.