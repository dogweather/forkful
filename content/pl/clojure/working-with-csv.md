---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obróbka plików CSV (Comma-Separated Values) polega na manipulowaniu danymi zapisanymi w postaci tabelarycznej, gdzie każdy wiersz to rekord, a kolumny oddzielone są przecinkami. Programiści robią to, by łatwo importować, eksportować oraz przetwarzać dane z i do systemów bazodanowych, arkuszy kalkulacyjnych i aplikacji.

## Jak to zrobić:
```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

; Czytanie pliku CSV
(with-open [reader (io/reader "dane.csv")]
  (let [parsed-csv (csv/read-csv reader)]
    (println parsed-csv)))

; Zapis do pliku CSV
(let [data [["id" "name" "age"]
            ["1" "Jan Kowalski" "28"]
            ["2" "Ewa Nowak" "35"]]]
  (with-open [writer (io/writer "wyjście.csv")]
    (csv/write-csv writer data)))

; Wynik:
; [["id" "name" "age"]
;  ["1" "Jan Kowalski" "28"]
;  ["2" "Ewa Nowak" "35"]]
```

## Dogłębniejsze informacje:
CSV to prosty format wymiany danych, który istnieje od wczesnych lat informatyki. Alternatywy dla CSV obejmują JSON, XML oraz formaty bazodanowe jak SQL. Implementacja obsługi CSV w Clojure polega głównie na wykorzystaniu biblioteki `clojure.data.csv`, która umożliwia efektywne czytanie i pisanie tych plików. O ile samo czytanie i pisanie jest proste, to trzeba pamiętać o poprawnym kodowaniu znaków czy obsłudze cudzysłowów i przecinków w danych.

## Zobacz także:
- Dokumentacja `clojure.data.csv`: https://clojure.github.io/data.csv/
- Przykłady i tutoriale CSV w Clojure: https://www.clojure-toolbox.com/
- Specyfikacja formatu CSV: https://tools.ietf.org/html/rfc4180
