---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:21.837610-07:00
description: "Jak to zrobi\u0107: Clojure nie ma wbudowanego parsowania CSV w swojej\
  \ standardowej bibliotece, ale mo\u017Cna u\u017Cy\u0107 biblioteki `clojure.data.csv`\
  \ do tego celu.\u2026"
lastmod: '2024-03-13T22:44:35.019680-06:00'
model: gpt-4-0125-preview
summary: "Clojure nie ma wbudowanego parsowania CSV w swojej standardowej bibliotece,\
  \ ale mo\u017Cna u\u017Cy\u0107 biblioteki `clojure.data.csv` do tego celu."
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:


### Czytanie pliku CSV
Clojure nie ma wbudowanego parsowania CSV w swojej standardowej bibliotece, ale można użyć biblioteki `clojure.data.csv` do tego celu. Najpierw dodaj bibliotekę do zależności Twojego projektu.

W swoim `project.clj` dodaj następującą zależność:
```clojure
[clojure.data.csv "1.0.0"]
```
Aby przeczytać plik CSV i wydrukować każdy wiersz:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "ścieżka/do/twój_plik.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Spowoduje to wypisanie każdego wiersza pliku CSV jako wektora Clojure.

### Pisanie do pliku CSV
Do zapisywania danych do pliku CSV można użyć tej samej biblioteki `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "ścieżka/do/plik_wyjściowy.csv")]
    (csv/write-csv writer data)))
```
To tworzy lub nadpisuje `plik_wyjściowy.csv`, wypełniając go określonymi danymi.

### Użycie biblioteki innej firmy: `clojure.data.csv`
Choć `clojure.data.csv` jest prawdopodobnie najprostszą biblioteką do obsługi CSV w Clojure, do bardziej złożonych zadań, takich jak obsługa CSV z specjalnymi znakami czy nietypowymi delimiterami, można rozważyć dodatkowe opcje w ekosystemie, a nawet rozważyć interop z Javą z bibliotekami takimi jak Apache Commons CSV. Jednak dla większości standardowych zadań przetwarzania CSV w Clojure, `clojure.data.csv` oferuje prosty i skuteczny zestaw narzędzi.
