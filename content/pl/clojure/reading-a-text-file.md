---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:54:00.831350-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i dlaczego?)

Czytanie pliku tekstowego to pobieranie jego zawartości do pamięci programu. Programiści robią to, by manipulować danymi, wyświetlić je użytkownikowi lub dokonać jakiejś analizy.

## How to:
(Jak to zrobić:)

Czytanie pliku w Clojure może być proste jak bułka z masłem. Oto przykłady:

```Clojure
;; Czytanie całego pliku na raz
(slurp "ścieżka/do/pliku.txt")

;; Czytanie linia po linii
(with-open [r (clojure.java.io/reader "ścieżka/do/pliku.txt")]
  (doseq [line (line-seq r)]
    (println line)))
```
Jeśli masz plik `przyklad.txt` z treścią "Cześć, Clojure!", to wynik będzie:

```Clojure
"Cześć, Clojure!"
```

## Deep Dive:
(Głębsze zanurzenie:)

Wczesne lata programowania opierały się na czytaniu danych z kart perforowanych i taśm magnetycznych. Dzisiaj pliki tekstowe to podstawowa forma wymiany i przechowywania danych.

Alternatywy dla `slurp` i `line-seq` obejmują stosowanie niskopoziomowego Java API, używając `java.io.BufferedReader` dla większej kontroli nad procesem czytania.

Szczegóły implementacji: `slurp` ładnie obsługuje małe pliki, ale nie nadaje się do dużych, bo zużywa zbyt wiele pamięci. Dla dużych plików lepiej używać `line-seq` w `with-open`, co zapewnia stopniowe czytanie i automatyczne zamknięcie zasobów.

## See Also:
(Zobacz również:)

- Clojure Documentation: https://clojure.org/guides/learn/functions#_file_io
- Clojure for the Brave and True (Rozdział o I/O): https://www.braveclojure.com/io/
- Clojure from the ground up: https://aphyr.com/posts/301-clojure-from-the-ground-up-welcome
