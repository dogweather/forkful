---
date: 2024-01-20 17:54:00.831350-07:00
description: "How to: (Jak to zrobi\u0107:) Czytanie pliku w Clojure mo\u017Ce by\u0107\
  \ proste jak bu\u0142ka z mas\u0142em. Oto przyk\u0142ady."
lastmod: '2024-04-05T21:53:36.458442-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Czytanie pliku w Clojure mo\u017Ce by\u0107 proste\
  \ jak bu\u0142ka z mas\u0142em."
title: Odczytywanie pliku tekstowego
weight: 22
---

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
