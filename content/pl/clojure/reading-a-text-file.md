---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces, w którym program odczytuje dane zawarte w pliku tekstowym. Programiści robią to, aby manipulować danymi, zrozumieć strukturę pliku, a nawet debugować błędy.

## Jak to zrobić:

Poniżej znajduje się przykład użycia Clojure do odczytania pliku tekstowego:

```clojure
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "ścieżka/do/pliku.txt")]
(let [lines (line-seq reader)]
  (doseq [line lines]
    (println line))))
```

Wykonanie powyższego kodu spowoduje wyświetlenie zawartości pliku `plik.txt`.

## Głębszy wgląd:

(1) **Kontekst historyczny:** Początki odczytu plików tekstowych w Clojure są ściśle związane z powstaniem samego języka, który zawsze był silnie związany z Javą. Dzięki temu Clojure oferuje doskonałe wsparcie dla operacji na plikach, w tym odczytu plików tekstowych.

(2) **Alternatywy:** Istnieje wiele różnych bibliotek i technik, które mogą być używane do odczytania plików tekstowych w Clojure, takie jak `clojure-csv` dla plików CSV.

(3) **Szczegóły implementacji:** W powyższym przykładzie użyliśmy `with-open`, aby otworzyć plik, `io/reader`, aby przetworzyć plik, `line-seq`, aby przechodzić przez każdą linię, a `println`, aby wyświetlić wszystkie linie.

## Zobacz też:

- [Clojure Docs - clojure.java.io/reader](https://clojuredocs.org/clojure.java.io/reader): Dokumentacja tej funkcji w Clojure.