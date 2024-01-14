---
title:                "Clojure: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto przeczytać plik tekstowy?

Jeśli jesteś programistą Clojure i potrzebujesz przetworzyć plik tekstowy, to ten artykuł jest dla Ciebie! Dowiesz się, jak w prosty sposób dokonać odczytu pliku tekstowego w języku Clojure.

## Jak to zrobić?

Pierwszym krokiem będzie użycie funkcji `slurp`, która wczyta całą zawartość pliku jako pojedynczy string. Przykład:

```Clojure
(def file-content (slurp "plik.txt"))

;; Output: "To jest przykładowy plik tekstowy."
```

Jeśli chcesz przetworzyć zawartość pliku linia po linii, możesz skorzystać z funkcji `line-seq`, która zwraca sekwencję zawierającą kolejne linie z pliku. Przykład:

```Clojure
(def lines (line-seq (clojure.java.io/reader "plik.txt")))

;; Output: ("To jest przykładowy plik tekstowy.")
```

Jeśli chcesz użyc different parser, możesz skorzystać z biblioteki `clojure.data.csv` oraz funkcji `csv/read-csv`, aby przetworzyć plik CSV. Przykład:

```Clojure
(require '[clojure.data.csv :as csv])

(csv/read-csv (clojure.java.io/reader "plik.csv"))

;; Output: (["Imię" "Nazwisko" "Wiek"] ["Jan" "Kowalski" "30"] ["Anna" "Nowak" "25"] ["Tomasz" "Nowicki" "40"])
```

## Głębsze wgląd

Funkcję `slurp` oraz `line-seq` możesz wykorzystać także z dodatkowymi opcjami, takimi jak ustawienie kodowania, trybu czytania pliku, czy wczytanie pliku z poziomu zasobów katalogu bieżącego. Więcej informacji na temat tych funkcji znajdziesz w dokumentacji języka Clojure.

## Zobacz również

- [Dokumentacja języka Clojure](https://clojure.org/documentation)
- [Biblioteka clojure.data.csv](https://clojure.github.io/data.csv/)
- [Przykładowe pliki testowe](https://github.com/clojure/clojure/tree/master/test)

Dzięki tym prostym przykładom i wskazówkom, bez problemu będziesz w stanie odczytać plik tekstowy w języku Clojure. W razie potrzeby, możesz także skorzystać z innych funkcji i bibliotek, które ułatwią przetwarzanie danych z plików tekstowych.