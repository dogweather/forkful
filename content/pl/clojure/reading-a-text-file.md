---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Clojure: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz pracować jako programista w Clojure, czy to jako hobbysta czy zawodowiec, musisz umieć czytać i manipulować plikami tekstowymi. Pliki tekstowe są podstawowym sposobem przechowywania danych i konieczne jest zrozumienie, jak je odczytywać w Clojure.

## Jak to zrobić

```Clojure
; Otwarcie pliku tekstowego przez nazwę (zwraca strumień)

(def file (clojure.java.io/reader "plik.txt"))

; Otwarcie pliku przez ścieżkę (zwraca strumień)

(def file (clojure.java.io/reader "ścieżka/do/pliku.txt"))

; Odczytanie zawartości pliku w całości (zwraca ciąg)

(slurp file)

; Odczytanie zawartości pliku wiersz po wierszu (zwraca sekwencję wierszy)

(line-seq file)

; Zamykanie strumienia pliku

(.close file)

```

Przykład:

Jeśli mamy plik o nazwie "dane.txt" z następującą zawartością:

```
Imię: Anna
Wiek: 30
Miasto: Kraków
```

To możemy go odczytać i przypisać wartości do zmiennych w ten sposób:

```Clojure
(def file (clojure.java.io/reader "dane.txt"))
(def imie (nth (line-seq file) 0))
(def wiek (nth (line-seq file) 1))
(def miasto (nth (line-seq file) 2))

; Wyświetlenie wyników

(imie) ; "Imię: Anna"
(wiek) ; "Wiek: 30"
(miasto) ; "Miasto: Kraków"

(.close file)
```

## Głębsza analiza

Funkcja `(clojure.java.io/reader)` zwraca strumień, który jest później wykorzystywany przez funkcje `slurp` i `line-seq`. Strumienie te są automatycznie zamykane, gdy funkcja `(.close)` jest wywołana. Pamiętaj, aby zawsze zamknąć strumień po zakończeniu pracy - jest to ważne dla wydajności i bezpieczeństwa.

## Zobacz również

- [Dokumentacja Clojure do funkcji `clojure.java.io/reader`](https://clojuredocs.org/clojure.java.io/reader)
- [Poradnik do manipulacji plikami w Clojure](https://techbeacon.com/how-manipulate-files-clojure)