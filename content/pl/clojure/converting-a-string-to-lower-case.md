---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana ciągu znaków na małe litery to operacja, która przekształca wszystkie duże litery na małe. Programiści robią to, aby ułatwić porównania i wyszukiwanie, czyli znormalizować dane tekstowe.

## Jak to zrobić:

Ze stringami w Clojure można pracować jak z sekwencjami znaków. Sprawdźmy to na przykładzie:
```Clojure
(apply str (map clojure.string/lower-case "Witaj, Świecie!"))
;; Output: "witaj, świecie!"
```
Ten kod bierze string "Witaj, Świecie!", mapuje przez wszystkie znaki funkcję `clojure.string/lower-case`, a następnie łączy wszystko z powrotem w string za pomocą `apply str`.

## Pogłębione informacje:

Funkcja zmieniająca duże litery na małe istnieje w wielu językach programowania, takich jak C, Java i Python, długo przed tym, jak pojawiła się w Clojure's. Alternatywą dla korzystania z `clojure.string/lower-case` jest napisanie własnej funkcji, która wykonuje tę samą operację. Jednak jest to zdecydowanie mniej efektywne.

Od strony implementacyjnej `lower-case` używa standardowych funkcji Javy do transformacji liter - tworzy obiekt `java.lang.String`, a następnie wywołuje na nim metodę `toLowerCase`.

## Zobacz także:

"Clojure przez przykład" - zestaw interaktywnych tutoriali prezentujących podstawowe koncepty języka Clojure: https://www.braveclojure.com/

"Dokumentacja API Clojure String" - dokładne wyjaśnienia i przykłady użycia funkcji przeprowadzających operacje na stringach: https://clojuredocs.org/clojure.string