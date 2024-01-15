---
title:                "Wycinanie podsłów"
html_title:           "Clojure: Wycinanie podsłów"
simple_title:         "Wycinanie podsłów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyciąganie podciągów jest często używane w programowaniu do manipulowania tekstami. Jest to przydatna umiejętność, która pozwala na wydobycie konkretnych informacji z dłuższego tekstu lub na zmianę jego formatowania.

## Sposób

Wyciągnięcie podciągu w Clojure jest bardzo proste. Możemy to zrobić za pomocą funkcji `subs` lub `substring`. Przykładowy kod wyglądałby następująco:

```Clojure
(def tekst "To jest przykładowy tekst.")
(subs tekst 3) ; wydobycie podciągu od indeksu 3 do końca tekstu
; output: "jest przykładowy tekst."

(subs tekst 4 10) ; wydobycie podciągu od indeksu 4 do 9 (indeks ostatniego znaku nie jest włączony)
; output: "jest p"

(substring tekst 3 6) ; wydobycie podciągu od indeksu 3 do 5
; output: "jest"
```

Możemy również użyć funkcji `split` do podziału tekstu na wiele podciągów, lub `replace` do zamiany części tekstu na inny podciąg.

## Głębsza analiza

W Clojure, podciągi są przechowywane jako sekwencje danych, co oznacza, że mogą być łatwo przekształcane i manipulowane. Funkcje `subs` i `substring` przyjmują argumenty w postaci indeksów oraz opcjonalnie również kierunku przetwarzania (domyślnie od lewej do prawej). Możemy także użyć negatywnych indeksów, aby odwołać się do końca tekstu.

Podczas manipulowania tekstami, ważne jest również pamiętać o używaniu odpowiednich funkcji do zarządzania kodowaniem znaków. Funkcje `subs` i `substring` działają poprawnie tylko dla niskopoziomowych kodowań, takich jak ASCII czy UTF-8.

## Zobacz również

- Dokumentacja Clojure na temat funkcji `subs`: https://clojuredocs.org/clojure.core/subs
- Dokumentacja Clojure na temat funkcji `substring`: https://clojuredocs.org/clojure.string/substring
- Przykłady użycia funkcji `subs` i `substring`: https://www.techcrashcourse.com/2021/03/clojure-substring.html