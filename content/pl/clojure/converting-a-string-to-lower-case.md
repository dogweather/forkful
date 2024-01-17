---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Clojure: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

"Co & Dlaczego?"

Zamiana tekstu na małe litery oznacza sprowadzenie wszystkich liter w ciągu znaków do ich niskiej formy. Programiści często wykonują to jako część procesu przetwarzania i analizowania danych, co ułatwia porównywanie i wycinanie określonych elementów.

"Jak to zrobić:"

```Clojure
(def tekst "PRZYKŁADOWY TEKST")
(clojure.string/lower-case tekst)

; Output: "przykładowy tekst"
```

Możemy użyć funkcji `lower-case` z biblioteki `clojure.string` aby zamienić wszystkie litery w ciągu `tekst` na małe. Wstawiamy ciąg do funkcji jako argument, a następnie otrzymujemy tekst w małych literach jako wynik.

"Zagłębienie"

Zamiana tekstu na małe litery jest bardzo często wykonywana w programowaniu, ponieważ ułatwia to porównywanie i filtrowanie danych. Wcześniej, programiści musieli pisać własne funkcje do zamieniania liter na małe, ale teraz jest to wbudowana funkcja w większości języków programowania.

Alternatywą dla funkcji `lower-case` w Clojure może być funkcja `clojure.string/upper-case`, która przekształca wszystkie litery w ciągu na formę wielkich liter.

Implementacja funkcji `lower-case` w bibliotece `clojure.string` wykorzystuje narzędzia z biblioteki Java, dzięki czemu jest wydajna i nie wymaga od programisty pisania własnego kodu do konwersji liter.

"Zobacz też"

- Oficjalna dokumentacja języka Clojure: https://clojure.org/
- Dokumentacja biblioteki `clojure.string`: https://clojure.github.io/clojure/clojure.string-api.html