---
title:                "Clojure: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym artykule dowiesz się, dlaczego warto używać funkcji do wyodrębniania podciągów w języku Clojure. Poznasz także sposoby, aby to zrobić oraz głębsze informacje na ten temat.

## Jak to zrobić

Do wyodrębniania podciągów w języku Clojure można użyć funkcji `subseq` lub `substring`. Oba te funkcje przyjmują jako argumenty oryginalny ciąg znaków oraz indeksy, które określają, które części ciągu chcemy wyodrębnić.

```Clojure
(def s "Hello world")

(subseq s 0 5)
; Output: Hello

(substring s 6)
; Output: world
```

Pamiętaj, że indeksy zaczynają się od zera. Możesz również wykorzystać ujemne indeksy, aby wyodrębnić podciągi od końca ciągu.

```Clojure
(subseq s 0 -1)
; Output: Hello worl

(substring s 0 -1)
; Output: Hello world
```

Funkcja `subseq` zwraca Ciąg znaków, podczas gdy funkcja `substring` zwraca Ciąg znaków. Oznacza to, że wynik z `subseq` można przekazać do innych funkcji, które przyjmują Ciągi znaków jako argumenty.

Jeśli chcesz wyodrębnić podciąg z jednego znaku, możesz użyć funkcji `nth`.

```Clojure
(nth "abcde" 2)
; Output: c
```

Możesz również użyć funkcji `join` w połączeniu z `subseq` lub `substring`, aby połączyć wyodrębnione podciągi w jeden Ciąg znaków.

## Głębsze informacje

Podczas wyodrębniania podciągów, warto pamiętać, że indeksy są włączane do wyniku. Oznacza to, że podany ostatni indeks jest włączony w wyodrębniony podciąg.

Możesz również wykorzystać dostępne funkcje `reverse` i `join` do odwrócenia i ponownego połączenia Ciągów znaków.

## Zobacz także

- Oficjalna dokumentacja Clojure dla funkcji `subseq`: https://clojuredocs.org/clojure.core/subseq
- Oficjalna dokumentacja Clojure dla funkcji `substring`: https://clojuredocs.org/clojure.core/substring
- Poradnik na temat wyodrębniania podciągów w języku Clojure: https://www.braveclojure.com/strings/