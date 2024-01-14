---
title:    "Clojure: Porównywanie dwóch dat"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być niezbędnym zadaniem w programowaniu, szczególnie jeśli pracujesz z różnymi zapisami dat lub potrzebujesz porównać datę z jakimś warunkiem, na przykład sprawdzając czy dany dzień jest przed lub po wskazanym dniem.

## Jak to zrobić

Do porównywania dwóch dat w Clojure można skorzystać z kilku wbudowanych funkcji. Pierwszą z nich jest `compare`, która porównuje dwie daty i zwraca wartość całkowitą. Wynik jest równy 0, jeśli daty są równe, wartość ujemna jeśli pierwsza data jest wcześniejsza, a wartość dodatnia jeśli pierwsza data jest późniejsza.

```Clojure
(compare "2021-06-20" "2021-06-25")
; wynik: -5

(compare "2021-06-20" "2021-06-20")
; wynik: 0

(compare "2021-06-30" "2021-06-25")
; wynik: 5
```

Inną przydatną funkcją jest `before?`, która zwraca `true`, jeśli pierwsza data jest wcześniejsza niż druga, lub `false` w przeciwnym przypadku.

```Clojure
(before? "2021-06-20" "2021-06-25")
; wynik: true

(before? "2021-06-20" "2021-06-20")
; wynik: false
```

Można również wykorzystać operatory porównawcze takie jak `<` i `>`, które również działają na datach.

```Clojure
(< "2021-06-20" "2021-06-25")
; wynik: true

(> "2021-06-20" "2021-06-25")
; wynik: false
```

## Pogłębiona analiza

Jeśli chcesz dokładniej poznać jak działa porównywanie dat w Clojure, warto zapoznać się ze szczegółami dotyczącymi funkcji `compare` oraz sposobu, w jaki Clojure traktuje daty. Warto również pamiętać, że porównywanie dat może różnić się w zależności od użytego formatu daty, dlatego zawsze warto sprawdzić jaki format będzie odpowiedni dla potrzeb Twojego projektu.

## Zobacz również

- Oficjalna dokumentacja Clojure dotycząca funkcji `compare`: https://clojuredocs.org/clojure.core/compare
- Wyjaśnienie działania dat w Clojure: https://www.baeldung.com/clojure-date-time