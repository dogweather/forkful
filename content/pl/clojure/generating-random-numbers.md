---
title:                "Generowanie losowych liczb"
html_title:           "Clojure: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważną częścią wielu programów i aplikacji. Pozwala to na symulowanie losowych rezultatów, testowanie funkcji i algorytmów oraz w wielu innych zastosowaniach, w których potrzebne są wartości bez ustalonego wzoru.

## Jak to zrobić

```Clojure
; Wykorzystanie funkcji rand z biblioteki clojure.core
(clojure.core/rand)

; Generowanie pojedynczej losowej liczby w zakresie od 0 do 10
(rand 10)

; Generowanie listy 10 losowych liczb w zakresie od 0 do 100
(repeat 10 #(rand 100))

; Generowanie losowego indexu dla listy
(nth (shuffle ["jabłko" "banan" "ananas" "gruszka"]) (rand 4))
```

Sample output:

6
(43 92 76 11 25 98 72 64 32 5)
gruszka

## Głębsza analiza

W Clojure istnieje wiele sposobów na generowanie losowych liczb. Oprócz już przykładowej funkcji rand, istnieją także funkcje rand-int i rand-nth, które pozwalają na generowanie losowych liczb całkowitych oraz losowych elementów z listy. Istnieje również możliwość ustawienia ziarna (seed), dzięki czemu możliwe jest uzyskanie powtarzalnych rezultatów z losowych generacji.

## Zobacz także
- [Dokumentacja Clojure do funkcji rand](https://clojuredocs.org/clojure.core/rand)
- [Inne sposoby na generowanie losowych liczb w Clojure](https://stackoverflow.com/questions/38515048/how-to-generate-random-number-in-clojure)
- [Przydatna aplikacja do testowania funkcji i algorytmów z wykorzystaniem losowych liczb](https://leetcode.com/)