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

## Co & Dlaczego? 
Generowanie losowych liczb to proces, w którym komputer tworzy liczby bez określonego porządku lub wzoru. Programiści często używają generatorów liczb losowych do symulacji, testowania i pseudorandomizacji danych w swoich aplikacjach.

## Jak wykonać: 
Możemy użyć funkcji `rand` w Clojure, aby wygenerować pojedynczą losową liczbę typu float z przedziału od 0 (włącznie) do 1 (wyłącznie). Możemy również użyć funkcji `rand-int`, aby wygenerować pojedynczą losową liczbę całkowitą z określonego przedziału, na przykład `(rand-int 10)` wygeneruje liczbę z przedziału od 0 do 9. Aby wygenerować listę losowych liczb, możemy wykorzystać funkcję `repeatedly` w połączeniu z `rand` lub `rand-int`. 

```Clojure
(rand) ; -> 0.7484451465112938 
(rand-int 100) ; -> 57
(doall (repeatedly 5 #(rand-int 50))) ; -> (7 18 34 21 42)
```

## Głębokie Nurty:
Generowanie losowych liczb ma długą historię, sięgającą XVIII wieku wraz z rozwojem matematyki prawdopodobieństwa. W dzisiejszych czasach istnieje wiele alternatywnych metod generowania losowych liczb, takich jak metody oparte na sprzęcie lub algorytmach kryptograficznych. Implementacja generatorów liczb losowych w Clojure korzysta z generatora Mersenne Twister, który jest szeroko stosowany w branży gier komputerowych. 

## Zobacz także: 
- Dokumentacja Clojure dotycząca generowania liczb losowych: https://clojuredocs.org/clojure.core/rand
- Oficjalna strona Mersenne Twister: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html