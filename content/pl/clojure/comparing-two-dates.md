---
title:                "Clojure: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być niezbędne w wielu programach. Może to ułatwić nam sprawdzanie kolejności wydarzeń, analizę danych lub określenie, czy dany termin jest przeszły czy przyszły.

## Jak to zrobić

Porównywanie dwóch dat w Clojure jest bardzo proste. Wykorzystujemy do tego funkcję `compare`, która zwraca wartość `-1`, `0` lub `1` w zależności od wyniku porównania. Dla przykładu, porównajmy datę dzisiejszą z datą urodzenia:

```Clojure
(def dzis (java.util.Date.))
(def urodz (java.util.Date. 1990 10 24))
(compare dzis urodz)
```

Wyjście powyższego kodu to `-1`, co oznacza, że data urodzenia jest wcześniejsza od dzisiejszej daty.

Możemy również porównywać daty, które nie są typu `java.util.Date`, na przykład daty zapisane jako ciągi znaków. Wtedy ważne jest, aby przekonwertować je na ten typ przed porównaniem, np. używając funkcji `strptime` z biblioteki `clj-time`.

## Głębsza analiza

Funkcja `compare` jest jednym ze sposobów porównywania dat w Clojure, jednak istnieje też wiele innych opcji. Możemy na przykład wykorzystać funkcję `<` lub `>`, która zwraca wartość logiczną `true` lub `false`. Możliwe jest również wykorzystanie specjalnego obiektu `chrono / LocalDate`, który pozwala na bardziej precyzyjne porównywanie dat, uwzględniając różnice w różnych strefach czasowych i kalendarzach.

Ważne jest również pamiętanie o różnicach w dokładności porównywania. Domyślnie funkcja `compare` porównuje daty z dokładnością do milisekund, jednak w niektórych przypadkach może to być niepożądane. W takiej sytuacji warto skorzystać z innych funkcji lub sposób precyzowania dat przed porównaniem.

## Zobacz również

- [Dokumentacja Clojure o porównywaniu dat](https://clojuredocs.org/clojure.core/compare)
- [Biblioteka clj-time](https://github.com/clj-time/clj-time)
- [Dokumentacja Javy o typie Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)