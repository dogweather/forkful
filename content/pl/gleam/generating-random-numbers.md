---
title:                "Generowanie losowych liczb"
html_title:           "Gleam: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cześć programiści!

W dzisiejszym artykule dowiesz się, czym jest generowanie losowych liczb w programowaniu i dlaczego jest to ważne dla naszej pracy. Będziemy również patrzeć na praktyczne przykłady kodu w języku Gleam oraz zagłębiać się nieco w historię i alternatywy tego procesu.

## Co i dlaczego?

Generowanie losowych liczb jest procesem, w którym komputer tworzy liczby w sposób losowy. Dzieje się tak za pomocą algorytmów, które wykorzystują takie aspekty, jak bieżący czas, stan pamięci lub inne dane, aby stworzyć pseudolosowe liczby. Programiści często wykorzystują ten proces do testowania swoich aplikacji lub do symulacji różnych prób i scenariuszy.

## Jak to zrobić?

Mając wyjaśnione czym jest generowanie losowych liczb, przejdźmy teraz do praktycznych kodów w języku Gleam. Oto przykładowy kod, który wygeneruje losową liczbę od 1 do 10:

```
Gleam.random.int(1, 10)
```

Jeśli chcesz wygenerować tylko jedną liczbę, można użyć funkcji `Gleam.random.int_uniform()` zamiast `Gleam.random.int()`. Poniżej przedstawiamy również przykład wygenerowania 10 losowych liczb w zakresie od 50 do 100:

```
for _ in List.range(0, 9) {
  Gleam.random.int_uniform(50, 100)
}
```

Aby sprawdzić wynik w konsoli lub terminalu, możesz użyć funkcji `IO.inspect()`:

```
IO.inspect(Gleam.random.int(1, 10))
```

Wyświetli to w konsoli wynik takiej operacji jak np.:

```
4
```

## Wnikliwa analiza

Generowanie losowych liczb jest procesem, który wywodzi się jeszcze z czasów komputerów analogowych. W dzisiejszych czasach jest to jednak nieodłączna część programowania, szczególnie w dziedzinach takich jak gry komputerowe, symulacje czy kryptografia. Istnieje wiele alternatywnych metod generowania losowych liczb, takich jak użycie generatora liczb pseudolosowych lub wykorzystanie czujnika losowego zewnętrznego. W języku Gleam generowanie losowych liczb jest możliwe dzięki bibliotece `gleam_rand`.

## Zobacz również

- Dokumentacja języka Gleam: https://gleam.run/
- Biblioteka do generowania losowych liczb w Gleam: https://github.com/gleam-lang/gleam_rand