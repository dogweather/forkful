---
title:    "Clojure: Generowanie losowych liczb"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często potrzebuje generować losowe liczby w swoim kodzie. Może to być potrzebne do testowania, symulacji lub prostych gier. W tym blogu dowiesz się, jak wygenerować losowe liczby w języku Clojure i dlaczego jest to ważna umiejętność dla programisty.

## Jak to zrobić

Aby wygenerować losową liczbę w języku Clojure, możemy skorzystać z funkcji ```rand```. Oto przykładowy kod:

```Clojure
(rand) ; wygeneruje losową liczbę z zakresu od 0 do 1
(rand 10) ; wygeneruje losową liczbę całkowitą z zakresu od 0 do 9
(rand-int 100) ; wygeneruje losową liczbę całkowitą z zakresu od 0 do 99
```

Możemy również użyć funkcji ```rand-nth```, aby wylosować jedną wartość z kolekcji lub funkcji ```shuffle```, aby wylosować wszystkie elementy kolekcji w losowej kolejności. Oto przykładowy kod:

```Clojure
(rand-nth ["jabłko" "gruszka" "banan"]) ; wylosuje losowo jedną z trzech wartości: jabłko, gruszka lub banan
(shuffle [1 2 3 4]) ; wylosuje wartości z kolekcji w losowej kolejności, np. (4 1 3 2)
```

Możemy również ustawić ziarno losowości za pomocą funkcji ```set!.seed```, aby uzyskać powtarzalne wyniki. Oto przykładowy kod:

```Clojure
(set! seed 42) ; ustawienie ziarna na 42
(rand) ; wygeneruje zawsze tę samą losową liczbę
```

## Deep Dive

Za każdym razem, gdy wywołujemy funkcję ```rand```, otrzymujemy różną losową liczbę. Ale czy jest ona naprawdę losowa? Otóż nie do końca. Język Clojure używa generatora liczb pseudolosowych, dzięki czemu wyniki są przewidywalne w pewnym stopniu. Jednak zazwyczaj nie jest to problem, jeśli chodzi o prostsze zastosowania. Jeśli jednak potrzebujesz bardziej precyzyjnych i losowych wyników, istnieją biblioteki do generowania liczb losowych w języku Clojure.

## Zobacz również

- [Oficjalna dokumentacja Clojure do funkcji rand](https://clojure.org/reference/other_functions#rand)
- [Funkcje generujące losowe liczby w bibliotece clojure.core](https://clojure.org/reference/other_functions#rand-family)
- [Klasy odwzorowujące zapotrzebowanie na wybieranie losowości w języku Clojure](https://github.com/dakrone/cheshire-clojure)
- [Biblioteka do generowania złożonych liczb losowych w języku Clojure](https://github.com/gfredericks/random)