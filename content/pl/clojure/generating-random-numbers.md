---
title:                "Clojure: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezbędnym elementem programowania w wielu językach, w tym w Clojure. Losowe liczby mogą być wykorzystywane do symulacji, generowania unikalnych identyfikatorów, szyfrowania danych oraz wielu innych zastosowań. W tym artykule dowiecie się, jak wygenerować losowe liczby w Clojure i jak można je wykorzystać.

## Jak to zrobić

Aby wygenerować losową liczbę w Clojure, można użyć wbudowanej funkcji "rand". Przykładowo, aby wylosować liczbę z przedziału od 1 do 10, należy użyć poniższego kodu:

```Clojure
(rand-int 10) ; zwraca losową liczbę całkowitą od 0 do 9
```

Można także wygenerować losową liczbę zmiennoprzecinkową używając funkcji "rand".

```Clojure
(rand) ; zwraca losową liczbę z przedziału od 0 do 1
```

Jeśli potrzebujemy większej precyzji lub chcemy wygenerować liczbę z innego przedziału, można użyć funkcji "rand-nth". Przykładowo, jeśli chcemy wylosować liczbę z przedziału od 50 do 100, możemy użyć poniższego kodu:

```Clojure
(rand-nth (range 50 101)) ; zwraca losową liczbę z przedziału od 50 do 100
```

Można także wylosować element z listy używając funkcji "rand-nth". Przykładowo:

```Clojure
(rand-nth ['jabłko 'banan 'truskawka]) ; zwraca losowy element z listy
```

## Mocne strony

Clojure ma wiele wbudowanych funkcji do generowania losowych liczb, co czyni ją wygodnym wyborem dla programistów. Dodatkowo, Clojure jest językiem funkcyjnym, co oznacza, że generowanie losowych liczb jest deterministyczne i zawsze zwraca ten sam wynik dla tego samego wejścia. Dzięki temu, można dokładnie kontrolować, jakie liczby zostaną wygenerowane.

## Wnikliwe zagłębienie

Clojure korzysta z algorytmu Mersenne Twister do generowania losowych liczb. Jest to algorytm o wysokim stopniu losowości i szybkości, który jest również szeroko stosowany w innych językach programowania.

Warto także pamiętać, że generowanie względnie "losowych" liczb w komputerach nie jest możliwe. Algorytmy używane w językach programowania są jedynie sposobem na symulowanie losowości, a ich wyniki są zawsze przewidywalne.

## Zobacz także

- [Dokumentacja Clojure o generowaniu liczb losowych](https://clojure.org/reference/numbers#_random_numbers)
- [Algorytm Mersenne Twister](https://pl.wikipedia.org/wiki/Mersenne_Twister)