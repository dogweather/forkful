---
title:                "Clojure: Tworzenie losowych liczb"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważną częścią programowania, ponieważ pozwala na tworzenie różnorodnych i losowych wyników, które są niezbędne w wielu aplikacjach, takich jak gry, symulacje czy testy jednostkowe.

## Jak to zrobić?

W Clojure istnieje wiele metod generowania liczb losowych. Jedną z najprostszych jest użycie funkcji `rand`, która zwraca losową liczbę z przedziału [0,1).

```Clojure
(rand) ; wyświetli na przykład: 0.30117199280951024
```

Możemy również określić własny przedział, podając odpowiednie argumenty do funkcji `rand`.

```Clojure
(rand 10) ; wygeneruje liczbę od 0 do 9, na przykład: 5.463217748465344
```

Inną opcją jest użycie funkcji `rand-int`, która zwraca losową liczbę całkowitą z podanego przedziału.

```Clojure
(rand-int 100) ; wygeneruje liczbę od 0 do 99, na przykład: 76
```

## Głębsza analiza

Clojure używa silnika Javy do generowania liczb losowych, co oznacza, że są one nieprzewidywalne i bardziej losowe niż w innych językach programowania. Istnieje również wiele innych funkcji i metod generowania liczb losowych w Clojure, takich jak `rand-nth`, `shuffle` czy `sample`, które można wykorzystać w zależności od potrzeb.

## Zobacz także

- Dokumentacja Clojure dotycząca generowania liczb losowych: https://clojuredocs.org/clojure.core/rand
- Wprowadzenie do programowania w języku Clojure: https://clojure.org/guides/getting_started
- Wideo o generowaniu liczb losowych w Clojure: https://www.youtube.com/watch?v=P5VuB9ldd34