---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Clojure: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie programów często wymaga używania aktualnej daty i czasu. Mogą być to różne dni lub godziny, na przykład kiedy dane zdarzenie miało miejsce lub kiedy trzeba coś zrobić. W języku Clojure jest wiele sposobów, aby uzyskać bieżącą datę i czas, a w tym artykule skupimy się na najprostszym i najczęściej używanym sposobie.

## Jak to zrobić

Aby uzyskać bieżącą datę w Clojure, wystarczy użyć funkcji `now` z biblioteki `clojure.java-time`. Przed wykonaniem kodu należy zaimportować bibliotekę za pomocą `require`:

```clojure
(require '[clojure.java-time :as time])

```

Teraz możemy wywołać funkcję `now`, aby uzyskać obiekt daty i godziny w bieżącej strefie czasowej:

```clojure
(def current-date (time/now))
```

Możemy również użyć funkcji `now` z argumentem, aby uzyskać obiekt `java.time.Instant`, który reprezentuje bieżący czas w GMT:

```clojure
(def current-time (time/now :instant))
```

Aby uzyskać bieżącą datę w postaci napisu w wybranym formacie, możemy użyć funkcji `now-formatted`:

```clojure
(def formatted-date (time/now-formatted "dd-MM-yyyy"))
```

## Deep Dive

Biblioteka `clojure.java-time` jest oparta na bibliotece Java Time API, więc posiada wiele funkcji i metod, które mogą być przydatne w manipulowaniu datami i czasem. Możemy między innymi sprawdzić różnicę między dwoma datami za pomocą funkcji `between`:

```clojure
(time/between (time/now) (time/now (time/timezone "GMT")) :hours)
```

Możemy również wykorzystać dostępne metody do formatowania daty, np. `DateTimeFormatter`, aby uzyskać bardziej precyzyjne wyświetlanie danych.

## Zobacz również

- [Oficjalna dokumentacja biblioteki `clojure.java-time`](https://clojure.github.io/java-time/)
- [Przewodnik po Java Time API](http://tutorials.jenkov.com/java-date-time/index.html)