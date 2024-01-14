---
title:                "Clojure: Uzyskiwanie bieżącej daty"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Dlaczego

Pobranie aktualnej daty jest jedną z podstawowych funkcjonalności, której potrzebujemy w wielu programach. Może to być związane z logowaniem, zaplanowanymi zadaniami lub po prostu wyświetlaniem aktualnej daty w naszej aplikacji. Dlatego warto poznać sposoby na jej pobranie w języku Clojure.

##Jak to zrobić

Poniżej przedstawiamy dwa sposoby na pobranie aktualnej daty w Clojure:

```Clojure
(require '[java.time :as t])
(t/local-date) ;;zwraca aktualną datę w formacie YYYY-MM-DD
```

```Clojure
(require '[clojure.java-time :as t])
(t/date-time) ;;zwraca aktualną datę i czas w formacie YYYY-MM-DD HH:MM:SSZ
```

Oba przykłady wykorzystują bibliotekę Java Time, co daje nam dostęp do wielu funkcji do pracy z datami.

Przykładowy output:

```
2021-01-13
2021-01-13T18:20:05.687811Z
```

##Głębsze zagadnienia

Poza podstawowymi funkcjami, warto poznać kilka dodatkowych informacji dotyczących pobierania daty w Clojure. 

1. Wykorzystanie strefy czasowej
Jeśli potrzebujemy aktualnej daty dla konkretnej strefy czasowej, możemy wykorzystać funkcję ```t/zoned-date-time```, która pobiera datę w wybranym formacie oraz strefie czasowej:

```Clojure
(t/zoned-date-time (t/ZoneId/of "Europe/Warsaw"))
```

2. Formatowanie daty
Aby zmienić format, w jakim otrzymujemy datę, możemy wykorzystać funkcję ```t/format```, np:

```Clojure
(t/format (t/local-date) "d MMMM yyyy") ;;zwraca np. 13 stycznia 2021
```

##Zobacz także

- Dokumentacja biblioteki Java Time: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html
- Inne sposoby na pobieranie aktualnej daty w Clojure: https://stackoverflow.com/questions/33158716/clojure-date-time-how-do-i-call-this-stuff-whats-the-equivalent-of-getlocalda
- Szybki przewodnik po języku Clojure: https://clojure.org/guides/getting_started