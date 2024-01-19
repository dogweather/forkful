---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat to proces, w którym sprawdzamy, czy jedna data jest wcześniejsza, późniejsza lub taka sama jak druga. Programiści robią to, aby zarządzać i manipulować danymi związanymi z czasem w swoich aplikacjach.

## Jak to zrobić:

Porównajmy dwie daty w Clojure. Użyjemy funkcji built-in.

```clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c])

(def date1 (c/to-local-date "2022-01-01"))
(def date2 (c/to-local-date "2022-12-31"))

(t/after? date2 date1)
```
Output jest: `true`. Oznacza to, że `date2` jest późniejsza niż `date1`.

## Głębsze Zanurzenie

(1) **Kontekst historyczny**: Clojure, dynamiczny język programowania funkcyjnego, nie miał na początku wbudowanej funkcji porównywania dat. Porównywanie dat było zatem trudniejsze i bardziej skomplikowane. Od czasu wprowadzenia biblioteki clj-time, porównywanie dat stało się znacznie prostsze.

(2) **Alternatywy**: Choć clj-time jest najpopularniejszą biblioteką do manipulowania i porównywania dat, istnieją również inne opcje takie jak `java.util.Date` i `java.util.Calendar` z Javy, które można wykorzystać w Clojure.

(3) **Szczegóły implementacji**: Funkcje `after?` i `before?` z clj-time porównują daty, sprawdzając, która data jest wcześniejsza lub późniejsza. Istnieje również funkcja `equal?`, która sprawdza, czy dwie daty są identyczne.

## Zobacz również

1. Oficjalna dokumentacja Clojure: [Link](https://clojure.org/)
2. Biblioteka clj-time: [Link](https://github.com/clj-time/clj-time)
3. Porównywanie dat na StackOverflow: [Link](https://stackoverflow.com/questions/39144997/clojure-date-comparison)
4. Documentacja java.util.Date: [Link](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html) and java.util.Calendar: [Link](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)