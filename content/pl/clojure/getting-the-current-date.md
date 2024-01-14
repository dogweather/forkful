---
title:                "Clojure: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Obecna data jest często potrzebna w programowaniu, na przykład do tworzenia harmonogramów, generowania raportów lub obsługi wydarzeń cyklicznych w aplikacjach. Dlatego warto nauczyć się w jaki sposób uzyskać aktualną datę w języku Clojure.

## Jak to zrobić

```Clojure
(import 'java.util.Date)

(def current-date (Date.))
(println current-date)
```

W powyższym przykładzie najpierw importujemy klasę `Date` z pakietu `java.util`, a następnie tworzymy zmienną `current-date` zawierającą obiekt daty z bieżącym czasem. Wypisujemy go na ekranie za pomocą funkcji `println`.

Możemy też pobrać obecną datę w postaci liczby milisekund od 1 stycznia 1970 roku (tzw. epoki Unix).

```Clojure
(def current-time (.getTime (Date.)))
(println current-time)
```

Wynikiem działania powyższych przykładów może być na przykład `#inst "2020-04-23T15:19:02.176+00:00"` lub `1587646742176`.

## Głębsza analiza

Data w języku Clojure jest reprezentowana przez typ `Instant`, który jest częścią biblioteki Java zwaną `java.time`. Warto zwrócić uwagę, że jest ona niemutowalna, co oznacza, że każda operacja na dacie zwraca nowy obiekt, a nie zmienia wartość istniejącego.

Jeśli chcemy dowiedzieć się więcej o operacjach na datach w Clojure, warto przeczytać dokumentację do biblioteki `java.time` oraz wypróbować dostępne funkcje w praktyce.

## Zobacz też

- Dokumentacja do biblioteki `java.time` (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Poradnik do języka Clojure (https://clojure.org/guides/getting_started)
- Kurs programowania w Clojure (https://www.udemy.com/course/clojure-dla-poczatkujacych/)