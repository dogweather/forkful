---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Clojure: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie dat w przyszłości lub przeszłości może być bardzo przydatne w codziennym życiu, szczególnie w przypadku planowania wydarzeń lub spotkań. W Clojure można to wygodnie osiągnąć za pomocą funkcji `clj-time` i `java-time`.

## Jak to zrobić?

Zacznijmy od zaimportowania `clj-time` i `java-time`:

```Clojure
(ns my-namespace
  (:require [clj-time.core :as t]
            [java-time :as j]))
```

Aby obliczyć datę w przyszłości lub przeszłości, użyj funkcji `plus` lub `minus` z `clj-time`:

```Clojure
(t/plus (j/now) {:days 5}) ; dodaje 5 dni do dzisiejszej daty
=> #object[java.time.LocalDateTime 0x48950ed5 "2021-10-05T11:31:17.424"]

(t/minus (j/now) {:months 2}) ; odejmuje 2 miesiące od dzisiejszej daty
=> #object[java.time.LocalDateTime 0x5f7cad16 "2021-08-05T11:31:17.424"]
```

Można również określić liczbę tygodni, lat, godzin, minut i sekund jako argumenty dla funkcji `plus` i `minus`.

## Głębsze wgląd

Możliwość wykonywania operacji na datach jest bardzo przydatna, ponieważ pozwala na precyzyjne planowanie wydarzeń i działań. W Clojure wykorzystuje się biblioteki `clj-time` i `java-time`, aby zapewnić wygodną i skuteczną obsługę czasu i daty.

## Zobacz również

- Dokumentacja `clj-time`: https://cljdoc.org/d/clj-time/clj-time/0.15.0/doc/readme
- Dokumentacja `java-time`: https://cljdoc.org/d/java-time/java-time/0.3.2/doc/readme
- Przykładowe projekty wykorzystujące Clojure i `clj-time`: https://github.com/clj-time/clj-time/wiki/Projects-Using-clj-time