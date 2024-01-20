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

## Co i dlaczego?

Policzmy datę przyszłą lub przeszłą. Programiści robią to, aby zrozumieć sekwencje wydarzeń i czas, który mają na ich wykonanie.

## Jak to zrobić:

Clojure oferuje funkcje do manipulowania danymi w pakiecie java.util.Date. Oto prosty przykład:
```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

(defn add-days [date days]
  (-> date
      c/to-local-date
      (t/plus (t/days days))
      c/from-local-date))
(add-days (new java.util.Date) 10)
```
Ten kod doda 10 dni do dzisiejszej daty. 

## Dogłębne zgłębianie tematu

Historia jest pełna przykładów kiedy obliczanie daty przyszłej lub przeszłej było kluczowe. I choć możemy to robić bezpośrednio w Java, Clojure oferuje wygodniejsze narzędzia. Alternatywnie, moglibyśmy użyć biblioteki Java 8 java.time, ale clj-time jest bardziej 'clojurystyczny' i łatwiejszy do użycia. Głównym pytaniem jest, jak wiele dni chcemy dodać lub odjąć i do jakiego typu daty chcemy to zastosować.

## Zobacz także

1. Dokumentacja clj-time: https://github.com/clj-time/clj-time
2. Dokumentacja Clojure: https://clojure.org/guides/getting_started
3. Dokumentacja java.util.Date: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
4. Dokumentacja java.time: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html