---
title:    "Clojure: Obliczanie daty w przyszłości lub przeszłości."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego?

Czasami, podczas pisania programów, musimy manipulować datami, włączając w to daty w przeszłości lub przyszłości. Może być to przydatne, gdy budujemy aplikacje, które wymagają wyświetlania przyszłych lub przeszłych wydarzeń lub planowania działań na przyszłość.

## Jak to zrobić

Nie martw się, bo w Clojure istnieją gotowe funkcje do obliczania dat w przyszłości lub przeszłości. Oto kilka przykładów:

```Clojure
(require '[clojure.java-time :as t])

(t/plus (t/local-now) (t/period 0 1 0 0)) ; dodaje 1 rok do aktualnej daty
; => #object[java.time.LocalDateTime 0x57d8ebaa "2022-06-25T10:55:08.254"]

(t/plus (t/local-now) (t/period 0 0 2 0)) ; dodaje 2 miesiące do aktualnej daty
; => #object[java.time.LocalDateTime 0xc695f3 "2021-09-25T10:55:08.262"]

(t/plus (t/local-now) (t/period 0 0 0 7)) ; dodaje 7 dni do aktualnej daty
; => #object[java.time.LocalDateTime 0x4fa47542 "2021-07-02T10:55:08.268"]
```

Możemy również wykorzystać funkcję `t/minus` aby odjąć określoną liczbę lat, miesięcy lub dni. Wszystkie wyżej wymienione funkcje działają na obiekcie `java.time.LocalDateTime` i zwracają taki sam obiekt jako wynik.

## Głębsze zanurzenie

W Clojure możemy również wykorzystać bibliotekę `clj-time` do manipulowania datami. Umożliwia ona wykonanie bardziej skomplikowanych operacji, takich jak dodanie lub odjęcie określonej liczby godzin, minut lub sekund.

Oto przykładowe wykorzystanie tej biblioteki:

```Clojure
(require '[clj-time.core :as t])

(t/plus (t/today) (t/days 10)) ; dodaje 10 dni do dzisiejszej daty
; => #inst "2021-07-04T00:00:00.000-00:00"

(t/plus (t/now) (t/hours 5)) ; dodaje 5 godzin do aktualnego czasu
; => #inst "2021-06-25T15:59:41.269-00:00"
```

Biblioteka `clj-time` również oferuje możliwość formatowania dat i czasów oraz wykonywania innych operacji z nimi.

## Zobacz również

- Dokumentacja funkcji `plus` i `minus` z biblioteki `clojure.java-time`: https://github.com/dm3/clojure.java-time#plusminus
- Dokumentacja funkcji `plus` i `minus` z biblioteki `clj-time`: https://github.com/clj-time/clj-time#functions