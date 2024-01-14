---
title:    "Clojure: Konwersja daty na ciąg znaków"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest ważną umiejętnością, której każdy programista Clojure powinien się nauczyć. Jest to szczególnie użyteczne w aplikacjach, które wykorzystują porównywanie lub sortowanie dat.

## Jak to zrobić

```Clojure
; Import biblioteki java.time
(ns programowanie-clojure.core
  (:import (java.time LocalDate)))

; Ustawienie daty
(def data (LocalDate/of 2020 5 12))

; Konwersja na ciąg znaków
(def data-tekst (str data))

; Wyświetlenie wyniku
(println data-tekst)

; Output: "2020-05-12"
```

## Głębszy szlif

W przypadku, gdy chcemy uzyskać inny format daty, możemy użyć funkcji `format` z biblioteki `java.time.format`. Możemy podać szablon, który określi, jak chcemy, aby data została sformatowana. Przykładowy kod poniżej pokazuje, jak uzyskać datę w formacie "dzień-miesiąc-rok".

```Clojure
; Ustawienie daty
(def data (LocalDate/of 2020 5 12))

; Używanie szablonu
(def data-tekst (format data "dd-MM-yyyy"))

; Wyświetlenie wyniku
(println data-tekst)

; Output: "12-05-2020"
```

## Zobacz także

- Oficjalna dokumentacja Clojure do operacji na datach: https://clojure.org/reference/java_interop#_java_time
- Przewodnik po konwersji daty na ciąg znaków w Clojure: https://clojure.org/guides/daytime_dates