---
title:                "Clojure: Porównywanie dwóch dat."
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównanie dwóch dat jest niezwykle ważne w programowaniu i może mieć wiele zastosowań. Może pomóc w sortowaniu danych, filtrowaniu wyników lub weryfikacji daty ważności. W tym wpisie dowiesz się, jak w języku Clojure porównać dwie daty i jak można użyć tej funkcji w swoim kodzie.

## Jak to zrobić

Porównywanie dat w Clojure jest dość proste dzięki bogatej bibliotece funkcji obsługujących operacje na datach. Aby porównać dwie daty, możemy użyć funkcji `before?`, `after?` lub `equal?`, w zależności od naszych potrzeb.

```Clojure
(def first-date (java.util.Date. 2021 5 15))  ; ustawiamy pierwszą datę
(def second-date (java.util.Date. 2021 8 20)) ; ustawiamy drugą datę

(before? first-date second-date) ; sprawdzamy czy pierwsza data jest przed drugą
; => true

(after? first-date second-date) ; sprawdzamy czy pierwsza data jest po drugiej
; => false

(equal? first-date second-date) ; sprawdzamy czy pierwsza data jest taka sama jak druga
; => false
```

W powyższym przykładzie, używając funkcji `before?` i `after?` możemy sprawdzić, która data jest wcześniejsza lub późniejsza. Natomiast używając `equal?` porównujemy dokładne wartości dat, niezależnie od czasu.

## Głębsza analiza

Podczas porównywania dat w Clojure ważne jest, aby używać typu `java.util.Date` zamiast `java.sql.Date`. Typ `java.sql.Date` ma ograniczenia związane z czasową precyzją i może powodować błędy w porównaniach. Aby uniknąć tych błędów, możemy użyć funkcji `clj-time.core/from-sql-date`, która zamienia `java.sql.Date` na `java.util.Date`.

Ponadto, w przypadku porównywania dat, ważne jest również uwzględnienie strefy czasowej. W języku Clojure, możemy użyć biblioteki `clj-time` i jej funkcji `with-time-zone` do ustawienia właściwej strefy czasowej podczas porównywania dat.

## Zobacz także

- Dokumentacja Clojure: https://clojure.org/
- Biblioteka clj-time: https://github.com/clj-time/clj-time
- Przewodnik po podstawach Clojure: https://purelyfunctional.tv/guide/clojure-syntax/