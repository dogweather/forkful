---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:37.066335-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
W programowaniu pozyskiwanie bieżącej daty to chwytanie momentu "teraz" w cyfrowych ramionach. Używamy tego do logowania, znakowania czasowego transakcji lub ustawiania limitów czasowych – w sumie, to podstawy.

## How to:
```Clojure
;; Podłączamy bibliotekę czasową
(require '[java-time :as jt])

;; Pobieranie aktualnej daty
(defn get-current-date []
  (jt/local-date))

;; Użycie funkcji i wydruk wyniku
(println (get-current-date))
```
Oczekiwany wynik wygląda mniej więcej tak:
```
2023-04-07
```

## Deep Dive
W Clojure data aktualna nie jest trudną sprawą, ale drzemie tu historia. `java-time` to wygodny wrapper do Java Time API znanego z Java 8. Alternatywa to starsze `clj-time`, które bazowało na Joda-Time. Detale? Clojure jest hostowany na JVM, więc wykorzystuje narzędzia Javy do obsługi czasu.

## See Also
- Porównanie `java-time` i `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Oficjalny przewodnik Java Time API: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
