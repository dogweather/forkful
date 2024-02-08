---
title:                "Konwersja daty na łańcuch znaków"
aliases:
- pl/clojure/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:04.984214-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konwersja daty do postaci łańcucha znaków umożliwia jej zapis i wyświetlanie w ludzko zrozumiałym formacie. Programiści wykonują tę operację, by łatwiej manipulować datami w interfejsach użytkownika, logach czy przy zapisie danych.

## Jak to zrobić:
```Clojure
(require '[java-time :as jt])

;; Przykład konwersji obecnej daty i czasu na łańcuch znaków
(defn get-current-date-string []
  (-> (jt/local-date-time)
      (jt/format "yyyy-MM-dd HH:mm:ss")))

;; Wywołanie funkcji i przykład wyniku
(println (get-current-date-string))
;; "2023-04-02 15:30:45"
```

## Wgłębienie się
W Clojure konwersja daty do postaci łańcucha znaków nie jest skomplikowana, ale wymaga zrozumienia biblioteki `java-time`. Ta biblioteka jest opakowaniem dla `java.time`, nowoczesnego API Javy do obsługi czasu, które zastąpiło przestarzałe klasy `java.util.Date` i `java.text.SimpleDateFormat`.

Inne metody:
- Stare API: Używając `java.util.Date` i `java.text.SimpleDateFormat`.
- String format: Można kontrolować format przy pomocy różnych wzorców w funkcji `jt/format`.
  
Szczegóły implementacji:
- `java-time` pomaga uniknąć błędów związanych ze strefami czasowymi.
- API pozwala na operacje takie jak dodawanie dni, miesięcy itp.

## Zobacz też
- [Oracle java.time package documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
