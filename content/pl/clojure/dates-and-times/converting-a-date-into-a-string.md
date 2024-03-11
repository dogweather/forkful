---
date: 2024-01-20 17:36:04.984214-07:00
description: "Konwersja daty do postaci \u0142a\u0144cucha znak\xF3w umo\u017Cliwia\
  \ jej zapis i wy\u015Bwietlanie w ludzko zrozumia\u0142ym formacie. Programi\u015B\
  ci wykonuj\u0105 t\u0119 operacj\u0119, by \u0142atwiej\u2026"
lastmod: '2024-03-11T00:14:08.180460-06:00'
model: gpt-4-1106-preview
summary: "Konwersja daty do postaci \u0142a\u0144cucha znak\xF3w umo\u017Cliwia jej\
  \ zapis i wy\u015Bwietlanie w ludzko zrozumia\u0142ym formacie. Programi\u015Bci\
  \ wykonuj\u0105 t\u0119 operacj\u0119, by \u0142atwiej\u2026"
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
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
