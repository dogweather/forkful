---
date: 2024-01-20 17:32:30.043659-07:00
description: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra data jest wcze\u015B\
  niejsza, a kt\xF3ra p\xF3\u017Aniejsza lub czy s\u0105 identyczne. W programowaniu\
  \ robimy to, aby zarz\u0105dza\u0107\u2026"
lastmod: '2024-03-13T22:44:35.009408-06:00'
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra data jest wcze\u015B\
  niejsza, a kt\xF3ra p\xF3\u017Aniejsza lub czy s\u0105 identyczne."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## Jak to zrobić:
```Clojure
;; Zaimportuj moduły
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as coerce])

;; Stwórz dwie daty do porównania
(def date1 (t/date-time 2021 3 10))  ; 2021-03-10
(def date2 (t/date-time 2023 1 15))  ; 2023-01-15

;; Porównanie dat
(println (t/before? date1 date2))  ; Wypisze: true
(println (t/after? date1 date2))   ; Wypisze: false
(println (t/equal? date1 date1))   ; Wypisze: true
```
Wyjście:
```
true
false
true
```

## Wgłębienie się
Porównywanie dat w Clojure często wykonuje się z użyciem biblioteki `clj-time`, która bazuje na Joda-Time - bibliotece dla Javy do zarządzania czasem przed wprowadzeniem `java.time` w Java 8. Mimo, że `java.time` jest teraz standardem, `clj-time` jest nadal używane w wielu projektach Clojure ze względu na swoją wygodę i prostotę. Istnieje także możliwość używania natywnych klas Javy (np. `java.util.Date` i `java.time`, zdefiniowanych w Java 8), ale `clj-time` oferuje przyjaźniejszy interfejs i lepszą integrację z Clojure.

## Zobacz też
- Dokumentacja `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Dokumentacja `java.time`: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Poradnik Joda-Time w Javie: [http://www.joda.org/joda-time/](http://www.joda.org/joda-time/)
